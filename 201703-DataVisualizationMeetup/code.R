# ws & packages -----------------------------------------------------------
rm(list = ls())

# general
library(tidyverse)
library(stringr)
library(lubridate)

# cors
library(widyr) # devtools::install_github("dgrtwo/widyr")
library(igraph)

# autoencoder
library(h2o)
localH2O <- h2o.init(nthreads = -2)

# viz
library(highcharter)
library(viridis)
library(hrbrthemes)
library(jbkmisc)
theme_set(theme_jbk())
options(highcharter.theme = hc_theme_elementary())

jbkmisc::wf_create_folders()

# downloadload data -------------------------------------------------------
#' http://www.dtpm.gob.cl/index.php/2013-04-29-20-33-57/matrices-de-viaje
urlfile <- "http://www.dtpm.gob.cl/descargas/archivos/2015.04_Subidas_paradero_mediahora_web.rar"
rarfile <- file.path("data", basename(urlfile))

if(!file.exists(rarfile)) {
  download.file(urlfile, file.path("data", basename(urlfile)), mode = "wb")
}

data <- read_csv2("data/2015.04_Subidas_paradero_mediahora_web/2015.04_Subidas_paradero_mediahora_web.csv")

# data cleaning -----------------------------------------------------------
data <- data %>% 
  mutate(subidas_laboral_promedio = as.numeric(subidas_laboral_promedio)) %>% 
  filter(!str_detect(paraderosubida, "^(T|L|I|E)?-")) %>% 
  mutate(paraderosubida = str_to_title(paraderosubida),
         mediahora = 1000*mediahora)

count(count(data, paraderosubida), n)

data <- mutate(data, mediahora = as.numeric(mediahora))

data <- complete(data, paraderosubida, mediahora,
                 fill = list(subidas_laboral_promedio = 0)) 

data <- filter(data, mediahora != 0)

glimpse(data)
saveRDS(data, "data/data_subidas_metro.rds")

# explore -----------------------------------------------------------------
d1 <- filter(data, paraderosubida %in% c("Plaza Maipu", "Laguna Sur")) 
hw_grid(
  d1 %>% 
    hchart("line", hcaes(mediahora, subidas_laboral_promedio, group = paraderosubida)) %>% 
    hc_xAxis(type = "datetime") %>% 
    hc_tooltip(sort = TRUE, table = TRUE, xDateFormat = "%H:%S"),
  d1 %>% 
    spread(paraderosubida, subidas_laboral_promedio) %>% 
    hchart("point", hcaes(`Laguna Sur`, `Plaza Maipu`, size = mediahora), maxSize = "5%") %>% 
    hc_xAxis(min = 0) %>% 
    hc_yAxis(min = 0)
) %>% htmltools::browsable()

d2 <- filter(data, paraderosubida %in% c("Universidad De Chile", "Plaza De Puente Alto"))
hw_grid(
  d2 %>% 
    hchart("line", hcaes(mediahora, subidas_laboral_promedio, group = paraderosubida)) %>% 
    hc_xAxis(type = "datetime") %>% 
    hc_tooltip(sort = TRUE, table = TRUE, xDateFormat = "%H:%S"),
  d2 %>% 
    spread(paraderosubida, subidas_laboral_promedio) %>% 
    hchart("point", hcaes(`Plaza De Puente Alto`, `Universidad De Chile`, size = mediahora), maxSize = "5%") %>% 
    hc_xAxis(min = 0) %>% 
    hc_yAxis(min = 0)
) %>% htmltools::browsable()

rm(d1, d2)

# correlations ------------------------------------------------------------
dcor <- data %>%
  pairwise_cor(paraderosubida, mediahora, subidas_laboral_promedio,
               upper = FALSE) %>% 
  arrange(desc(correlation))

dcor

saveRDS(dcor, "data/data_subidas_metro_cor.rds")

# network -----------------------------------------------------------------
dcorf <- dcor %>%
  arrange(desc(correlation)) %>%
  filter(row_number() <= 400)

g <- graph_from_data_frame(dcorf, directed = FALSE)
hchart(g)
g

E(g)$weight <- dcorf$correlation^2

wc <- cluster_fast_greedy(g)
nc <- length(unique(membership(wc)))

dvert <- data_frame(
  paraderosubida = V(g)$name
  ) %>% 
  mutate(
    comm = membership(wc)
  ) %>% 
  left_join(
    data %>%
      group_by(paraderosubida) %>%
      summarise(n = sum(subidas_laboral_promedio))) %>% 
  left_join(
    data %>%
      group_by(paraderosubida) %>% 
      summarise(tend = cor(seq(1, 37), subidas_laboral_promedio))) %>% 
  ungroup()

dvert
count(dvert, paraderosubida)

# g <- graph_from_data_frame(dcor1, directed = FALSE, vertices = dvert) 
# 
# wc <- cluster_edge_betweenness (g)
# nc <- length(unique(membership(wc)))

V(g)$label <- dvert$paraderosubida
V(g)$size <- dvert$n
V(g)$subidas_totales_miles <- round(dvert$n/1000, 2)
V(g)$Comunidad <- membership(wc)
V(g)$tendencia <- round(dvert$tend, 2)

# V(g)$color <- colorize(dvert$tend)
V(g)$color <- colorize(dvert$comm)

saveRDS(g, "data/data_g.rds")


set.seed(1)
hchart(g) %>% 
  hc_add_theme(
    hc_theme_elementary(
      yAxis = list(visible = FALSE),
      xAxis = list(visible = FALSE)
    )
  )


# autoencoder -------------------------------------------------------------
data2 <- data %>% 
  mutate(mediahora = paste0("m", mediahora/1e3)) %>% 
  ungroup() %>% 
  spread(mediahora, subidas_laboral_promedio)
data2

saveRDS(data2, "data/data_subidas_metro_autoencoder.rds")

dh2o <- as.h2o(data2)

mod_autoenc <- h2o.deeplearning(
  x = names(dh2o)[-1],
  training_frame = dh2o,
  hidden = c(400, 200, 2, 200, 400),
  epochs = 50,
  activation = "Tanh",
  autoencoder = TRUE
)

dautoenc <- h2o.deepfeatures(mod_autoenc, dh2o, layer = 3) %>% 
  as.data.frame() %>% 
  tbl_df() %>% 
  setNames(c("x", "y")) %>% 
  mutate(paraderosubida = data2$paraderosubida)


saveRDS(dautoenc, "data/data_subidas_metro_autoencoder_output.rds")

hchart(dautoenc, "point", hcaes(x = x, y = y, name = paraderosubida),
       dataLabels = list(enabled = TRUE, format =  "{point.name}"))
  
dkmod <- map_df(1:15, function(k){
  mod.km <- h2o.kmeans(training_frame = as.h2o(dautoenc), k = k, x = c("x", "y"))  
  mod.km@model$model_summary
})

dkmod <- dkmod %>%
  mutate(wc_ss = within_cluster_sum_of_squares/total_sum_of_squares,
         bt_ss = between_cluster_sum_of_squares/total_sum_of_squares)

hchart(dkmod, "line", hcaes(number_of_clusters, wc_ss), name = "WC SS")

mod_km <- h2o.kmeans(training_frame = as.h2o(dautoenc), k = 4, x = c("x", "y"))  

dautoenc <- dautoenc %>% 
  mutate(group = as.vector(h2o.predict(object = mod_km, newdata = as.h2o(.))),
         group = as.numeric(group) + 1,
         group = paste("group", group))

saveRDS(dautoenc, "data/data_subidas_metro_autoencoder_output_km.rds")

hchart(dautoenc, "point", hcaes(x, y, group = group, name = paraderosubida)) %>% 
  hc_colors(hex_to_rgba(viridis(4)))

data <- left_join(data, select(dautoenc, paraderosubida, group))

data <- data %>% 
  mutate(mh = as.POSIXct(mediahora/1000, origin="1970-01-01"),
         mh = mh + hours(2) + minutes(30))

saveRDS(data, "data/data_subidas_metro_km.rds")

ggplot(data, aes(mh, subidas_laboral_promedio)) + 
  geom_line(aes(group = paraderosubida), alpha = 0.1) + 
  geom_smooth(aes(group = group, color = group), line = 1.2, se = FALSE) + 
  scale_color_viridis(discrete = TRUE) +
  scale_y_comma() + 
  scale_x_datetime(date_labels = "%H:%M") + 
  facet_wrap(~group, scales = "free_x")  +
  theme(legend.position = "bottom") + 
  labs(
    x = "Hora", y = "Subida promedio",
    caption = "jkunst.com"
  )
ggsave("trendgroup.png", width = 16, height = 9)
