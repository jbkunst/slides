# init --------------------------------------------------------------------
#+include=FALSE
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
#+include=TRUE
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
options(highcharter.theme = hc_theme_smpl(chart = list(backgroundColor = "transparent")))

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

data <- data %>% 
  mutate(paraderosubida = str_replace(paraderosubida, " L\\d$", "")) %>% 
  group_by(paraderosubida, mediahora) %>% 
  summarise(subidas_laboral_promedio = sum(subidas_laboral_promedio)) %>% 
  ungroup()
  
count(count(data, paraderosubida), n)

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
  filter(row_number() <= 250)

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
str(dh2o)

mod_autoenc <- h2o.deeplearning(
  x = names(dh2o)[-1],
  training_frame = dh2o,
  hidden = c(400, 100, 2, 100, 400),
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
  
dkmod <- map_df(seq(1, 20, by = 3), function(k){
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
         group = paste("grupo", group))

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


makechart <- function(d) {
  
  col <- viridis(4)[as.numeric(str_extract(unique(d$group), "\\d"))]
  col <- str_replace(col, "FF$", "")
  print(col)
  
  g <- unique(d$group)
  
  hchart(d, "line",
         hcaes(mediahora, subidas_laboral_promedio, group = paraderosubida),
         enableMouseTracking = FALSE, showInLegend = FALSE, color = hex_to_rgba("gray", 0.3)) %>% 
    hc_subtitle(text = g) %>% 
    hc_xAxis(type = "datetime", title = list(text = "")) %>% 
    hc_tooltip(sort = TRUE, table = TRUE, xDateFormat = "%H:%S") %>% 
    hc_add_series(lm(subidas_laboral_promedio ~ poly(mediahora, 5, raw = TRUE), data = d),
                  name = "ploy smooth", color = col, showInLegend = FALSE) %>% 
    hc_yAxis(min = 0, max = 5100, title = list(text = ""))

}

smoothcharts <- data %>% 
  mutate(g = str_extract(group, "\\d"), g = as.numeric(g)) %>% 
  group_by(group, g) %>% 
  do(chart = makechart(.))

chartlst <- smoothcharts$chart
  
saveRDS(chartlst, "data/data_subidas_metro_polysmooths.rds")

hw_grid(chartlst, ncol = 4) %>% htmltools::browsable()
  

# gtfs --------------------------------------------------------------------
# rm(list = ls())
# https://analyzegps.carto.com/tables/comunas_santiago/public
# stgo <- geojsonio::geojson_read("data/comunas_santiago.geojson")

# https://github.com/jlhonora/geo/blob/master/low_res/region_metropolitana_de_santiago/all.geojson
# stgo <- geojsonio::geojson_read("data/all.geojson")
# stgo <- rmapshaper::ms_simplify(stgo, keep = 0.001)

routes <- read_csv("data/routes.txt")
trips <- read_csv("data/trips.txt")
stops <- read_csv("data/stops.txt")
shapes <-read_csv("data/shapes.txt")

stops_metro <- stops %>%
  filter(!grepl("\\d", stop_id)) %>% 
  mutate(stop_url = basename(stop_url))

routes_metro <- filter(routes, grepl("^L\\d",route_id))

shapes_metro <- routes %>% 
  filter(grepl("^L\\d",route_id)) %>% 
  semi_join(trips, .) %>% 
  semi_join(shapes, .) %>% 
  ### IMPORTANTE
  filter(str_detect(shape_id, "-I")) %>% 
  mutate(shape_id2 = str_replace(shape_id, "-I", ""))

colors_metro <- distinct(shapes, shape_id) %>% 
  left_join(distinct(trips, shape_id, route_id)) %>% 
  left_join(distinct(routes, route_id, route_color)) %>% 
  semi_join(shapes_metro) %>% 
  mutate(route_color = paste0("#", route_color))
  
str_to_id2 <- function(x) {
  str_to_id(x) %>% 
    str_replace_all("á", "a") %>%
    str_replace_all("é", "e") %>% 
    str_replace_all("í", "i") %>% 
    str_replace_all("ó", "o") %>% 
    str_replace_all("ú", "u") %>% 
    str_replace_all("ñ", "n") %>% 
    str_replace_all("`", "") %>% 
    str_replace_all("_de_", "_")
}

dautoenc <- mutate(dautoenc, id = str_to_id2(paraderosubida))

data3 <- data %>% 
  group_by(paraderosubida, group) %>% 
  do(tmsr = { list_parse(select(., x = mediahora, y = subidas_laboral_promedio) ) }) %>%
  # do(sequence = list(.$subidas_laboral_promedio)) %>% 
  ungroup() %>% 
  mutate(id = str_to_id2(paraderosubida))

data4 <- data %>% 
  group_by(paraderosubida, group) %>% 
  summarise(median = median(subidas_laboral_promedio)) %>% 
  ungroup() %>% 
  mutate(id = str_to_id2(paraderosubida))
data4

stops_metro_data <- stops_metro %>% 
  mutate(id = str_to_id2(stop_name)) %>% 
  left_join(data3) %>% 
  left_join(data4) %>% 
  filter(!is.na(group))

rm(shapes, routes, stops, trips, data3, data4)

glimpse(stops_metro_data)  

count(stops_metro_data, group)

stopsmarkeropts <- list(
  enabled = TRUE,
  symbol = "circle",
  lineWidth = 1,
  radius = 4
  )

hcsw <- highchart(type = "map") %>% 
  hc_add_series(mapData = NULL, showInLegend = FALSE) %>% 
  hc_add_series(stops_metro_data, "point", hcaes(stop_lon, stop_lat, group = group),
                marker = stopsmarkeropts,
                color = hex_to_rgba(viridis(4), alpha = 0.75), minSize = "1%", maxSize = "3%",
                tooltip = list(headerFormat = "{point.name}")) %>%
  hc_add_series(shapes_metro, "line", hcaes(shape_pt_lon, shape_pt_lat, group = shape_id2),
                color = hex_to_rgba(colors_metro$route_color, 0.5),
                enableMouseTracking = FALSE, lineWidth = 6, zIndex = -4) %>% 
  hc_legend(align = "right", verticalAlign = "top", layout = "vertical") %>% 
  hc_yAxis(reversed = FALSE) %>% 
  hc_tooltip(
    useHTML = TRUE,
    positioner = JS("function () { return { x: this.chart.plotLeft + 15, y: 300 + 0 }; }"),
    headerFormat = "{point.stop_name}",
    pointFormatter = JS("
function(){
  
  var thiz = this;
  console.log(thiz);

  setTimeout(function() {
          $('#minichart').highcharts({
            title : {
              text: ''
            },
          	subtitle: {
            	text: thiz.stop_name,
              align: 'left'
            },
            exporting: {
              enabled: false
            },
            legend: {
            	enabled: false
            },
            credits: {
            	enabled: false
            },
            series: [{
            	animation: false,
              color: thiz.color,
              data: thiz.tmsr
            }],
            yAxis: {
            	title: ''
            },
            xAxis: {
            	type: 'datetime'
            }
          });
        }, 0);
        return '<div id=\"minichart\" style=\"width: 250px; height: 150px;\"></div>';
}                        
                        ")
  )

hcsw

saveRDS(hcsw, "data/data_subidas_metro_hcsw.rds")

# htmlwidgets::saveWidget(hcsw, "hcsw.html")
