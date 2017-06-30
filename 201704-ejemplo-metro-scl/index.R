#' ---
#' title: "Ejemplo Metro SantiagoCL"
#' author: "Joshua Kunst"
#' output:
#'  html_document:
#'    theme: yeti
#'    toc: true
#'    toc_float: true
#' ---
#' 
#+echo=FALSE
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
ggplot2::theme_set(jbkmisc::theme_jbk())
#' ## Introducción
#' 
#' Ejercicio de manipulación de datos y visualización de datos 
#' de subidas de paraderos del metro de Santiago.
#' 
#' Se utilizará:
#' 
#' - `readr` para la lectura de datos.
#' - `dplyr` y `tidy` para manipulación de información.
#' - `ggplot2` para la visualización.
#' - `stringr` para tratamiento de strings.#' 
#' 
#' ## Cargando paquetes
#' 
library(readr)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(stringr)
library(tidyr)
library(purrr)

#'
#' ## Estudiando las dubidad de 
#' 
#' Los datos de _matrices de viaje son obtenidos de Directorio de Transporte
#' Público: http://www.dtpm.gob.cl/index.php/2013-04-29-20-33-57/matrices-de-viaje.
#' 
dfsubidas <- read_csv2("data/2015.04_Subidas_paradero_mediahora_web.csv")

#' Primera inspección;
dfsubidas

#' La tabla contiene información de subidas promedios en días laborales por 
#' cada media hora. La variable `subidas_laboral_promedio` se ha cargado como 
#' carácter en lugar de numérica. 
dfsubidas <- mutate(dfsubidas, subidas_laboral_promedio = as.numeric(subidas_laboral_promedio))
dfsubidas

#' Cuantos paraderos contiene la tabla:
count(dfsubidas, paraderosubida)

#' Contiene `r nrow(count(dfsubidas, paraderosubida))`. Filtramos solamente 
#' paradas de metro. 
#' 
set.seed(1234)
sample(dfsubidas$paraderosubida, 20)

#' Luego de explorar un poco está columna se selecciona lo que __no__ posea
#' un string que comience con T, L, I o E y un guión: 
dfsubidas <- filter(dfsubidas, !str_detect(paraderosubida, "^(T|L|I|E)?-")) 
count(dfsubidas, paraderosubida)

#' Y luego del filtros nos quedamos con 106 estaciones (de metro supuestamente).
#' Sin embargo vemos que aparece _Baquedano_ dos veces dado que es una estación
#' de combinación. En esta oportunidad la consideraremos como una por lo que 
#' al nomnre de la estación removeremos la parte que hace distición a que línea 
#' es `" L\\d"` donde `"\\d"` es algún número.
#' 
#' Entonces removemos, agrupamos para sumar los casos de combinaciones:
#' 
dfsubidas <- dfsubidas %>% 
  mutate(paraderosubida = str_replace(paraderosubida, " L\\d$", "")) %>% 
  group_by(paraderosubida, mediahora) %>% 
  summarise(subidas_laboral_promedio = sum(subidas_laboral_promedio)) %>% 
  ungroup()

count(dfsubidas, paraderosubida)

#' ;)!.
#' 
#' Ahora contemos si todas las estaciones contiene la misma cantidad de 
#' registros
dfsubidas %>% 
  count(paraderosubida) %>% 
  count(n)

#' Existe una estación que contiene 38 registros y otras 38 estaciones
#' que contiene 36 regitros.
#' 
dfsubidas %>% 
  count(mediahora)

dfsubidas %>% 
  count(mediahora) %>% 
  filter(n != 101)

# Eliminaremos las 12AM.
dfsubidas <- filter(dfsubidas, mediahora != 0)

# dfsubidas <- complete(dfsubidas, paraderosubida, mediahora,
                 # fill = list(subidas_laboral_promedio = 0)) 

#' Ahora visualizaremos los registros por líneas y veremos la tencendia
#' a través de un suavizamientos: 
gg <- ggplot(dfsubidas) + 
  geom_line(aes(mediahora, subidas_laboral_promedio,
                group = paraderosubida), alpha = 0.25) +
  geom_smooth(aes(mediahora, subidas_laboral_promedio), size = 1.3) + 
  scale_x_time() +
  scale_y_comma()
gg
#' Se observa lo esperado: mucha gente ingresa a las 8 y 18.30 app además
#' de un leve peak a las 13.00 horas. 
#' 
#' Por diversión agregaremos las horas puntas:
library(lubridate)

dfhorarios <- data_frame(
  xmin = c(hms("7:00:00"), hms("18:00:00")),
  xmax = c(hms("8:59:59"), hms("19:59:69")),
  ymin = c(0, 0),
  ymax = rep(7000, 2),
  g = c(1, 2)
)

gg <- gg +
  geom_rect(data = dfhorarios,
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax), alpha = 0.2)

gg
#'
#'  ## Agrupando Estaciones 
#' 
#' Vemos que algunas estaciones 


dfsubidas2 <- spread(dfsubidas, mediahora, subidas_laboral_promedio)
dfsubidas2

dfsubidas3 <- select(dfsubidas2, -1) %>% 
  mutate_all(function(x) ifelse(is.na(x), 0, x)) %>% 
  mutate_all(scale)

kmeans <- map_df(1:10, function(k){ # k <- 6
  set.seed(123)
  kmod <- kmeans(dfsubidas3, centers = k)
  data_frame(k = k, wcss = 1 - kmod$betweenss/kmod$totss, kmod = list(kmod))
})

ggplot(kmeans) +
  geom_line(aes(k, wcss)) + 
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(breaks = seq(1:10), minor_breaks = NULL)

K <- 4

dfsubidas2 <- mutate(dfsubidas2, grupo = kmeans$kmod[[K]]$cluster)

dfsubidas <- left_join(dfsubidas, select(dfsubidas2, paraderosubida, grupo))
dfsubidas

ggplot(dfsubidas, aes(mediahora, subidas_laboral_promedio)) + 
  geom_line(aes(group = paraderosubida), alpha = 0.15) +
  geom_smooth(aes(group = grupo, color = factor(grupo))) + 
  scale_color_viridis(discrete = TRUE) + 
  scale_x_time() +
  facet_wrap(~grupo) +
  theme(legend.position = "none")

#' ## Geográficamente
#' 
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
  x %>% 
    str_trim() %>%
    str_to_lower() %>%
    str_replace_all("\\s+", "_") %>%
    str_replace_all("\\s+de\\s+", "") %>% 
    str_replace_all("á", "a") %>%
    str_replace_all("é", "e") %>% 
    str_replace_all("í", "i") %>% 
    str_replace_all("ó", "o") %>% 
    str_replace_all("ú", "u") %>% 
    str_replace_all("ñ", "n") %>% 
    str_replace_all("`", "") %>% 
    str_replace_all("_de_", "_") %>% 
    str_replace_all("rondizonni", "rondizzoni")
}

dfsubidas2 <- mutate(dfsubidas2, id = str_to_id2(paraderosubida))
 
stops_metro_data <- stops_metro %>% 
  mutate(id = str_to_id2(stop_name)) %>% 
  left_join(select(dfsubidas2, grupo, id)) 

ggplot()+
  geom_path(data = shapes_metro,
            aes(x = shape_pt_lon, y = shape_pt_lat, group = shape_id, color = shape_id),
            size = 1.2) + 
  
  geom_point(data = stops_metro_data,
             aes(x = stop_lon, y = stop_lat, color = factor(grupo)),
             size = 2, alpha = 0.95) +
  
  scale_color_manual(values = c(colors_metro$route_color, viridis(K))) +
  # scale_color_viridis(discrete = TRUE) +
  facet_wrap(~grupo) +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) + 
  coord_fixed() +
  theme_minimal() + 
  theme(legend.position = "left")
