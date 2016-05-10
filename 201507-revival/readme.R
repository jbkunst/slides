#' ---
#' title: "UseRChile Revival"
#' author: "Joshua"
#' output: 
#'  html_document: 
#'    theme: journal
#'    toc: true
#'    keep_md: yes
#' ---

#' <style>svg { font-family: "News Cycle","Arial Narrow Bold",sans-serif;}
#' .metricsgraphics {font-size: 2em}
#' </style>

#' # Introducción

#' Hola a todos! La idea de este documento/script/html/etc es intentar despertar al grupo, incentivar una junta,
#' conocernos *descriptivamente*, en fin, para lo que sirva. Ha estado
#' con muy poco movimiento nuestro grupo y ya es tiempo de moverrrrnos.

#' Descargué los datos desde el meetup e intenté hacer algo simple con para ir calentando motores, por lo que
#' al final de revisar este documento/script/html/etc pasará al menos un de las siguientes cosas:
#' 
#'   - El script lo encontrarás tan chanta/desordenado que querrás enseñarme alguno de tus tips.
#'   - No sabrás algunas cosas y querrás que nos juntemos con unos cuantos usuarios para ver
#'   lo que se lleva en la comunidad de R hoy en día.
#'   - No pasará ni lo uno ni lo otro pero de todas formas te nacerá un incentivo de juntarno
#'   para compartir ideas/conociminetos/organizar nuevas presentaciones. 

#' ## Sobre este script
#' 
#' El script/código está [acá](https://github.com/jbkunst/useRchile/blob/master/20150707-revival/readme.R), pero estará
#' el resultado por [aquí](https://rawgit.com/jbkunst/useRchile/master/20150707-revival/readme.html).
#' 
#' Los datos los descargué de la mismísima página de [meetup](http://www.meetup.com/es/useRchile/members/).

#+ warning=FALSE, message=FALSE, echo=FALSE
rm(list = ls())
library("dplyr") # para realizar agrupaciones
library("lubridate") # para hace agradable el trabajar con fechas
library("metricsgraphics") # una de las tantas librerías de viz de js
library("rcdimple") # otra similar a la anterior
library("broom") # simplemente para mostrar en un dataframe los coef de los modelos
library("tm") # para trabajar con las respuestas
library("d3wordcloud") # mi paquete para hacer nubes de palabras usando d3js!
library("printr") # esto se usa para que las tablas aparezcan en formato html cuando este script se transforma se 'compila a lo netbook' 

#' # Veamos que nos puede decir los datos

#' ## Lectura de Datos

#' Leer el archivo
data <- read.table("useRchile_Member_List_on_07-20-15.txt",
                   sep = "\t", fileEncoding = "UTF-8", header = TRUE,
                   stringsAsFactors = FALSE)

meetups <- read.table("meetups.txt",
                   sep = "\t",  header = TRUE,
                   stringsAsFactors = FALSE)

names(data)

#' Ugh, esos nombres!
names(data) <- names(data) %>% # seleccionamos los nombres
  tolower() %>% # lo llevamos a minúsculas
  gsub("^x\\.|\\.$", "", .) %>% # limpiamos 'puntos' y la 'x' del comienzo
  gsub("\\.", "_", .) %>% 
  iconv(to = "ASCII//TRANSLIT") # removemos tildes

head(data[,1:5])


#' ## De donde (decimos que) somos?
#' 
#' Para esta parte agrupamos y obtenemos conteos usando las funciones del paquete `dplyr` y graficamos con el paquete `rcdimple` 
#' (wrapper de la librería de javascript dimple).

t <- data %>% 
  group_by(ubicacion) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% # los ordenos segun tamaño para que al realizar el 'factor' queden ordenados y plotearlos
  mutate(ubicacion = factor(ubicacion, levels = ubicacion))

tail(t)

t %>% 
  # filter(freq > 2) %>%
  dimple(x ="ubicacion", y = "n", type = "bar") %>%
  add_title(html = "<h4>Ubicación?</h4>")

#' Mucha centralización!
#'
#' ## ¿Qué tan rápido crecemos?
#' 
#' Con la fecha de cuando nos hemos integrado podemos ver a que ritmo crecemos como grupo :).

data <- data %>% 
  mutate(se_unio_al_grupo_el_date = gsub("/", "-", se_unio_al_grupo_el)) %>%  
  mutate(se_unio_al_grupo_el_date = as.Date(mdy(se_unio_al_grupo_el_date)))

t <- data %>% 
  group_by(se_unio_al_grupo_el_date) %>% 
  summarise(n = n()) %>% 
  arrange(se_unio_al_grupo_el_date) %>% 
  mutate(integrantes = cumsum(n))
class(t) <- "data.frame"
head(t)

t %>% filter(integrantes > 100) %>% head(1)

plot <- t %>%
  mjs_plot(x=se_unio_al_grupo_el_date, y=integrantes, width=800) %>%
  mjs_line() %>%
  mjs_axis_x(xax_format="date") %>% 
  mjs_line(animate_on_load = TRUE) %>% 
  mjs_add_baseline(100, "Septiembre 2014, pasamos los 100 integrantes que aún no conozco!")
 
#' Incorporemos los meetups pasados

meetups <- meetups %>% 
  mutate(fecha_en_formato_fecha = paste(dia, mes, año)) %>% 
  mutate(fecha_en_formato_fecha = as.Date(fecha_en_formato_fecha, "%d %b %Y")) %>% 
  arrange(fecha_en_formato_fecha)
  
for(fila in seq(nrow(meetups))){
  fecha <- meetups[fila, "fecha_en_formato_fecha"]
  plot <- plot %>% mjs_add_marker(fecha, sprintf("M%s", fila))
}
  
plot

crecimiento_diario <- lm(integrantes ~ se_unio_al_grupo_el_date, data = t)

tidy(crecimiento_diario)


#' ## Nuestro (autoevaluado) Nivel

respuestas <- data$a_que_nivel_manejas_r

head(respuestas)

respuestas <- respuestas %>% 
  tolower() %>% # lo llevamos a minúsculas
  gsub("(?!')[[:punct:]]", " ", ., perl = TRUE) %>% # limpiamos caracteres de puntuación
  iconv(to = "ASCII//TRANSLIT") # removemos tildes

sw <- c(stopwords(kind = "es"), "través")

corpus <- Corpus(VectorSource(respuestas)) %>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(function(x){ removeWords(x, stopwords(kind = "es")) })

t <- TermDocumentMatrix(corpus) %>%
  as.matrix() %>%
  rowSums() %>%
  sort(decreasing = TRUE) %>%
  data.frame(word = names(.), freq = .) %>%
  tbl_df() %>%
  arrange(desc(freq))

#' Haremos el gráfico usando la librería `rcdimple`

t %>% 
  filter(freq > 2) %>%
  dimple(x ="word", y = "freq", type = "bar") %>%
  add_title(html = "<h4>Palabras más comunes entre las respuesta: ¿a que nivel manejas R?</h4>")


#' ## Como conocimos el grupo?
#' 
respuestas <- data$como_encontraste_este_grupo

head(respuestas)

respuestas <- respuestas %>% 
  tolower() %>% # lo llevamos a minúsculas
  gsub("(?!')[[:punct:]]", " ", ., perl = TRUE) %>% # limpiamos caracteres de puntuación
  iconv(to = "ASCII//TRANSLIT") # removemos tildes


corpus <- Corpus(VectorSource(respuestas)) %>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(function(x){ removeWords(x, stopwords(kind = "es")) })

t <- TermDocumentMatrix(corpus) %>%
  as.matrix() %>%
  rowSums() %>%
  sort(decreasing = TRUE) %>%
  data.frame(word = names(.), freq = .) %>%
  tbl_df() %>%
  arrange(desc(freq))

#' <script src="https://rawgit.com/jbkunst/d3wordcloud/master/inst/htmlwidgets/lib/d3.layout.cloud.js"></script>
d3wordcloud(t$word, t$freq, scale = "log", width = 800)

#' # Conclusión!
#' 
#' Espero que esto incentive una respuesta :D y partamos reuniéndonos 
#' para programar unas presentaciones, discusiones, etc, les parece?
#' 
#' Nos vemos!
