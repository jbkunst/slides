#' ---
#' title: "useRChile Revival"
#' author: "Joshua"
#' output: 
#'  html_document: 
#'    theme: journal
#'    toc: true
#'    keep_md: yes
#' ---

#' # Introducción

#' (Esto se puede ver también por [acá](https://rawgit.com/jbkunst/useRchile/master/20150707-revival/readme.html))
#' <br>
#' Con el fin de *re*activar el grupo pretendo realizar un tipo de estudio descritivo usando... R?

#' Paquetes (no librerías!) a necesitar!
#+ fig.width=10, fig.height=5, warning=FALSE, message=FALSE
rm(list = ls())
library("dplyr")
library("ggplot2")
library("lubridate")
library("rcdimple")
library("scales")
library("broom")
library("tm")
library("htmlwidgets")
library("knitr")
library("d3wordcloud")

#' # Lectura de Datos

#' Leer el archivo
data <- read.table("useRchile_Member_List_on_07-07-15.txt",
                   sep = "\t", fileEncoding = "UTF-8", header = TRUE,
                   stringsAsFactors = FALSE)

#' Le agregamos la clase tbl (paquete dplyr) que uno de sus ventajas es como se visualiza la realizar el print en consola.

data <- tbl_df(data)

head(data)

#' Ugh, esos nombres!
names(data) <- names(data) %>% # seleccionamos los nombres
  tolower() %>% # lo llevamos a minúsculas
  gsub("^x\\.|\\.$", "", .) %>% # limpiamos 'puntos' y la 'x' del comienzo
  gsub("\\.", "_", .) %>% 
  iconv(to = "ASCII//TRANSLIT") # removemos tildes

head(data)

#' Un poco mejor! Ahora por unos descriptivos.
#' 

#' # Análisis Descriptivo
 
#' Para esta parte agrupamos y obtenemos conteos usando las funciones del paquete `dplyr` y graficamos con el paquete `rcdimple` (wrapper de la librería de javascript dimple).

t <- data %>% 
  group_by(ubicacion) %>% 
  summarise(n = n()) %>% 
  arrange(n) %>% # los ordenos segun tamaño para que al realizar el 'factor' queden ordenados y plotearlos
  mutate(ubicacion = factor(ubicacion, levels = ubicacion))

tail(t)

#' Mucha centralización!

t %>%
  dimple(x ="ubicacion", y = "n", type = "bar") %>%
  add_title(html = "<h4>De donde Somos?</h4>")


#' Con la fecha de cuando nos hemos integrado podemos ver a que ritmo crecemos como grupo :D.

data <- data %>% 
  mutate(se_unio_al_grupo_el_date = gsub("/", "-", se_unio_al_grupo_el)) %>%  
  mutate(se_unio_al_grupo_el_date = as.Date(mdy(se_unio_al_grupo_el_date)))

data %>% select(se_unio_al_grupo_el, se_unio_al_grupo_el_date) %>% head()

t <- data %>% 
  group_by(se_unio_al_grupo_el_date) %>% 
  summarise(n = n()) %>% 
  arrange(se_unio_al_grupo_el_date) %>% 
  mutate(integrantes = cumsum(n))

head(t)


ggplot(t, aes(se_unio_al_grupo_el_date, integrantes)) +
  geom_line(color = "darkred", size = 1.2) +
  geom_smooth(method="lm", se = FALSE) # groseramente puse el auste de una rl para que en ver el promedio de ingresos por día.
  
crecimiento_diario <- lm(integrantes ~ se_unio_al_grupo_el_date, data = t)

tidy(crecimiento_diario)

#' Ahora veamos el nivel autoevaluado
respuestas <- data$como_encontraste_este_grupo

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

t %>% 
  filter(freq > 2) %>%
  dimple(x ="word", y = "freq", type = "bar") %>%
  add_title(html = "<h4>Palabras más comunes entre las respuesta de como encontraron al grupo</h4>")


#' Ahora estudiemos como conocimos al grupo
respuestas <- data$como_encontraste_este_grupo

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

#' <script src="https://rawgit.com/jbkunst/d3wordcloud/master/inst/htmlwidgets/lib/d3.layout.cloud.js"></script>
d3wordcloud(t$word, t$freq)
