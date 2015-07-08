#' ---
#' title: "userRChile Revival"
#' author: "Joshua Kunst"
#' output: 
#'  html_document: 
#'    keep_md: yes
#' ---

#' (Esto se puede ver también por [acá](https://rawgit.com/jbkunst/useRchile/master/20150707-revival/readme.html))

#' Con el fin de *re*activar el grupo pretendo realizar un tipo de estudio descritivo usando... R?

#' Paquetes (no librerías!) a necesitar!
#+ fig.width=10, fig.height=5, warning=FALSE, message=FALSE
rm(list = ls())
library("dplyr")
library("ggplot2")
library("lubridate")

#' Leer el archivo
data <- read.table("useRchile_Member_List_on_07-07-15.txt",
                   sep = "\t", fileEncoding = "UTF-8", header = TRUE,
                   stringsAsFactors = FALSE)

#' Le agregamos la clase tbl (paquete dplyr) que uno de sus ventajas es como se visualiza la realizar el print en consola.

data <- tbl_df(data)

data

#' Ugh, esos nombres!
names(data) <- names(data) %>% # seleccionamos los nombres
  tolower() %>% # lo llevamos a minúsculas
  gsub("^x\\.|\\.$", "", .) %>% # limpiamos 'puntos' y la 'x' del comienzo
  gsub("\\.", "_", .) %>% 
  iconv(to = "ASCII//TRANSLIT") # removemos tildes

data

#' Un poco mejor! Ahora por unos descriptivos
 
#' De donde somos?
t <- data %>% 
  group_by(ubicacion) %>% 
  summarise(n = n()) %>% 
  arrange(n) %>% # los ordenos segun tamaña para que al realizar el 'factor' queden ordenados y plotearlos
  mutate(ubicacion = factor(ubicacion, levels = ubicacion))

tail(t)

ggplot(t) + 
  geom_bar(aes(ubicacion, n), stat = "identity") +
  coord_flip() +
  ggtitle("Somos un grupo de R en Chile internacional?!")

#' Como hemos crecido en tamaño durante el tiempo
data <- data %>% 
  mutate(se_unio_al_grupo_el_date = gsub("/", "-", se_unio_al_grupo_el)) %>%  
  mutate(se_unio_al_grupo_el_date = dmy(se_unio_al_grupo_el_date)) 

data %>% select(se_unio_al_grupo_el, se_unio_al_grupo_el_date)

t <- data %>% 
  group_by(se_unio_al_grupo_el) %>% 
  summarise(n = n()) %>% 
  arrange(se_unio_al_grupo_el)
