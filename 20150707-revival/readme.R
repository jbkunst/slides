#' ---
#' title: "useRChile Revival"
#' author: "Joshua"
#' output: 
#'  html_document: 
#'    theme: journal
#'    toc: true
#'    keep_md: yes
#' ---

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
# library("printr")

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

#' Un poco mejor! Ahora por unos descriptivos
 
#' De donde somos?
t <- data %>% 
  group_by(ubicacion) %>% 
  summarise(n = n()) %>% 
  arrange(n) %>% # los ordenos segun tamaña para que al realizar el 'factor' queden ordenados y plotearlos
  mutate(ubicacion = factor(ubicacion, levels = ubicacion))

tail(t)

t %>%
  dimple(x ="ubicacion", y = "n", type = "bar") %>%
  add_title(html = "<h4>Unit Sales by Month for Fictional Store</h4>")



#' Como hemos crecido en tamaño durante el tiempo
data <- data %>% 
  mutate(dat = gsub("/", "-", se_unio_al_grupo_el)) %>%  
  mutate(dat = as.Date(mdy(dat)))

data %>% select(se_unio_al_grupo_el, dat)

t <- data %>% 
  group_by(dat) %>% 
  summarise(n = n()) %>% 
  arrange(dat) %>% 
  mutate(integrantes = cumsum(n))

t

# 
# ggplot(t) +
#   geom_line(aes(se_unio_al_grupo_el_date, integrantes), color = "darkred", size = 1.2)
