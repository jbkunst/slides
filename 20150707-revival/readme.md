# useRChile Revival
Joshua  
<style>
  text {
      font-family: Georgia,"Times New Roman",Times,serif;
  }
</style>
(Esto se puede ver también por [acá](https://rawgit.com/jbkunst/useRchile/master/20150707-revival/readme.html))
<br>
Con el fin de *re*activar el grupo pretendo realizar un tipo de estudio descritivo usando... R?
Paquetes (no librerías!) a necesitar!


```r
rm(list = ls())
library("dplyr")
library("ggplot2")
library("lubridate")
library("rcdimple")
# library("printr")
```

Leer el archivo


```r
data <- read.table("useRchile_Member_List_on_07-07-15.txt",
                   sep = "\t", fileEncoding = "UTF-8", header = TRUE,
                   stringsAsFactors = FALSE)
```

Le agregamos la clase tbl (paquete dplyr) que uno de sus ventajas es como se visualiza la realizar el print en consola.


```r
data <- tbl_df(data)

head(data)
```

```
## Source: local data frame [6 x 22]
## 
##                          Nombre Identificación.del.usuario Título
## 1                Adolfo Alvarez              user 80526342     NA
## 2 Adrian Leonardo Escobar Gomez             user 189602319     NA
## 3                   Ale Alarcon             user 182152812     NA
## 4           Alejandra Ormazabal              user 14436601     NA
## 5                     Alejandro             user 160635342     NA
## 6                     Alejandro             user 170556652     NA
## Variables not shown: Identificación.del.miembro (int), Ubicación (chr),
##   Se.unió.al.Grupo.el (chr), Última.visita.al.grupo.el (chr),
##   Último.evento.presenciado (chr), Total.de.reservaciones (int), Con.RSVP
##   (int), RSVP.marcada.como.Quizás (int), Sin.RSVP (int),
##   Meetups.presenciados (int), Ausentes (int), Presentación (chr), Foto
##   (chr), Organizador.asistente (chr), Lista.de.correo (chr),
##   URL.del.perfil.del.miembro (chr), X.Qué.esperas.de.este.grupo. (chr),
##   X.Cómo.encontraste.este.grupo. (chr), X.A.qué.nivel.manejas.R. (chr)
```

Ugh, esos nombres!


```r
names(data) <- names(data) %>% # seleccionamos los nombres
  tolower() %>% # lo llevamos a minúsculas
  gsub("^x\\.|\\.$", "", .) %>% # limpiamos 'puntos' y la 'x' del comienzo
  gsub("\\.", "_", .) %>% 
  iconv(to = "ASCII//TRANSLIT") # removemos tildes

head(data)
```

```
## Source: local data frame [6 x 22]
## 
##                          nombre identificacion_del_usuario titulo
## 1                Adolfo Alvarez              user 80526342     NA
## 2 Adrian Leonardo Escobar Gomez             user 189602319     NA
## 3                   Ale Alarcon             user 182152812     NA
## 4           Alejandra Ormazabal              user 14436601     NA
## 5                     Alejandro             user 160635342     NA
## 6                     Alejandro             user 170556652     NA
## Variables not shown: identificacion_del_miembro (int), ubicacion (chr),
##   se_unio_al_grupo_el (chr), ultima_visita_al_grupo_el (chr),
##   ultimo_evento_presenciado (chr), total_de_reservaciones (int), con_rsvp
##   (int), rsvp_marcada_como_quizas (int), sin_rsvp (int),
##   meetups_presenciados (int), ausentes (int), presentacion (chr), foto
##   (chr), organizador_asistente (chr), lista_de_correo (chr),
##   url_del_perfil_del_miembro (chr), que_esperas_de_este_grupo (chr),
##   como_encontraste_este_grupo (chr), a_que_nivel_manejas_r (chr)
```

Un poco mejor! Ahora por unos descriptivos
De donde somos?


```r
t <- data %>% 
  group_by(ubicacion) %>% 
  summarise(n = n()) %>% 
  arrange(n) %>% # los ordenos segun tamaña para que al realizar el 'factor' queden ordenados y plotearlos
  mutate(ubicacion = factor(ubicacion, levels = ubicacion))

tail(t)
```

```
## Source: local data frame [6 x 2]
## 
##      ubicacion   n
## 1       Sydney   1
## 2        Talca   1
## 3 Waterloo, ON   1
## 4         Lima   2
## 5 Viña del Mar   2
## 6     Santiago 103
```

```r
t %>%
  dimple(x ="ubicacion", y = "n", type = "bar") %>%
  add_title(html = "<h4>Unit Sales by Month for Fictional Store</h4>")
```

<!--html_preserve--><div id="htmlwidget-2526" style="width:672px;height:480px;" class="dimple"></div>
<script type="application/json" data-for="htmlwidget-2526">{"x":{"options":{"chart":[],"xAxis":{"type":"addCategoryAxis"},"yAxis":{"type":"addMeasureAxis"},"zAxis":[],"colorAxis":[],"defaultColors":[],"layers":[],"legend":[],"x":"ubicacion","y":"n","type":"bar","title":{"text":null,"html":"<h4>Unit Sales by Month for Fictional Store</h4>"}},"data":{"ubicacion":["Berlin","Brussels","Cajamarca","Chillán","Concepción","Copenhagen","Curicó","Honolulu, HI","Isla de Maipo","Machalí","Mannheim","Medellín","México City","Poznan","Puente Alto","Sydney","Talca","Waterloo, ON","Lima","Viña del Mar","Santiago"],"n":[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,103]}},"evals":[]}</script><!--/html_preserve-->

Como hemos crecido en tamaño durante el tiempo


```r
data <- data %>% 
  mutate(se_unio_al_grupo_el_date = gsub("/", "-", se_unio_al_grupo_el)) %>%  
  mutate(se_unio_al_grupo_el_date = as.Date(mdy(se_unio_al_grupo_el_date)))

data %>% select(se_unio_al_grupo_el, se_unio_al_grupo_el_date)
```

```
## Source: local data frame [125 x 2]
## 
##    se_unio_al_grupo_el se_unio_al_grupo_el_date
## 1           02/13/2013               2013-02-13
## 2           07/07/2015               2015-07-07
## 3           05/28/2015               2015-05-28
## 4           11/27/2013               2013-11-27
## 5           08/16/2014               2014-08-16
## 6           09/12/2014               2014-09-12
## 7           02/06/2013               2013-02-06
## 8           02/12/2013               2013-02-12
## 9           04/14/2015               2015-04-14
## 10          07/02/2014               2014-07-02
## ..                 ...                      ...
```

```r
t <- data %>% 
  group_by(se_unio_al_grupo_el_date) %>% 
  summarise(n = n()) %>% 
  arrange(se_unio_al_grupo_el_date) %>% 
  mutate(integrantes = cumsum(n))

t
```

```
## Source: local data frame [111 x 3]
## 
##    se_unio_al_grupo_el_date n integrantes
## 1                2013-01-22 1           1
## 2                2013-02-06 1           2
## 3                2013-02-07 4           6
## 4                2013-02-12 1           7
## 5                2013-02-13 1           8
## 6                2013-02-18 1           9
## 7                2013-02-28 1          10
## 8                2013-03-06 1          11
## 9                2013-03-07 1          12
## 10               2013-03-11 1          13
## ..                      ... .         ...
```

```r
ggplot(t) +
  geom_line(aes(se_unio_al_grupo_el_date, integrantes), color = "darkred", size = 1.2)
```

![](readme_files/figure-html/unnamed-chunk-6-1.png) 


---
title: "readme.R"
author: "Joshua K"
date: "Wed Jul 08 23:45:11 2015"
---
