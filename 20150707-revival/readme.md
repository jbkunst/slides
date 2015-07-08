# userRChile Revival
Joshua Kunst  
(Esto se puede ver también por [acá](https://rawgit.com/jbkunst/useRchile/master/20150707-revival/index.html))
Con el fin de *re*activar el grupo pretendo realizar un tipo de estudio descritivo usando... R?
Paquetes (no librerías!) a necesitar!


```r
rm(list = ls())
library("dplyr")
library("ggplot2")
library("lubridate")
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

data
```

```
## Source: local data frame [125 x 22]
## 
##                           Nombre Identificación.del.usuario Título
## 1                 Adolfo Alvarez              user 80526342     NA
## 2  Adrian Leonardo Escobar Gomez             user 189602319     NA
## 3                    Ale Alarcon             user 182152812     NA
## 4            Alejandra Ormazabal              user 14436601     NA
## 5                      Alejandro             user 160635342     NA
## 6                      Alejandro             user 170556652     NA
## 7               Alejandro Zahler              user 77138632     NA
## 8              alexander vergara              user 80398702     NA
## 9                Alvaro Alliende             user 186169964     NA
## 10              Alvaro Hernandez             user 152416642     NA
## ..                           ...                        ...    ...
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

data
```

```
## Source: local data frame [125 x 22]
## 
##                           nombre identificacion_del_usuario titulo
## 1                 Adolfo Alvarez              user 80526342     NA
## 2  Adrian Leonardo Escobar Gomez             user 189602319     NA
## 3                    Ale Alarcon             user 182152812     NA
## 4            Alejandra Ormazabal              user 14436601     NA
## 5                      Alejandro             user 160635342     NA
## 6                      Alejandro             user 170556652     NA
## 7               Alejandro Zahler              user 77138632     NA
## 8              alexander vergara              user 80398702     NA
## 9                Alvaro Alliende             user 186169964     NA
## 10              Alvaro Hernandez             user 152416642     NA
## ..                           ...                        ...    ...
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
ggplot(t) + 
  geom_bar(aes(ubicacion, n), stat = "identity") +
  coord_flip() +
  ggtitle("Somos un grupo de R en Chile internacional?!")
```

![](readme_files/figure-html/unnamed-chunk-5-1.png) 

Como hemos crecido en tamaño durante el tiempo


```r
data <- data %>% 
  mutate(se_unio_al_grupo_el_date = gsub("/", "-", se_unio_al_grupo_el)) %>%  
  mutate(se_unio_al_grupo_el_date = dmy(se_unio_al_grupo_el_date)) 

data %>% select(se_unio_al_grupo_el, se_unio_al_grupo_el_date)
```

```
## Source: local data frame [125 x 2]
## 
##    se_unio_al_grupo_el se_unio_al_grupo_el_date
## 1           02/13/2013      2014-01-02 00:00:00
## 2           07/07/2015      2015-07-07 00:00:00
## 3           05/28/2015      2078-06-28 23:06:17
## 4           11/27/2013      2071-03-06 00:59:44
## 5           08/16/2014      2052-02-12 19:58:56
## 6           09/12/2014      2014-12-09 00:00:00
## 7           02/06/2013      2013-06-02 00:00:00
## 8           02/12/2013      2013-12-02 00:00:00
## 9           04/14/2015      1960-07-26 21:24:42
## 10          07/02/2014      2014-02-07 00:00:00
## ..                 ...                      ...
```

```r
t <- data %>% 
  group_by(se_unio_al_grupo_el) %>% 
  summarise(n = n()) %>% 
  arrange(se_unio_al_grupo_el)
```


---
title: "readme.R"
author: "jkunst"
date: "Wed Jul 08 12:10:46 2015"
---
