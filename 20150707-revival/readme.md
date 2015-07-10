# useRChile Revival
Joshua  
# Introducción
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
library("scales")
library("broom")
library("tm")
library("htmlwidgets")
library("knitr")
library("d3wordcloud")
```

# Lectura de Datos
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

Un poco mejor! Ahora por unos descriptivos.

# Análisis Descriptivo
Para esta parte agrupamos y obtenemos conteos usando las funciones del paquete `dplyr` y graficamos con el paquete `rcdimple` (wrapper de la librería de javascript dimple).


```r
t <- data %>% 
  group_by(ubicacion) %>% 
  summarise(n = n()) %>% 
  arrange(n) %>% # los ordenos segun tamaño para que al realizar el 'factor' queden ordenados y plotearlos
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

Mucha centralización!


```r
t %>%
  dimple(x ="ubicacion", y = "n", type = "bar") %>%
  add_title(html = "<h4>De donde Somos?</h4>")
```

<!--html_preserve--><div id="htmlwidget-9694" style="width:672px;height:480px;" class="dimple"></div>
<script type="application/json" data-for="htmlwidget-9694">{"x":{"options":{"chart":[],"xAxis":{"type":"addCategoryAxis"},"yAxis":{"type":"addMeasureAxis"},"zAxis":[],"colorAxis":[],"defaultColors":[],"layers":[],"legend":[],"x":"ubicacion","y":"n","type":"bar","title":{"text":null,"html":"<h4>De donde Somos?</h4>"}},"data":{"ubicacion":["Berlin","Brussels","Cajamarca","Chillán","Concepción","Copenhagen","Curicó","Honolulu, HI","Isla de Maipo","Machalí","Mannheim","Medellín","México City","Poznan","Puente Alto","Sydney","Talca","Waterloo, ON","Lima","Viña del Mar","Santiago"],"n":[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,103]}},"evals":[]}</script><!--/html_preserve-->

Con la fecha de cuando nos hemos integrado podemos ver a que ritmo crecemos como grupo :D.


```r
data <- data %>% 
  mutate(se_unio_al_grupo_el_date = gsub("/", "-", se_unio_al_grupo_el)) %>%  
  mutate(se_unio_al_grupo_el_date = as.Date(mdy(se_unio_al_grupo_el_date)))

data %>% select(se_unio_al_grupo_el, se_unio_al_grupo_el_date) %>% head()
```

```
## Source: local data frame [6 x 2]
## 
##   se_unio_al_grupo_el se_unio_al_grupo_el_date
## 1          02/13/2013               2013-02-13
## 2          07/07/2015               2015-07-07
## 3          05/28/2015               2015-05-28
## 4          11/27/2013               2013-11-27
## 5          08/16/2014               2014-08-16
## 6          09/12/2014               2014-09-12
```

```r
t <- data %>% 
  group_by(se_unio_al_grupo_el_date) %>% 
  summarise(n = n()) %>% 
  arrange(se_unio_al_grupo_el_date) %>% 
  mutate(integrantes = cumsum(n))

head(t)
```

```
## Source: local data frame [6 x 3]
## 
##   se_unio_al_grupo_el_date n integrantes
## 1               2013-01-22 1           1
## 2               2013-02-06 1           2
## 3               2013-02-07 4           6
## 4               2013-02-12 1           7
## 5               2013-02-13 1           8
## 6               2013-02-18 1           9
```

```r
ggplot(t, aes(se_unio_al_grupo_el_date, integrantes)) +
  geom_line(color = "darkred", size = 1.2) +
  geom_smooth(method="lm", se = FALSE) # groseramente puse el auste de una rl para que en ver el promedio de ingresos por día.
```

![](readme_files/figure-html/unnamed-chunk-7-1.png) 

```r
crecimiento_diario <- lm(integrantes ~ se_unio_al_grupo_el_date, data = t)

tidy(crecimiento_diario)
```

```
##                       term      estimate    std.error statistic
## 1              (Intercept) -2138.8883235 31.156663233 -68.64947
## 2 se_unio_al_grupo_el_date     0.1365681  0.001931678  70.69923
##        p.value
## 1 1.556673e-91
## 2 6.757266e-93
```

Ahora veamos el nivel autoevaluado


```r
respuestas <- data$como_encontraste_este_grupo

head(respuestas)
```

```
## [1] "Twitter!"                                        
## [2] "Por El curso de EDX."                            
## [3] "por meetup"                                      
## [4] "Por casualidad encontré este grupo."             
## [5] "googleando"                                      
## [6] "Del curso en Linea \\Explore Statistics with R\\"
```

```r
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
```

<!--html_preserve--><div id="htmlwidget-6766" style="width:672px;height:480px;" class="dimple"></div>
<script type="application/json" data-for="htmlwidget-6766">{"x":{"options":{"chart":[],"xAxis":{"type":"addCategoryAxis"},"yAxis":{"type":"addMeasureAxis"},"zAxis":[],"colorAxis":[],"defaultColors":[],"layers":[],"legend":[],"x":"word","y":"freq","type":"bar","title":{"text":null,"html":"<h4>Palabras más comunes entre las respuesta de como encontraron al grupo</h4>"}},"data":{"word":["meetup","google","buscando","internet","amigo","googleando","pagina","traves","web","chile","grupos","busqueda","grupo","navegando","recomendacion","twitter","curso","invitacion","knitr","project","vega"],"freq":[18,17,12,10,8,7,7,7,6,5,5,4,4,4,4,4,3,3,3,3,3]}},"evals":[]}</script><!--/html_preserve-->

Ahora estudiemos como conocimos al grupo


```r
respuestas <- data$como_encontraste_este_grupo

head(respuestas)
```

```
## [1] "Twitter!"                                        
## [2] "Por El curso de EDX."                            
## [3] "por meetup"                                      
## [4] "Por casualidad encontré este grupo."             
## [5] "googleando"                                      
## [6] "Del curso en Linea \\Explore Statistics with R\\"
```

```r
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
```

<script src="https://rawgit.com/jbkunst/d3wordcloud/master/inst/htmlwidgets/lib/d3.layout.cloud.js"></script>


```r
d3wordcloud(t$word, t$freq)
```

<!--html_preserve--><div id="htmlwidget-2138" style="width:672px;height:480px;" class="d3wordcloud"></div>
<script type="application/json" data-for="htmlwidget-2138">{"x":{"data":{"text":["meetup","google","buscando","internet","amigo","googleando","pagina","traves","web","chile","grupos","busqueda","grupo","navegando","recomendacion","twitter","curso","invitacion","knitr","project","vega","buscador","correo","encontre","george","informacion","invito","meetups","nodoschile","oficial","principal","revolution","statistics","trabajo","usuarios","ver","yon","youtube","administrador","analytical","analytics","aplicacion","app","aqui","arme","asociado","bajo","bkn","bloggers","buecando","busque","captura","casualidad","cerda","charla","chilenos","colega","com","comentaron","community","companero","conferencia","conocer","conoci","control","coursera","cran","cuenta","datos","dentro","desarrolladores","desarrollar","dio","dude","dynlang","edx","electronico","emprendimientos","empresa","encargado","engine","estadistica","estadistico","excarvando","explorando","explore","facebook","fundador","fundadores","gente","gogleando","graficas","group","groups","habla","hispana","ingeniero","inscrito","interesaba","interesante","jefe","joshua","librerias","ligas","linea","lista","llego","llegue","mail","mediante","medio","miembro","mineria","mirando","moviles","network","newsletter","nombre","novia","org","organizador","orientada","pajina","paquete","parecio","parte","participar","patricio","pidio","predictiva","procesos","promueve","proyecto","queremos","red","redes","regionalizado","revisando","salio","santiago","search","senalo","sito","sociales","solamente","stgo","sugerido","the","unas","unirme","usando","usara","use","user","usurio","via","video","viendo","wiki","with"],"size":[18,17,12,10,8,7,7,7,6,5,5,4,4,4,4,4,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]},"pars":{"font":"Open Sans","fontweight":400,"padding":1,"rotmin":-30,"rotmax":30,"scale":"linear","spiral":"archimedean"}},"evals":[]}</script><!--/html_preserve-->


---
title: "readme.R"
author: "Joshua K"
date: "Fri Jul 10 02:14:29 2015"
---
