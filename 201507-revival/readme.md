# UseRChile Revival
Joshua  
<style>svg { font-family: "News Cycle","Arial Narrow Bold",sans-serif;}
.metricsgraphics {font-size: 2em}
</style>
# Introducción
Hola a todos! La idea de este documento/script/html/etc es intentar despertar al grupo, incentivar una junta,
conocernos *descriptivamente*, en fin, para lo que sirva. Ha estado
con muy poco movimiento nuestro grupo y ya es tiempo de moverrrrnos.
Descargué los datos desde el meetup e intenté hacer algo simple con para ir calentando motores, por lo que
al final de revisar este documento/script/html/etc pasará al menos un de las siguientes cosas:

  - El script lo encontrarás tan chanta/desordenado que querrás enseñarme alguno de tus tips.
  - No sabrás algunas cosas y querrás que nos juntemos con unos cuantos usuarios para ver
  lo que se lleva en la comunidad de R hoy en día.
  - No pasará ni lo uno ni lo otro pero de todas formas te nacerá un incentivo de juntarno
  para compartir ideas/conociminetos/organizar nuevas presentaciones. 
## Sobre este script

El script/código está [acá](https://github.com/jbkunst/useRchile/blob/master/20150707-revival/readme.R), pero estará
el resultado por [aquí](https://rawgit.com/jbkunst/useRchile/master/20150707-revival/readme.html).

Los datos los descargué de la mismísima página de [meetup](http://www.meetup.com/es/useRchile/members/).



# Veamos que nos puede decir los datos
## Lectura de Datos
Leer el archivo


```r
data <- read.table("useRchile_Member_List_on_07-20-15.txt",
                   sep = "\t", fileEncoding = "UTF-8", header = TRUE,
                   stringsAsFactors = FALSE)

meetups <- read.table("meetups.txt",
                   sep = "\t",  header = TRUE,
                   stringsAsFactors = FALSE)

names(data)
```

```
##  [1] "Nombre"                         "Identificación.del.usuario"    
##  [3] "Título"                         "Identificación.del.miembro"    
##  [5] "Ubicación"                      "Se.unió.al.Grupo.el"           
##  [7] "Última.visita.al.grupo.el"      "Último.evento.presenciado"     
##  [9] "Total.de.reservaciones"         "Con.RSVP"                      
## [11] "RSVP.marcada.como.Quizás"       "Sin.RSVP"                      
## [13] "Meetups.presenciados"           "Ausentes"                      
## [15] "Presentación"                   "Foto"                          
## [17] "Organizador.asistente"          "Lista.de.correo"               
## [19] "URL.del.perfil.del.miembro"     "X.Qué.esperas.de.este.grupo."  
## [21] "X.Cómo.encontraste.este.grupo." "X.A.qué.nivel.manejas.R."
```

Ugh, esos nombres!


```r
names(data) <- names(data) %>% # seleccionamos los nombres
  tolower() %>% # lo llevamos a minúsculas
  gsub("^x\\.|\\.$", "", .) %>% # limpiamos 'puntos' y la 'x' del comienzo
  gsub("\\.", "_", .) %>% 
  iconv(to = "ASCII//TRANSLIT") # removemos tildes

head(data[,1:5])
```



nombre                          identificacion_del_usuario   titulo    identificacion_del_miembro  ubicacion   
------------------------------  ---------------------------  -------  ---------------------------  ------------
Adolfo Alvarez                  user 80526342                NA                          80526342  Poznan      
Adrian Leonardo Escobar Gomez   user 189602319               NA                         189602319  Santiago    
Ale Alarcon                     user 182152812               NA                         182152812  Puente Alto 
Alejandra Ormazabal             user 14436601                NA                          14436601  Santiago    
Alejandro                       user 160635342               NA                         160635342  Santiago    
Alejandro                       user 170556652               NA                         170556652  Santiago    

## De donde (decimos que) somos?

Para esta parte agrupamos y obtenemos conteos usando las funciones del paquete `dplyr` y graficamos con el paquete `rcdimple` 
(wrapper de la librería de javascript dimple).


```r
t <- data %>% 
  group_by(ubicacion) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% # los ordenos segun tamaño para que al realizar el 'factor' queden ordenados y plotearlos
  mutate(ubicacion = factor(ubicacion, levels = ubicacion))

tail(t)
```



ubicacion        n
-------------  ---
Medellín         1
México City      1
Poznan           1
Sydney           1
Talca            1
Waterloo, ON     1

```r
t %>% 
  # filter(freq > 2) %>%
  dimple(x ="ubicacion", y = "n", type = "bar") %>%
  add_title(html = "<h4>Ubicación?</h4>")
```

<!--html_preserve--><div id="htmlwidget-9615" style="width:672px;height:480px;" class="dimple"></div>
<script type="application/json" data-for="htmlwidget-9615">{"x":{"options":{"chart":[],"xAxis":{"type":"addCategoryAxis"},"yAxis":{"type":"addMeasureAxis"},"zAxis":[],"colorAxis":[],"defaultColors":[],"layers":[],"legend":[],"x":"ubicacion","y":"n","type":"bar","title":{"text":null,"html":"<h4>Ubicación?</h4>"}},"data":{"ubicacion":["Santiago","Lima","Puente Alto","Viña del Mar","Berlin","Brussels","Cajamarca","Chillán","Concepción","Copenhagen","Curicó","Honolulu, HI","Isla de Maipo","Machalí","Mannheim","Medellín","México City","Poznan","Sydney","Talca","Waterloo, ON"],"n":[102,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]}},"evals":[]}</script><!--/html_preserve-->

Mucha centralización!

## ¿Qué tan rápido crecemos?

Con la fecha de cuando nos hemos integrado podemos ver a que ritmo crecemos como grupo :).


```r
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
```



se_unio_al_grupo_el_date     n   integrantes
-------------------------  ---  ------------
2013-01-22                   1             1
2013-02-06                   1             2
2013-02-07                   4             6
2013-02-12                   1             7
2013-02-13                   1             8
2013-02-18                   1             9

```r
t %>% filter(integrantes > 100) %>% head(1)
```



se_unio_al_grupo_el_date     n   integrantes
-------------------------  ---  ------------
2014-10-05                   1           101

```r
plot <- t %>%
  mjs_plot(x=se_unio_al_grupo_el_date, y=integrantes, width=800) %>%
  mjs_line() %>%
  mjs_axis_x(xax_format="date") %>% 
  mjs_line(animate_on_load = TRUE) %>% 
  mjs_add_baseline(100, "Septiembre 2014, pasamos los 100 integrantes que aún no conozco!")
```

Incorporemos los meetups pasados


```r
meetups <- meetups %>% 
  mutate(fecha_en_formato_fecha = paste(dia, mes, año)) %>% 
  mutate(fecha_en_formato_fecha = as.Date(fecha_en_formato_fecha, "%d %b %Y")) %>% 
  arrange(fecha_en_formato_fecha)
  
for(fila in seq(nrow(meetups))){
  fecha <- meetups[fila, "fecha_en_formato_fecha"]
  plot <- plot %>% mjs_add_marker(fecha, sprintf("M%s", fila))
}
  
plot
```

<!--html_preserve--><div id="mjs-1ff302c1ab2f7990287271fb9ba8be" class="metricsgraphics" style="width:800px;height:480px;"></div>
<div id="mjs-1ff302c1ab2f7990287271fb9ba8be-legend" class="metricsgraphics-legend"></div>
<script type="application/json" data-for="mjs-1ff302c1ab2f7990287271fb9ba8be">{"x":{"orig_posix":false,"data":{"se_unio_al_grupo_el_date":["2013-01-22","2013-02-06","2013-02-07","2013-02-12","2013-02-13","2013-02-18","2013-02-28","2013-03-06","2013-03-07","2013-03-11","2013-03-12","2013-03-18","2013-03-25","2013-04-01","2013-04-17","2013-04-25","2013-05-03","2013-05-04","2013-05-09","2013-05-13","2013-05-16","2013-05-27","2013-05-28","2013-06-09","2013-06-10","2013-06-13","2013-06-14","2013-06-19","2013-06-25","2013-07-01","2013-07-03","2013-07-10","2013-08-07","2013-08-24","2013-08-28","2013-08-30","2013-10-06","2013-10-22","2013-10-31","2013-11-27","2013-12-08","2013-12-14","2013-12-16","2014-01-07","2014-02-15","2014-02-17","2014-02-19","2014-02-22","2014-02-24","2014-03-07","2014-03-08","2014-03-14","2014-03-16","2014-03-24","2014-03-25","2014-04-02","2014-04-03","2014-04-09","2014-04-13","2014-04-20","2014-04-24","2014-04-28","2014-04-29","2014-05-03","2014-05-04","2014-05-06","2014-05-08","2014-05-12","2014-05-15","2014-05-21","2014-05-24","2014-05-29","2014-06-12","2014-06-20","2014-07-01","2014-07-02","2014-07-09","2014-07-11","2014-07-26","2014-07-27","2014-07-28","2014-08-02","2014-08-08","2014-08-13","2014-08-16","2014-08-25","2014-09-12","2014-09-16","2014-09-24","2014-10-05","2014-10-10","2014-10-29","2014-11-13","2014-12-12","2015-01-22","2015-02-19","2015-02-26","2015-03-07","2015-03-28","2015-04-06","2015-04-14","2015-05-08","2015-05-09","2015-05-28","2015-05-31","2015-06-03","2015-06-05","2015-06-22","2015-07-03","2015-07-07","2015-07-08"],"n":[1,1,4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,2,1,1,1,1,1,2,1,2,1,1,1,1,1,1,2,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,3,1,1,1],"integrantes":[1,2,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,29,30,31,32,33,34,36,37,38,39,40,41,43,44,46,47,48,49,50,51,52,54,55,56,57,58,59,60,61,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,86,87,88,89,90,91,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,112,113,114,115,116,117,118,119,122,123,124,125]},"x_axis":true,"y_axis":true,"baseline_accessor":null,"predictor_accessor":null,"show_confidence_band":null,"chart_type":"line","xax_format":"date","x_label":null,"y_label":null,"markers":[{"se_unio_al_grupo_el_date":"2013-03-16","label":"M1"},{"se_unio_al_grupo_el_date":"2013-05-11","label":"M2"},{"se_unio_al_grupo_el_date":"2013-06-15","label":"M3"},{"se_unio_al_grupo_el_date":"2013-07-13","label":"M4"},{"se_unio_al_grupo_el_date":"2014-02-20","label":"M5"},{"se_unio_al_grupo_el_date":"2014-03-22","label":"M6"},{"se_unio_al_grupo_el_date":"2014-04-23","label":"M7"},{"se_unio_al_grupo_el_date":"2014-05-14","label":"M8"}],"baselines":[{"value":100,"label":"Septiembre 2014, pasamos los 100 integrantes que aún no conozco!"}],"linked":false,"title":null,"description":null,"left":80,"right":10,"bottom":60,"buffer":8,"format":"count","y_scale_type":"linear","yax_count":5,"xax_count":6,"x_rug":false,"y_rug":false,"area":false,"missing_is_hidden":false,"size_accessor":null,"color_accessor":null,"color_type":"number","color_range":["blue","red"],"size_range":[1,5],"bar_height":20,"min_y":null,"max_y":null,"bar_margin":1,"binned":false,"bins":null,"least_squares":false,"interpolate":"cardinal","decimals":2,"show_rollover_text":true,"x_accessor":"se_unio_al_grupo_el_date","y_accessor":"integrantes","multi_line":null,"geom":"line","yax_units":"","legend":null,"legend_target":null,"y_extended_ticks":false,"x_extended_ticks":false,"target":"#mjs-1ff302c1ab2f7990287271fb9ba8be","full_height":true,"animate_on_load":true},"evals":[]}</script><!--/html_preserve-->

```r
crecimiento_diario <- lm(integrantes ~ se_unio_al_grupo_el_date, data = t)

tidy(crecimiento_diario)
```



term                             estimate    std.error   statistic   p.value
-------------------------  --------------  -----------  ----------  --------
(Intercept)                 -2123.9897429   30.5657206   -69.48927         0
se_unio_al_grupo_el_date        0.1355866    0.0018941    71.58198         0

## Nuestro (autoevaluado) Nivel


```r
respuestas <- data$a_que_nivel_manejas_r

head(respuestas)
```

```
## [1] "Todos los días aprendo algo nuevo" "Básico."                          
## [3] "Usuario"                           "Excelente herramienta."           
## [5] "Intermedio"                        "Recién aprendiendo"
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

Haremos el gráfico usando la librería `rcdimple`


```r
t %>% 
  filter(freq > 2) %>%
  dimple(x ="word", y = "freq", type = "bar") %>%
  add_title(html = "<h4>Palabras más comunes entre las respuesta: ¿a que nivel manejas R?</h4>")
```

<!--html_preserve--><div id="htmlwidget-7389" style="width:672px;height:480px;" class="dimple"></div>
<script type="application/json" data-for="htmlwidget-7389">{"x":{"options":{"chart":[],"xAxis":{"type":"addCategoryAxis"},"yAxis":{"type":"addMeasureAxis"},"zAxis":[],"colorAxis":[],"defaultColors":[],"layers":[],"legend":[],"x":"word","y":"freq","type":"bar","title":{"text":null,"html":"<h4>Palabras más comunes entre las respuesta: ¿a que nivel manejas R?</h4>"}},"data":{"word":["basico","intermedio","medio","nivel","avanzado","analisis","principiante","usuario","manejo","mas","aprender","datos","estadistica","ninguno","paquetes","recien","trabajo"],"freq":[45,20,14,14,9,6,6,5,4,4,3,3,3,3,3,3,3]}},"evals":[]}</script><!--/html_preserve-->

## Como conocimos el grupo?



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
d3wordcloud(t$word, t$freq, scale = "log", width = 800)
```

<!--html_preserve--><div id="htmlwidget-351" style="width:800px;height:480px;" class="d3wordcloud"></div>
<script type="application/json" data-for="htmlwidget-351">{"x":{"data":{"text":["google","meetup","buscando","internet","amigo","googleando","pagina","traves","web","chile","grupos","busqueda","grupo","navegando","recomendacion","twitter","curso","invitacion","knitr","project","vega","correo","encontre","george","informacion","invito","meetups","nodoschile","oficial","principal","revolution","statistics","trabajo","usuarios","ver","yon","youtube","administrador","analytical","analytics","aplicacion","app","aqui","arme","asociado","bajo","bkn","bloggers","buecando","buscador","busque","captura","casualidad","cerda","charla","chilenos","colega","com","comentaron","community","companero","conferencia","conocer","conoci","control","coursera","cran","cuenta","datos","dentro","desarrolladores","desarrollar","dio","dude","dynlang","edx","electronico","emprendimientos","empresa","encargado","engine","estadistica","estadistico","excarvando","explorando","explore","facebook","fundador","fundadores","gente","gogleando","graficas","group","groups","habla","hispana","ingeniero","inscrito","interesaba","interesante","jefe","joshua","librerias","ligas","linea","lista","llego","llegue","mail","mediante","medio","miembro","mineria","mirando","moviles","network","newsletter","nombre","novia","org","organizador","orientada","pajina","paquete","parecio","parte","participar","patricio","pidio","predictiva","procesos","promueve","proyecto","queremos","red","redes","regionalizado","revisando","salio","santiago","search","senalo","sito","sociales","solamente","stgo","sugerido","the","unas","unirme","usando","usara","use","user","usurio","via","video","viendo","wiki","with"],"size":[17,17,13,10,8,7,7,7,6,5,5,4,4,4,4,4,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]},"pars":{"font":"Open Sans","fontweight":400,"padding":1,"rotmin":-30,"rotmax":30,"scale":"log","spiral":"archimedean"}},"evals":[]}</script><!--/html_preserve-->

# Conclusión!

Espero que esto incentive una respuesta :D y partamos reuniéndonos 
para programar unas presentaciones, discusiones, etc, les parece?

Nos vemos!

---
title: "readme.R"
author: "Joshua K"
date: "Tue Jul 21 00:18:26 2015"
---
