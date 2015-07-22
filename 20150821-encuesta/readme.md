# Que nos dice la encuesta!



Me alegro que harta gente (sí, para mi 30 personas es muucho!) haya 
contestado el formulario.

Los datos los los puedes ver en este [lonk]()


```r
lonk_largo_que_me_dio_google_para_descargar_el_archivo <- "https://docs.google.com/spreadsheets/d/1WeZA4rnkoHgKvGkeJY0h2ec9__kzkKlF5cW89BR6p6s/pub?gid=675352622&single=true&output=csv"

# esto lo aprendí por http://stackoverflow.com/questions/19890633/r-produces-unsupported-url-scheme-error-when-getting-data-from-https-sites
data <- getURL(lonk_largo_que_me_dio_google_para_descargar_el_archivo, .encoding = "UTF-8", 
               ssl.verifypeer=0L, followlocation=1L)

encuesta <- read.csv(text=data)
encuesta <- tbl_df(encuesta)
```

conteo oficial!


```r
message(nrow(encuesta), " encuestados al ", Sys.Date(), Sys.time())
> 39 encuestados al 2015-07-222015-07-22 00:32:58

names(encuesta) <- c("tiempo", "actividad", "rubro", "agendar", "lugar", "dia")

encuesta$tiempo <- NULL

encuesta[1:5,]
```



actividad                                                   rubro                         agendar          lugar                                                                                                 dia         
----------------------------------------------------------  ----------------------------  ---------------  ----------------------------------------------------------------------------------------------------  ------------
De conversación, compartir ideas, (su rato ameno)           independiente                 Más de un mes?   Santiago Centro                                                                                       Sábado      
Aprendizaje con videotutoriales que apunten a un proyecto   Ciencias (Bio/Quim/Fis/Mat)   Más de un mes?   Soy de Perú.  Normalmente en casa o Universidad pero  con la laptop para cualquier videoconferencia   Sábado      
De aprendizaje como presentaciones                          Financiero/Negocios           2 semanas más    Santiago Centro                                                                                       Día Laboral 
De aprendizaje como presentaciones                          SIG                           1 semana más     Santiago Oriente (Providencia)                                                                        Día Laboral 
De conversación, compartir ideas, (su rato ameno)           Financiero/Negocios           1 semana más     Las Condes, Santiago                                                                                  Sábado      

Procederemos a reducir el texto de las respuesta:


```r
head(encuesta$actividad)
> [1] "De conversación, compartir ideas, (su rato ameno)"        
> [2] "Aprendizaje con videotutoriales que apunten a un proyecto"
> [3] "De aprendizaje como presentaciones"                       
> [4] "De aprendizaje como presentaciones"                       
> [5] "De conversación, compartir ideas, (su rato ameno)"        
> [6] "De aprendizaje como presentaciones"

ggplot(encuesta) +
  geom_bar(aes(actividad)) +
  coord_flip()
```

<img src="readme_files/figure-html/unnamed-chunk-4-1.png" title="" alt="" style="display: block; margin: auto;" />

```r

opciones <- c("mail", "proyecto", "conversación", "presentaciones")

for(opcion in opciones){
  # para cada palabra opción la buscamos en la respuesta y si está la reemplazamos ??grepl
  encuesta$actividad <- ifelse(grepl(opcion, encuesta$actividad), opcion, encuesta$actividad)
}

ggplot(encuesta) +
  geom_bar(aes(actividad), width = 0.3) + 
  coord_flip()
```

<img src="readme_files/figure-html/unnamed-chunk-4-2.png" title="" alt="" style="display: block; margin: auto;" />

```r


opciones <- c("Investigación", "Ciencias", "Financiero")
for(opcion in opciones)  encuesta$rubro <- ifelse(grepl(opcion, encuesta$rubro), opcion, encuesta$rubro)
encuesta$rubro <- ifelse(!encuesta$rubro %in% opciones, "Otra", encuesta$rubro)


opciones <- c("1 semana más", "2 semanas más", "Más de un mes?")
encuesta$agendar <- ifelse(!encuesta$agendar %in% opciones, "Otra", encuesta$agendar)


opciones <- c("Centro", "Oriente", "Otro")
for(opcion in opciones) encuesta$lugar <- ifelse(grepl(opcion, encuesta$lugar), opcion, encuesta$lugar)
encuesta$lugar <- ifelse(!encuesta$lugar %in% opciones, "Otra", encuesta$lugar)


opciones <- c("Día Laboral", "Sábado")
for(opcion in opciones) encuesta$dia <- ifelse(grepl(opcion, encuesta$dia), opcion, encuesta$dia)
encuesta$dia <- ifelse(!encuesta$dia %in% opciones, "Otra", encuesta$dia)
```

Teniendo un poco más limpio los datos, podemos hacer todos los gráficos


```r
ggplot(encuesta) + geom_bar(aes(rubro), width = 0.3)
```

<img src="readme_files/figure-html/unnamed-chunk-5-1.png" title="" alt="" style="display: block; margin: auto;" />

En la pregunta sobre el rubro es la única donde la alternativa **otra** toma relevancia. Esto 
habla sobre lo obvio: ho en día en cualquier sector hay datos que necesitan analizar.
   
Podemos hacer algo más entretenido para graficar todo, esto es un poco rebuscado pero sirve y 
no son muchas líneas de código. 


```r
encuesta2 <- encuesta %>% gather(pregunta, respuesta)
sample_n(encuesta2, 5)
```



pregunta   respuesta     
---------  --------------
dia        Sábado        
rubro      Investigación 
agendar    2 semanas más 
rubro      Financiero    
lugar      Otra          

```r
ggplot(encuesta2) +
  geom_bar(aes(respuesta), width = 0.5) + 
  facet_wrap(~ pregunta , scales = "free", nrow = 3) 
```

<img src="readme_files/figure-html/unnamed-chunk-7-1.png" title="" alt="" style="display: block; margin: auto;" />

Les tinca hacer gráficos con más de una variable?


```r
ggplot(encuesta) +
  geom_bar(aes(actividad, fill=rubro), width = 0.5) +
  scale_fill_hc()
```

<img src="readme_files/figure-html/unnamed-chunk-8-1.png" title="" alt="" style="display: block; margin: auto;" />

O mejor


```r
ggplot(encuesta) +
  geom_point(aes(actividad, rubro)) +
  ggtitle("Tipo de Actividad según Rubro")
```

<img src="readme_files/figure-html/unnamed-chunk-9-1.png" title="" alt="" style="display: block; margin: auto;" />

?! :P
Esto lo aprendí hace poco, el **jitter**.


```r
ggplot(encuesta) +
  geom_point(aes(actividad, rubro),
             position = position_jitter(w = 0.1, h = 0.1)) +
  ggtitle("Actividad y Rubro")
```

<img src="readme_files/figure-html/unnamed-chunk-10-1.png" title="" alt="" style="display: block; margin: auto;" />


---
title: "readme.R"
author: "Joshua K"
date: "Wed Jul 22 00:32:55 2015"
---
