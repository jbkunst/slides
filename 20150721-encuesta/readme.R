#' ---
#' title: "Que nos dice la encuesta!"
#' output: 
#'  html_document: 
#'    theme: journal
#'    toc: true
#'    keep_md: yes
#' ---

#' Antes de partir:   
#' - Una buena fuente para ver opciones de ggplot2 están en este este [post](http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/).   
#' - El script [aca](https://github.com/jbkunst/useRchile/blob/master/20150721-encuesta/readme.R) y link para verlo [aqui](https://rawgit.com/jbkunst/useRchile/master/20150721-encuesta/readme.html).   
#' - Esta no es la única (y quizás tampoco la mejor) forma de hacerlo, si alguen se anima con probar otro paquete para analizar los datos sería ideal.

#+ echo=FALSE, messages=FALSE, warning=FALSE
rm(list = ls())
options(stringsAsFactors = FALSE, digits = 3, knitr.table.format = "markdown")
suppressPackageStartupMessages(library("RCurl"))
suppressPackageStartupMessages(library("dplyr"))
library("tidyr")
library("ggplot2")
library("ggthemes")

# esto es para cuando hagamos de nuestro script un notebook
# (no es estrictamente necesario)
library("printr")
knitr::opts_chunk$set(collapse = TRUE, comment = ">", warning = FALSE,
                      fig.width = 12, fig.height = 6,
                      fig.align = "center", dpi = 72)

# Esto son estilos que escogeré para los gráficos de ggplot2
theme_set(theme_fivethirtyeight() +
            theme(strip.background = element_rect(fill = "#434348"),
                  strip.text = element_text(color = "#F0F0F0"),
                  plot.title = element_text(face = "plain")))
update_geom_defaults("line", list(colour = "#434348", size = 1.05))
update_geom_defaults("point", list(colour = "#434348", size = 3))
update_geom_defaults("bar", list(fill = "#7cb5ec"))
update_geom_defaults("text", list(size = 4, colour = "gray30"))


#' Me alegro que mucha gente (sí, para mi 40 personas es muucho!) contestó el formulario.
#' 
#' Los datos los los puedes ver en este [lonk](https://docs.google.com/spreadsheets/d/1WeZA4rnkoHgKvGkeJY0h2ec9__kzkKlF5cW89BR6p6s/pub?gid=675352622&single=true&output=csv).

lonk_largo_que_me_dio_google_para_descargar_el_archivo <- "https://docs.google.com/spreadsheets/d/1WeZA4rnkoHgKvGkeJY0h2ec9__kzkKlF5cW89BR6p6s/pub?gid=675352622&single=true&output=csv"

# esto lo aprendí por http://stackoverflow.com/questions/19890633/r-produces-unsupported-url-scheme-error-when-getting-data-from-https-sites
data <- getURL(lonk_largo_que_me_dio_google_para_descargar_el_archivo, .encoding = "UTF-8", 
               ssl.verifypeer = 0L, followlocation = 1L)

encuesta <- read.csv(text = data)
encuesta <- tbl_df(encuesta)

#' conteo oficial!
message(nrow(encuesta), " encuestados al ", Sys.Date(), Sys.time())

names(encuesta) <- c("tiempo", "actividad", "rubro", "agendar", "lugar", "dia")

encuesta$id <- seq(nrow(encuesta))
encuesta$tiempo <- NULL

encuesta[1:5,]

#' Procederemos a reducir el texto de las respuesta:
head(encuesta$actividad)

ggplot(encuesta) +
  geom_bar(aes(actividad)) +
  coord_flip() + 
  ggtitle("Actividad")
  

opciones <- c("Mail", "Proyecto", "Conversación", "Presentaciones")

for (opcion in opciones){
  # para cada palabra 'opción' la buscamos en la respuesta y si está la reemplazamos ??grepl
  encuesta$actividad <- ifelse(grepl(opcion, encuesta$actividad, ignore.case = TRUE),
                               opcion, encuesta$actividad)
}

#' Veamos que tal que ahora:

ggplot(encuesta) +
  geom_bar(aes(actividad), width = 0.3) + 
  coord_flip() +
  ggtitle("Actividad")


opciones <- c("Investigación", "Ciencias", "Financiero")
for (opcion in opciones)  encuesta$rubro <- ifelse(grepl(opcion, encuesta$rubro), opcion, encuesta$rubro)
encuesta$rubro <- ifelse(!encuesta$rubro %in% opciones, "Otra", encuesta$rubro)


opciones <- c("1 semana más", "2 semanas más", "Más de un mes?")
encuesta$agendar <- ifelse(!encuesta$agendar %in% opciones, "Otra", encuesta$agendar)


opciones <- c("Centro", "Oriente", "Otro")
for (opcion in opciones) encuesta$lugar <- ifelse(grepl(opcion, encuesta$lugar), opcion, encuesta$lugar)
encuesta$lugar <- ifelse(!encuesta$lugar %in% opciones, "Otra", encuesta$lugar)


opciones <- c("Día Laboral", "Sábado")
for (opcion in opciones) encuesta$dia <- ifelse(grepl(opcion, encuesta$dia), opcion, encuesta$dia)
encuesta$dia <- ifelse(!encuesta$dia %in% opciones, "Otra", encuesta$dia)

#' Teniendo un poco más limpio los datos, podemos hacer todos los gráficos

ggplot(encuesta) +
  geom_bar(aes(rubro), width = 0.3) +
  scale_y_continuous(labels = function (x) floor(x)) + # evita mostrar decimales en eje Y
  ggtitle("Rubro") 

#' En la pregunta sobre el rubro es la única donde la alternativa **otra** toma relevancia. Esto 
#' habla sobre lo obvio: hoy en día en cualquier sector hay datos que necesitan analizar.
#'    
#' Podemos hacer algo más entretenido para graficar todo, esto es un poco rebuscado pero sirve y 
#' no son muchas líneas de código. 
encuesta2 <- encuesta %>%
  gather(pregunta, respuesta, -id)

#' Comparar esto
encuesta %>% filter(id == 1)

#' Con esto
encuesta2 %>% filter(id == 1)

#+ fig.height=12
ggplot(encuesta2) +
  geom_bar(aes(respuesta), width = 0.5) + 
  facet_wrap(~pregunta, scales = "free", nrow = 3)  +
  scale_y_continuous(labels = function(x) floor(x)) + 
  ggtitle("Resutado Encuesta")


#' gráficos con más de una variable?

ggplot(encuesta) +
  geom_bar(aes(actividad, fill = rubro), width = 0.4) +
  scale_fill_hc(palette = "darkunica") + 
  ggtitle("Actividad y Rubro")

#' O mejor!:
ggplot(encuesta) +
  geom_point(aes(actividad, rubro)) +
  ggtitle("Tipo de Actividad según Rubro")

#' ?! :P
#' Esto lo aprendí hace poco, el **[jitter](http://docs.ggplot2.org/0.9.3/position_jitter.html)**.

ggplot(encuesta) +
  geom_point(aes(actividad, rubro),
             position = position_jitter(width = 0.1, height = 0.1)) +
  ggtitle("Actividad y Rubro")

#' Otra alternativa:
#' 
encuesta3 <- encuesta %>% 
  group_by(actividad, rubro) %>% 
  summarize(conteo = n())

head(encuesta3)

ggplot(encuesta3, aes(actividad, rubro)) +
  geom_tile(aes(fill = conteo)) + 
  geom_text(aes(label = conteo), color = "#F0F0F0") + 
  coord_equal() +
  ggtitle("Actividad y Rubro")

#' > Los de rubro **Investigación** tienden a preferir las **presentaciones**,
#' > a los del área **Financiera** los **proyectos**.
#'     
#' Finalmente mostrar que con ggplot2 puedes eventualmente hacer lo que quieras si te das el tiempo
#' de hacerlo ;)
#' Eso es una suerte de *parallel set*, cada linea es uno de nosotros. El tamaño de cada círculo
#' es proporcional al tamaño de la gente que repondió dicha alternativa.

encuesta_pos <- plyr::ldply(names(encuesta)[-ncol(encuesta)], function(col){
  respuesta <- unique(encuesta[[col]])
  df_aux <- data_frame(respuesta,
                       pregunta = col,
                       respuesta_value = (seq(length(respuesta)) - 0.5)/length(respuesta),
                       pregunta_value = which(names(encuesta) == col))
  df_aux
}) %>% tbl_df()

encuesta_pos <- encuesta_pos %>% 
  left_join(encuesta2 %>% 
              group_by(pregunta, respuesta) %>% 
              summarize(conteo = n()),
            by = c("pregunta", "respuesta"))

head(encuesta_pos)

encuestaxp <- encuesta2 %>% left_join(encuesta_pos, by =  c("pregunta", "respuesta")) 

set.seed(100)
d <- 0.5

ggplot(encuestaxp) + 
  geom_line(aes(pregunta_value, respuesta_value, group = id, color = id),
            alpha = 0.1, size = 2, color = "#7cb5ec",
            position = position_jitter(width = 0.03, height = 0.03)) +
  geom_point(data = encuesta_pos,
             aes(pregunta_value, respuesta_value, size = sqrt(conteo), color = pregunta),
             alpha = 0.25) +
  scale_size_area(max_size = 35) +
  scale_color_hc() + 
  geom_text(data = encuesta_pos, aes(pregunta_value, respuesta_value, label = respuesta)) +
  geom_text(data = encuesta_pos, aes(pregunta_value, 1, label = toupper(pregunta)), size = 4.2) +
  theme_map() + theme(legend.position = "none") +
  xlim((1 - d),(5 + d))




