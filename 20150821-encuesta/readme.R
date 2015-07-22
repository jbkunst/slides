#' ---
#' title: "Que nos dice la encuesta!"
#' output: 
#'  html_document: 
#'    theme: journal
#'    toc: true
#'    keep_md: yes
#' ---

#+ echo=FALSE, messages=FALSE, warning=FALSE
options(stringsAsFactors = FALSE)
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
                  strip.text = element_text(color = "#F0F0F0")))
update_geom_defaults("line", list(colour = "#434348", size = 1.05))
update_geom_defaults("point", list(colour = "#434348", size = 3))
update_geom_defaults("bar", list(fill = "#7cb5ec"))
update_geom_defaults("text", list(size = 4, colour = "gray30"))


#' Me alegro que harta gente (sí, para mi 30 personas es muucho!) haya 
#' contestado el formulario.
#' 
#' Los datos los los puedes ver en este [lonk]()

lonk_largo_que_me_dio_google_para_descargar_el_archivo <- "https://docs.google.com/spreadsheets/d/1WeZA4rnkoHgKvGkeJY0h2ec9__kzkKlF5cW89BR6p6s/pub?gid=675352622&single=true&output=csv"

# esto lo aprendí por http://stackoverflow.com/questions/19890633/r-produces-unsupported-url-scheme-error-when-getting-data-from-https-sites
data <- getURL(lonk_largo_que_me_dio_google_para_descargar_el_archivo, .encoding = "UTF-8", 
               ssl.verifypeer=0L, followlocation=1L)

encuesta <- read.csv(text=data)
encuesta <- tbl_df(encuesta)

#' conteo oficial!
message(nrow(encuesta), " encuestados al ", Sys.Date(), Sys.time())

names(encuesta) <- c("tiempo", "actividad", "rubro", "agendar", "lugar", "dia")

encuesta$tiempo <- NULL

encuesta[1:5,]

#' Procederemos a reducir el texto de las respuesta:
head(encuesta$actividad)

ggplot(encuesta) +
  geom_bar(aes(actividad)) +
  coord_flip()

opciones <- c("mail", "proyecto", "conversación", "presentaciones")

for(opcion in opciones){
  # para cada palabra opción la buscamos en la respuesta y si está la reemplazamos ??grepl
  encuesta$actividad <- ifelse(grepl(opcion, encuesta$actividad), opcion, encuesta$actividad)
}

ggplot(encuesta) +
  geom_bar(aes(actividad), width = 0.3) + 
  coord_flip()


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

#' Teniendo un poco más limpio los datos, podemos hacer todos los gráficos

ggplot(encuesta) + geom_bar(aes(rubro), width = 0.3)

#' En la pregunta sobre el rubro es la única donde la alternativa **otra** toma relevancia. Esto 
#' habla sobre lo obvio: ho en día en cualquier sector hay datos que necesitan analizar.
#'    
#' Podemos hacer algo más entretenido para graficar todo, esto es un poco rebuscado pero sirve y 
#' no son muchas líneas de código. 
encuesta2 <- encuesta %>% gather(pregunta, respuesta)
sample_n(encuesta2, 5)

#+ fig.height=12
ggplot(encuesta2) +
  geom_bar(aes(respuesta), width = 0.5) + 
  facet_wrap(~ pregunta , scales = "free", nrow = 3) 


#' Les tinca hacer gráficos con más de una variable?

ggplot(encuesta) +
  geom_bar(aes(actividad, fill=rubro), width = 0.5) +
  scale_fill_hc()

#' O mejor
ggplot(encuesta) +
  geom_point(aes(actividad, rubro)) +
  ggtitle("Tipo de Actividad según Rubro")

#' ?! :P
#' Esto lo aprendí hace poco, el **jitter**.

ggplot(encuesta) +
  geom_point(aes(actividad, rubro),
             position = position_jitter(w = 0.1, h = 0.1)) +
  ggtitle("Actividad y Rubro")




