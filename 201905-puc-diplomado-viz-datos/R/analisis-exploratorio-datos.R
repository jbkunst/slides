# Cargar paquetes ---------------------------------------------------------
# tidyverse para manipular/graficar
library(tidyverse)

# lubridate para manipular fechas/horas
library(lubridate)

# Para un poco de magia
library(plotly)


# Carga y limpieza de datos -----------------------------------------------
data <- read_delim(url("https://tinyurl.com/data-metro-scl"), delim = ";")
data

head(data)

View(data)

# Esto elimina todas los paraderos que contentan algo de la forma numero-numero: 
# L-30-51-80-SN
data <- data %>% 
  filter(!str_detect(paraderosubida, "[0-9]+-[0-9]"))

# Todas las estaciones distintas de "-"
data <- data %>% 
  filter(paraderosubida != "-")

# Y consideramos solamente los registros que 
data <- data %>% 
  filter(hour(mediahora) > 0)

# Renombramos paraderosubida a estacion pues consideramos solamente
# estaciones de metro
data <- data %>% 
  rename(estacion = paraderosubida)

# Explorar ----------------------------------------------------------------

glimpse(data)

# Notar/Preguntarse:
# - Cantidad de observaciones (filas). Te hace sentido en un principio?
# - Canidad de columnas, y que tipo
# - Los nombres hacen sentido con el contenido?
# - Luego el contenido hace sentido? Tiene sentido que ingresen 2.6 personas entre las 5:30 y 6:00? 

data

# Explorar es mirar, preguntar, responder.

# P: Si tuvieras que hacer un primer gráfico, cual sería?
# R: 

# P: Que (crees) puedes responder con la información que poseemos?
# R:

# Podríamos hacer un gráfico de subidad y media hora
ggplot(data) +
  geom_line(aes(subidas_laboral_promedio, mediahora))

# Mejor puntos (no se)
ggplot(data) +
  geom_point(aes(subidas_laboral_promedio, mediahora))

# P: Que vez? Puntos, líneas?
# R:

# P: Que representa cada punto?
# R:

# P: Que implica que un punto esté más a la derecha?
#    Que significa que un punto se mueva a la derecha?# 
# R:

# P: Que tiene de bueno el gráfico?
# R: 

# P: Que tiene de malo el gráfico?
# R: 

# Equivocarse es bueno!!!!! (Mientras nadie te pille)
ggplot(data) +
  geom_point(aes(x = mediahora, y = subidas_laboral_promedio))

# P: Que representa cada punto?
# R:

# P: Que implica que un punto esté más a la derecha?
# R:

# P: Puedes *ver* cuantos *puntos* menor de mil hay a las 8:00?
# R:

# P: Como lo respondo?
# R:
data %>% 
  filter(mediahora == hm("8:00"), subidas_laboral_promedio <=1000)

data %>% 
  filter(mediahora == hm("8:00"), subidas_laboral_promedio <=1000) %>% 
  count()

# P: El resultado, sigue siendo puntos?
# R:

# P: Que significa el resultado obtenido?
# R:

# Imaginate tres puntos:
# - (2, 1)
# - (1, 2)
# - (1, 2)
# Ahora dibujalos
# Ahora imagina que agregas 20 puntos iguales (1, 2)
# Dibujalos

df <- data_frame(
  la_x = c(2, rep(1, 10)),
  la_y = c(1, rep(2, 10))
  )
df

ggplot(df) +
  geom_point(aes(x = la_x, y = la_y), size = 10)

ggplot(df) +
  geom_point(aes(x = la_x, y = la_y), size = 10, alpha = 0.2)


# Apliquemos lo mismo a lo anterior
ggplot(data) +
  geom_point(aes(x = mediahora, y = subidas_laboral_promedio), alpha = 0.02, size = 2)


# Podemos mejorar agregando un suaviasmiento con geom_smooth
# Esto intenta hacer una linea que pase cerca de los puntos
ggplot(data) +
  geom_point(aes(x = mediahora, y = subidas_laboral_promedio), alpha = 0.02, size = 2) +
  geom_smooth(aes(x = mediahora, y = subidas_laboral_promedio))


# Coloriemos los puntos por estacion
ggplot(data) +
  geom_point(aes(x = mediahora, y = subidas_laboral_promedio, color = estacion),
             alpha = 1, size = 2) +
  geom_smooth(aes(x = mediahora, y = subidas_laboral_promedio))

# Muy bien!
# Podemos esconder la legenda con theme(legend.position = "none")
ggplot(data) +
  geom_point(aes(x = mediahora, y = subidas_laboral_promedio, color = estacion),
             alpha = 1, size = 2) +
  geom_smooth(aes(x = mediahora, y = subidas_laboral_promedio)) +
  theme(legend.position = "none")

# P: Puedes identificar la estacion/un punto, sin una leyenda?
# R

# P: Que pasa en este caso con muchas estaciones?

# P: Tiene sentido colorear en este caso?
# R:

# P: Identifica la hora del peak de ingresos
data %>% 
  filter(subidas_laboral_promedio == max(subidas_laboral_promedio))

data %>% 
  arrange(desc(subidas_laboral_promedio))

# P: Identifica el peak de ingresos en la mañana (AM) sin filtrar :D
# R:


# Los graficos se pueden modificar e ir guardando las modificaciones 
# continuamente
p <- ggplot(data) +
  geom_point(aes(x = mediahora, y = subidas_laboral_promedio, color = estacion),
             alpha = 1, size = 2) +
  geom_smooth(aes(x = mediahora, y = subidas_laboral_promedio)) +
  scale_color_viridis_d() +
  theme(legend.position = "none")

p

# Pudes guardar un gráfico ggplot como un objeto el cual puedes seguir modificadno
p <- p +
  labs(
    x = "Hora",
    title = "Mi super titulo"
  )

p

# Repito: Identifica el peak de ingresos en la mañana (AM) sin filtrar :D
# R:
ggplotly(p)



# Dividir -----------------------------------------------------------------
data_alcantara <- data %>% 
  filter(estacion == "ALCANTARA")

glimpse(data_alcantara)

View(data_alcantara)

ggplot(data_alcantara) +
  geom_point(aes(x = mediahora, y = subidas_laboral_promedio),
             alpha = 1, size = 2) 

ggplot(data_alcantara) +
  geom_line(aes(x = mediahora, y = subidas_laboral_promedio)) 

data_uchile <- data %>% 
  filter(estacion == "UNIVERSIDAD DE CHILE")

ggplot(data_uchile) +
  geom_line(aes(x = mediahora, y = subidas_laboral_promedio)) 



# Quizás algunas son relevantes para analisar
estaciones <- c("ALCANTARA", "UNIVERSIDAD DE CHILE", "PLAZA MAIPU",
                "BELLAS ARTES", "ESCUELA MILITAR", "NUBLE", 
                "PLAZA DE PUENTE ALTO")

data_estaciones <- data %>% 
  filter(estacion %in% estaciones)

data_estaciones %>%
  count(estacion)

# P: Grafico de lineas por estacion
p2 <- ggplot(data_estaciones) +
  geom_line(aes(x = mediahora, y = subidas_laboral_promedio, color = estacion), size = 2) +
  scale_color_viridis_d(option = "B")
p2
 
# Compare el grafico anterior con el siguiente
p2 +
  facet_wrap(vars(estacion), scales = "free")


# P: Cuales son las consideraciones de tener sobre una y otra
# R:


# P: Volviendo a los datos, que opinas sobre las formas 
# R:




# Un paso más allá: crear variables de interés ----------------------------

# P: Genere una tabla que resuma por estación la cantidad total de subidas
# 
data_subidas <- data %>% 
  group_by(estacion) %>% 
  summarise(subidas_total = sum(subidas_laboral_promedio)) 

data_subidas

# Perfecto.
# P: Ahora genere una variable que indique si la subidas totales
# de cada estacion es mayor o menor que el promedio de subidas totales
data_subidas <- data_subidas %>% 
  mutate(tipo_1 = ifelse(subidas_total > mean(subidas_total), "congestion", "expedito"))


# P: Explore y ve si hace sentido a la intuición
# R:
View(data_subidas)


# P: A continuación, en la tabla original cree una variable que indique si el horario
# es am o pm
data_horario <- data %>% 
  mutate(horario = ifelse(mediahora < hm("12:00"), "am", "pm")) 
data_horario

# P: Agrupe por estacion/horario y sume  las subidas
data_horario <- data_horario %>% 
  group_by(estacion, horario) %>% 
  summarise(subidas_total_horario = sum(subidas_laboral_promedio)) %>% 
  ungroup()
data_horario


# P: Compare data_subidas, con data_horario
# R:
data_subidas
data_horario

p3 <- ggplot(data_horario) + 
  geom_line(aes(horario, subidas_total_horario, group = estacion)) +
  theme(legend.position = "none")

p3

ggplotly(p3)

# P: Considere solamente los "am" 
# R:
data_horario <- data_horario %>% 
  filter(horario == "am")
data_horario

# P: Usando left join junte data_subidas data_horario
# R:
data_resumen <- left_join(data_subidas, data_horario)

# P: Ahora genere un nueva variable para determinar si las subidas en am
#    son mayor o no respecto a la mitad de las subidas del dia
data_resumen <- data_resumen %>% 
  mutate(tipo_2 = ifelse(subidas_total_horario > subidas_total/2, "dormitorio", "laboral"))

# P: Explore :/
# R:
View(data_resumen)

# P: Simplifique la tabla. Seleccione columnas de _resultado_
# R:
data_resumen <- data_resumen %>% 
  select(estacion, tipo_1, tipo_2)
data_resumen

# P: Usando left_joint, adjunte los tipos a la data primaria
data <- left_join(data, data_resumen)
data



# Un resultado: -----------------------------------------------------------
p4 <- ggplot(data) +
  geom_line(aes(x = mediahora, y = subidas_laboral_promedio, group = estacion),
            alpha = 0.1, size = 1) +
  geom_smooth(aes(x = mediahora, y = subidas_laboral_promedio), color = "red") +
  facet_wrap(tipo_2 ~ tipo_1) +
  theme_minimal() +
  labs(
    x = "Horario",
    y = "Subidas",
    title = "Comportamiento de estaciones de acuerdo a los grupos"
  )

p4

ggplotly(p4)

