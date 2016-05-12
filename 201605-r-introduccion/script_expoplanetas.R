# install.packages("dplyr") # manipulacion de datos
library("dplyr")


# lectura de datos
data <- read.csv("exoplanet.eu_catalog.csv")



## (meta) Información de los datos:

# dimensiones
dim(data)

# variables
names(data)

# str: estructura
str(data)

# Obtener una columna en particular
# data$nombre_variable
data$mass

# asignarlo en otra variable (no necesario)
masa <- data$mass

str(masa)

summary(masa)

# NAs?
is.na(NA)
is.na(c(1, 2))
is.na(c(1, NA, 2))

!TRUE

# limpiando los datos
data <- select(data, star_name, publication_status, mass, radius, orbital_period)
data <- filter(data, !is.na(mass) & !is.na(radius) & !is.na(orbital_period))

str(data)

summary(data)

boxplot(data$mass, horizontal = TRUE)
boxplot(data$mass, horizontal = TRUE, outline = FALSE) # super poco atractivo

boxplot(data$radius, horizontal = TRUE)

boxplot(data$orbital_period, horizontal = TRUE, outline = FALSE)

symbols(data$mass, data$orbital_period, circles = data$radius)


symbols(data$mass, data$orbital_period, circles = data$radius, inches = 0.1)


data <- filter(data, mass <= 25 & orbital_period <= 20)

symbols(data$mass, data$orbital_period, circles = data$radius, inches = 0.1)

symbols(data$mass, data$orbital_period, circles = data$radius, inches = 0.1,
        xlab = "Masa", ylab = "Periodo Órbita", fg = "gray50", bg = "gray30",
        main = "Ditribucion de planetas con Masa, Periodo y Radio conocido")


library("highcharter")
str(data)

hchart(data$mass)

hchart(data$publication_status, type = "pie")

hchart(data$publication_status, type = "bar")

hc <- highchart()
hc <- hc_add_serie_scatter(hc, x = data$mass, y = data$orbital_period, z = data$radius, label = data$star_name,
                           color = data$publication_status, minSize = 5, maxSize = 10)
hc <- hc_chart(hc, zoomType = "xy")

hc

