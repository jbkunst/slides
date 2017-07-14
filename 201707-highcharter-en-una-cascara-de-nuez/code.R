#' ---
#' title: "Código ejemplo"
#' author: "Joshua Kunst</div>"
#' output:
#'   html_document:
#'     self_contained: false
#'     theme: paper
#'     keep_md: true
#'     lib_dir: index_files
#' mathjax: null
#' ---

knitr::opts_chunk$set(message = FALSE, warning = FALSE, echo = TRUE)
# setup -------------------------------------------------------------------
rm(list = ls())
library(highcharter)
library(tidyverse)
library(rvest)
library(stringr)
library(forcats)
library(janitor)
library(tidyr)
library(readr)
library(lubridate)

options(highcharter.theme = hc_theme_smpl(chart = list(backgroundColor = "transparent")))

# users -------------------------------------------------------------------
users <- read_tsv("useRchile_Member_List_on_07-14-17.xls.txt")
users <- clean_names(users)
glimpse(users)


users <- mutate(users, unio = mdy(se_unió_al_grupo_el))
filter(users, str_detect(nombre, "Joshua"))

users2 <- users %>% 
  group_by(unio) %>% 
  summarise(
    n = n(),
    quienes = as.character(tags$ul(map(nombre, tags$li)))
  )
  
hchart(users2, "line", hcaes(unio, n), name = "usuarios")

hc <- hchart(users2, "line", hcaes(unio, cumsum(n)), name = "usuarios")

hc <- hc %>% 
  hc_tooltip(
    useHTML = TRUE,
    pointFormat = "{point.y}<br>{point.quienes}"
  ) %>% 
  hc_chart(
    zoomType = "x"
  )

hc

meetups <- read_tsv("meetups.txt")
meetups <- meetups %>% 
  mutate(dt = paste(dia, mes, anio),
         id = row_number()) %>% 
  mutate(dt = as.Date(dt, "%d %b %Y"),
         dt = ymd(dt)) %>% 
  arrange(dt)

mtps <- meetups %>% 
  rowwise() %>% 
  do(l = 
    list(label = list(text = sprintf("#%s", .$id)),
         color = "#AFAFAF",
         width = 1,
         zIndex = 2,
         value = datetime_to_timestamp(.$dt))
  ) %>% 
  pull(l)

hc %>% 
  hc_xAxis(
    plotLines = mtps
  )



# hchart en data frames ---------------------------------------------------
data(mpg, package = "ggplot2")
glimpse(mpg)
mpg2 <- count(mpg, year, manufacturer)

hchart(mpg2, "column", hcaes(manufacturer, n, group = year))
hchart(mpg2, "bar", hcaes(manufacturer, n, group = year))

# temas -------------------------------------------------------------------
ventas <- 1:12 + rnorm(12) + 2
ventas2 <- ventas*.7 + rnorm(12, sd = 0.8)
hcf <- highchart() %>%
  hc_title(text = "Stock") %>% 
  hc_subtitle(text = "Fuente: Imaginación") %>% 
  hc_add_series(data = ventas, name = "Ventas")  %>% 
  hc_add_series(data = ventas2, name = "Compentencia") %>% 
  hc_xAxis(categories = month.abb) %>% 
  hc_yAxis(labels = list(format = "${value:.1f}")) %>% 
  hc_tooltip(valueDecimals = 2, valuePrefix = "$", table = TRUE)  

hcf

hcf %>% 
  hc_add_theme(hc_theme_ggplot2())

hc_add_theme(hcf, hc_theme_elementary())

hc_theme <-  hc_theme(
  colors = c('red', 'blue', 'yellow'),
  chart = list(
    style = list(
      fontFamily = "Comic Sans MS"
    ),
    colors = c('red', 'green', 'blue')
  )
)

hc_add_theme(hcf, hc_theme)



# ejemplo 1 ---------------------------------------------------------------
dtemp <- read_csv("http://graphics8.nytimes.com/newsgraphics/2016/01/01/weather/assets/santiago_chile.csv")
glimpse(dtemp)

# data <- mutate(data, dt = datetime_to_timestamp(date))

dtempgather <- dtemp %>% 
  select(date, starts_with("temp")) %>% 
  select_if(function(x) all(!is.na(x))) %>% 
  rename(temp_actual_max = temp_max,
         temp_actual_min = temp_min) %>% 
  gather(key, value, -date) %>% 
  mutate(key = str_replace(key, "temp_", ""),
         value = (value - 32)/1.8) %>% 
  separate(key, c("serie", "type"), sep = "_") %>% 
  spread(type, value) %>% 
  mutate(serie = factor(serie, levels = c("avg", "actual")),
         serie = fct_recode(serie, Normal = "avg", Observed = "actual"))

head(dtempgather)

hct <- hchart(dtempgather, "columnrange",
             hcaes(date, low = min, high = max, group = serie),
             color = c( "#C8B8B9", "#A90048")) 
hct

hct %>% 
  hc_size(height = 400) %>% 
  hc_plotOptions(series = list(borderWidth = 0, pointWidth = 4)) %>% 
  hc_tooltip(valueDecimals = 2) %>% 
  hc_yAxis(min = -10, max = 40, labels = list(format = "{value} °C"))


# ejemplo 3 ---------------------------------------------------------------
dsis <- read_html("http://ds.iris.edu/seismon/eventlist/index.phtml") %>% 
  html_node("table") %>% 
  html_table(fill = TRUE) 

dsis <- dsis %>% 
  clean_names() %>% 
  separate(date_and_time_utc, into = c("date", "time"), sep = " ") %>% 
  mutate(date = as.Date(date, format = "%d-%B-%Y")) %>%
  mutate(location_shows_interactive_map = str_to_title(location_shows_interactive_map))

hcmap(showInLegend = FALSE, download_map_data = TRUE) %>% 
  hc_add_series(data = dsis, type = "mapbubble",
                hcaes(lat = lat, lon = lon, z = mag,
                      name = location_shows_interactive_map),
                name = "Sismos", minSize = "1%", maxSize = "5%") %>% 
  hc_add_theme(hc_theme_db()) %>% 
  hc_mapNavigation(enabled = TRUE) 


# ejemplo 2 ---------------------------------------------------------------
dpop <- read_html("https://es.wikipedia.org/wiki/Regiones_de_Chile") %>% 
  html_table(fill = TRUE) %>% 
  last() %>% 
  clean_names() %>% 
  rename(id = cr_3_u_200b_4_u_200b) %>% 
  mutate(población_2020 = str_replace_all(población_2020, "\\s+", ""),
         población_2020 = as.numeric(población_2020)) %>% 
  filter(id != 16)

mapdata <- get_data_from_map(download_map_data("countries/cl/cl-all"))
glimpse(mapdata)
mapdata <- select(mapdata, `woe-name`, fips)
dpop <- dpop %>% 
  mutate(fips = str_pad(id, 2, pad = "0"),
         fips = paste0("CI", fips))

# mapdata <- mapdata %>% 
#   mutate(id = str_extract_all(fips, "\\d+"),
#          id = as.numeric(id))
# glimpse(mapdata)


hcmap(map = "countries/cl/cl-all", data = dpop, download_map_data = TRUE,
      joinBy = "fips", value = "población_2020", name = "población_2020")

dsis <- read_html("http://www.sismologia.cl/links/tabla.html") %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  file.path("http://www.sismologia.cl", .) %>% 
  map(function(x) tryCatch (read_html(x), error = function (e) NULL)) %>% 
  keep(negate(is.null)) %>% 
  map(html_table) %>% 
  map(first) %>% 
  map(spread_, "X1", "X2") %>% 
  reduce(bind_rows)

glimpse(dsis)

dsis <- dsis %>% 
  mutate(mag = str_extract(Magnitud, "\\d\\.\\d"),
         mag = as.numeric(mag))

hcmap("countries/cl/cl-all", showInLegend = FALSE) %>% 
  hc_add_series(data = dsis, type = "mapbubble",
                hcaes(lat = Latitud, lon = Longitud, z = mag,
                      name = Referencia),
                name = "Sismos", maxSize = '10%')



# avanzado? ---------------------------------------------------------------
data(gapminder, package = "gapminder")
glimpse(gapminder)

gp <- gapminder %>% 
  arrange(desc(year)) %>% 
  distinct(country, .keep_all = TRUE)
gp

hc <- hchart(gp, "point", hcaes(lifeExp, gdpPercap, size = pop, group = continent))

hc %>% 
  hc_yAxis(type = "logarithmic")

gp2 <- gapminder %>% 
  group_by(country) %>% 
  do(lifeexpdata = .$lifeExp)
gp2

gp <- left_join(gp, gp2)


hc <- hchart(gp, "point", hcaes(lifeExp, gdpPercap, size = pop, group = continent)) %>% 
  hc_yAxis(type = "logarithmic")

hc

minichart <- "function(){
var thiz = this;
console.log(thiz);
setTimeout(function() {
$('#minichart').highcharts({
title : {
text: ''
},
subtitle: {
text: thiz.country,
align: 'left'
},
exporting: {
enabled: false
},
legend: {
enabled : false
},
series: [{
animation: false,
color: thiz.color,
pointStart: 1952,
data: thiz.lifeexpdata
}],
yAxis: {
title: ''
},
xAxis: {
}
});
}, 0);
return '<div id=\"minichart\" style=\"width: 250px; height: 150px;\"></div>';
}                        
"

hc <- hc %>% 
  hc_tooltip(
    useHTML = TRUE,
    positioner = JS("function () { return { x: this.chart.plotLeft + 0, y: 0 }; }"),
    headerFormat = "{point.country}",
    pointFormatter = JS(minichart)
      )
hc

gp3 <- gapminder %>% 
  select(country, x = lifeExp, y = gdpPercap, z = pop) %>% 
  nest(-country) %>% 
  rename(sequence = data) %>% 
  mutate(sequence = map(sequence, list_parse))

gp <- left_join(gp, gp3)

hc <- hchart(gp, "point", hcaes(lifeExp, gdpPercap, size = pop, group = continent, name = country),
             dataLabels = list(enebled = TRUE)) %>% 
  hc_yAxis(type = "logarithmic")

hc

hc <- hc %>% 
  hc_motion(enabled = TRUE, series = 0:4, labels = sort(unique(gapminder$year)),
            loop = FALSE, autoPlay = TRUE, 
            updateInterval = 500, magnet = list(step =  1)) %>% 
  hc_xAxis(min = 20, max = 90) %>% 
  hc_yAxis(type = "logarithmic", min = 100, max = 100000) %>% 
  hc_tooltip(
    useHTML = TRUE,
    positioner = JS("function () { return { x: this.chart.plotLeft + 50, y: 50 }; }"),
    headerFormat = "{point.country}",
    pointFormatter = JS(minichart)
  ) %>% 
  hc_title(text = "Honorando a <i>Hans Rosling</i>", useHTML = TRUE)
hc
  

