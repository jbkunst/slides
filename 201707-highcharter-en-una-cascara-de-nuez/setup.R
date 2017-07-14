library(highcharter)
library(shiny)
library(dplyr)
library(htmlwidgets)

knitr::opts_chunk$set(message = FALSE, warning = FALSE, echo = TRUE)
options(highcharter.theme = hc_theme_smpl(chart = list(backgroundColor = "transparent")))

# remove temporal widgets
file.remove(setdiff(dir(pattern = ".html"), "index.html"))

# helpers -----------------------------------------------------------------
ico <- function(x = "tv", color = NULL) {
  # color <- "red"
  as.character(tags$span(icon(x), style = sprintf("color:%s", color)))
}

R <- function() {
  as.character(tags$span("R", style = "color:#2066B9;font-weight:500"))
}

sw <- function(w, f) {
  saveWidget(widget = w, file = f, libdir = "index_files/", selfcontained = FALSE, background = "transparent")
}


# demo_0 ------------------------------------------------------------------
n <- 500
x <- seq(-pi/2, pi, length.out = n)
y <- 1 - cos(x) + rnorm(n) * 0.05 
e <- abs(rnorm(n) * 0.5) + 0.1
df <- data_frame(x, y, e)
hc <- hchart(df, "columnrange",
       hcaes(x, low = y - e, high = y + e, color = y)) %>% 
  hc_add_theme(
    hc_theme_merge(
      hc_theme_null(), hc_theme_flatdark()
    )
  ) %>% 
  hc_plotOptions(series = list(borderWidth = 0))
hc
sw(hc, "demo_0.html")
rm(n, x, y, e, df, hc)


# demo_1 ------------------------------------------------------------------
dpie <- data.frame(
  pie = c("Limon", "Berries", "Manzana", "Maracuya", "Frambuesas"),
  porcentaje = c(100, 85, 75, 64, 57)
)
dbar <- data.frame(
  bar = c("Radicales", "Singular", "Liguria", "Clinic", "Clan"),
  porcentaje = c(30, 28, 27, 12, 3)
)

hc <- hchart(dpie, "column", hcaes(x = pie, y = porcentaje), name = "Pies") %>%
  hc_add_series(dbar, "pie", hcaes(name = bar, y = porcentaje), name = "Bars") %>%
  # Options for each type of series
  hc_plotOptions(
    series = list(
      showInLegend = FALSE,
      tooltip = list(pointFormat = "<span style=\"color:{point.color}\">\u25CF</span> {series.name}: <b>{point.y}%</b><br/>")
    ),
    column = list(
      colorByPoint = TRUE
    ),
    pie = list(
      colorByPoint = TRUE, center = c('85%', '15%'),
      size = 140, dataLabels = list(enabled = FALSE)
    )) %>%
  # Axis
  hc_yAxis(
    title = list(text = "porcentaje de sabor"),
    labels = list(format = "{value}%"), max = 100
  ) %>% 
  # Titles and credits
  hc_title(
    text = "Un <i>bar</i> chart que describes mis <i>pies</i> favoritos
    incluyendo un <i>pie</i> chart describiendo mis <i>bars</i> favoritos",
    useHTML = TRUE
  ) %>%
  hc_subtitle(text = "En porcentaje de sabor y fantasticidad") %>% 
  hc_credits(
    enabled = TRUE, text = "Un chiste <i>mal</i> traducido de HIMYM",
    href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng", 
    useHTML = TRUE,
    style = list(fontSize = "12px")
  )
hc
sw(hc, "demo_1.html")
rm(dpie, dbar, hc)
