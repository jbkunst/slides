library(highcharter)
library(shiny)

knitr::opts_chunk$set(message = FALSE, warning = FALSE, echo = TRUE)
options(highcharter.theme = hc_theme_smpl(chart = list(backgroundColor = "transparent")))


ico <- function(x = "tv", color = NULL) {
  # color <- "red"
  as.character(tags$span(icon(x), style = sprintf("color:%s", color)))
}

R <- function() {
  as.character(tags$span("R", style = "color:#2066B9;font-weight:500"))
}
