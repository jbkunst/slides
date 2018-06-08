rm(list = ls())
library(tidyverse)

knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  echo = TRUE,
  dev = "svg",
  cache = TRUE
  )

R <- function() {
  as.character(htmltools::tags$span("R", style = "color:#2066B9;font-weight:500"))
}


ico <- function(x = "tv", color = NULL) {
  # color <- "red"
  as.character(htmltools::tags$span(shiny::icon(x), style = sprintf("color:%s", color)))
}
