library(highcharter)
library(shiny)
library(dplyr)

knitr::opts_chunk$set(message = FALSE, warning = FALSE, echo = TRUE)
options(highcharter.theme = hc_theme_smpl(chart = list(backgroundColor = "transparent")))

# remove temporal widgets
file.remove(setdiff(dir(pattern = ".html"), "index.html"))

# helpers for icons
ico <- function(x = "tv", color = NULL) {
  # color <- "red"
  as.character(tags$span(icon(x), style = sprintf("color:%s", color)))
}

R <- function() {
  as.character(tags$span("R", style = "color:#2066B9;font-weight:500"))
}


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
htmlwidgets::saveWidget(hc, "demo_0.html")
rm(n, x, y, e, df, hc)
