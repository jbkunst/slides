library(highcharter)
library(shiny)
library(tidyverse)
library(htmlwidgets)

library(hrbrthemes)

knitr::opts_chunk$set(
  dev = "svg",
  fig.width = 10,
  fig.height = 6,
  message = FALSE,
  warning = FALSE
)

theme_set(
  theme_ipsum(
    base_family = "Roboto",
    plot_title_face = "plain",
    plot_title_size = "plain",
    plot_margin = margin(5, 5, 5, 5)) +
    theme(
      title = element_text(colour = "#444444"),
      panel.grid.major = element_line(colour = "grey90"),
      panel.grid.minor = element_line(colour = "grey90"),
      legend.position = "bottom"
    )
)

options(highcharter.theme = hc_theme_smpl(chart = list(backgroundColor = "transparent")))

# helpers -----------------------------------------------------------------
ico <- function(x = "tv", color = NULL, ...) {
  # color <- "red"
  as.character(tags$span(icon(x, ...), style = sprintf("color:%s", color)))
}

R <- function() {
  as.character(tags$span("R", style = "color:#2066B9;font-weight:500"))
}

sw <- function(w, f) {
  saveWidget(widget = w, file = f, libdir = "index_files/", selfcontained = FALSE, background = "transparent")
}



# highcharter -------------------------------------------------------------
library(highcharter)

fntfmly <- '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol"' 

options(
  highcharter.theme =
    hc_theme_smpl(
      chart = list(
        style = list(
          fontFamily = fntfmly
        )
      ),
      xAxis = list(labels = list(style = list(fontSize = 13))),
      yAxis = list(labels = list(style = list(fontSize = 13))),
      title = list(style = list(fontFamily = fntfmly)),
      subtitle = list(style = list(fontFamily = fntfmly)),
      credits = list(style = list(fontFamily = fntfmly))
    )
)

hc_opts <- getOption("highcharter.chart")
hc_opts$chart <- list(style = list(fontFamily = fntfmly))
