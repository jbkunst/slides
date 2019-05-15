library(tidyverse)
library(scales)
library(hrbrthemes)

knitr::opts_chunk$set(
  echo = FALSE,
  fig.width = 10,
  fig.height = 6
)

theme_set(
  theme_ipsum(
    base_family = "Segoe UI",
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

theme_null <- function(...) {
  theme(...,
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())
}

knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  echo = FALSE,
  dev = "svg",
  cache = FALSE,
  cache.path = ".cache/"
)