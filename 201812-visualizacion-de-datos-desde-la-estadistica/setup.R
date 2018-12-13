library(tidyverse)
library(ggrepel)

knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  echo = FALSE,
  dev = "svg",
  cache = TRUE,
  fig.path = "imgs/",
  fig.width = 10,
  fig.height = 6
)

theme_set(
  jbkmisc::theme_jbk(base_family = "Roboto Condensed") +
    theme(
      panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
      plot.background = element_rect(fill = "transparent",colour = NA)  
    )
  ) 
  
main_color <- "#272822"

update_geom_defaults("line",  list(colour = main_color, size = 1.2))
update_geom_defaults("point", list(colour = main_color, size = 3))
update_geom_defaults("bar",   list(fill = main_color))
update_geom_defaults("text",  list(size = 4, colour = "#666666"))

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