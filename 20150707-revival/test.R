library(rcdimple)
demo(dimple)

ex_data <- read.delim("http://pmsi-alignalytics.github.io/dimple/data/example_data.tsv")
tbl_df(ex_data)

dimple(
 UnitSales ~ Month,
 data = subset(ex_data, Owner %in% c("Aperture","Black Mesa")),
 type = "area"
) %>%
 xAxis(orderRule = "Date")

ex_data %>%
 subset(Owner %in% c("Aperture","Black Mesa")) %>%
 dimple(
     UnitSales ~ Month,
     type = "line"
   ) %>%
 xAxis(orderRule = "Date")

library(metricsgraphics)

set.seed(1492)
dat <- data.frame(date=seq(as.Date("2014-01-01"),
                           as.Date("2014-01-31"),
                           by="1 day"),
                  value=rnorm(n=31, mean=0, sd=2))

str(dat)
dat %>%
  mjs_plot(x=date, y=value) %>%
  mjs_line() %>%
  mjs_axis_x(xax_format = "date")

str(t)
el <- t %>% 
  mjs_plot(x=dat, y=integrantes) %>% 
  mjs_line() %>%
  mjs_axis_x(xax_format = "date")
el
