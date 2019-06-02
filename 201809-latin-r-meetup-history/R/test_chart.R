library(highcharter)
library(htmltools)

members <- members %>%
  mutate(photo_link = ifelse(is.na(photo_link), "https://www.qualiscare.com/wp-content/uploads/2017/08/default-user-300x300.png", photo_link))

brks <- ymd(
  "20100101",
  "20150101",
  "20170301",
  "20200101"
)
brks

lbls <- c("El comienzo", "El fin?", "El Renacimiento")

members <- members %>% 
  mutate(joined2cat = cut(as.Date(joined2), breaks = brks, labels = lbls))

# memebers
tooltip <- c("name", "country") %>%
  map(function(x){
    tags$tr(
      tags$th(str_replace_all(str_to_title(x), "_", " ")),
      tags$td(paste0("{point.", x, "}"))
    )
  }) %>%
  do.call(tagList, .) %>%
  tags$table() %>% 
  tagList(
    tags$img(src = "{point.photo_link}", width = "200px")
  ) %>%
  as.character()

plns <- events %>%
  # sample_n(2) %>%
  # mutate(local_date = highcharter::datetime_to_timestamp(as.Date(local_date))) %>%
  select(value = local_date) %>%
  pull() %>% 
  # map(function(x) sprintf("Date.UTC(%s,%s,%s)", year(x), month(x) - 1, day(x))) %>% 
  # map(function(x) list(value = JS(x))) %>% 
  map(function(x) {
    list(
      value = highcharter::datetime_to_timestamp(x), 
      label = list(text = ""),
      color = "#DCDCDC",
      width = 4,
      zIndex = -1
    )
  }) 

members %>% 
  select(joined2, nacum, photo_link, name, country, joined2cat) %>% 
  hchart(
    "line", hcaes(as.Date(joined2), nacum, group = joined2cat),
    visible = c(TRUE, FALSE, FALSE),
    lineWidth = 10
  ) %>%
  hc_tooltip(
    useHTML = TRUE,
    backgroundColor = "white",
    borderColor = "transparent",
    headerFormat = "",
    shadow = FALSE,
    style = list(fontSize = "1.0em", fontWeight = "normal"),
    positioner = JS("function () { return { x: this.chart.plotLeft + 15, y: this.chart.plotTop + 0 }; }"),
    shape = "square",
    pointFormat = tooltip
  ) %>%
  hc_add_theme(hc_theme_smpl()) %>% 
  hc_xAxis(
    type = "datetime",
    minTickInterval = 24 * 3600 * 1000 * 31 * 12,
    pointStart = JS("Date.UTC(2013, 0, 1)"),
    title = list(text = ""),
    labels = list(style = list(fontSize = 20)),
    dateTimeLabelFormats = list(year = "%Y", month = "%Y %b"),
    # plotLines = list(
    #   list(
    #     label = list(text = "This is a plotLine"),
    #     color = "#FF0000",
    #     width = 5,
    #     value = datetime_to_timestamp(as.Date('2014-01-01', tz = 'UTC'))
    #   ) 
    plotLines = plns
  ) %>% 
  hc_yAxis(
    title = list(text = ""),
    labels = list(style = list(fontSize = 20))
  ) %>% 
  hc_size(height = 500)







