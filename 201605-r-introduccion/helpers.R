knit_print.htmlwidget <- function(x, ..., options = NULL){
  
  options(pandoc.stack.size = "2048m")
  
  wdgtclass <- setdiff(class(x), "htmlwidget")[1]
  wdgtrndnm <- paste0(sample(letters, size = 7), collapse = "")
  wdgtfname <- sprintf("wdgts/%s_%s.html", wdgtclass, wdgtrndnm)
  
  htmlwidgets::saveWidget(x, file = wdgtfname, selfcontained = TRUE, background = "transparent")
  
  iframetxt <- sprintf("<iframe  frameBorder=\"0\" src=\"%s\" width=\"100%%\"   height=\"600\"></iframe>", wdgtfname)
  
  knitr::asis_output(iframetxt)
}
