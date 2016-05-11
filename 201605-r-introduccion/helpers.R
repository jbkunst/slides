knit_print.htmlwidget <- function(x, ..., options = NULL){
  
  options(pandoc.stack.size = "2048m")
  
  wdgtclass <- setdiff(class(x), "htmlwidget")[1]
  wdgtrndnm <- paste0(sample(letters, size = 7), collapse = "")
  wdgtfname <- sprintf("%s_%s.html", wdgtclass, wdgtrndnm)
  
  htmlwidgets::saveWidget(x, file = wdgtfname, selfcontained = TRUE, background = "transparent")
  
  file.copy(wdgtfname, file.path("wdgts", wdgtfname))
  file.remove(wdgtfname)
  
  iframetxt <- sprintf("<iframe  frameBorder=\"0\" src=\"%s\" width=\"100%%\"   height=\"600\"></iframe>",
                       file.path("wdgts", wdgtfname))
  
  knitr::asis_output(iframetxt)
}
