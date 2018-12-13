library(tidyverse)
library(rvest)

html <- read_html("http://0xrgb.com/")

colors <- html %>% 
  html_nodes(".palette > div") 

hexcol <- colors %>% 
  html_nodes(".rgb") %>% 
  html_text()

namecol <- colors %>% 
  html_nodes(".name") %>% 
  html_text() %>% 
  str_to_lower() %>% 
  str_replace("\\s+", "-")


# example:
# .inverse { background-color: #23373B }

bkgd <- map2_chr(namecol, hexcol, ~ str_glue(".bkgd-{ name } {{ background-color: { hex } }}", name = .x, hex = .y))

write_lines(bkgd, "201812-visualizacion-de-datos-desde-la-estadistica/assets/bkgd.css")

