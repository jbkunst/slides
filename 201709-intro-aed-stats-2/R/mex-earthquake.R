library(tidyverse)
library(stringr)
library(scales)

data <- read_delim("https://earthquake.usgs.gov/fdsnws/event/1/query?starttime=2017-09-01&endtime=2017-09-31&format=text", delim = "|")
glimpse(data)

data <- data %>% 
  filter(str_detect(EventLocationName, "Mexico"))

data %>% 
  count(EventLocationName)

glimpse(data)


ggplot(data) + 
  geom_histogram(aes(Magnitude)) 


ggplot(data) + 
  geom_linerange(aes(x = Time, ymin = 0, ymax = Magnitude))


ggplot(data) + 
  geom_linerange(aes(x = Time, ymin = 0, ymax = Magnitude), alpha = 0.25) +
  geom_point(aes(x = Time, y = Magnitude), alpha = 0.25)


# Visualizando el mensaje
data <- data %>% 
  mutate(KKI = 10^(1.5*Magnitude - 3) / 1000)

# http://www.english.ucla.edu/all-faculty/335-kelly-kiloton-index-of-earthquake-moment-magnitudes
ggplot(data) + 
  geom_linerange(aes(Time, ymin = 0, ymax = KKI), alpha = 0.5) +
  scale_y_continuous(label = comma)


