library(tidyverse)
library(meetupr)

# my ultra secret api key :)
Sys.setenv(MEETUP_KEY = "0415f259076560493e4968452863")

members <- meetupr::get_members("useRchile")
events <- meetupr::get_events("useRchile", "past")

glimpse(members)
glimpse(events)

saveRDS(members, "data/members.rds")
saveRDS(events, "data/events.rds")


members <- readRDS("data/members.rds")
events <- readRDS("data/events.rds")
