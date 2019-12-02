###making trimmed-down versions of the spreadsheets to improve performance of site

library(tidyverse)
library(janitor)

loc <- "data/final"

constellation_background <- clean_names(read_csv(file.path(loc, "all_protests_major_tags.csv")))

constellation_background_condensed <- constellation_background %>% filter(week >= as.Date("2017-01-16")) %>% group_by(week, location_id) %>% 
  summarise(latitude = max(latitude), longitude = max(longitude), collective_bargaining = sum(collective_bargaining),
  education = sum(education), environment = sum(environment), executive = sum(executive), guns = sum(guns), 
  healthcare = sum(healthcare), immigration = sum(immigration), police = sum(police), race_confed = sum(race_confed),
  supreme_court = sum(supreme_court), women = sum(women), other = sum(other))

write_csv(constellation_background_condensed, file.path(loc, "constellation_background_condensed.csv"))
