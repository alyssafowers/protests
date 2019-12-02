library(tidyverse)
library(lubridate)
library(janitor)

loc <- "data/final"

protest <- clean_names(read_csv(file.path(loc, "all_protests_major_tags.csv")))

week_protest <- protest %>% group_by(week) %>% summarise(count = n()) %>% arrange(week) %>% mutate(perc = count/nrow(protest), week_id = 1:length(unique(week)))

write_csv(week_protest, file.path(loc, "protest_per_week_usa.csv"))
