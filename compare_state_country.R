library(tidyverse)
library(janitor)
library(muckrakr)
library(lubridate)

####################################
######### Get data sources #########
####################################

loc <- "data/working"

protests <- clean_names(read_csv(file.path(loc, "protest_addtltags_nov1_withstate.csv")))
protests$month <- month(protests$date)

nation_month_topic <- clean_names(read_csv(file.path(loc, "month_top_topics_long.csv")))


####################################
### summaries of topics and tags ###
####################################

topics <- unique(nation_month_topic$topic)

nation_total_topic <- nation_month_topic %>% group_by(topic) %>% 
  summarise(protests = sum(value)) %>% arrange(desc(protests))

###summarize tags--whole country

tag <- untangle(protests, "tags", pattern = ";")

tag_start <- which(colnames(tag) == "month") + 1

all_tag_frequency <- tag[,tag_start:ncol(tag)] %>% 
  lapply(sum) %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>% filter(V1>0) %>%
  arrange(desc(V1)) %>% mutate(perc = round(V1/nrow(tag),4))

###summarize tags--Florida

tag_fl <- untangle(filter(protests, state == "FL"), "tags", pattern = ";")

fl_tag_frequency <- tag_fl[,tag_start:ncol(tag_fl)] %>% 
  lapply(sum) %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>% filter(V1>0) %>%
  arrange(desc(V1)) %>% mutate(perc = round(V1/nrow(tag_fl),4))

#compare

fl_nat <- rbind(mutate(fl_tag_frequency, place = "FL"), mutate(all_tag_frequency, place = "US"))
ggplot(fl_nat, aes(x=rowname, y = perc, fill = place))+geom_bar(stat="identity", position = "dodge")

fl_nat_wide <- fl_nat %>% pivot_wider(id_cols = rowname, names_from = place, values_from = perc)
fl_nat_wide[is.na(fl_nat_wide$FL),"FL"] <- 0
fl_nat_wide <- mutate(fl_nat_wide, fl_minus_us = FL - US)
fl_nat_wide <- mutate(fl_nat_wide, z = fl_minus_us/sd(fl_nat_wide$fl_minus_us))

View(fl_nat_wide %>% filter(z >= 2 | z <= -2))

##pulls out same outliers with/without filters (as expected)

##need some way to determine what should be included/what shouldn't--
#do i want National Walkout Day if I have guns, for instance? could just
#stick to the big-heading topics


fl_nat_wide_filter <- filter(fl_nat_wide, US >= .01 | FL >= .01)
View(fl_nat_wide_filter %>% filter(z >= 2 | z <= -2))

View(fl_nat_wide %>% filter(z >= 2 | z <= -2, rowname %in% topics))



ggplot(fl_nat_wide, aes(x=US, y = FL))+geom_point(alpha = .5)
ggplot(fl_nat_wide_filter, aes(x=US, y = FL))+geom_point(alpha = .5)

fl_nat_wide_filter <- mutate(fl_nat_wide_filter, us_minus_fl = US - FL)

sd(fl_nat_wide$us_minus_fl)
summary(fl_nat_wide)
ggplot(fl_nat_wide_filter, aes(x=us_minus_fl))+geom_histogram()
