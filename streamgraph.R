#### load packages

library(tidyverse)
library(janitor)
library(stringr)
library(muckrakr)
library(lubridate)

#### load data

loc <- "data/working"

protest <- clean_names(read_csv(file.path(loc, "protest_additional_tags_nov1.csv")))

protest <- mutate(protest, week=as.Date(cut(date, breaks="week", starts.on.monday=TRUE)))

protest$month_yr <- format(as.Date(protest$date), "%Y-%m")

protest$month_yr_bw <- format(as.Date(protest$week), "%Y-%m")

all_tag <- untangle(protest, "tags", pattern = ";")

#### get down to topics (not stances--all stance tags begin with "for" or "against")

tag_topic <- all_tag[,!(str_detect(colnames(all_tag), "^for") | str_detect(colnames(all_tag), "^against"))]

topic_start <- which(colnames(tag_topic) == "healthcare")

#had decided that "civil rights" was too broad (and created some more specific
#subcategories as a part of preprocessing), so taking that one out, as well as 
#"other":

tag_topic <- tag_topic[,colnames(tag_topic)!="civil_rights"&colnames(tag_topic) != "other"]

#### what topics to include

topic_tag_frequency <- tag_topic[,topic_start:ncol(tag_topic)] %>% 
  lapply(sum) %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>% filter(V1>0) %>%
  arrange(desc(V1)) %>% mutate(perc = round(V1/nrow(tag_topic),4))

ggplot(topic_tag_frequency, aes(x = V1)) + geom_histogram()

summary(topic_tag_frequency$V1)

#what % of all protests do protests with the top 10 topics cover? (76%)

top_10_topics <- topic_tag_frequency[1:10, "rowname"]

sum(apply(tag_topic[top_10_topics], MARGIN = 1, sum) > 0)/nrow(tag_topic)

#top 5? (60%)

top_5_topics <- topic_tag_frequency[1:5, "rowname"]

sum(apply(tag_topic[top_5_topics], MARGIN = 1, sum) > 0)/nrow(tag_topic)

###which topics were the #1 each week?

top_week <- data.frame(rowname = character(), V1 = integer(), perc = numeric())

weeks <- unique(protest$week)

for(i in 1:length(weeks)){
  week_list <- tag_topic[tag_topic$week == weeks[i],topic_start:ncol(tag_topic)] %>% 
    lapply(sum) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column() %>% filter(V1>0) %>%
    arrange(desc(V1)) %>% mutate(perc = round(V1/nrow(tag_topic),4))
  
  top_week <- rbind(top_week, week_list[1,])  
  }

View(top_week %>% group_by(rowname) %>% summarise(count = sum(V1), max = max(V1)) %>% 
  arrange(desc(count)))

#which of these are not already in the top 10 topics?
unique(top_week$rowname[!top_week$rowname %in% top_10_topics])
#which of the top 10 topics are not included in these?
unique(top_10_topics[!top_10_topics %in% top_week$rowname])

#which of these are not already in the top 5 topics?
unique(top_week$rowname[!top_week$rowname %in% top_5_topics])
#which of the top 5 topics are not included in these?
unique(top_5_topics[!top_5_topics %in% top_week$rowname])


#by month (by week is going to be too chaotic for bump chart or streamgraph)

months <- unique(tag_topic$month_yr)

top_month <- data.frame(rowname = character(), V1 = integer(), perc = numeric())

for(i in 1:length(months)){
  month_list <- tag_topic[tag_topic$week == weeks[i],topic_start:ncol(tag_topic)] %>% 
    lapply(sum) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column() %>% filter(V1>0) %>%
    arrange(desc(V1)) %>% mutate(perc = round(V1/nrow(tag_topic),4))
  
  top_month <- rbind(top_month, month_list[1:2,])  
}

View(top_month %>% group_by(rowname) %>% summarise(count = sum(V1), max = max(V1)) %>% 
  arrange(desc(count)))

unique(top_week$rowname[!top_week$rowname %in% top_10_topics])

#looking at month by "month where week started," to keep whole weeks together

months <- unique(tag_topic$month_yr_bw)

top_month <- data.frame(rowname = character(), V1 = integer(), perc = numeric())

for(i in 1:length(months)){
  month_list <- tag_topic[tag_topic$week == weeks[i],topic_start:ncol(tag_topic)] %>% 
    lapply(sum) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column() %>% filter(V1>0) %>%
    arrange(desc(V1)) %>% mutate(perc = round(V1/nrow(tag_topic),4))
  
  top_month <- rbind(top_month, month_list[1:2,])  
}

View(top_month %>% group_by(rowname) %>% summarise(count = sum(V1), max = max(V1)) %>% 
       arrange(desc(count)))

unique(top_month$rowname[!top_month$rowname %in% top_10_topics])

###looking at the month results--there are issues that get hidden here
###but show up in the by-week results, bc they are split over multiple
###months (ie, supreme court). going to include topics that were top in
###specific weeks, even if wind up splitting chart down by month


###FINAL FILTERING: is it in the top 10, or was it the top protest
###subject for a week (in a week where the top protest subject had 100+ 
###protests about it)

top_week_topics <- filter(top_week, V1 >= 100)

topics_for_chart <- c(top_10_topics, unique(top_week_topics$rowname[!top_week_topics$rowname %in% top_10_topics]))

#looking at this, national_walkout_day is a subset of guns, 
#and families is almost entirely a subset of immigration (except for
#5 cases that were protests about custody or government overreach):

nrow(filter(tag_topic, guns == 1, national_walkout_day == 1))/nrow(filter(tag_topic,national_walkout_day == 1))

nrow(filter(tag_topic, immigration == 1, families == 1))/nrow(filter(tag_topic,families == 1))
filter(tag_topic, immigration != 1, families == 1)

#going to take out "families" and "national walkout day," as well as 
#"march for science"--a specific movement, not a topic

topics_for_chart <- topics_for_chart[topics_for_chart != "families" & topics_for_chart != "national_walkout_day" & topics_for_chart != "march_for_science"]
sum(apply(tag_topic[topics_for_chart], MARGIN = 1, sum) > 0)/nrow(tag_topic)

#this covers 77% of all protests

#adding in "police" and "healthcare" as the next 2 most common topic 
#tags after the top 10 (because I pulled 2 that were in the top 10).
#this now covers 81% of protests

topics_for_chart <- c(topics_for_chart, "healthcare", "police")
sum(apply(tag_topic[topics_for_chart], MARGIN = 1, sum) > 0)/nrow(tag_topic)

#### protests about each topic by week/month, for charts

tag_topic$for_chart <- apply(tag_topic[topics_for_chart], MARGIN = 1, sum) > 0

tag_topic$chart_other <- !tag_topic$for_chart

month_topic <- tag_topic %>% group_by(month_yr) %>% summarise(guns = sum(guns),
                 immigration = sum(immigration), women = sum(women), 
                 race_confed = sum(race_confed), executive = sum(executive),
                 collective_bargaining = sum(collective_bargaining), 
                 education = sum(education), environment = sum(environment),
                 supreme_court = sum(supreme_court), 
                 healthcare = sum(healthcare),
                 police = sum(police), other = sum(chart_other))

#taking out Sept 2019 (cuts off partway through month)--no longer need to do
#this since data updated through Oct 31 2019:
#month_topic <- month_topic[month_topic$month_yr != "2019-09",]

month_topic_long <- pivot_longer(month_topic, cols = colnames(month_topic)[2:13], names_to = "topic")

week_topic <- tag_topic %>% group_by(week) %>% summarise(guns = sum(guns),
              immigration = sum(immigration), women = sum(women), 
              race_confed = sum(race_confed), executive = sum(executive),
              collective_bargaining = sum(collective_bargaining), 
              education = sum(education), environment = sum(environment),
              supreme_court = sum(supreme_court), healthcare = sum(healthcare), 
              police = sum(police))

week_topic_long <- pivot_longer(week_topic, cols = colnames(week_topic)[2:12], names_to = "topic")


#### write out

write_csv(month_topic, file.path(loc, "month_top_topics.csv"))
write_csv(month_topic_long, file.path(loc, "month_top_topics_long.csv"))

write_csv(week_topic, file.path(loc, "week_top_topics.csv"))
write_csv(week_topic_long, file.path(loc, "week_top_topics_long.csv"))

###took these files into RawGraphs to make actual stream graphs

#### what other tags were used during high months of protest for particular tags

tag_topic %>% filter(education == 1) %>% group_by(month_yr) %>% 
  summarise(count = n()) %>% arrange(desc(count))

filter(all_tag[tag_topic$education == 1,], month_yr == "2018-04")[topic_start:ncol(all_tag)] %>% 
  lapply(sum) %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>% filter(V1>0) %>%
  arrange(desc(V1)) %>% mutate(perc = round(V1/nrow(all_tag),4))

View(filter(all_tag[tag_topic$education == 1,], month_yr == "2018-04") %>%
  group_by(location) %>% summarise(count = n()) %>% arrange(desc(count)))
  