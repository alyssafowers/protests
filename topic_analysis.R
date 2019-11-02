##### Looking at most common tags and where those protests happen #####

library(tidyverse)
library(janitor)
library(stringr)
library(sf)
library(data.table)
library(muckrakr)
library(lubridate)
library(ggrepel)


####################################
########### Load in data ###########
####################################

loc <- "data/working"

protest <- read_csv(file.path(loc, "protest_additional_tags.csv"))
protest <- protest %>% clean_names()

####################################
########### Getting tags ###########
####################################


tag <- untangle(protest, "tags", pattern = ";")
tag <- clean_names(tag)

tag_no_position <- tag[,!(str_detect(colnames(tag), "^for") | str_detect(colnames(tag), "^against"))]
tag_position <- tag[,(str_detect(colnames(tag), "^for") | str_detect(colnames(tag), "^against"))]

#sum(!(str_detect(colnames(tag), "^for") | str_detect(colnames(tag), "^against")))

tag_start <- which(colnames(tag) == "location_name") + 1

all_tag_frequency <- tag[,tag_start:ncol(tag)] %>% 
  lapply(sum) %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>% filter(V1>0) %>%
  arrange(desc(V1)) %>% mutate(perc = round(V1/nrow(tag),4))

View(tag[,tag_start:ncol(tag)] %>% 
       lapply(sum) %>% 
       as.data.frame() %>% 
       t() %>% 
       as.data.frame() %>% 
       tibble::rownames_to_column() %>% filter(V1>0) %>%
       arrange(desc(V1)))

topic <- tag_no_position[,tag_start:ncol(tag_no_position)] %>% 
  lapply(sum) %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>% filter(V1>0) %>%
  arrange(desc(V1)) %>% mutate(perc = round(V1/nrow(topic),4))

position <- tag_position[,tag_start:ncol(tag_position)] %>% 
  lapply(sum) %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>% filter(V1>0) %>%
  arrange(desc(V1)) %>% mutate(perc = round(V1/nrow(position),4))

summary(topic$V1)
summary(position$V1)

topic %>% filter(V1 >= 90) %>% arrange(desc(V1)) %>%
  ggplot(aes(y = V1, x = reorder(rowname, V1)))+geom_bar(stat = "identity")+
           coord_flip()+labs(title = "Most frequent topic tags",
           subtitle = "75th percentile and up", x = "Topic", 
           y ="Count of protests")+theme_classic()

position %>% filter(V1 >= 38) %>% arrange(desc(V1)) %>%
  ggplot(aes(y = V1, x = reorder(rowname, V1)))+geom_bar(stat = "identity")+
  coord_flip()+labs(title = "Most frequent position tags",
                    subtitle = "80th percentile and up", x = "Topic", 
                    y ="Count of protests")+theme_classic()

all_tag_graph <- all_tag_frequency %>% filter(V1 >= 150) %>% arrange(desc(V1)) %>%
  ggplot(aes(y = V1, x = reorder(rowname, V1)))+geom_bar(stat = "identity")+
  coord_flip()+labs(title = "Most frequent topic tags",
                    subtitle = "75th percentile and up", x = "Topic", 
                    y ="Count of protests")+theme_classic()

ggplot(topic, aes(x = V1))+geom_histogram(binwidth = 50)+
  labs(title = "Distribution of protest counts for topic tags")+
  theme_classic()

ggplot(position, aes(x = V1))+geom_histogram(binwidth = 50)+
  labs(title = "Distribution of protest counts for topic tags")+
  theme_classic()

quantile(x = position$V1, probs = seq(0, 1, .1))

#####Positions under tags

find_positions <- function(position, file = tag){
  tag_start <- which(colnames(file) == "week") + 1
  
  tag_start
  file[,tag_start:ncol(file)] %>%
  filter(get(position) == 1) %>%
  lapply(sum) %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>% filter(V1>0) %>%
  arrange(desc(V1)) %>% 
  mutate(perc = round(V1/nrow(file),4))
}

find_positions(position = "civil_rights")
find_positions(position = "guns")
find_positions(position = "immigration")
#other is a true grab bag
find_positions(position = "other")
#
find_positions(position = "executive")
#national walkout day is just guns:
find_positions(position = "national_walkout_day")
find_positions(position = "collective_bargaining")
#families means keeping immigrant families together:
find_positions(position = "families")
find_positions(position = "education")
find_positions(position = "environment")
find_positions(position = "healthcare")
find_positions(position = "counter_protest")
find_positions(position = "police")
find_positions(position = "legislative")

#"civil rights" is HELLA vague--can I get that to more specific issues?
summary(find_positions(position = "civil_rights"))
filter(find_positions(position = "civil_rights"), V1 >= 10)

#
#for_womens_rights, womens_march, for_abortion_rights, against_abortion_rights, for_planned_parenthood, day_without_a_woman, against_planned_parenthood, international_womens_day
#police, for_criminal_justice, for_criminal_justice_reform, prisons
#pro_lgbtq, for_transgender_rights, pride, anti_lgbtq
#national_anthem, against_invited_speaker, for_freedom_of_speech
#for_religious_tolerance, anti_muslim
#voting, for_redistricting
#for_disability_rights



#for_greater_accountability and for_greater_access are too vague to tell what they're actually about


###taxonomy for civil_rights: 
##race, white supremacy, and confederate symbols: 
#for_racial_justice, against_white_supremacy, charlottesville, martin_luther_king_jr, national_anthem, against_confederate_symbol, for_white_supremacy, for_confederate_symbol, black_womens_march
##women's rights:
#for_womens_rights, womens_march, for_abortion_rights, against_abortion_rights, for_planned_parenthood, day_without_a_woman, against_planned_parenthood, international_womens_day, against_sexual_domestic_violence, black_womens_march
##police and criminal justice
#police, for_criminal_justice, for_criminal_justice_reform, prisons, for_supporting_police, against_police_presence, national_anthem
##gay rights:
#pro_lgbtq, for_transgender_rights, pride, anti_lgbtq
##freedom of speech
#national_anthem, against_invited_speaker, for_freedom_of_speech
##religion
#for_religious_tolerance, anti_muslim
##voting
#voting, for_redistricting
##disability rights
#for_disability_rights

####################################
######   adding supplementary  #####
####    civil rights tags      ####
####################################

tag$add <- ""

for(i in 1:nrow(tag)){
  if(sum(tag[i, c("for_racial_justice", "against_white_supremacy", 
              "charlottesville", "martin_luther_king_jr", 
              "national_anthem", "against_confederate_symbol", 
              "for_white_supremacy", "for_confederate_symbol", 
              "black_womens_march")]) >= 1){
    tag[i, "add"] <- "race_confed"
  }

  if(sum(tag[i, c("for_womens_rights", "womens_march", "for_abortion_rights",
                  "against_abortion_rights", "for_planned_parenthood", 
                  "day_without_a_woman", "against_planned_parenthood", 
                  "international_womens_day", "against_sexual_domestic_violence", 
                  "black_womens_march")]) >= 1){
    if(tag[i, "add"] == ""){
      tag[i, "add"] <- "women"
    } else {
    tag[i, "add"] <- paste(tag[i, "add"], "women", sep = "; ")
    }
  }
  if(sum(tag[i, c("police", "for_criminal_justice", "for_criminal_justice_reform", 
                  "prisons", "for_supporting_police", "against_police_presence", 
                  "national_anthem")]) >= 1){
    if(tag[i, "add"] == ""){
      tag[i, "add"] <- "police_criminal_justice"
    } else {
      tag[i, "add"] <- paste(tag[i, "add"], "police_criminal_justice", sep = "; ")
    }
  }
  
  if(sum(tag[i, c("pro_lgbtq", "for_transgender_rights", 
                  "pride", "anti_lgbtq")]) >= 1){
    if(tag[i, "add"] == ""){
      tag[i, "add"] <- "lgbtq"
    } else {
      tag[i, "add"] <- paste(tag[i, "add"], "lgbtq", sep = "; ")
    }
  }
  
  if(sum(tag[i, c("national_anthem", "against_invited_speaker", 
                  "for_freedom_of_speech")]) >= 1){
    if(tag[i, "add"] == ""){
      tag[i, "add"] <- "freedom_of_speech"
    } else {
      tag[i, "add"] <- paste(tag[i, "add"], "freedom_of_speech", sep = "; ")
    }
  }
  
  if(sum(tag[i, c("for_religious_tolerance", "anti_muslim")]) >= 1){
    if(tag[i, "add"] == ""){
      tag[i, "add"] <- "religion"
    } else {
      tag[i, "add"] <- paste(tag[i, "add"], "religion", sep = "; ")
    }
  }
  
  if(sum(tag[i, c("voting", "for_redistricting")]) >= 1){
    if(tag[i, "add"] == ""){
      tag[i, "add"] <- "voting"
    } else {
      tag[i, "add"] <- paste(tag[i, "add"], "voting", sep = "; ")
    }
  }
  
  if(tag[i, "add"] != ""){
    tag[i, "tags"] <- paste(tag[i, "tags"], tag[i, "add"], sep = "; ")
  }
}

##sanity check:
View(tag %>% filter(add != "") %>% select(tags, add))

#write out:
new_tags <- select(tag, 1:32)
write_csv(new_tags, file.path(loc, "protest_additional_tags.csv"))

####################################
####### week-by-week charts ########
####################################

week_by_week <- function(data_file, topic){
  week <- data_file %>% filter(get(topic) == 1) %>% group_by(week) %>% 
    summarise(count = n()) %>% arrange(week)
  
  tag_start <- which(colnames(data_file) == "location_name")
  
  topic_protest <- filter(data_file, get(topic) == 1)[,1:tag_start]
  
  write_csv(week, file.path("weekly_counts", paste(topic, "_weekly_count.csv", sep = "")))
  write_csv(topic_protest, file.path("weekly_counts", paste(topic, "_all_protests.csv", sep = "")))
  
 week
}

week_graph <- function(data_file, topic_name, event_list = NA){
  month_start <- data.frame(start = seq(as.Date("2017-01-01"), length = 33, by = "month"))
  graph <- ggplot()+
  geom_bar(data = data_file, aes(x = week, y = count), stat = "identity", 
           fill = "#D1D1D1")+
  geom_segment(data = month_start, aes(x = start, xend = start, y = 0, 
               yend = -40), color = "#A0A0A0", size = .2)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        plot.background = element_rect(fill = "#707589",
        colour = "#707589", size = 0.5, linetype = "solid"), 
        axis.text.x = element_text(colour="#A0A0A0"),
        axis.text.y = element_text(colour="#A0A0A0"), 
        axis.ticks = element_line(color="#A0A0A0"), 
        axis.ticks.length=unit(5, "points"),
        plot.title = element_text(color = "#D1D1D1"))+
    labs(x="total",y="", title = paste(topic_name, "protests per week")) +
    scale_y_continuous(expand = c(0,0), breaks = c(0, 200, 400, 600, 
        800, 1000, 1200, 1400), limits = c(-40, 1400))
  if(is.na(event_list) == FALSE){
    graph <- graph + geom_segment(data = event_list, aes(x = date, xend = date, y = 0, yend = 1400)) +
      geom_text_repel(data = event_list, aes(x = date, y = 1000, label = event), size = 2)
  }
  
  graph + ggsave(paste("weekly_counts/",topic_name, "per_week.pdf", sep = ""),width=8.3, height = 3)
  
  }


gun_week <- week_by_week(data_file = tag, topic = "guns")
week_graph(gun_week, "Guns")

immigration_week <- week_by_week(data_file = tag, topic = "immigration")
week_graph(immigration_week, "Immigration")

women_week <- week_by_week(data_file = tag, topic = "women")
week_graph(women_week, "Women")

women_week <- week_by_week(data_file = tag, topic = "women")
week_graph(women_week, "Women")

race_confed_week <- week_by_week(data_file = tag, topic = "race_confed")
week_graph(race_confed_week, "Race, white supremacy, and Confederate monuments")

race_confed_week <- week_by_week(data_file = tag, topic = "race_confed")
week_graph(race_confed_week, "Race, white supremacy, and Confederate monuments")

executive_week <- week_by_week(data_file = tag, topic = "executive")
week_graph(executive_week, "Executive")

collective_bargaining_week <- week_by_week(data_file = tag, topic = "collective_bargaining")
week_graph(collective_bargaining_week, "collective_bargaining")

environment_week <- week_by_week(data_file = tag, topic = "environment")
week_graph(environment_week, "Environment")

education_week <- week_by_week(data_file = tag, topic = "education")
week_graph(education_week, "Education")

voting_week <- week_by_week(data_file = tag, topic = "voting")
week_graph(voting_week, "Voting")

police_criminal_justice_week <- week_by_week(data_file = tag, topic = "police_criminal_justice")
week_graph(police_criminal_justice_week, "police_criminal_justice")


police_criminal_justice_week <- week_by_week(data_file = tag, topic = "police_criminal_justice")
week_graph(police_criminal_justice_week, "police_criminal_justice")


###adding in event dates for context:
gun_event <- clean_names(read_csv("shooting_dates.csv"))
gun_event$date <- mdy(gun_event$date)

week_graph(gun_week, "guns", gun_event)

####################################
##### how to decide if a place #####
#### belongs in a constellation ####
####################################

####this was just tinkering--for the actual constellation maps and final
####criteria, see constellation_function.R

###sample topic: collective bargaining
#want to include places that have high *rate* of collective bargaining protests
#(per 10k people) AND that the collective bargaining makes up a large percentage
#of the protesting going on in that place.

#what % of nationwide protests are related to collective bargaining? roughly 5%

nrow(filter(tag, collective_bargaining == 1))/nrow(tag)

#what % of places (that I am tracking) have at least one collective bargaining protest?
#29% of them
cb <- filter(tag, collective_bargaining == 1)
length(unique(cb$location_id))/length(unique(protest$location_id))

nrow(filter(summary, cb_protest >= 1))

#what does the distribution of %-protests-that-are-cb-protests look like?
summary <- tag %>% group_by(location_id) %>% 
  summarise(protest_count = n(), cb_protest = sum(collective_bargaining)) %>%
  mutate(perc_cb = cb_protest/protest_count)

ggplot(summary, aes(x = perc_cb))+geom_histogram()
ggplot(filter(summary, cb_protest != 0), aes(x = perc_cb))+geom_histogram()

ggplot(summary, aes(y=perc_cb, x = cb_protest))+geom_point()
summary(filter(summary, cb_protest !=0))

#3x national rate is reasonable here, or could use 3rd quartile (.125)--I like
#3rd quartile because it can be consistent across places

#how many places have just one protest, which is cb? 7
nrow(filter(summary, protest_count == 1, cb_protest == 1))

#what is the rate per 10k people?

place <- read_csv(file.path(loc, "all_county_cbsa_summary.csv"))
place <- place %>% clean_names()

summary <- left_join(summary, place[,c("location_id", "pop", "name")])

summary$cb_per_10k <- (summary$cb_protest/summary$pop)*10000
quantile(filter(summary, cb_per_10k != 0)$cb_per_10k, probs = seq(0, 1, .25))

summary(filter(summary, cb_protest !=0))
ggplot(filter(summary, cb_protest != 0), aes(x = cb_per_10k))+geom_histogram()
ggplot(filter(summary, cb_protest != 0), aes(x = perc_cb, y = cb_per_10k))+geom_point()
ggplot(filter(summary, cb_protest >= 2), aes(x = perc_cb, y = cb_protest))+geom_point()

#maybe something like: the place that has the MOST protests in that category,
#then the places that are at the 75th percentile of percent-of-these-protests-that-are-category AND
#have at least 2 protests in the category AND have above the median category_per_10k?

nrow(filter(summary, cb_protest >= 2 & cb_per_10k >= .06 & perc_cb >= .125 | cb_protest == 79))

nrow(filter(summary, cb_protest >= 1))

#that gets me to 26 out of 255 

cb_constellation <- filter(summary, cb_protest >= 2 & cb_per_10k >= .06 & perc_cb >= .125 | cb_protest == 79)

cb_constellation <- filter(summary, cb_protest >= 4 & cb_per_10k >= .06 & perc_cb >= .07)

cb_constellation <- filter(summary, cb_protest >= 4 & cb_per_10k >= .06 & perc_cb >= .07 | cb_protest >= 14 & perc_cb >= 0.05 & cb_per_10k >= .05)



tag_start <- which(colnames(tag) == "location_name")

topic_protest <- filter(data_file, get(topic) == 1)[,1:tag_start]

tag %>% filter(guns == 1, location_name == "Bridgeport-Stamford-Norwalk, CT") %>% ggplot(aes(x = week))+geom_bar()
View(filter(tag, guns == 1, location_name == "Bridgeport-Stamford-Norwalk, CT")[,1:tag_start])




ggplot()+geom_bar(data = environment_week, aes(x = week, y = count), stat = "identity", 
             fill = "#D1D1D1")+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.background = element_blank(), 
          plot.background = element_rect(fill = "#707589",
                                         colour = "#707589", size = 0.5, linetype = "solid"), 
          axis.text.x = element_text(colour="#A0A0A0"),
          axis.text.y = element_text(colour="#A0A0A0"), 
          axis.ticks = element_line(color="#A0A0A0"), 
          axis.ticks.length=unit(5, "points"),
          plot.title = element_text(color = "#D1D1D1"))+
    labs(x="total",y="", title = "Environment protests per week") 

View(filter(tag, education == 1, week == ymd("2019-04-15"))[,1:tag_start])



#####What protests about state executives are happening in Alaska?

tag_start <- which(colnames(tag) == "location_name") + 1

 tag[tag$state == "AK",tag_start:ncol(tag)] %>% 
  lapply(sum) %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>% filter(V1>0) %>%
  arrange(desc(V1)) %>% mutate(perc = round(V1/nrow(tag),4))

View(filter(tag, state == "AK", against_state_executive == 1)[,1:tag_start])

ak_exec <- filter(tag, state == "AK", against_state_executive == 1)[,1:tag_start]

ggplot(ak_exec) + geom_point(aes(x = date, y = location_name), alpha = .3)
ggplot(ak_exec) + geom_point(aes(x = week, y = location_name), alpha = .3)

ak_exec[ak_exec$county_name_short == "Aleutians West", "internal_point_longitude"] <- ak_exec[ak_exec$county_name_short == "Aleutians West", "internal_point_longitude"]*-1


usa <- map_data("usa")

states <- map_data("state")

alaska <- ggplot(data = usa) +
    geom_sf(fill = "cornsilk") +
    coord_sf(crs = st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000, 
    2500000), expand = FALSE, datum = NA)

ak <- map_data('worldHires','USA:Alaska')
ak <- filter(us_states(), name == "Alaska")

ggplot() + geom_sf(data = ak) +
  coord_sf(crs = st_crs(3467), xlim = c(-190, -140), ylim = c(50, 
   80), expand = FALSE, datum = NA)+  geom_point(data = ak_exec, 
   aes(x = internal_point_longitude, y = internal_point_latitude), 
   color = "white")


+
  geom_path(data = const, aes(x=internal_point_longitude, y = internal_point_latitude), color = "#8A91A8")+
  geom_point(data = const, aes(x = internal_point_longitude, y = internal_point_latitude, size = topic_protest), 
             color = "white") + xlim(-125,-65)+ylim(25,50) +
  scale_size_continuous(range = c(.2,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
  coord_map("albers", lat0=30, lat1=40)+ggtitle(paste(topic, "protests"))