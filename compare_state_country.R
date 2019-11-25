#run through streamgraph.R first to get counts of topic by month for whole
#country

library(tidyverse)
library(janitor)
library(muckrakr)
library(lubridate)
library(ggbeeswarm)
library(ggrepel)

####################################
######### Get data sources #########
####################################

loc <- "data/final"

protests <- clean_names(read_csv(file.path(loc, "protest_nov1_addtl_tags.csv")))
protests$date <- as.Date(protests$date, "%m/%d/%y")
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

tag_start <- which(colnames(tag) == "month")+1

all_tag_frequency <- tag[,tag_start:ncol(tag)] %>% 
  lapply(sum) %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>% filter(V1>0) %>%
  arrange(desc(V1)) %>% mutate(perc = round(V1/nrow(tag),4))

####################################
#### looking specifically at FL ####
####################################

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

#would be better to compare Florida to each of the other states, rather 
#than the entire country--then could do 95th percentile or something

####################################
###### looking at all states #######
####################################

#getting only protests about main topics
main_topics <- tag[,c(1:(tag_start-1), which(colnames(tag) %in% topics))]

#summarise by state

state_topic_summary <- function(topic_file_old) {
  
  topic_file_new <- data.frame(state = character(), topic = character(), 
         count = integer(), perc_of_state_protest = numeric())
  
  for(i in 1:length(unique(topic_file_old$state))){
    this_state <- unique(topic_file_old$state)[i]
    
    st_protests <- filter(topic_file_old, state == this_state)
    
    st_tag_frequency  <- st_protests[,tag_start:ncol(st_protests)] %>% 
      lapply(sum) %>% 
      as.data.frame() %>% 
      t() %>% 
      as.data.frame() %>% 
      tibble::rownames_to_column() %>% filter(V1>0) %>%
      arrange(desc(V1)) %>% mutate(perc = round(V1/nrow(st_protests),4)) 
    
    colnames(st_tag_frequency) <- c("topic", "count", "perc_of_state_protest")
    
    st_tag_frequency$state <- this_state
    
    topic_file_new <- rbind(topic_file_new, st_tag_frequency)
  }
  
  topic_file_new
  
}


main_topic_state <- state_topic_summary(main_topics)

all_topic_state <- state_topic_summary(tag)



#checking out distribution for individual topics

ggplot(filter(main_topic_state, topic == "guns"), aes(x = perc_of_state_protest))+geom_histogram()
ggplot(filter(main_topic_state, topic == "guns"), aes(x = perc_of_state_protest, y = 1))+geom_point(alpha = .2)
ggplot(filter(main_topic_state, topic == "guns"), aes(x = perc_of_state_protest, y = 1))+
  geom_quasirandom(groupOnX = FALSE, size = 3,width = .3)+ylim(0,2)

quantile(filter(main_topic_state, topic == "guns")$perc_of_state_protest, probs = .1)

main_topic_state %>% filter(topic == "guns", perc_of_state_protest < .1) %>% arrange(desc(perc_of_state_protest))

quantile(filter(main_topic_state, topic == "immigration")$perc_of_state_protest, probs = .9)
quantile(filter(main_topic_state, topic == "immigration")$perc_of_state_protest, probs = .1)


main_topic_state %>% filter(topic == "immigration", perc_of_state_protest > .1867) %>% arrange(desc(perc_of_state_protest))

ggplot(filter(main_topic_state, topic == "immigration"), aes(x = perc_of_state_protest, y = 1))+
  geom_point(alpha = .2)+geom_text_repel(data = filter(main_topic_state, topic == "immigration",
  perc_of_state_protest > .1867 | perc_of_state_protest < .092), aes(label = state))

ggplot(filter(main_topic_state, topic == "immigration"), aes(x = perc_of_state_protest))+geom_histogram()

####################################
####### function for finding #######
###### particularly high/low #######
##### protest topics per state #####
####################################

high_low_state <- function(topic_by_state, threshhold = .9){
  
  topic_percentiles <- data.frame(topic = character(), count = integer(),
                                  perc_of_state_protest = numeric(), state = character(),
                                  percentile = numeric(), high_low = factor(levels = c("high", "middle", "low")))
  
  for(t in 1:length(unique(topic_by_state$topic))){
    
   this_topic <- filter(topic_by_state, topic == unique(topic_by_state$topic)[t])
    
    #high_bound <- quantile(this_topic$perc_of_state_protest, probs = threshhold)
    #low_bound <- quantile(this_topic$perc_of_state_protest, probs = 1-threshhold)
    
    percentile <- ecdf(this_topic$perc_of_state_protest)
    
    this_topic <- mutate(this_topic, percentile = percentile(perc_of_state_protest), high_low = NA)
    
    for(s in 1:nrow(this_topic)){
      this_percentile <- this_topic[s, "percentile"]
      
        if(this_percentile > threshhold){
          this_topic[s, "high_low"] <- "high"
        } else if (this_percentile < 1-threshhold){
          this_topic[s, "high_low"] <- "low"
        } else {
          this_topic[s, "high_low"] <- "middle"
        }
      
     }
    topic_percentiles <- rbind(topic_percentiles, this_topic)
    
    p <- ggplot(filter(this_topic), aes(x = perc_of_state_protest, y = 1)) +
      geom_hline(yintercept=1, alpha = .2) +
      geom_point(aes(color = high_low), alpha = .5, size = 3) +
      geom_text_repel(data = filter(this_topic, high_low == "high" | high_low == "low"),
          aes(label = state), direction = "y") + 
      theme_classic()+ 
      labs(title = paste(unique(topic_by_state$topic)[t], "by state"), 
           x = paste("Percent of protests in state that are related to", 
          unique(topic_by_state$topic)[t]), y = NULL ) + ylim(.8, 1.2) + 
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
            axis.title.y = element_blank(), axis.line.y = element_blank())
    
    print(p)
    
  }
  
  topic_percentiles
}

topic_high_low <- high_low_state(main_topic_state)

##grid of each main topic by state:

ggplot(filter(topic_high_low), aes(x = perc_of_state_protest, y = 1)) +
  geom_hline(yintercept=1, alpha = .2) +
  geom_point(aes(color = high_low), alpha = .5, size = 3) +
  geom_text_repel(data = filter(topic_high_low, high_low == "high" | high_low == "low"),
                  aes(label = state)) + 
  theme_classic()+ 
  labs(title = "Percent of state protest about specific topics", 
       x = "Percent of protests in state about topic", y = NULL ) + ylim(.8, 1.2) + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(), axis.line.y = element_blank())+
  facet_grid(rows = vars(topic))


###what states tend to protest about other topics? Hawaii is the huge outlier here:
topic_high_low %>% filter(high_low == "high", topic == "other") %>% arrange(desc(perc_of_state_protest))
#and HI appears to protest mostly about "against_development"--several different projects protested against 
#(telescope on Mauna Kea looks most common, but also against development in a state park and a wind farm)
all_topic_state %>% filter(state == "HI", ! topic %in% topics) %>% arrange(desc(perc_of_state_protest))

tag %>% filter(state == "HI", against_development == 1) %>% select(date, tags, source)

##what was going on in alaska re: women? looks like widespread participation in women's march
tag %>% filter(state == "AK", women == 1) %>% select(date, tags, source)

###what topics came up most per state

#does every state have SOMETHING in this?
unique(filter(topic_high_low, high_low == "high" | high_low == "low")$state)
length(unique(filter(topic_high_low, high_low == "high" | high_low == "low")$state))

###missing some--what about any topic where they were in the top 20th percentile?
#every state has at least one in the top 20th percentile and at least one in the bottom 25th percentile

View(filter(topic_high_low, percentile > .8) %>% group_by(state) %>% summarise(count_in_80th_perc = n()))
View(filter(topic_high_low, percentile < .25) %>% group_by(state) %>% summarise(count_in_80th_perc = n()))

View(filter(topic_high_low, percentile > .8 | percentile < .25) %>% arrange(state, desc(percentile)))

#exporting the percentile information
write_csv(topic_high_low, file.path(loc,"topic_percentile_by_state.csv"))


####################################
#### specific figures for copy ####
####################################

###environment-related protests per 10,000 residents in Florida, 
###California, New York, New Jersey, and Hawaii. population info is 
###as-of 2017 from the US Census

#Florida:
(sum(filter(main_topic_state, state == "FL", topic == "environment")$count)/20980000)*10000
filter(main_topic_state, state == "FL", topic == "environment")$count/sum(filter(main_topic_state, state == "FL")$count)
473192/20980000

#California:
(sum(filter(main_topic_state, state == "CA", topic == "environment")$count)/39400000)*10000
filter(main_topic_state, state == "CA", topic == "environment")$count/sum(filter(main_topic_state, state == "CA")$count)

#New York:
(sum(filter(main_topic_state, state == "NY", topic == "environment")$count)/19590000)*10000
filter(main_topic_state, state == "NY", topic == "environment")$count/sum(filter(main_topic_state, state == "NY")$count)

#Hawaii:
(sum(filter(main_topic_state, state == "HI", topic == "environment")$count)/1424000)*10000
filter(main_topic_state, state == "HI", topic == "environment")$count/sum(filter(main_topic_state, state == "HI")$count)

#New Jersey:
(sum(filter(main_topic_state, state == "NJ", topic == "environment")$count)/8889000)*10000
filter(main_topic_state, state == "NJ", topic == "environment")$count/sum(filter(main_topic_state, state == "NJ")$count)

#West Virginia
(sum(filter(main_topic_state, state == "WV", topic == "environment")$count)/1817000)*10000
filter(main_topic_state, state == "WV", topic == "environment")$count/sum(filter(main_topic_state, state == "WV")$count)

(sum(filter(main_topic_state, state == "WV", topic == "education")$count)/1817000)*10000
filter(main_topic_state, state == "WV", topic == "education")$count/sum(filter(main_topic_state, state == "WV")$count)



#Monroe County, WV:
(5/13373)*10000

#All US:
(sum(filter(main_topic_state, topic == "environment")$count)/325700000)*10000
sum(filter(main_topic_state, topic == "environment")$count)/sum(main_topic_state$count)

(sum(filter(main_topic_state, topic == "education")$count)/325700000)*10000
sum(filter(main_topic_state, topic == "education")$count)/sum(main_topic_state$count)



##gun control in Florida vs nationally

ggplot()+geom_density(data=filter(tag, guns == 1, state == "GA"), aes(x = date), fill = "green", alpha = .5)+geom_density(data=filter(tag, guns == 1), aes(x = date), fill = "blue", alpha = .5)+labs(title = "percent of gun protests by date", subtitle = "green is FL, blue is nation")
filter(main_topic_state, state == "FL", topic == "guns")$count/sum(filter(main_topic_state, topic == "guns")$count)

fl_gunweek <- tag %>% filter(state == "FL", guns == 1) %>% group_by(date) %>% summarise(count = n()) %>% mutate(place = "FL")
ntl_gundate <- tag %>% filter(guns == 1) %>% group_by(date) %>% summarise(count = n()) %>% mutate(place = "US")

gundate <- rbind(fl_gundate, ntl_gundate)
ggplot(gundate, aes(x = date, y = count, color = place))+geom_line(alpha = .5)


ggplot()+geom_bar(data=filter(tag, guns == 1, state == "GA"), aes(x = date))
