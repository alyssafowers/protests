library(tidyverse)
library(lubridate)
library(muckrakr)
library(janitor)

loc <- "data/working"

protest <- clean_names(read_csv(file.path(loc, "protest_addtltags_nov1_withstate.csv")))
protest <- mutate(protest, week=as.Date(cut(date, breaks="week",starts.on.monday=TRUE)))

events <- read_csv("shooting_dates.csv")
events$Date <- as.Date(events$Date, "%m/%d/%y")

tag <- clean_names(untangle(protest, "tags", pattern = ";"))
tag_start <- which(colnames(tag) == "short_loc") + 1

week_by_week <- function(data_file, topic){
  week <- data_file %>% filter(get(topic) == 1) %>% group_by(week) %>% 
    summarise(count = n()) %>% arrange(week)
  
  #tag_start <- which(colnames(data_file) == "short_loc")
  
  #topic_protest <- filter(data_file, get(topic) == 1)[,1:tag_start]
  
  #write_csv(week, file.path("weekly_counts", paste(topic, "_weekly_count.csv", sep = "")))
  #write_csv(topic_protest, file.path("weekly_counts", paste(topic, "_all_protests.csv", sep = "")))
  
  week
}

women <- week_by_week(tag, "women")
immigration <- week_by_week(tag, "immigration")
guns <- week_by_week(tag, "guns")
environment <- week_by_week(tag, "environment")
education <- week_by_week(tag, "education")




week_graph <- function(data_file, topic_name, event_list = NA){
  
  month_seg_length <- max(data_file$count)*.028
  
  month_start <- data.frame(start = seq(as.Date("2017-02-01"), length = 33, by = "month"))
  graph <- ggplot()+
    geom_bar(data = data_file, aes(x = week, y = count), stat = "identity", 
             fill = "black")+
    geom_segment(data = month_start, aes(x = start, xend = start, y = 0, 
                                         yend = -month_seg_length), color = "white", size = .2)+
    geom_segment(data=data_file, aes(as.Date("2017-01-01"), xend=as.Date("2019-10-31"),y=0, yend=0), color = "white", size = .2)+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.background = element_blank(), 
          plot.background = element_rect(fill = "#abacb7",
                                         colour = "#abacb7", size = 0.5, linetype = "solid"), 
          axis.text.x = element_text(colour="white"),
          axis.text.y = element_text(colour="white"), 
          axis.ticks = element_line(color="white"), 
          axis.ticks.length=unit(5, "points"),
          plot.title = element_text(color = "black"))+
    labs(x="total",y="", title = paste(topic_name, "protests per week"))+
    scale_y_continuous(breaks = seq(from = 0, to = max(data_file$count)+10, by = 10))
   
    # scale_y_continuous(expand = c(0,0), breaks = c(0, 200, 400, 600, 
                                                 #  800, 1000, 1200, 1400), limits = c(-40, 1400))
  if(is.na(event_list) == FALSE){
    
  segment_length <- max(data_file$count)
    graph <- graph + geom_segment(data = event_list, aes(x = Date, xend = Date, y = 0, yend = segment_length), color = "white") +
      geom_text_repel(data = event_list, aes(x = Date, y = segment_length, label = Event), size = 2)
  }
  
  graph
  graph + ggsave(paste("weekly_counts/",topic_name, "per_week.pdf", sep = ""),width=9, height = 4.5)
  
}
week_graph(guns, "Gun")

week_graph(guns, "Gun", event_list = events)

week_graph(environment, "environment")

week_graph(education,"education")

#education protests pulling out Oklahoma, Kentucky, West Virginia:
edu <- tag %>% filter(education == 1) %>% mutate(wv = state == "WV") %>% group_by(week) %>% 
  summarise(total = n(),wv = sum(wv)) %>% mutate(rest_of_country = total - wv) %>% arrange(week)

edu <- pivot_longer(edu, cols = -week, names_to = "place", values_to = "count") %>% filter(place != "total")

ggplot(edu, aes(x=week, y=count, fill=place))+geom_bar(stat="identity")

View(tag %>% filter(education == 1) %>% mutate(year = year(date)) %>% group_by(state, year) %>% summarise(count=n()) %>% arrange(year, desc(count)))

edu_strike <- read_csv("edu_strike.csv")
edu_strike$start_date <- as.Date(edu_strike$start_date, "%m/%d/%y")
edu_strike$end_date <- as.Date(edu_strike$end_date, "%m/%d/%y")
start_date <- as.Date("1/1/17", "%m/%d/%y")
end_date <- as.Date("10/31/19", "%m/%d/%y")

break_lines <- seq(-.5, -14.5)

edu_label <- c("West Virginia", "Kentucky", "Oklahoma", "Georgia", "Arizona", "Colorado", "North Carolina", "Washington", "California", "Virginia", "South Carolina", "Tennessee", "Oregon", "Illinois")[14:1]

ggplot(data = edu_strike)+
  geom_segment(aes(y=-order, yend=-order, x = start_date, xend=end_date), size = 5)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#abacb7", colour = "#abacb7", 
             size = 0.5, linetype = "solid"), axis.text.x = element_text(colour="white"),
        axis.text.y = element_text(colour="white"), 
        axis.ticks.x = element_line(color="white"),
        axis.ticks.y = element_blank())+
  scale_x_date(limits = c(start_date, end_date), date_breaks = "1 year")+
  geom_hline(yintercept=break_lines, color = "white", size = .25)+
  scale_y_continuous(breaks = seq(-14, -1), 
     labels = edu_label)

