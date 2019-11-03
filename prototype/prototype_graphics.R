library(tidyverse)
library(janitor)
library(muckrakr)


loc <- "data/original"
protest <- clean_names(read_csv(file.path(loc, "protest_lat_long.csv")))

loc_2 <- "data/working"
protest_clean <- clean_names(read_csv(file.path(loc_2, "protest_additional_tags.csv")))
tag <- untangle(protest_clean, "tags", pattern = ";")
place <- clean_names(read_csv(file.path(loc_2, "all_county_cbsa_summary.csv")))


usa <- map_data("usa")


ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_point(data = protest, aes(x = longitude, y = latitude), color = "white", size = .1, alpha = .2)+
  xlim(-125,-65)+ylim(25,50) +
  scale_size_continuous(range = c(.2,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
  coord_map("albers", lat0=30, lat1=40)
wd <- getwd() 

month_start <- data.frame(start = seq(as.Date("2017-01-01"), length = 33, by = "month"))
year_start <- data.frame(start = seq(as.Date("2017-01-01"), length = 3, by = "year"))


week_protest <- protest_clean %>% group_by(week) %>% summarise(count = n()) %>% arrange(week) %>% mutate(perc = count/nrow(protest_clean))

write_csv(week_protest, "data/working/protest_per_week_usa.csv") 

  ggplot() + geom_bar(data = week_protest, aes(x = week, y = count), stat = "identity", 
           fill = "#D1D1D1")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        plot.background = element_blank(), 
        axis.text.x = element_text(colour="#A0A0A0"),
        axis.text.y = element_text(colour="#A0A0A0"), 
        axis.ticks.y = element_line(color="#A0A0A0"), 
        axis.ticks.x = element_blank(),
        axis.title = element_text(colour = "#A0A0A0"),
        axis.ticks.length=unit(5, "points"),
        plot.title = element_text(color = "#D1D1D1"))+
 geom_segment(data = month_start, aes(x = start, xend = start, y = 0, 
                                         yend = -40), color = "#A0A0A0", size = .2)+
    geom_segment(data = year_start, aes(x = start, xend = start, y = 0, 
                                         yend = -80), color = "#A0A0A0", size = .4)+
  labs(x="",y="Protests per week") +
  scale_y_continuous(expand = c(0,0), breaks = c(0, 200, 400, 600, 
                                                 800, 1000, 1200, 1400), limits = c(-80, 1600))
  
  
  
  
  place <- read_csv(file.path(loc, "all_county_cbsa_summary.csv"))
  place <- place %>% clean_names()
  place <- mutate(place, protest_per_10k = (protest_count/pop)*10000)
  place$has_protest <- place$protest_count > 0

  place_protest <- filter(place, has_protest == TRUE)
  
  ggplot(place, aes(x = pop, y = protest_count))+geom_point(alpha = .2)+
    labs(title  = "Protest count by population", 
         subtitle = "all CBSAs and non-affiliated counties") + theme_classic()
  ggplot(place, aes(x = log(pop), y = protest_count))+geom_point(alpha = .2)+
    labs(title  = "Protest count by log of population", 
         subtitle = "all CBSAs and non-affiliated counties") + theme_classic()
  ggplot(place, aes(x = log(pop), y = protest_per_10k))+geom_point(alpha = .2)+
    labs(title  = "Protest rate by log of population", 
         subtitle = "all CBSAs and non-affiliated counties") + theme_classic()
  
  
  ggplot(place_protest, aes(x = log(pop), y = protest_per_10k))+
    geom_point(alpha = .2)+labs(title  = "Protest rate by log of population", 
                                subtitle = "CBSAs and counties with protests") + theme_classic()
  
  ggplot(place, aes(x = percent_rank(pop), y = protest_count))+
    geom_point(alpha = .2)+labs(title  = "Protest count by population rank", 
                                subtitle = "All areas") + theme_classic()
  

####getting protests by month  
protest_clean <-  mutate(protest_clean, month=as.Date(cut(date, breaks="month")))

month_protest <- protest_clean %>% group_by(month) %>% summarise(count = n()) %>% mutate(perc = count/nrow(protest_clean))
ggplot(month_protest, aes(x = month, y = perc))+geom_bar(stat = "identity")



####trying to make individual-place graphics

###Frankfort, KY
frankfort <- filter(tag, metro_micro_stat_area == 23180)
tag_count(frankfort)[1:10,]
View(frankfort)
ggplot(frankfort, aes(x = week))+geom_bar()+
  labs(title = "All protests in Frankfort, KY")+theme_classic()
ggplot(filter(frankfort, education == 1), aes(x = week))+geom_bar()+
  labs(title = "Education protests in Frankfort, KY")+theme_classic()

frankfort <- arrange(frankfort, date)
f_week <- unique(frankfort$week)
frankfort$pos <- NA

for(i in 1:length(f_week)){
  p_w <- filter(frankfort, week == f_week[i])
  p_w$seq <- 1:nrow(p_w)
 frankfort[frankfort$week == f_week[i],"pos"] <- p_w$seq
}

frankfort$pos_perc <- frankfort$pos/nrow(frankfort)
  
ggplot(frankfort, aes(x = week, y = pos_perc))+geom_point()


frankfort <-  mutate(frankfort, month=as.Date(cut(date, breaks="month")))

f_month <- unique(frankfort$month)
frankfort$pos <- NA

for(i in 1:length(f_month)){
  p_m <- filter(frankfort, month == f_month[i])
  p_m$seq <- 1:nrow(p_m)
  frankfort[frankfort$month == f_month[i],"pos"] <- p_m$seq
}
frankfort$pos_perc <- frankfort$pos/nrow(frankfort) - .01
frankfort_by_month <- frankfort %>% group_by(month) %>% summarise(count = n()) %>% mutate(perc = count/nrow(frankfort) - .01)


ggplot()+geom_area(data = month_protest, aes(x = month, y = perc), fill = "gray")+
  geom_point(data = frankfort, aes(x = month, y = pos_perc), size = 5, color = "white")+
  theme_dark()

ggplot()+geom_area(data = month_protest, aes(x = month, y = perc), fill = "#707589")+
  geom_point(data = frankfort, aes(x = month, y = pos_perc), size = 3, color = "white")+
  theme(panel.background= element_rect(fill = "black"), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+labs(title = "Frankfort, KY protests per month")+
geom_segment(data = frankfort_by_month, aes(x = month, xend = month, y = 0, yend = perc), color = "white")+
  scale_y_continuous(limits = c(0, .25))

ggplot()+geom_area(data = month_protest, aes(x = month, y = perc), fill = "#707589")+
  geom_segment(data = frankfort_by_month, aes(x = month, xend = month, y = 0, yend = perc), color = "white")+
  geom_point(data = frankfort, aes(x = month, y = pos_perc, color = as.character(education)), pch=21, fill = "white", size = 2, stroke = 2)+
  scale_color_manual(values = c("white", "yellow"))+
  theme(panel.background= element_rect(fill = "black"), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), legend.position = "none")+labs(title = "Frankfort, KY protests per month")+
  scale_y_continuous(limits = c(0, .25))


pittsfield <- filter(tag, metro_micro_stat_area == 38340)

pittsfield <-  mutate(pittsfield, month=as.Date(cut(date, breaks="month")))

p_month <- unique(pittsfield$month)
pittsfield$pos <- NA

for(i in 1:length(p_month)){
  p_m <- filter(pittsfield, month == p_month[i])
  p_m$seq <- 1:nrow(p_m)
  pittsfield[pittsfield$month == p_month[i],"pos"] <- p_m$seq
}
pittsfield$pos_perc <- pittsfield$pos/nrow(pittsfield) - .01
pittsfield_by_month <- pittsfield %>% group_by(month) %>% summarise(count = n()) %>% mutate(perc = count/nrow(pittsfield) - .01)

ggplot()+geom_area(data = month_protest, aes(x = month, y = perc), fill = "#707589")+
  geom_segment(data = pittsfield_by_month, aes(x = month, xend = month, y = 0, yend = perc), color = "white")+
  geom_point(data = pittsfield, aes(x = month, y = pos_perc, color = as.character(environment)), pch=21, fill = "white", size = 2, stroke = 2)+
  scale_color_manual(values = c("white", "yellow"))+
  theme(panel.background= element_rect(fill = "black"), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), legend.position = "none")+labs(title = "Pittsfield, MA protests per month")+
  scale_y_continuous(limits = c(0, .25))


data_file <- tag
charlottesville <- filter(tag, metro_micro_stat_area == 16820)

charlottesville <-  mutate(charlottesville, month=as.Date(cut(date, breaks="month")))

c_month <- unique(charlottesville$month)
charlottesville$pos <- NA

for(i in 1:length(c_month)){
  c_m <- filter(charlottesville, month == c_month[i])
  c_m$seq <- 1:nrow(c_m)
  charlottesville[charlottesville$month == c_month[i],"pos"] <- c_m$seq
}
charlottesville$pos_perc <- charlottesville$pos/nrow(charlottesville) - .01

charlottesville_by_month <- charlottesville %>% group_by(month) %>% summarise(count = n()) %>% mutate(perc = count/nrow(charlottesville) - .01)

ggplot()+geom_area(data = month_protest, aes(x = month, y = perc), fill = "#707589")+
  geom_point(data = charlottesville, aes(x = month, y = pos_perc), size = 3, color = "white")+
  theme(panel.background= element_rect(fill = "black"), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+labs(title = "Charlottesville, VA protests per month")+
  scale_y_continuous(limits = c(0, .25))+
  geom_segment(data = charlottesville_by_month, aes(x = month, xend = month, y = 0, yend = perc), color = "white")


ggplot()+geom_area(data = month_protest, aes(x = month, y = perc), fill = "#707589")+
  geom_segment(data = charlottesville_by_month, aes(x = month, xend = month, y = 0, yend = perc), color = "white")+
  geom_point(data = charlottesville, aes(x = month, y = pos_perc, color = as.character(against_white_supremacy)), pch=21, fill = "white", size = 2, stroke = 2)+
  scale_color_manual(values = c("white", "yellow"))+
  theme(panel.background= element_rect(fill = "black"), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), legend.position = "none")+labs(title = "Charlottesville, VA protests per month")+
  scale_y_continuous(limits = c(0, .25))


lexington <- filter(tag, location_id == "51678")

lexington <-  mutate(lexington, month=as.Date(cut(date, breaks="month")))

l_month <- unique(lexington$month)
lexington$pos <- NA

for(i in 1:length(l_month)){
  l_m <- filter(lexington, month == l_month[i])
  l_m$seq <- 1:nrow(l_m)
  lexington[lexington$month == l_month[i],"pos"] <- l_m$seq
}
lexington$pos_perc <- lexington$pos/nrow(lexington) - .01

lexington_by_month <- lexington %>% group_by(month) %>% summarise(count = n()) %>% mutate(perc = count/nrow(lexington))
  
ggplot()+geom_area(data = month_protest, aes(x = month, y = perc), fill = "#707589")+
  geom_point(data = lexington, aes(x = month, y = pos_perc), size = 5, color = "white")+
  theme(panel.background= element_rect(fill = "black"), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+labs(title = "Lexington City, VA protests per month")+
  scale_y_continuous(limits = c(0, .5)) + 
  geom_segment(data = lexington_by_month, aes(x = month, xend = month, y = 0, yend = perc), color = "white")




miami <- filter(tag, location_id == "33100")

miami <-  mutate(miami, month=as.Date(cut(date, breaks="month")))

mi_month <- unique(miami$month)
miami$pos <- NA

for(i in 1:length(mi_month)){
  mi_m <- filter(miami, month == mi_month[i])
  mi_m$seq <- 1:nrow(mi_m)
  miami[miami$month == mi_month[i],"pos"] <- mi_m$seq
}
miami$pos_perc <- miami$pos/nrow(miami) - .0001

miami_by_month <- miami %>% group_by(month) %>% summarise(count = n()) %>% mutate(perc = count/nrow(miami) - .0001)

ggplot()+geom_area(data = month_protest, aes(x = month, y = perc), fill = "#707589")+
  geom_point(data = miami, aes(x = month, y = pos_perc), size = .01, color = "white")+
  theme(panel.background= element_rect(fill = "black"), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+labs(title = "Miami-Ft Ld-West Palm Beach protests per month")+
  scale_y_continuous(limits = c(-.01, .2)) + 
  geom_segment(data = miami_by_month, aes(x = month, xend = month, y = 0, yend = perc), color = "white", size = .5)


ggplot()+geom_area(data = month_protest, aes(x = month, y = perc), fill = "#707589")+
  geom_segment(data = miami_by_month, aes(x = month, xend = month, y = 0, yend = perc), color = "white")+
  geom_point(data = miami, aes(x = month, y = pos_perc, color = as.character(guns)), size = .5)+
  scale_color_manual(values = c("white", "yellow"))+
  theme(panel.background= element_rect(fill = "black"), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), legend.position = "none")+labs(title = "Miami, FL protests per month")+
  scale_y_continuous(limits = c(0, .25))




los_ang <- filter(tag, location_id == "31080")

los_ang <-  mutate(los_ang, month=as.Date(cut(date, breaks="month")))

la_month <- unique(los_ang$month)
los_ang$pos <- NA

for(i in 1:length(la_month)){
  la_m <- filter(los_ang, month == la_month[i])
  la_m$seq <- 1:nrow(la_m)
  los_ang[los_ang$month == la_month[i],"pos"] <- la_m$seq
}
los_ang$pos_perc <- los_ang$pos/nrow(los_ang) - .0001

los_ang_by_month <- los_ang %>% group_by(month) %>% summarise(count = n()) %>% mutate(perc = count/nrow(los_ang) - .0001)

ggplot()+geom_area(data = month_protest, aes(x = month, y = perc), fill = "#707589")+
  geom_point(data = los_ang, aes(x = month, y = pos_perc), size = .01, color = "white")+
  theme(panel.background= element_rect(fill = "black"), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+labs(title = "Los Angeles protests per month")+
  scale_y_continuous(limits = c(-.01, .2)) + 
  geom_segment(data = los_ang_by_month, aes(x = month, xend = month, y = 0, yend = perc), color = "white", size = .5)




nyc <- filter(tag, location_id == "35620")

nyc <-  mutate(nyc, month=as.Date(cut(date, breaks="month")))

nyc_month <- unique(nyc$month)
nyc$pos <- NA

for(i in 1:length(nyc_month)){
  nyc_m <- filter(nyc, month == nyc_month[i])
  nyc_m$seq <- 1:nrow(nyc_m)
  nyc[nyc$month == nyc_month[i],"pos"] <- nyc_m$seq
}
nyc$pos_perc <- nyc$pos/nrow(nyc) - .0001

nyc_by_month <- nyc %>% group_by(month) %>% summarise(count = n()) %>% mutate(perc = count/nrow(nyc) - .0001)

ggplot()+geom_area(data = month_protest, aes(x = month, y = perc), fill = "#707589")+
  geom_point(data = nyc, aes(x = month, y = pos_perc), size = .005, color = "white")+
  theme(panel.background= element_rect(fill = "black"), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+labs(title = "NYC protests per month")+
  scale_y_continuous(limits = c(-.01, .2)) + 
  geom_segment(data = nyc_by_month, aes(x = month, xend = month, y = 0, yend = perc), color = "white", size = .25)





loc <- "data/working"

place <- read_csv(file.path(loc, "all_county_cbsa_summary.csv"))
place <- place %>% clean_names()

place <- mutate(place, protest_per_10k = (protest_count/pop)*10000)


ggplot(place, aes(x = log(pop), y = protest_count))+geom_point(alpha = .4, color = "white", stroke = 0)+
  labs(title  = "Protest count by log of population", 
       subtitle = "all CBSAs and non-affiliated counties") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"))+
  scale_x_continuous(breaks = c(4.6, 6.9, 9.2, 11.5, 13.8, 16.1))

ggplot(place, aes(x = log(pop), y = protest_per_10k))+geom_point(alpha = .4, color = "white", stroke = 0)+
  labs(title  = "Protest rate per 10k people by log of population", 
       subtitle = "all CBSAs and non-affiliated counties") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"))+
  scale_x_continuous(breaks = c(4.6, 6.9, 9.2, 11.5, 13.8, 16.1))

place <- mutate(place, alaska = state == "AK" & is.na(state) == FALSE)

ggplot(place, aes(x = log(pop), y = protest_per_10k, color = alaska))+geom_point(alpha = .6, stroke = 0)+
  labs(title  = "Protest rate per 10k people by log of population", 
       subtitle = "Alaska highlight", x = "Population (log scale)", y = "Protests per 10,000 residents") + 
  scale_color_manual(values = c("white", "yellow"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"), legend.position = "none")+
  scale_x_continuous(breaks = c(4.6, 6.9, 9.2, 11.5, 13.8, 16.1), labels = c("100", "1,000", "10,000", "100,000", "1,000,000",
       "10,000,000"))

