#### Assessing: should constellations have a temporal component?


library(tidyverse)
library(janitor)
library(stringr)
library(muckrakr)
library(sf)
library(nngeo)

loc <- "data/working"
protest <- clean_names(read_csv(file.path(loc, "protest_additional_tags.csv")))
#protest <- clean_names(read_csv(file.path(loc, "protest_all_states.csv")))

place <- clean_names(read_csv(file.path(loc, "all_county_cbsa_summary.csv")))

tag <- untangle(protest, "tags", pattern = ";")

protest$month_yr <- format(as.Date(protest$date), "%Y-%m")


#looking at racial justice/white supremacy/etc first:

race <- tag %>% filter(race_confed == 1) %>% select(1:32)

race_state_week <- race %>% group_by(state, week) %>% summarise(count = n())

ggplot(race, aes(y = state, x = date))+geom_point(alpha = .4)
ggplot(race_state_week, aes(x = week, y = count, color = state))+geom_line()

police <- tag %>% filter(police == 1) %>% select(1:32, "month_yr")
police_state_week <- police %>% group_by(state, week) %>% summarise(count = n())
police_state_month <- police %>% group_by(state, month_yr) %>% summarise(count = n())

ggplot(police, aes(y = state, x = date))+geom_point(alpha = .4)
ggplot(police_state_week, aes(x = week, y = count, color = state))+geom_line()
ggplot(police_state_week, aes(x = week, fill = state))+geom_bar()
ggplot(police_state_week, aes(x = week, y = state, height = count))+geom_density_ridges()
ggplot(police, aes(y = state, x = date))+geom_density_ridges()
ggplot(police_state_month, aes(x = month_yr, y = state))+geom_density_ridges()

police_order <- arrange(police, month_yr, state, cbsa_name) %>% filter(date < "2018-01-01", date > "2017-01-01") 
race_order <- arrange(police, month_yr, state, cbsa_name)

usa <- map_data("usa")
ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_path(data = race_order, aes(x=internal_point_longitude, y = internal_point_latitude), color = "white", alpha = .05)+ xlim(-125,-65)+ylim(25,50) +
  scale_size_continuous(range = c(.1,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
  coord_map("albers", lat0=30, lat1=40)

police_unique_states <- police %>% group_by(state, week) %>% summarise(count = n()) %>%
  group_by(week) %>% summarise(unique_states = n(), protest_total = sum(count))
ggplot(police_unique_states, aes(x = week, y = unique_states))+geom_line()

police %>% group_by(week, state) %>% summarise(count = n()) %>% ggplot(aes(x = count))+geom_histogram()

collective_bargaining <- tag %>% filter(collective_bargaining == 1) %>% select(1:32, "month_yr")
cb_state_week <- collective_bargaining %>% group_by(state, week) %>% summarise(count = n())

ggplot(cb_state_week, aes(x = week, y = count, color = state))+geom_line()
ggplot(cb_state_week, aes(x = week, y = count, color = state))+geom_line()
ggplot(collective_bargaining, aes(x = week, y = state))+geom_density_ridges()


environment <- tag %>% filter(environment == 1)%>% select(1:32, "month_yr")
ggplot(environment, aes(x = month_yr))+geom_bar()
environment_order <- arrange(environment, date, state, cbsa_name)
ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_path(data = environment_order, aes(x=internal_point_longitude, y = internal_point_latitude), color = "white", alpha = .05)+ xlim(-125,-65)+ylim(25,50) +
  scale_size_continuous(range = c(.1,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
  coord_map("albers", lat0=30, lat1=40)


###connect each place that protests within the same week/month?

environment %>% filter(month_yr == "02-2017") %>% 
  
ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_point(data = filter(environment, month_yr == "2017-05"), aes(x = internal_point_longitude, y = internal_point_latitude), color = "white", fill = "white", size = 1, alpha = .2)+ xlim(-125,-65)+ylim(25,50) +
  scale_size_continuous(range = c(.2,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
  coord_map("albers", lat0=30, lat1=40)




find_near_c <- function(df){
  df$id <- 1:nrow(df)
  df_sf <- st_as_sf(df, coords = c("internal_point_longitude", "internal_point_latitude"),
                    crs = 4326)
  miami <- data.frame(longitude = -80.49914, latitude = 25.61058)
  miami <- st_as_sf(miami, coords = c("longitude", "latitude"), crs = 4326)
  miami_near <- which(st_nn(miami[1,], df_sf, k = 1, sparse = FALSE))
  order <- df_sf[miami_near,]
  df_sf <- df_sf[-miami_near,]
  pos <- integer()
  for(i in 2:nrow(df_sf)){
    r <- order[i-1,]
    near <- which(st_nn(r, df_sf, k = 1, sparse = FALSE))
    order[i,] <- df_sf[near,]
    df_sf <- df_sf[-near,]
  }
  order <- rbind(order, df_sf)
  df <- df[order$id,]
  df
}

env_const_test <- environment %>% filter(month_yr == "2017-05")

env_const_test <- find_near_c(env_const_test)

ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_polygon(data = env_const_test, aes(x = internal_point_longitude, y = internal_point_latitude, group = week), fill = "white", size = 1, alpha = .05)+ xlim(-125,-65)+ylim(25,50) +
  scale_size_continuous(range = c(.2,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
  coord_map("albers", lat0=30, lat1=40)

env_const <- find_near_c(environment)
env_place <- env_const %>% group_by(cbsa_name, internal_point_longitude, internal_point_latitude) %>% summarise(count = n())


ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_path(data = env_const, aes(x = internal_point_longitude, y = internal_point_latitude, group = date), color = "white", size = 1, alpha = .05)+ xlim(-125,-65)+ylim(25,50) +
  geom_point(data = env_place, aes(x = internal_point_longitude, y = internal_point_latitude, size = count), color = "white", alpha = .2)+
  scale_size_continuous(range = c(.2,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
  coord_map("albers", lat0=30, lat1=40)+ggtitle("Police")

police_const_test <- police %>% filter(month_yr == "2017-09" | month_yr == "2017-10")
police_const_test <- find_near_c(police_const_test)
police_place <- police_const_test %>% group_by(cbsa_name, internal_point_longitude, internal_point_latitude) %>% summarise(count = n())

ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_path(data = police_const_test, aes(x = internal_point_longitude, y = internal_point_latitude, group = week), color = "white", size = 1, alpha = .05)+ xlim(-125,-65)+ylim(25,50) +
  geom_point(data = police_place, aes(x = internal_point_longitude, y = internal_point_latitude, size = count), color = "white", alpha = .2)+
  scale_size_continuous(range = c(.2,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
  coord_map("albers", lat0=30, lat1=40)+ggtitle("Police, September and October 2017")


police_const_test <- police %>% filter(month_yr == "2018-09" | month_yr == "2018-10")
police_const_test <- find_near_c(police_const_test)
police_place <- police_const_test %>% group_by(cbsa_name, internal_point_longitude, internal_point_latitude) %>% summarise(count = n())


ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_path(data = police_const_test, aes(x = internal_point_longitude, y = internal_point_latitude, group = week), color = "white", size = 1, alpha = .05)+ xlim(-125,-65)+ylim(25,50) +
  geom_point(data = police_place, aes(x = internal_point_longitude, y = internal_point_latitude, size = count), color = "white", alpha = .2)+
  scale_size_continuous(range = c(.2,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
  coord_map("albers", lat0=30, lat1=40)+ggtitle("Police, September and October 2018")

police_const <- find_near_c(police)
ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_path(data = police_const, aes(x = internal_point_longitude, y = internal_point_latitude, group = week), color = "white", size = .5, alpha = .05)+ xlim(-125,-65)+ylim(25,50) +
  geom_point(data = police_place, aes(x = internal_point_longitude, y = internal_point_latitude, size = count), color = "white", alpha = .5)+
  scale_size_continuous(range = c(.2,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
  coord_map("albers", lat0=30, lat1=40)+ggtitle("Police")


police_const <- find_near_c(police)
ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_path(data = police_const, aes(x = internal_point_longitude, y = internal_point_latitude, group = date), color = "white", size = 1, alpha = .05)+ xlim(-125,-65)+ylim(25,50) +
  geom_point(data = police_place, aes(x = internal_point_longitude, y = internal_point_latitude, size = count), color = "white", alpha = .2)+
  scale_size_continuous(range = c(.2,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
  coord_map("albers", lat0=30, lat1=40)+ggtitle("Police")


###bringing in constellation places for police

const_loc <- "constellations"

pol_const_list <- read_csv(file.path(const_loc, "police/police_constellation_places.csv"))

###events for those places

pol_const_event <- filter(police, location_id %in% pol_const_list$location_id)

###which constellation places had events on the same weeks (or consecutive weeks...?)

View(pol_const_event %>% group_by(week, location_name) %>% summarise(count = n()) %>% arrange(week))
View(pol_const_event %>% group_by(week, location_name) %>% summarise(count = n()) %>% group_by(week) %>% summarise(protest_count = sum(count), place_count = n()) %>% arrange(week))

pol_const_order <- find_near_c(pol_const_event)

ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_polygon(data = pol_const_order, aes(x = internal_point_longitude, y = internal_point_latitude, group = week), colour=alpha("white", 0.05), size = 1, fill = "white", alpha = 0.05)+ xlim(-125,-65)+ylim(25,50) +
  geom_point(data = pol_const_list, aes(x = internal_point_longitude, y = internal_point_latitude, size = protest_count), color = "white", alpha = .5)+
  scale_size_continuous(range = c(.2,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
  coord_map("albers", lat0=30, lat1=40)+ggtitle("Police")

##connecting all places with protests in the same week
pol_const_test <- filter(pol_const_event, week == "2017-08-28")
pol_const_test_combine <- as.data.frame(t(combn(pol_const_test$location_id, m = 2)), stringsAsFactors = FALSE)
colnames(pol_const_test_combine) <- c("location_id_1", "location_id_2")
pol_const_test_combine$location_id_1 <- as.integer(pol_const_test_combine$location_id_1)
pol_const_test_combine$location_id_2 <- as.integer(pol_const_test_combine$location_id_2)

pol_const_test_combine <- left_join(pol_const_test_combine, pol_const_list[,c("location_id", "internal_point_longitude", "internal_point_latitude")], by = c("location_id_1" = "location_id"))
colnames(pol_const_test_combine) <- c("location_id_1", "location_id_2", "long_1", "lat_1")
pol_const_test_combine <- left_join(pol_const_test_combine, pol_const_list[,c("location_id", "internal_point_longitude", "internal_point_latitude")], by = c("location_id_2" = "location_id"))
colnames(pol_const_test_combine) <- c("location_id_1", "location_id_2", "long_1", "lat_1", "long_2", "lat_2")



ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_segment(data = pol_const_test_combine, aes(x = long_1, y = lat_1, xend = long_2, yend = lat_2), color = "white", alpha =  .05)+
  geom_point(data = pol_const_list, aes(x = internal_point_longitude, y = internal_point_latitude, size = protest_count), color = "white", alpha = .5)+
  scale_size_continuous(range = c(.2,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
  coord_map("albers", lat0=30, lat1=40)+ggtitle("Police")

##function to make combinations for every point within every week?

const_comb <- function(event, const_place){
  
  event <- filter(event, location_id %in% const_place$location_id)
  
  comb_full <- data.frame(week = as.Date(character()), 
               location_id_1 = integer(),
               location_id_2 = integer(),
               long_1 = numeric(),
               lat_1 = numeric(),
               long_2 = numeric(),
               lat_2 = numeric())
  multi_week <- event %>% group_by(location_id, week) %>% 
    summarise(count = n()) %>% group_by(week) %>% summarise(count = n()) %>% filter(count > 1)
  
  for(i in 1:length(unique(event$week))){
    #print(unique(event$week)[i])
      week_i <- filter(event, week == unique(event$week)[i])
      if(length(unique(week_i$location_id)) > 1){
      #print(unique(week_i$week))
      loc_i <- week_i %>% group_by(location_id) %>% summarise(count = n())
      
      comb_i <- as.data.frame(t(combn(loc_i$location_id, m = 2)), stringsAsFactors = FALSE)
      #print(comb_i)
      
      colnames(comb_i) <- c("location_id_1", "location_id_2")
      comb_i$location_id_1 <- as.integer(comb_i$location_id_1)
      comb_i$location_id_2 <- as.integer(comb_i$location_id_2)
      
      comb_i <- left_join(comb_i, const_place[,c("location_id", "internal_point_longitude", "internal_point_latitude")], by = c("location_id_1" = "location_id"))
      colnames(comb_i) <- c("location_id_1", "location_id_2", "long_1", "lat_1")
      comb_i <- left_join(comb_i, const_place[,c("location_id", "internal_point_longitude", "internal_point_latitude")], by = c("location_id_2" = "location_id"))
      colnames(comb_i) <- c("location_id_1", "location_id_2", "long_1", "lat_1", "long_2", "lat_2")
      
      comb_i$week <- unique(event$week)[i]
      comb_full <- rbind(comb_full, comb_i)
      comb_i <- NA
      }
  }
  comb_full
}

pol_comb <- const_comb(police, pol_const_list)

pol_comb$id <- 1:nrow(pol_comb)

pol_comb_melt <- pivot_longer(data = pol_comb[,c("location_id_1", 
                   "location_id_2", "id", "week")], c("location_id_1", 
                   "location_id_2"), values_to = "location_id") %>% arrange(location) %>%
                    group_by(id, week) %>% summarise(pair = paste(location))

pol_comb_desc <- pol_comb %>% group_by(location_id_1, location_id_2, long_1, lat_1, long_2, lat_2) %>% summarise(count = n()) %>%
  arrange(desc(count))

library(cooccur)
library(udpipe)

pol_comb_cooc <- pol_comb_melt %>% select(id, location_id)
colnames(pol_comb_cooc) <- c("group", "term")

cooccurrence(pol_comb_melt, group = "id", term = "location_id")

pol_comb_desc_2 <- pol_comb %>% group_by(location_id_2, location_id_1, long_1, lat_1, long_2, lat_2) %>% summarise(count = n()) %>%
  arrange(desc(count))

ggplot(pol_comb_desc, aes(x = count))+geom_histogram(binwidth = 1)

pol_comb_min <- filter(pol_comb_desc, count > 2)

ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_segment(data = pol_comb_min, aes(x = long_1, y = lat_1, xend = long_2, yend = lat_2), color = "white", alpha =  .1)+
  geom_point(data = pol_const_list, aes(x = internal_point_longitude, y = internal_point_latitude, size = protest_count), color = "white")+
  scale_size_continuous(range = c(.2,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
  coord_map("albers", lat0=30, lat1=40)+ggtitle("Police")
