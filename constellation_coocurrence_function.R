
library(tidyverse)
library(janitor)
library(stringr)
library(muckrakr)
library(sf)
library(nngeo)

loc <- "data/final"
protest <- clean_names(read_csv(file.path(loc, "protest_nov1_addtl_tags.csv")))
protest$date <- as.Date(protest$date, "%m/%d/%y")
protest$week <- as.Date(cut(protest$date, breaks="week", starts.on.monday=TRUE))

place <- clean_names(read_csv(file.path(loc, "nov1_all_county_cbsa_summary.csv")))

protest$month_yr <- format(as.Date(protest$date), "%Y-%m")

tag <- untangle(protest, "tags", pattern = ";")

usa <- map_data("usa")

const_loc <- "constellations"


##make matrix of location x week for police constellation protests

##for each location, find OTHER locations that had protests on the same weeks,
##and sum up how often co-occurrence happened

##LIM VERSION IS THE GOOD ONE!!! there is also a version that allows ties and counts
##by total # of protest instead of count of co-occurring weeks, but don't do it
const_lim_pairs <- function(events = tag, topic){
  
  #do NOT talk to me about piping this function!!!!
  
  #const_place <- read_csv(file.path(const_loc, paste(topic,"/",topic,"_constellation_places.csv", sep = "")))
  
  const_place <- police_const_place_2
  
  ####get down to events about the topic that occurred in constellation places:
  
  topic_event <- events %>% filter(get(topic) == 1) %>% select(1:21)
  
  const_event <- filter(topic_event, location_id %in% const_place$location_id)
  
  contingency <- as.data.frame.matrix(table(const_event[,c("week", "location_id")]))
  
  ####set up dataframe for pairs of places
  
  all_pair_summary <- data.frame(place_1 = as.character(), place_2 = as.character(), count = integer(), stringsAsFactors = FALSE)
  
  ####for each location, find out how many times OTHER places protested 
  ####about the topic on the same week
  
  for(i in 1:ncol(contingency)){
    
    this_pair_summary <- data.frame(place_1 = character(), 
                                    place_2 = character(), 
                                    count = integer(), stringsAsFactors = FALSE)
    
    this_place <- colnames(contingency)[i]
    
    this_place_events <- contingency %>% filter(get(this_place) >= 1)
    
    this_place_events <-  select(this_place_events, -this_place)
    
    this_place_events <- t(as.matrix(this_place_events))
    
    this_pair_summary <- data.frame(place_1 = as.character(this_place), 
                                    place_2 = as.character(rownames(this_place_events)), 
                                    stringsAsFactors = FALSE)
    
    #this_pair_summary$count <- apply(this_place_events, MARGIN = 1, sum)
    
    this_pair_summary$count <- apply(this_place_events, MARGIN = 1, function(x){sum(x >=1)})
    
    
    all_pair_summary <- rbind(all_pair_summary, this_pair_summary)
  }
  
  
  ####set a threshhold--if any two places protest together more often
  ####than this, they should be connected in the constellation, even
  ####if not the max combination.
  
  top_percentile <- quantile(filter(all_pair_summary, count != 0)$count, probs = .98)
  
  ####set up dataframe for top pairs of places
  
  const_pair_summary <- data.frame(place_1 = character(), place_2 = character(), count = integer())
  
  ####for each place, find the place that protests with it MOST often,
  ####as well as any places that protest together often enough to
  ####pass the threshhold set above.
  
  for(p in 1:length(unique(all_pair_summary$place_1))){
    
    this_place <-unique(all_pair_summary$place_1)[p]
    
    #print(this_place) 
    
    this_place_pairs <- filter(all_pair_summary, place_1 == this_place)
    
    #function to find nearest pair-neighbor if multiple tied for top
    
    find_near <- function(df, const_place, place){
      this_place_loc <- const_place[const_place$location_id == place, 
                                    c("internal_point_longitude", 
                                      "internal_point_latitude")]
      df_sf <- st_as_sf(df, coords = c("internal_point_longitude", 
                                       "internal_point_latitude"),
                        crs = 4326)
      df_sf <- filter(df_sf, location_id != place)
      this_place_loc <- st_as_sf(this_place_loc, 
                                 coords = c("internal_point_longitude", 
                                            "internal_point_latitude"), 
                                 crs = 4326)
      this_place_near <- df_sf[st_nn(this_place_loc[1,], df_sf, k = 1, sparse = FALSE),]
      this_place_near
    }
    
    #look for places with maximum # of shared-week protests:  
    max_pair <- filter(this_place_pairs, 
                       count == max(this_place_pairs$count))
    
    #if multiple top places, get down to physically closest one:
    
    top_pair <- if(nrow(max_pair)>1){
      max_place_details <- filter(const_place, location_id %in% max_pair$place_2)
      pair_id <- find_near(max_place_details, const_place, this_place)$location_id
      
      filter(max_pair, place_2 == pair_id)
      
    } else {
      max_pair
    }
    
    const_pair_summary <- rbind(const_pair_summary, top_pair)
    
    
    add_in <- this_place_pairs %>% filter(count >= top_percentile, ! place_2 %in% top_pair$place_2)
    
    if(nrow(add_in) >0){
      const_pair_summary <-rbind(const_pair_summary, add_in)
    }
    
  }
  
  const_place$location_id <- as.character(const_place$location_id)
  
  lat_long_const_pair_summary <- left_join(const_pair_summary, 
                                           const_place[,c("location_id", "location_name", 
                                                          "internal_point_longitude", 
                                                          "internal_point_latitude")],
                                           by = c("place_1" = "location_id"))
  colnames(lat_long_const_pair_summary) <- c("place_1_id", "place_2_id", "count", "place_1_name", "place_1_long", "place_1_lat")
  
  lat_long_const_pair_summary <- left_join(lat_long_const_pair_summary, 
                                           const_place[,c("location_id", "location_name", 
                                                          "internal_point_longitude", 
                                                          "internal_point_latitude")],
                                           by = c("place_2_id" = "location_id"))
  
  colnames(lat_long_const_pair_summary) <- c("place_1_id", "place_2_id", "count", "place_1_name", "place_1_long", "place_1_lat", 
                                             "place_2_name", "place_2_long", "place_2_lat")
  
  
  const_plot <- ggplot() + 
    geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
    geom_point(data = topic_event, aes(x=longitude, y = latitude), 
               size = .1, color = "gray", alpha = .1)+
    geom_segment(data = lat_long_const_pair_summary, aes(x = place_1_long,
               y = place_1_lat, xend = place_2_long, yend = place_2_lat, alpha = count), 
               color = "white") +
    geom_point(data = const_place, aes(x = internal_point_longitude, 
               y = internal_point_latitude, size = protest_count), color = "white") +
    scale_size_continuous(range = c(.2,3))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_rect(fill="#707589", color="#707589"),
          axis.line = element_blank(), axis.title.x=element_blank(), 
          axis.title.y=element_blank(), axis.text.x=element_blank(), 
          axis.text.y=element_blank(), axis.ticks = element_blank())+
    xlim(-125,-65)+ylim(25,50) +
    coord_map("albers", lat0=30, lat1=40) +
    ggtitle(paste(topic, ", connecting places that coocur most often", sep=""))
  
  print(const_plot)
  
  lat_long_const_pair_summary
}

police_const_lim_pairs <- const_lim_pairs(topic = "police")
guns_const_lim_pairs <- const_lim_pairs(topic = "guns")
women_const_lim_pairs <- const_lim_pairs(topic = "women")








#########################################
##### Old scratch code, do not use ######
#########################################

pol_const_list <- read_csv(file.path(const_loc, "police/police_constellation_places.csv"))
environment_const_list <- read_csv(file.path(const_loc, "environment/environment_constellation_places.csv"))
healthcare_const_list <- read_csv(file.path(const_loc, "healthcare/healthcare_constellation_places.csv"))
women_const_list <- read_csv(file.path(const_loc, "women/women_constellation_places.csv"))
supreme_court_const_list <- read_csv(file.path(const_loc, "supreme_court/supreme_court_constellation_places.csv"))
race_confed_const_list <- read_csv(file.path(const_loc, "race_confed/race_confed_constellation_places.csv"))
education_const_list <- read_csv(file.path(const_loc, "education/education_constellation_places.csv"))
immigration_const_list <- read_csv(file.path(const_loc, "immigration/immigration_constellation_places.csv"))
executive_const_list <- read_csv(file.path(const_loc, "executive/executive_constellation_places.csv"))
guns_const_list <- read_csv(file.path(const_loc, "guns/guns_constellation_places.csv"))
collective_bargaining_const_list <- read_csv(file.path(const_loc, "collective_bargaining/collective_bargaining_constellation_places.csv"))


police_const_all_pairs <- const_pairs(const_place = pol_const_list, topic = "police")
environment_const_all_pairs <- const_pairs(const_place = environment_const_list, topic = "environment")
healthcare_const_all_pairs <- const_pairs(const_place = healthcare_const_list, topic = "healthcare")
women_const_all_pairs <- const_pairs(const_place =women_const_list, topic = "women")
supreme_court_const_all_pairs <- const_pairs(const_place =supreme_court_const_list, topic = "supreme_court")
race_confed_const_all_pairs <- const_pairs(const_place =race_confed_const_list, topic = "race_confed")
education_const_all_pairs <- const_pairs(const_place = education_const_list, topic = "education")


ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_point(data = police, aes(x=longitude, y = latitude), size = .1, color = "gray", alpha = .1)+
  geom_segment(data = police_const_all_pairs, aes(x = place_1_long, y = place_1_lat, xend = place_2_long, yend = place_2_lat, alpha = count), color = "white")+
  geom_point(data = pol_const_list, aes(x = internal_point_longitude, y = internal_point_latitude, size = protest_count), color = "white")+
  scale_size_continuous(range = c(.2,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
   xlim(-125,-65)+ylim(25,50) +
  coord_map("albers", lat0=30, lat1=40)+ggtitle("Police, connecting places that coocur most often")

police_const_lim_pairs <- const_pairs_limit_pairs(const_place = pol_const_list, topic = "police")

ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_point(data = police, aes(x=longitude, y = latitude), size = .1, color = "gray", alpha = .1)+
  geom_segment(data = police_const_lim_pairs, aes(x = place_1_long, y = place_1_lat, xend = place_2_long, yend = place_2_lat, alpha = count), color = "white")+
  geom_point(data = pol_const_list, aes(x = internal_point_longitude, y = internal_point_latitude, size = protest_count), color = "white")+
  scale_size_continuous(range = c(.2,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
  xlim(-125,-65)+ylim(25,50) +
  coord_map("albers", lat0=30, lat1=40)+ggtitle("Police, connecting places that coocur most often")



environment <- tag %>% filter(environment == 1) %>% select(1:21)

ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_point(data = environment, aes(x=longitude, y = latitude), size = .1, color = "gray", alpha = .1)+
  geom_segment(data = environment_const_all_pairs, aes(x = place_1_long, y = place_1_lat, xend = place_2_long, yend = place_2_lat, alpha = count), color = "white")+
  geom_point(data = environment_const_list, aes(x = internal_point_longitude, y = internal_point_latitude, size = protest_count), color = "white")+
  scale_size_continuous(range = c(.2,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
  xlim(-125,-65)+ylim(25,50) +
  coord_map("albers", lat0=30, lat1=40)+ggtitle("Environment, connecting places that coocur most often")

environment_const_lim_pairs <- const_pairs_limit_pairs(const_place =environment_const_list, topic = "environment")
ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_point(data = environment, aes(x=longitude, y = latitude), size = .1, color = "gray", alpha = .1)+
  geom_segment(data = environment_const_lim_pairs, aes(x = place_1_long, y = place_1_lat, xend = place_2_long, yend = place_2_lat, alpha = count), color = "white")+
  geom_point(data = environment_const_list, aes(x = internal_point_longitude, y = internal_point_latitude, size = protest_count), color = "white")+
  scale_size_continuous(range = c(.2,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
  xlim(-125,-65)+ylim(25,50) +
  coord_map("albers", lat0=30, lat1=40)+ggtitle("Environment, connecting places that coocur most often")


supreme_court <- tag %>% filter(supreme_court == 1) %>% select(1:21)

ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_point(data = supreme_court, aes(x=longitude, y = latitude), size = .1, color = "gray", alpha = .1)+
  geom_segment(data = supreme_court_const_all_pairs, aes(x = place_1_long, y = place_1_lat, xend = place_2_long, yend = place_2_lat, alpha = count), color = "white")+
  geom_point(data = supreme_court_const_list, aes(x = internal_point_longitude, y = internal_point_latitude, size = protest_count), color = "white")+
  scale_size_continuous(range = c(.2,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
  xlim(-125,-65)+ylim(25,50) +
  coord_map("albers", lat0=30, lat1=40)+ggtitle("Supreme Court, connecting places that coocur most often")

supreme_court_const_lim_pairs <- const_pairs_limit_pairs(const_place =supreme_court_const_list, topic = "supreme_court")

ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_point(data = supreme_court, aes(x=longitude, y = latitude), size = .1, color = "gray", alpha = .1)+
  geom_segment(data = supreme_court_const_lim_pairs, aes(x = place_1_long, y = place_1_lat, xend = place_2_long, yend = place_2_lat, alpha = count), color = "white")+
  geom_point(data = supreme_court_const_list, aes(x = internal_point_longitude, y = internal_point_latitude, size = protest_count), color = "white")+
  scale_size_continuous(range = c(.2,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
  xlim(-125,-65)+ylim(25,50) +
  coord_map("albers", lat0=30, lat1=40)+ggtitle("Supreme Court, connecting places that coocur most often")




healthcare <- tag %>% filter(healthcare == 1) %>% select(1:21)

ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_point(data = healthcare, aes(x=longitude, y = latitude), size = .1, color = "gray", alpha = .1)+
  geom_segment(data = healthcare_const_all_pairs, aes(x = place_1_long, y = place_1_lat, xend = place_2_long, yend = place_2_lat, alpha = count), color = "white")+
  geom_point(data = healthcare_const_list, aes(x = internal_point_longitude, y = internal_point_latitude, size = protest_count), color = "white")+
  scale_size_continuous(range = c(.2,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
  xlim(-125,-65)+ylim(25,50) +
  coord_map("albers", lat0=30, lat1=40)+ggtitle("Healthcare, connecting places that coocur most often")

healthcare_const_lim_pairs <- const_pairs_limit_pairs(const_place =healthcare_const_list, topic = "healthcare")
ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_point(data = healthcare, aes(x=longitude, y = latitude), size = .1, color = "gray", alpha = .1)+
  geom_segment(data = healthcare_const_lim_pairs, aes(x = place_1_long, y = place_1_lat, xend = place_2_long, yend = place_2_lat, alpha = count), color = "white")+
  geom_point(data = healthcare_const_list, aes(x = internal_point_longitude, y = internal_point_latitude, size = protest_count), color = "white")+
  scale_size_continuous(range = c(.2,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
  xlim(-125,-65)+ylim(25,50) +
  coord_map("albers", lat0=30, lat1=40)+ggtitle("Healthcare, connecting places that coocur most often")



women <- tag %>% filter(women == 1) %>% select(1:21)


ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_point(data = women, aes(x=longitude, y = latitude), size = .1, color = "gray", alpha = .1)+
  geom_segment(data = women_const_all_pairs, aes(x = place_1_long, y = place_1_lat, xend = place_2_long, yend = place_2_lat, alpha = count, size = count), color = "white")+
  geom_point(data = women_const_list, aes(x = internal_point_longitude, y = internal_point_latitude, size = protest_count), color = "white")+
  scale_size_continuous(range = c(.2,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
  xlim(-125,-65)+ylim(25,50) +
  coord_map("albers", lat0=30, lat1=40)+ggtitle("Women's Rights, connecting places that coocur most often")

women_const_lim_pairs <- const_pairs_limit_pairs(const_place =women_const_list, topic = "women")

ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_point(data = women, aes(x=longitude, y = latitude), size = .1, color = "gray", alpha = .1)+
  geom_segment(data = women_const_lim_pairs, aes(x = place_1_long, y = place_1_lat, xend = place_2_long, yend = place_2_lat, alpha = count, size = count), color = "white")+
  geom_point(data = women_const_list, aes(x = internal_point_longitude, y = internal_point_latitude, size = protest_count), color = "white")+
  scale_size_continuous(range = c(.2,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
  xlim(-125,-65)+ylim(25,50) +
  coord_map("albers", lat0=30, lat1=40)+ggtitle("Women's Rights, connecting places that coocur most often")


race_confed <- tag %>% filter(race_confed == 1) %>% select(1:21)


ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_point(data = race_confed, aes(x=longitude, y = latitude), size = .1, color = "gray", alpha = .1)+
  geom_segment(data = race_confed_const_all_pairs, aes(x = place_1_long, y = place_1_lat, xend = place_2_long, yend = place_2_lat, alpha = count, size = count), color = "white")+
  geom_point(data = race_confed_const_list, aes(x = internal_point_longitude, y = internal_point_latitude, size = protest_count), color = "white")+
  scale_size_continuous(range = c(.2,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
  xlim(-125,-65)+ylim(25,50) +
  coord_map("albers", lat0=30, lat1=40)+ggtitle("Racial justice, connecting places that coocur most often")

race_confed_const_lim_pairs <- const_pairs_limit_pairs(const_place =race_confed_const_list, topic = "race_confed")

ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_point(data = race_confed, aes(x=longitude, y = latitude), size = .1, color = "gray", alpha = .1)+
  geom_segment(data = race_confed_const_lim_pairs, aes(x = place_1_long, y = place_1_lat, xend = place_2_long, yend = place_2_lat, alpha = count, size = count), color = "white")+
  geom_point(data = race_confed_const_list, aes(x = internal_point_longitude, y = internal_point_latitude, size = protest_count), color = "white")+
  scale_size_continuous(range = c(.2,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
  xlim(-125,-65)+ylim(25,50) +
  coord_map("albers", lat0=30, lat1=40)+ggtitle("Racial justice, connecting places that coocur most often")


education <- tag %>% filter(education == 1) %>% select(1:21)

ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_point(data = education, aes(x=longitude, y = latitude), size = .1, color = "gray", alpha = .1)+
  geom_segment(data = education_const_all_pairs, aes(x = place_1_long, y = place_1_lat, xend = place_2_long, yend = place_2_lat, alpha = count), color = "white")+
  geom_point(data = education_const_list, aes(x = internal_point_longitude, y = internal_point_latitude, size = protest_count), color = "white")+
  scale_size_continuous(range = c(.2,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
  xlim(-125,-65)+ylim(25,50) +
  coord_map("albers", lat0=30, lat1=40)+ggtitle("Education, connecting places that coocur most often")

education_const_lim_pairs <- const_pairs_limit_pairs(const_place = education_const_list, topic = "education")
ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_point(data = education, aes(x=longitude, y = latitude), size = .1, color = "gray", alpha = .1)+
  geom_segment(data = education_const_lim_pairs, aes(x = place_1_long, y = place_1_lat, xend = place_2_long, yend = place_2_lat, alpha = count), color = "white")+
  geom_point(data = education_const_list, aes(x = internal_point_longitude, y = internal_point_latitude, size = protest_count), color = "white")+
  scale_size_continuous(range = c(.2,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
  xlim(-125,-65)+ylim(25,50) +
  coord_map("albers", lat0=30, lat1=40)+ggtitle("Education, connecting places that coocur most often")


immigration_const_lim_pairs <- const_pairs_limit_pairs(const_place =immigration_const_list, topic = "immigration")
immigration <- tag %>% filter(immigration == 1) %>% select(1:21)

ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_point(data = immigration, aes(x=longitude, y = latitude), size = .1, color = "gray", alpha = .1)+
  geom_segment(data = immigration_const_lim_pairs, aes(x = place_1_long, y = place_1_lat, xend = place_2_long, yend = place_2_lat, alpha = count, size = count), color = "white")+
  geom_point(data = immigration_const_list, aes(x = internal_point_longitude, y = internal_point_latitude, size = protest_count), color = "white")+
  scale_size_continuous(range = c(.2,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
  xlim(-125,-65)+ylim(25,50) +
  coord_map("albers", lat0=30, lat1=40)+ggtitle("Immigration, connecting places that coocur most often")


executive <- tag %>% filter(executive == 1) %>% select(1:21)
executive_const_lim_pairs <- const_pairs_limit_pairs(const_place =executive_const_list, topic = "executive")

ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_point(data = executive, aes(x=longitude, y = latitude), size = .1, color = "gray", alpha = .1)+
  geom_segment(data = executive_const_lim_pairs, aes(x = place_1_long, y = place_1_lat, xend = place_2_long, yend = place_2_lat, alpha = count, size = count), color = "white")+
  geom_point(data = executive_const_list, aes(x = internal_point_longitude, y = internal_point_latitude, size = protest_count), color = "white")+
  scale_size_continuous(range = c(.2,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
  xlim(-125,-65)+ylim(25,50) +
  coord_map("albers", lat0=30, lat1=40)+ggtitle("Executive, connecting places that coocur most often")


guns <- tag %>% filter(guns == 1) %>% select(1:21)
guns_const_lim_pairs <- const_pairs_limit_pairs(const_place =guns_const_list, topic = "guns")

ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_point(data = guns, aes(x=longitude, y = latitude), size = .1, color = "gray", alpha = .1)+
  geom_segment(data = guns_const_lim_pairs, aes(x = place_1_long, y = place_1_lat, xend = place_2_long, yend = place_2_lat, alpha = count, size = count), color = "white")+
  geom_point(data = guns_const_list, aes(x = internal_point_longitude, y = internal_point_latitude, size = protest_count), color = "white")+
  scale_size_continuous(range = c(.2,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
  xlim(-125,-65)+ylim(25,50) +
  coord_map("albers", lat0=30, lat1=40)+ggtitle("Guns, connecting places that coocur most often")





collective_bargaining <- tag %>% filter(collective_bargaining == 1) %>% select(1:21)
collective_bargaining_const_lim_pairs <- const_pairs_limit_pairs(const_place =collective_bargaining_const_list, topic = "collective_bargaining")

ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_point(data = collective_bargaining, aes(x=longitude, y = latitude), size = .1, color = "gray", alpha = .1)+
  geom_segment(data = collective_bargaining_const_lim_pairs, aes(x = place_1_long, y = place_1_lat, xend = place_2_long, yend = place_2_lat, alpha = count, size = count), color = "white")+
  geom_point(data = collective_bargaining_const_list, aes(x = internal_point_longitude, y = internal_point_latitude, size = protest_count), color = "white")+
  scale_size_continuous(range = c(.2,3))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"),
        axis.line = element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks = element_blank())+
  xlim(-125,-65)+ylim(25,50) +
  coord_map("albers", lat0=30, lat1=40)+ggtitle("Collective bargaining, connecting places that coocur most often")


##this version allows for ties in the maximum co-occurrence

const_pairs <- function(events = tag, const_place, topic){
  
  #do NOT talk to me about piping this function!!!!
  
  ####get down to events about the topic that occurred in constellation places:
  
  topic_event <- events %>% filter(get(topic) == 1) %>% select(1:21)
  
  const_event <- filter(topic_event, location_id %in% const_place$location_id)
  
  contingency <- as.data.frame.matrix(table(const_event[,c("week", "location_id")]))
  
  ####set up dataframe for pairs of places
  
  all_pair_summary <- data.frame(place_1 = as.character(), place_2 = as.character(), count = integer(), stringsAsFactors = FALSE)
  
  ####for each location, find out how many times OTHER places protested 
  ####about the topic on the same week
  
  for(i in 1:ncol(contingency)){
    
    this_pair_summary <- data.frame(place_1 = character(), 
                                    place_2 = character(), 
                                    count = integer(), stringsAsFactors = FALSE)
    
    this_place <- colnames(contingency)[i]
    
    this_place_events <- contingency %>% filter(get(this_place) >= 1)
    
    this_place_events <-  select(this_place_events, -this_place)
    
    this_place_events <- t(as.matrix(this_place_events))
    
    this_pair_summary <- data.frame(place_1 = as.character(this_place), 
                                    place_2 = as.character(rownames(this_place_events)), 
                                    stringsAsFactors = FALSE)
    
    this_pair_summary$count <- apply(this_place_events, MARGIN = 1, sum)
    
    all_pair_summary <- rbind(all_pair_summary, this_pair_summary)
  }
  
  
  ####set a threshhold--if any two places protest together more often
  ####than this, they should be connected in the constellation, even
  ####if not the max combination.
  
  top_percentile <- quantile(filter(all_pair_summary, count != 0)$count, probs = .98)
  
  ####set up dataframe for top pairs of places
  
  const_pair_summary <- data.frame(place_1 = character(), place_2 = character(), count = integer())
  
  ####for each place, find the place that protests with it MOST often,
  ####as well as any places that protest together often enough to
  ####pass the threshhold set above.
  
  for(p in 1:length(unique(all_pair_summary$place_1))){
    
    this_place <-unique(all_pair_summary$place_1)[p]
    
    #print(this_place) 
    
    this_place_pairs <- filter(all_pair_summary, place_1 == this_place)
    
    #function to find nearest pair-neighbor if multiple tied for top
    
    find_near <- function(df, const_place, place){
      this_place_loc <- const_place[const_place$location_id == place, 
                                    c("internal_point_longitude", 
                                      "internal_point_latitude")]
      df_sf <- st_as_sf(df, coords = c("internal_point_longitude", 
                                       "internal_point_latitude"),
                        crs = 4326)
      df_sf <- filter(df_sf, location_id != place)
      this_place_loc <- st_as_sf(this_place_loc, 
                                 coords = c("internal_point_longitude", 
                                            "internal_point_latitude"), 
                                 crs = 4326)
      this_place_near <- df_sf[st_nn(this_place_loc[1,], df_sf, k = 1, sparse = FALSE),]
      this_place_near
    }
    
    #look for places with maximum # of shared-week protests:  
    max_pair <- filter(this_place_pairs, 
                       count == max(this_place_pairs$count))
    
    #if multiple top places, get down to physically closest one:
    
    top_pair <- if(nrow(max_pair)>1){
      max_place_details <- filter(const_place, location_id %in% max_pair$place_2)
      pair_id <- find_near(max_place_details, const_place, this_place)$location_id
      
      filter(max_pair, place_2 == pair_id)
      
    } else {
      max_pair
    }
    
    const_pair_summary <- rbind(const_pair_summary, max_pair)
    
    
    add_in <- this_place_pairs %>% filter(count >= top_percentile, ! place_2 %in% max_pair$place_2)
    
    if(nrow(add_in) >0){
      const_pair_summary <-rbind(const_pair_summary, add_in)
    }
    
  }
  
  const_place$location_id <- as.character(const_place$location_id)
  
  lat_long_const_pair_summary <- left_join(const_pair_summary, 
                                           const_place[,c("location_id", "location_name", 
                                                          "internal_point_longitude", 
                                                          "internal_point_latitude")],
                                           by = c("place_1" = "location_id"))
  colnames(lat_long_const_pair_summary) <- c("place_1_id", "place_2_id", "count", "place_1_name", "place_1_long", "place_1_lat")
  
  lat_long_const_pair_summary <- left_join(lat_long_const_pair_summary, 
                                           const_place[,c("location_id", "location_name", 
                                                          "internal_point_longitude", 
                                                          "internal_point_latitude")],
                                           by = c("place_2_id" = "location_id"))
  
  colnames(lat_long_const_pair_summary) <- c("place_1_id", "place_2_id", "count", "place_1_name", "place_1_long", "place_1_lat", 
                                             "place_2_name", "place_2_long", "place_2_lat")
  
  
  lat_long_const_pair_summary
}


