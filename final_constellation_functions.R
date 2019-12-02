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
tag <- untangle(protest, "tags", pattern = ";")
usa <- map_data("usa")

#########################################
### Function to determine what places ###
####### should be in constellation ######
#########################################

constellation_places <- function(list = protest, place_summary = place, topic, write_loc = "constellations"){
  tag <- untangle(protest, "tags", pattern = ";")
  summary <- tag %>% group_by(location_id, location_name) %>% 
    summarise(protest_count = n(), topic_protest = sum(get(topic)), internal_point_latitude = mean(latitude), internal_point_longitude = mean(longitude)) %>%
    mutate(perc_topic = topic_protest/protest_count)
  
  ##adding in population
  summary <- left_join(summary, place_summary[,c("location_id", "pop")])
  summary <- mutate(summary, topic_per_10k = (topic_protest/pop)*10000)
  
  #getting criteria for inclusion in constellation
  t_sum <- filter(summary, topic_protest > 0)
  
  crit_10k <- unname(quantile(t_sum$topic_per_10k, .9))
  crit_perctopic <- unname(quantile(t_sum$perc_topic, .9))
  crit_count <- unname(quantile(t_sum$topic_protest, .9))
  
  min_10k <- unname(quantile(t_sum$topic_per_10k, .4))
  min_perctopic <- unname(quantile(t_sum$perc_topic, .5))
  min_count <- unname(quantile(t_sum$topic_protest, .8))
  
  
  #getting constellation:
  
  const <- filter(summary, topic_per_10k >= crit_10k & 
                    perc_topic >= crit_perctopic &
                    topic_protest >= min_count |
                    topic_per_10k >= min_10k & 
                    perc_topic >= min_perctopic &
                    topic_protest >= crit_count)
  
  wd <- getwd() 
  dir.create(file.path(write_loc, topic))
  setwd(paste(write_loc, topic, sep = "/"))
  
  write_csv(const, paste(topic, "_constellation_places.csv", sep = ""))
  write_csv(t_sum, paste(topic, "_all_places.csv", sep = ""))
  
  setwd(wd)
  
  const
  
}


#########################################
### Function to determine how places ####
######### should be connected in ########
############## constellation ############
#########################################

const_map <- function(events = tag, topic){
  
  #do NOT talk to me about piping this function!!!!
  
  const_place <- constellation_places(topic = topic)
  
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
                                       y = internal_point_latitude, size = topic_protest), color = "white") +
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
  
  ggsave(file.path("constellations",topic,paste(topic,"_constellation_plot.pdf", sep = "")))
  
  wd <- getwd() 
  setwd(paste(write_loc, topic, sep = "/"))
  
  write_csv(lat_long_const_pair_summary, paste(topic, "_pair_summary.csv", sep = ""))
  
  setwd(wd)
  
  lat_long_const_pair_summary
}

#########################################
### Getting maps and saving out files ###
#########################################

const_map(topic = "guns")
const_map(topic = "women")
const_map(topic = "supreme_court")
const_map(topic = "immigration")
const_map(topic = "education")
const_map(topic = "environment")
const_map(topic = "executive")
const_map(topic = "healthcare")
const_map(topic = "police")
const_map(topic = "race_confed")
const_map(topic = "collective_bargaining")


#########################################
### Making big combined files for D3 ####
#########################################

setwd("constellations")

###points file

dir_list <- list.dirs(recursive = FALSE)

dir_list <- dir_list[! dir_list %in% c("./second_round", "./first_round")]

all_const_places <- data.frame(location_id = integer(), 
                               location_name = character(), 
                               protest_count = integer(), 
                               topic_protest = integer(), 
                               internal_point_latitude = numeric(), 
                               internal_point_longitude = numeric(), 
                               perc_topic = numeric(), 
                               pop = integer(), 
                               topic_per_10k = numeric(),
                               topic = character())

for(i in 1:length(dir_list)){
  
  this_topic <- substr(dir_list[i], start = 3, stop = nchar(dir_list[i]))
  
  wd <- getwd()
  new_wd <- dir_list[i]
  setwd(new_wd)
  
  this_const_place <- read_csv(paste(this_topic, "_constellation_places.csv", sep = ""))
  
  this_const_place$topic <- this_topic
  
  print(paste(this_topic, "read okay"))
  
  all_const_places <- rbind(all_const_places, this_const_place)
  
  print(paste(this_topic, "bound ok"))
  
  setwd("..")
  
  print(paste(this_topic, "reset ok"))
}

all_const_places$internal_point_latitude <- round(all_const_places$internal_point_latitude, digits = 3)
all_const_places$internal_point_longitude <- round(all_const_places$internal_point_longitude, digits = 3)

write_csv(all_const_places, "all_constellation_points.csv")

###segments file

all_const_lines <- data.frame(place_1_id = integer(),
                              place_2_id = integer(),
                              count = integer(),
                              place_1_name= character(),
                              place_1_long = numeric(),
                              place_1_lat = numeric(),
                              place_2_name = character(),
                              place_2_long = numeric(),
                              place_2_lat = numeric())

for(i in 1:length(dir_list)){
  
  this_topic <- substr(dir_list[i], start = 3, stop = nchar(dir_list[i]))
  
  wd <- getwd()
  new_wd <- dir_list[i]
  setwd(new_wd)
  
  this_const_line <- read_csv(paste(this_topic, "_pair_summary.csv", sep = ""))
  
  this_const_line$topic <- this_topic
  
  print(paste(this_topic, "read okay"))
  
  all_const_lines <- rbind(all_const_lines, this_const_line)
  
  print(paste(this_topic, "bound ok"))
  
  setwd("..")
  
  print(paste(this_topic, "reset ok"))
}

write_csv(all_const_lines, "all_constellation_lines.csv")

###tagged places

setwd("..")

protest_w_tags <- select(tag, date, week, location, attendees, 
                         latitude, longitude, event, tags, curated, 
                         source, total_articles, state, short_loc, 
                         location_type, location_id, location_name,
                         collective_bargaining, education, environment,
                         executive, guns, healthcare, immigration,
                         police, race_confed, supreme_court, women)

protest_w_tags$topic_sum <- apply(protest_w_tags[,c("collective_bargaining", "education", "environment", "executive", 
                                "guns", "healthcare", "immigration", "police", "race_confed", "supreme_court", 
                                "women")], MARGIN = 1, sum)

protest_w_tags <- protest_w_tags %>% select(-topic_sum) %>% filter(date >= as.Date("2017-01-16"))

protest_w_tags[protest_w_tags$topic_sum < 1, "other"] <- 1
protest_w_tags[protest_w_tags$topic_sum >= 1, "other"] <- 0

write_csv(protest_w_tags, file.path(loc, "all_protests_major_tags.csv"))
