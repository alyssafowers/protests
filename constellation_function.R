####Making constellation maps####

library(tidyverse)
library(janitor)
library(stringr)
library(muckrakr)
library(sf)
library(nngeo)

loc <- "data/final"
protest <- clean_names(read_csv(file.path(loc, "protest_nov1_addtl_tags.csv")))
#protest <- clean_names(read_csv(file.path(loc, "protest_all_states.csv")))

place <- clean_names(read_csv(file.path(loc, "nov1_all_county_cbsa_summary.csv")))

constellation <- function(list = protest, place_summary = place, topic, write_loc = "constellations"){
  tag <- untangle(protest, "tags", pattern = ";")
  summary <- tag %>% group_by(location_id, location_name) %>% 
      summarise(protest_count = n(), topic_protest = sum(get(topic)), internal_point_latitude = mean(latitude), internal_point_longitude = mean(longitude)) %>%
      mutate(perc_topic = topic_protest/protest_count)

  ##adding in population
  summary <- left_join(summary, place_summary[,c("location_id", "pop")])
  summary <- mutate(summary, topic_per_10k = (topic_protest/pop)*10000)

  #getting criteria for inclusion in constellation
  t_sum <- filter(summary, topic_protest > 0)
  
  crit_10k <- unname(quantile(t_sum$topic_per_10k, .4))
  crit_perctopic <- unname(quantile(t_sum$perc_topic, .5))
  crit_count <- unname(quantile(t_sum$topic_protest, .8))
  
  #getting constellation:
  
  const <- filter(summary, topic_per_10k >= crit_10k, 
                  perc_topic >= crit_perctopic,
                  topic_protest >= crit_count)


  #getting ready to make a map
  
  
  ####commented out find_near function had a more random start point; I got better
  ####results for the line-drawing when I started in the southeast corner of the US
  
 # find_near_c <- function(df){
#    df$id <- 1:nrow(df)
 #   df_sf <- st_as_sf(df, coords = c("internal_point_longitude", "internal_point_latitude"),
  #                    crs = 4326)
   # order <- df_sf[1,]
    #df_sf <- df_sf[2:nrow(df),]
#    pos <- integer()
#    for(i in 2:nrow(df_sf)){
#      r <- order[i-1,]
#      near <- which(st_nn(r, df_sf, k = 1, sparse = FALSE))
#      order[i,] <- df_sf[near,]
#      df_sf <- df_sf[-near,]
#    }
#    order <- rbind(order, df_sf)
#    df <- df[order$id,]
#    df
#  }
 
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
  
  const <- find_near_c(const)
  usa <- map_data("usa")
  
 const_plot<- ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
   geom_point(data = t_sum, aes(x = internal_point_longitude, y = internal_point_latitude), color = "white", size = .2, alpha = .2)+
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
 wd <- getwd() 
 
  dir.create(file.path(write_loc, topic))
  setwd(file.path(write_loc, topic))
  write_csv(const, paste(topic, "_constellation_places.csv", sep = ""))
  write_csv(summary, paste(topic, "_all_place_summary.csv", sep = ""))
  ggsave(paste(topic,"_constellation_plot.pdf", sep = ""))
  
  setwd(wd)
  
  const_plot
  }

constellation(topic = "collective_bargaining")
constellation(topic = "police")
constellation(topic = "race_confed")
constellation(topic = "immigration")
constellation(topic = "executive")
constellation(topic = "education")
constellation(topic = "environment")
constellation(topic = "healthcare")
constellation(topic = "supreme_court")
constellation(topic = "guns")
constellation(topic = "women")
constellation(topic = "police")




tag <- mutate(tag, women_bucket = sum(for_womens_rights, womens_march, 
             for_abortion_rights, against_abortion_rights, 
             for_planned_parenthood, day_without_a_woman, 
             against_planned_parenthood, international_womens_day, 
             against_sexual_domestic_violence, black_womens_march)>0)
