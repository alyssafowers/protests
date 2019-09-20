####Creating working data file of protests in 50 states plus DC, with state and short location

library(tidyverse)
library(janitor)
library(stringr)
library(tigris)
library(sf)
library(data.table)

####################################
########### Load in data ###########
####################################

loc <- "data/original"

protest <- read_csv(file.path(loc, "protest_lat_long.csv"))
protest <- protest %>% clean_names()

####################################
## Narrowing to 50 states plus DC ##
####################################

##getting down to 50 states plus DC:

state <- "(?-i:A[LKSZRAEP]|C[AOT]|D[EC]|F[LM]|G[AU]|HI|I[ADLN]|K[SY]|LA|M[ADEHINOPST]|N[CDEHJMVY]|O[HKR]|P[AW]|RI|S[CD]|T[NX]|UT|V[AIT]|W[AIVY])$"
state_true <- str_detect(protest$location,state)

#which values do not have a 2-letter state abbreviation at the end:
unique(protest$location[!str_detect(protest$location,state)])

protest[protest$location == "Honolulu, Hi","location"] <- "Honolulu, HI"
which(protest$location == "Honolulu, Hi")

protest[protest$location == "Panama City, Fl","location"] <- "Panama City, FL"
which(protest$location == "Panama City, Fl")

protest[protest$location == "Wyoming Godfrey-Lee High School, Wyoming, Mi","location"] <- "Wyoming Godfrey-Lee High School, Wyoming, MI"
which(protest$location == "Wyoming Godfrey-Lee High School, Wyoming, Mi")

#looked up lat and long for Fredon Township to figure out where it is--turns out it's in New Jersey
as.numeric(protest[protest$location == "Fredon Township",c("latitude", "longitude")])
protest[protest$location == "Fredon Township","location"] <- "Fredon Township, NJ"
which(protest$location == "Fredon Township")

#checking again to make sure I reformatted all the ones in 50 states + DC:

unique(protest$location[!str_detect(protest$location,state)])

#getting down to protests in 50+DC:
protest <- protest[state_true,]

#####################################
## Adding state and short location ##
#####################################

##Adding state:

#code from here: https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

protest$state <- substrRight(protest$location, 2)

#sanity checking state values/frequency. looks good:
length(unique(protest$state))
View(protest %>% group_by(state) %>% summarise(count = n()))

##Adding short location:

shortLoc <- function(x){
  substrRight <- function(y, n){
    substr(as.character(y), nchar(y)-n+1, nchar(y))
  }
  
  comma_count <- str_count(x$location, ",")
  
  short_loc <- character()
  
  for(i in 1:nrow(x)){
    if(comma_count[i] > 1){
      loc <- as.character(x[i,"location"])
      cc <- as.integer(comma_count[i])
      #find location of second-to-last comma (got idea from here: https://stackoverflow.com/questions/14249562/find-the-location-of-a-character-in-string)
      two_c_loc <- which(strsplit(loc,"")[[1]] == ",")[cc-1]
      #extract part of string after second comma
      short_loc[i] <- substrRight(loc, nchar(loc)-two_c_loc - 1)
      #short_loc[i] <- two_c_loc
    } else{
      short_loc[i] <- x[i,"location"]
    }
  }
  unlist(short_loc)
}

protest$short_loc <- shortLoc(protest)

########################################
## Determining counties from lat/long ##
########################################

#getting county shapefile from Tiger/LINE:
county <- tigris::counties()
str(county)
county <- st_as_sf(county)

st_crs(county)
#I want this in WGS 84 to match the protest data, so:
county_wgs <- st_transform(county, crs = 4326)

#turning protests into sf with WGS 84 coordinate system
protest_sf <- st_as_sf(protest, coords = c("longitude", "latitude"))
st_crs(protest_sf) <- 4326

ggplot() + geom_sf(data = filter(county_wgs, STATEFP == 12)) + geom_sf(data = filter(protest_sf, state == "FL"))

protest_county <- st_join(protest_sf, county_wgs, join = st_within)

##get down to only the columns that I want:
protest_final <- select(protest_county, "date", "location", "attendees", 
                        "event_legacy_see_tags", "tags", "curated", "source",
                        "total_articles", "state", "short_loc", "geometry", "STATEFP",
                        "COUNTYFP", "GEOID", "NAME", "NAMELSAD", "CSAFP", "CBSAFP", "ALAND", "AWATER", 
                        "INTPTLAT", INTPTLON)
colnames(protest_final) <- c(colnames(protest_final)[1:11], "state_fips", "county_fips",
                             "state_and_county_fips", "county_name_short",
                             "county_name_long", "combined_statistical_area", "metro_micro_stat_area", "land_area",
                             "water_area", "internal_point_latitude", "internal_point_longitude")

#testing out a few of these designations:
View(protest_final[sample(1:nrow(protest_final), 10),])
#these all looked correct! however, should note that at least one city (Chapel Hill) was
#spread across multiple counties. will have to keep this in mind later (maybe look
#at combined statistical areas or metropolitan areas, which are composed of counties?)

########################################
##### Adding population information ####
########################################

pop <- read_csv(file.path(loc, "county_population_data.csv"))

#population data is by town, not by county (???) so have to combine all together:
pop_2017 <- select(pop, "STATE", "COUNTY", "POPESTIMATE2017")

colnames(pop_2017) <- c("state_fips", "county_fips", "population_estimate_2017")

protest_final <- left_join(protest_final, pop_2017)

########################################
###### Adding voting information ######
########################################

pres <- read_csv(file.path(loc, "countypres_2000-2016.csv"))

pres_2016 <- filter(pres, year == 2016)

###calculating percent votes for republican, democrat, and other

vote_perc <- dcast(pres_2016, state_po+county+FIPS+totalvotes~party, value.var = "candidatevotes")
vote_perc$perc_democrat <- vote_perc$democrat/vote_perc$totalvotes
vote_perc$perc_republican <- vote_perc$republican/vote_perc$totalvotes

#adding that into protest:
protest_final$fips_int <- as.integer(protest_final$state_and_county_fips)
protest_final <- left_join(protest_final, vote_perc[,c("FIPS", "totalvotes", 
                "perc_democrat", "perc_republican")], by = c("fips_int" = "FIPS"))
#sanity checking:
sum(is.na(protest_final$perc_democrat))
sum(is.na(protest_final$perc_republican))
sum(is.na(protest_final$population_estimate_2017))

###might not be able to use Alaska for voting data--something real weird going on with the
#county/FIPS. In the voting data, Alaska has numbered districts with FIPS codes that don't
#line up to anything in the rest of the data. (apparently because Alaska doesn't actually
#have counties, and so returns voting information by state legislative voting
#district instead of by boroughs, which is what the population data is by/what the TIGER/line
#shapefile gave me in place of counties)

#I think what I need to do is, find a shapefile for Alaskan legislative districts,
#pull out the Alaskan protests, match them to their corresponding legislative districts,
#and then match THAT to population data, if I can find population-by-district. It looks
#like the districts are at least supposed to be equal in population? The problem is,
#I'm not too confident in the precision of the lat/longs I have, and the districts cut
#through cities--so I might put protests in the wrong district. Maybe merge the 
#districts that cut through the same statistical areas or boroughs....?
