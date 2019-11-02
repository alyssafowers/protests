####Creating working data file of protests in lower 48 states plus DC, with CBSA where applicable, county where no CBSA,
####population information, and voting information

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
#which(protest$location == "Honolulu, Hi")

protest[protest$location == "Panama City, Fl","location"] <- "Panama City, FL"
#which(protest$location == "Panama City, Fl")

protest[protest$location == "Wyoming Godfrey-Lee High School, Wyoming, Mi","location"] <- "Wyoming Godfrey-Lee High School, Wyoming, MI"
#which(protest$location == "Wyoming Godfrey-Lee High School, Wyoming, Mi")

#looked up lat and long for Fredon Township to figure out where it is--turns out it's in New Jersey
#as.numeric(protest[protest$location == "Fredon Township",c("latitude", "longitude")])
protest[protest$location == "Fredon Township","location"] <- "Fredon Township, NJ"
#which(protest$location == "Fredon Township")

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

####################################
##### CBSA to County crosswalk #####
####################################

county_ref <- counties()
county_ref <- st_as_sf(county_ref)

#county to cbsa crosswalk:
cbsa_county_ref <- as.data.frame(county_ref) %>% filter(is.na(CBSAFP) == FALSE) %>% select(STATEFP, COUNTYFP, GEOID, NAME, NAMELSAD, CBSAFP)

#####################################
## Counties and CBSAs for protests ##
#####################################

county_ref <- st_transform(county_ref, crs = 4326)
protest_sf <- st_as_sf(protest, coords = c("longitude", "latitude"))
st_crs(protest_sf) <- 4326

protest_county <- st_join(protest_sf, county_ref, join = st_within)

####################################
###### Population information ######
####################################

loc <- "data/original"

pop <- read_csv(file.path(loc, "county_population_data.csv"))

pop <- left_join(pop, cbsa_county_ref[,c("STATEFP", "COUNTYFP", "CBSAFP")], by = c("STATE" = "STATEFP", "COUNTY" = "COUNTYFP"))

cbsa_pop <- pop %>% filter(is.na(CBSAFP) == FALSE) %>% group_by(CBSAFP) %>% summarise(pop = sum(POPESTIMATE2017))
no_cbsa_pop <- pop %>% filter(is.na(CBSAFP) == TRUE) %>% group_by(STATE, COUNTY, STNAME, CTYNAME) %>% 
  summarise(pop = sum(POPESTIMATE2017)) %>% mutate(GEOID = paste(STATE, COUNTY, sep = ""))

####################################
######## Voting information ########
####################################

pres <- read_csv(file.path(loc, "countypres_2000-2016.csv"))
pres_2016 <- filter(pres, year == 2016)

vote_perc_county <- dcast(pres_2016, state_po+county+FIPS+totalvotes~party, value.var = "candidatevotes")

vote_perc_county$char_fips <- as.character(vote_perc_county$FIPS)

#four of the characters are NA, but they don't have FIPS codes anyways, removing them.
unique(nchar(vote_perc_county$char_fips))
vote_perc_county[is.na(vote_perc_county$char_fips),]
vote_perc_county <- vote_perc_county[!is.na(vote_perc_county$char_fips),]

#formatting a county FIPS id that can be used to match to the county/cbsa crosswalk:

for(i in 1:nrow(vote_perc_county)){
  if(nchar(vote_perc_county[i,"char_fips"])==4){
    vote_perc_county[i, "char_fips"] <- paste("0", vote_perc_county[i, "char_fips"], sep = "")
  }
}

#adding CBSA ID to vote file:

vote_perc_county <- left_join(vote_perc_county, as.data.frame(cbsa_county_ref)[,c("GEOID", "CBSAFP")], by = c("char_fips" = "GEOID"))

#votes per CBSA:

cbsa_vote <- vote_perc_county %>% filter(is.na(CBSAFP) == FALSE) %>% group_by(CBSAFP) %>% summarise(democrat = sum(democrat), 
             republican = sum(republican), totalvotes = sum(totalvotes)) %>% mutate(perc_democrat = democrat/totalvotes)
county_vote <- vote_perc_county %>% filter(is.na(CBSAFP) == TRUE) %>% mutate(perc_democrat = democrat/totalvotes)


####################################
### Add vote and pop information ###
######## to located protests #######
####################################

#split protests that are in CBSAs versus those that are not in CBSAs

protest_cbsa_only <- as.data.frame(protest_county) %>% filter(is.na(CBSAFP) == FALSE) %>% group_by(CBSAFP) %>% summarise(protest_count = n()) %>%
  left_join(cbsa_vote) %>% left_join(cbsa_pop) %>% mutate(location_type = "CBSA", location_id = CBSAFP)
protest_no_cbsa <- as.data.frame(protest_county) %>% filter(is.na(CBSAFP) == TRUE) %>% group_by(GEOID, NAMELSAD, state) %>% summarise(protest_count = n()) %>%
  left_join(county_vote, by = c("GEOID" = "char_fips")) %>% left_join(no_cbsa_pop) %>% mutate(location_type = "county", location_id = GEOID)

#add names of CBSAs, whoops

cbsa <- as.data.frame(st_as_sf(core_based_statistical_areas(cb = TRUE)))
protest_cbsa_only <- left_join(protest_cbsa_only, as.data.frame(cbsa)[,c("CBSAFP", "NAME")])

####################################
### Recombining CBSA and county ####
#############  protests ############
####################################

colnames(protest_cbsa_only)[! colnames(protest_cbsa_only) %in% colnames(protest_no_cbsa)]
colnames(protest_no_cbsa)[which(colnames(protest_no_cbsa) == "NAMELSAD")] <- "NAME"
colnames(protest_no_cbsa)[which(colnames(protest_no_cbsa) == "NA")] <- "no_party"
colnames(protest_no_cbsa)[which(colnames(protest_no_cbsa) == "STATE")] <- "state_fips"
colnames(protest_no_cbsa)[which(colnames(protest_no_cbsa) == "COUNTY")] <- "county_fips"

protest_no_cbsa <- protest_no_cbsa[, c("GEOID", "NAME", "state", "protest_count", "totalvotes", "democrat", "republican", "no_party", "CBSAFP",
                                       "perc_democrat", "state_fips", "county_fips", "CTYNAME", "pop", "location_type", "location_id")]

colnames(protest_no_cbsa)[! colnames(protest_no_cbsa) %in% colnames(protest_cbsa_only)]

protest_cbsa_only$GEOID <- NA
protest_cbsa_only$state <- NA
protest_cbsa_only$no_party <- NA
protest_cbsa_only$state_fips <- NA
protest_cbsa_only$county_fips <- NA
protest_cbsa_only$CTYNAME <- NA

#double-checking to make sure the columns match
colnames(protest_no_cbsa)[! colnames(protest_no_cbsa) %in% colnames(protest_cbsa_only)]
colnames(protest_cbsa_only)[! colnames(protest_cbsa_only) %in% colnames(protest_no_cbsa)]

protest_cbsa_only <- protest_cbsa_only[,colnames(protest_no_cbsa)]

protest_summary <- rbind(protest_cbsa_only, protest_no_cbsa)

#######################################
## Adding in places with no protests ##
#######################################

####counties with no CBSAs:

county_no_cbsa <- as.data.frame(county_ref) %>% filter(is.na(CBSAFP) == TRUE) %>% select(STATEFP, COUNTYFP, GEOID, NAME, NAMELSAD, CBSAFP)
county_no_cbsa <- left_join(county_no_cbsa, as.data.frame(protest_summary)[,c("GEOID", "protest_count")])

county_no_cbsa_no_protest <- county_no_cbsa %>% filter(is.na(protest_count)) %>% 
  left_join(no_cbsa_pop[,c("GEOID", "pop")]) %>% left_join(county_vote, by = c("GEOID" = "char_fips"))

#counties without CBSAs, protests, and population counts aren't in the lower 48+DC, clearing them out
sum(is.na(county_no_cbsa_no_protest$pop))
View(county_no_cbsa_no_protest[is.na(county_no_cbsa_no_protest$pop),])
county_no_cbsa_no_protest <- county_no_cbsa_no_protest[! is.na(county_no_cbsa_no_protest$pop),]

#counties without CBSAs, protests, and votes are all in Alaska (as expected), except for one in 
#South Dakota (Oglala Lakota County). leaving them in.
sum(is.na(county_no_cbsa_no_protest$totalvotes))
View(county_no_cbsa_no_protest[is.na(county_no_cbsa_no_protest$totalvotes),])


#cbsas:
cbsa_no_protest <- left_join(cbsa[,c("CBSAFP", "NAME")], as.data.frame(protest_cbsa_only)[,c("CBSAFP", "protest_count")]) %>% 
  filter(is.na(protest_count) == TRUE) %>% left_join(cbsa_pop) %>% left_join(cbsa_vote)

#CBSAs without voting information are all in Puerto Rico, taking them out:
sum(is.na(cbsa_no_protest$totalvotes))
View(cbsa_no_protest[is.na(cbsa_no_protest$totalvotes),])
cbsa_no_protest <- cbsa_no_protest[! is.na(cbsa_no_protest$totalvotes),]
#all CBSAs have pop information:
sum(is.na(cbsa_no_protest$pop))

###Making sure column names line up:

colnames(protest_summary)[! colnames(protest_summary) %in% colnames(cbsa_no_protest)]
colnames(cbsa_no_protest)[! colnames(cbsa_no_protest) %in% colnames(protest_summary)]

cbsa_no_protest$location_type <- "CBSA"
cbsa_no_protest$location_id <- cbsa_no_protest$CBSAFP
cbsa_no_protest$GEOID <- NA
cbsa_no_protest$state <- NA
cbsa_no_protest$no_party <- NA
cbsa_no_protest$state_fips <- NA
cbsa_no_protest$county_fips <- NA
cbsa_no_protest$CTYNAME <- NA

cbsa_no_protest <- cbsa_no_protest[, colnames(protest_summary)]
cbsa_no_protest$protest_count <- 0

colnames(protest_summary)[! colnames(protest_summary) %in% colnames(county_no_cbsa_no_protest)]
colnames(cbsa_no_protest)[! colnames(cbsa_no_protest) %in% colnames(protest_summary)]

county_no_cbsa_no_protest <- mutate(county_no_cbsa_no_protest, CBSAFP = NA, location_type = "county", 
                                    location_id = GEOID, state = state_po, state_fips = STATEFP, 
                                    county_fips = COUNTYFP, CTYNAME = NAMELSAD, no_party = NA)

county_no_cbsa_no_protest <- county_no_cbsa_no_protest[,colnames(protest_summary)]
county_no_cbsa_no_protest$protest_count <- 0

###Grouping together places with no protests:

no_protest_summary <- rbind(county_no_cbsa_no_protest, cbsa_no_protest)


###creating full summary with all places:

all_place_summary <- rbind(protest_summary, no_protest_summary)

#############################################
#### Adding location ID to protest file #####
#############################################

#add location_type, location_summary

protest$location_type <- ""
protest$location_id <- ""

for(i in 1:nrow(protest)){
  if(is.na(protest[i,"metro_micro_stat_area"]) == FALSE){
    protest[i, "location_id"] <- protest[i, "metro_micro_stat_area"]
    protest[i, "location_type"] <- "CBSA"
  } else {
    protest[i, "location_id"] <- protest[i, "state_and_county_fips"]
    protest[i, "location_type"] <- "county"
  }
}

#Adding in names for CBSAs and counties

protest <- left_join(protest, cbsa[,c("GEOID", "NAME")], by = c("location_id" = "GEOID"))
colnames(protest)[which(colnames(protest) == "NAME")] <- "cbsa_name"

protest$location_name <- ""

for(i in 1:nrow(protest)){
  if(protest[i, "location_type"] == "county"){
    protest[i, "location_name"] <- protest[i, "county_name_long"]
  } else {
    protest[i, "location_name"] <- protest[i, "cbsa_name"]
  }
}

#################################################
## Identifying which places are state capitals ##
#################################################

capital <- read_csv(file.path(loc, "state_capitals.csv"))

capital_sf <- st_as_sf(capital, coords = c("lon", "lat"))
st_crs(capital_sf) <- 4326

capital_county <- st_join(capital_sf, county_ref, join = st_within)

sum(is.na(capital_county$CBSAFP))
capital_county[is.na(capital_county$CBSAFP),]

county_check <- left_join(capital_county[,c("GEOID", "state", "NAME")], cbsa_county_ref[,c("GEOID", "CBSAFP", "NAMELSAD")])

capital_county$include_capital <- TRUE

all_place_summary$CBSAFP <- as.character(all_place_summary$CBSAFP)

all_place_summary <- left_join(all_place_summary, capital_county[is.na(capital_county$CBSAFP) == FALSE,c("CBSAFP", "include_capital")])

sum(all_place_summary$include_capital, na.rm = TRUE)

all_place_summary[all_place_summary$location_id == "38009", "include_capital"] <- TRUE

View(filter(all_place_summary, include_capital == TRUE))
nrow(filter(all_place_summary, include_capital == TRUE))

all_place_summary[is.na(all_place_summary$include_capital) == TRUE, "include_capital"] <- FALSE

####################################
############ Writing out ###########
####################################

write_loc <- "data/working"
write_csv(protest_summary, file.path(write_loc, "county_cbsa_protest_summary.csv"))
write_csv(all_place_summary, file.path(write_loc, "all_county_cbsa_summary.csv"))
write_csv(no_protest_summary, file.path(write_loc, "county_cbsa_no_protest_summary.csv"))
write_csv(protest, file.path(write_loc, "protest_all_states.csv"))


