






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

#sanity checking one state to make sure protests land in it:
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

#or, maybe I treat Alaska as one REALLY LARGE COUNTY?

summary(pop_2017$population_estimate_2017)

sum(pop_2017$population_estimate_2017 >= 739786)

#there are 136 counties in the US with more people in them than Alaska... 
#that's maybe not a terrible idea? especially if the alternative is to leave it
#out entirely? there were 188 protests in Alaska-the-state, and 10 counties that
#had more protests than that... don't love it, though.

sum(protest_final$state == "AK")
nrow(protest_final %>% group_by(state_and_county_fips) %>% summarise(count = n()) %>% filter(count >= 188))

##FOR NOW, going to focus on lower 48+DC. do a special section on protest in
#hawaii and alaska? or include hawaii and just exclude alaska?

protest_49 <- filter(protest_final, state != "AK" & state != "HI")
protest_not_alaska <- filter(protest_final, state != "AK")

########################################
####### Split data into weeks ##########
########################################

protest_final <- mutate(protest_final, week=as.Date(cut(date, breaks="week", starts.on.monday=TRUE)))
protest_49 <- mutate(protest_49, week=as.Date(cut(date, breaks="week", starts.on.monday=TRUE)))
protest_not_alaska <- mutate(protest_not_alaska, week=as.Date(cut(date, breaks="week", starts.on.monday=TRUE)))

########################################
############# Writing out ##############
########################################

write_csv(protest_49, "data/working/protest_lower_49.csv")
write_csv(protest_final, "data/working/protest_all_states.csv")
write_csv(protest_not_alaska, "data/working/protest_not_alaska.csv")


########################################
########### Fixing errors ##############
########################################

#Discovered elsewhere that there are two protests labelled as "Los Angeles County, Oregon"
#that are actually in Albany, Oregon (Linn County), so fixing that

all_states <- read_csv("data/working/protest_all_states.csv")
View(all_states[all_states$state == "OR" & all_states$county_fips == "037",])

all_states[all_states$short_loc == "Albany, OR","state_fips"] <- "41"
all_states[all_states$short_loc == "Albany, OR","county_fips"] <- "043"
all_states[all_states$short_loc == "Albany, OR","state_and_county_fips"] <- 41043
all_states[all_states$short_loc == "Albany, OR","county_name_short"] <- "Linn"
all_states[all_states$short_loc == "Albany, OR","county_name_long"] <- "Linn County"
all_states[all_states$short_loc == "Albany, OR","combined_statistical_area"] <- "440"
all_states[all_states$short_loc == "Albany, OR","metro_micro_stat_area"] <- "10540"
all_states[all_states$short_loc == "Albany, OR","land_area"] <- 5923493230
all_states[all_states$short_loc == "Albany, OR","water_area"] <- 50497756
all_states[all_states$short_loc == "Albany, OR","internal_point_latitude"] <- 44.4889
all_states[all_states$short_loc == "Albany, OR","internal_point_longitude"] <- -122.5372
all_states[all_states$short_loc == "Albany, OR","population_estimate_2017"] <- 124977
all_states[all_states$short_loc == "Albany, OR","fips_int"] <- 41043
all_states[all_states$short_loc == "Albany, OR","perc_democrat"] <- 0.3064596
all_states[all_states$short_loc == "Albany, OR","perc_republican"] <- 0.5703094
all_states[all_states$short_loc == "Albany, OR","totalvotes"] <- 58719

protest_49 <- filter(all_states, state != "AK" & state != "HI")
protest_not_alaska <- filter(all_states, state != "AK")

write_csv(all_states, "data/working/protest_all_states.csv")
write_csv(protest_not_alaska, "data/working/protest_not_alaska.csv")
write_csv(protest_49, "data/working/protest_lower_49.csv")



####reconsidered: what if I did this with metro/micropolitan areas instead (where
####applicable) and then counties where not applicable?

mma <- core_based_statistical_areas(cb = TRUE)
mma <- st_as_sf(mma)

###DIDN'T WIND UP USING THIS, BUT SAVING THE CODE IN CASE I WANT IT LATER:

########################################
## Determining core-based statistical ##
######### areas from lat/long ##########
########################################

#getting CBSA shapefile from Tiger/LINE:
cbsa_geom <- tigris::core_based_statistical_areas()
cbsa_geom <- st_as_sf(cbsa_geom)

st_crs(cbsa_geom)
#I want this in WGS 84 to match the protest data, so:
cbsa_geom <- st_transform(cbsa_geom, crs = 4326)

#turning protests into sf with WGS 84 coordinate system
protest_sf <- st_as_sf(protest, coords = c("longitude", "latitude"))
st_crs(protest_sf) <- 4326

protest_cbsa <- st_join(protest_sf, cbsa_geom[,c("CBSAFP", "NAME")], join = st_within)

sum(is.na(protest_cbsa$CBSAFP))

library(tigris)
library(sf)

####CBSA reference table####

county_ref <- counties()
county_ref <- st_as_sf(county_ref)

#county to cbsa crosswalk:
cbsa <- county_ref %>% filter(is.na(CBSAFP) == FALSE) %>% select(STATEFP, COUNTYFP, GEOID, NAME, NAMELSAD, CBSAFP)
#cbsa <- mutate(cbsa, int_county_fips = as.integer(COUNTYFP), int_state_fips = as.integer(STATEFP))


#adding in population data:
loc <- "data/original"
pop <- read_csv(file.path(loc, "county_population_data.csv"))
pop <- left_join(pop, cbsa[,c("STATEFP", "COUNTYFP", "CBSAFP")], by = c("STATE" = "STATEFP", "COUNTY" = "COUNTYFP"))

cbsa_pop <- pop %>% filter(is.na(CBSAFP) == FALSE) %>% group_by(CBSAFP) %>% summarise(pop = sum(POPESTIMATE2017))


#####voting data######

pres <- read_csv(file.path(loc, "countypres_2000-2016.csv"))

pres_2016 <- filter(pres, year == 2016)

###calculating percent votes for republican, democrat, and other

vote_perc <- dcast(pres_2016, state_po+county+FIPS+totalvotes~party, value.var = "candidatevotes")
vote_perc$perc_democrat <- vote_perc$democrat/vote_perc$totalvotes
vote_perc$perc_republican <- vote_perc$republican/vote_perc$totalvotes

vote_perc$char_fips <- as.character(vote_perc$FIPS)
#four of the characters are NA, but they don't have FIPS codes anyways, removing them.
unique(nchar(vote_perc$char_fips))
vote_perc[is.na(vote_perc$char_fips),]
vote_perc <- vote_perc[!is.na(vote_perc$char_fips),]

#have to add a zero on to the character fips so it can be matched to the CBSA reference list

for(i in 1:nrow(vote_perc)){
  if(nchar(vote_perc[i,"char_fips"])==4){
    vote_perc[i, "char_fips"] <- paste("0", vote_perc[i, "char_fips"], sep = "")
  }
}

vote_perc <- left_join(vote_perc, as.data.frame(cbsa)[,c("GEOID", "CBSAFP")], by = c("char_fips" = "GEOID"))
cbsa_vote <- vote_perc %>% filter(is.na(CBSAFP) == FALSE) %>% group_by(CBSAFP) %>% 
  summarise(totalvotes = sum(totalvotes), democrat = sum(democrat), republican = sum(republican)) %>%
  mutate(perc_republican = republican/totalvotes)

#alaska CBSAs don't have voting information in here, as expected

cbsa_protest_count <- as.data.frame(protest_cbsa) %>% filter(is.na(CBSAFP) == FALSE) %>% group_by(CBSAFP,NAME) %>%
  summarise(protest_count = n())

###which protesets do not have CBSAs associated with them, and which county are they in:

protest_no_cbsa <- filter(protest_cbsa, is.na(CBSAFP) == TRUE)

protest_no_cbsa <- st_join(protest_no_cbsa, county_ref[,c("GEOID", "NAMELSAD", "NAME")], join = st_within)
protest_cbsa <- st_join(protest_sf, cbsa_geom[,c("CBSAFP", "NAME")], join = st_within)


cbsa_protest_count <- left_join(cbsa_protest_count, cbsa_ref)

cbsa_protest_modelled <- cbsa_protest_count[is.na(cbsa_protest_count$perc_republican) == FALSE,]


cbsa.lm <- lm(protest_count~pop+perc_republican, data = cbsa_protest_modelled)
summary(cbsa.lm)

cbsa_protest_modelled$residual <- residuals(cbsa.lm)
cbsa_protest_modelled$predicted <- predict(cbsa.lm)

ggplot(cbsa_protest_modelled, aes(x=residual))+geom_histogram(binwidth = 10)




poisson.model <- glm(protest_count~pop+perc_republican,
                     cbsa_protest_modelled, family = poisson(link = "log"))
summary(poisson.model)

quasi.poisson.model <- glm(protest_count~pop+perc_republican,
                           cbsa_protest_modelled, family = quasipoisson(link = "log"))
summary(quasi.poisson.model)

#well this is bad! doesn't fit at allllll
with(poisson.model, cbind(res.deviance = deviance, df = df.residual,
                          p = pchisq(deviance, df.residual, lower.tail=FALSE)))


pop.model <- lm(protest_count~pop, data = cbsa_protest_modelled)
summary(pop.model)
cbsa_protest_modelled$pop_residual <- residuals(pop.model)
cbsa_protest_modelled$pop_predicted <- predict(pop.model)
ggplot(cbsa_protest_modelled, aes(x=pop_residual))+geom_histogram(binwidth = 10)


cbsa_protest_modelled$perc_democrat <- cbsa_protest_modelled$democrat/cbsa_protest_modelled$totalvotes




cbsa.d.lm <- lm(protest_count~pop+perc_democrat, data = cbsa_protest_modelled)
summary(cbsa.d.lm)


cbsa_protest_modelled$d.residual <- residuals(cbsa.d.lm)
cbsa_protest_modelled$d.predicted <- predict(cbsa.d.lm)
ggplot(cbsa_protest_modelled, aes(x=d.residual))+geom_histogram(binwidth = 10)
ggplot(cbsa_protest_modelled, aes(x=d.predicted))+geom_histogram(binwidth = 10)





poisson.d.model <- glm(protest_count~pop+perc_democrat,
                       cbsa_protest_modelled, family = poisson(link = "log"))
summary(poisson.d.model)

cbsa_protest_modelled$poisson.d.residual <- residuals(poisson.d.model, type = "response")
cbsa_protest_modelled$poisson.d.predicted <- predict(poisson.d.model, type = "response")
ggplot(cbsa_protest_modelled, aes(x=poisson.d.residual))+geom_histogram(binwidth = 3)
ggplot(cbsa_protest_modelled, aes(x=poisson.d.predicted))+geom_histogram(binwidth = 3)


quasi.poisson.model <- glm(protest_count~pop+perc_republican,
                           cbsa_protest_modelled, family = quasipoisson(link = "log"))
summary(quasi.poisson.model)

#well this is bad! doesn't fit at allllll
with(poisson.d.model, cbind(res.deviance = deviance, df = df.residual,
                            p = pchisq(deviance, df.residual, lower.tail=FALSE)))


hist(cbsa_protest_modelled$pop)
ggplot(cbsa_protest_modelled, aes(x=pop))+geom_histogram()
summary(cbsa_protest_modelled$pop)

cbsa_protest_modelled$rate_per_10k <- (cbsa_protest_modelled$protest_count/(cbsa_protest_modelled$pop))*10000
ggplot(cbsa_protest_modelled, aes(x=log(rate_per_10k), y = perc_democrat))+geom_point()


View(protest_cbsa)

cbsa_protest_modelled$rate_per_10k <- (cbsa_protest_modelled$protest_count/(cbsa_protest_modelled$pop))*10000
ggplot(cbsa_protest_modelled, aes(x = log(rate_per_10k))) + geom_histogram(binwidth = .5)
ggplot(cbsa_protest_modelled, aes(x = perc_democrat)) + geom_histogram(binwidth = .04)


lm.rate.d <- lm(log(rate_per_10k)~perc_democrat, cbsa_protest_modelled)
summary(lm.rate.d)
cbsa_protest_modelled$antilog_predicted <- 10^(predict(lm.rate.d))
cbsa_protest_modelled$predicted <- predict(lm.rate.d)
cbsa_protest_modelled$residuals <- residuals(lm.rate.d)

cbsa_protest_modelled$antilog_residuals <- cbsa_protest_modelled$rate_per_10k -  10^(predict(lm.rate.d))
cbsa_protest_modelled$rate_log  <- log(cbsa_protest_modelled$rate_per_10k)
cbsa_protest_modelled$std_resid  <- cbsa_protest_modelled$residuals/sd(cbsa_protest_modelled$residuals)

ggplot(cbsa_protest_modelled, aes(x=std_resid))+geom_histogram()

outlier <- filter(cbsa_protest_modelled, std_resid >= 1.96 | std_resid <= -1.96)

hist(residuals(lm.rate.d))
hist(10^(predict(lm.rate.d)))

ggplot(cbsa_protest_modelled, aes(x = log(rate_per_10k), y = perc_democrat))+geom_point()


cbsa_geom <- left_join(cbsa_geom, cbsa_protest_modelled[,c("CBSAFP", "rate_per_10k")])

View(protest_cbsa[protest_cbsa$CBSAFP == "12740",])
ggplot(data = protest_cbsa[protest_cbsa$CBSAFP == "12740",], aes(x = week))+geom_bar()

View(protest_cbsa[protest_cbsa$CBSAFP == "23180",])
ggplot(data = protest_cbsa[protest_cbsa$CBSAFP == "23180",], aes(x = week))+geom_bar()

View(protest_cbsa[protest_cbsa$CBSAFP == "47240",])
ggplot(data = protest_cbsa[protest_cbsa$CBSAFP == "23180",], aes(x = week))+geom_bar()

View(protest_cbsa[protest_cbsa$CBSAFP == "27220",])
ggplot(data = protest_cbsa[protest_cbsa$CBSAFP == "23180",], aes(x = week))+geom_bar()