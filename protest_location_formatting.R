####Creating working data file of protests in 50 states plus DC, with state and short location

library(tidyverse)
library(janitor)
library(stringr)
library(tigris)

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

#turning protests into sf with CRS matching county data--WILL UPDATE ONCE I HAVE UPDATE
#FROM NATHAN AND TOMMY ON ORIGINAL GCS:
protest_sf <- st_as_sf(protest, coords = c("latitude", "longitude"))
st_crs(protest_sf) <- st_crs(county)



protest_county <- st_join(protest_sf, county, join = st_within)
