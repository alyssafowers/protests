library(tidyverse)
library(data.table)
library(stringr)
library(janitor)

protest <- fread("data/original/protest_lat_long.csv")
protest <- protest %>% clean_names()

state <- "(?-i:A[LKSZRAEP]|C[AOT]|D[EC]|F[LM]|G[AU]|HI|I[ADLN]|K[SY]|LA|M[ADEHINOPST]|N[CDEHJMVY]|O[HKR]|P[ARW]|RI|S[CD]|T[NX]|UT|V[AIT]|W[AIVY])$"
state_true <- str_detect(protest$location,state)

protest <- protest[state_true,]

#code from here: https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

protest$state <- substrRight(protest$location, 2)

protest$state <- apply(protest$location, MARGIN = 1, function(x){substr(protest[protest$location == x, "location"], nchar(protest[protest$location == x,"location"]) - 2, nchar(protest[protest$location == x, "location"]))})


##looking to see how many unique lat/long combinations there are
protest$lat_long <- paste(protest$latitude, protest$longitude)
View(protest %>% group_by(lat_long, location) %>% summarise(freq = n()) %>% arrange(desc(freq)))

#still want to collapse locations as much as possible--just get town name and state?
protest$comma_count <- str_count(protest$location, ",")
protest %>% group_by(comma_count) %>% summarise(freq = n(), avg_length = mean(nchar(location))) %>% arrange(desc(freq))

shortLoc <- function(x){
  substrRight <- function(y, n){
    substr(as.character(y), nchar(y)-n+1, nchar(y))
  }
  
  short_loc <- character()
  
  for(i in 1:nrow(x)){
    if(x[i,"comma_count"] > 1){
      loc <- as.character(x[i,"location"])
      cc <- as.integer(x[i,"comma_count"])
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

View(protest %>% group_by(short_loc) %>% summarise(count = n()) %>% arrange(desc(count)))
protest %>% group_by(short_loc) %>% summarise(count = n()) %>% arrange(desc(count)) %>% ggplot(aes(x = count))+geom_histogram()
