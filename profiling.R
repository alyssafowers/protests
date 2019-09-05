###Data Profiling###
##trying to answer completeness, unique values, validity, 
##look at formatting/frequency of data within variables

library(tidyverse)
library(data.table)
library(lubridate)
library(stringr)

loc <- "data/original"

protest <- read_csv(file.path(loc,"protest_2019_09_05.csv"))

str(protest)
head(protest)

###making "date" field into actual date
protest$Date <- ymd(protest$Date)

###renaming columns:
colnames(protest) <- c("date", "location", "attendees", "event_legacy", 
                       "tags", "curated", "source", "total_articles")

###to store results:
profile <- data.frame(variable = colnames(protest), 
                      completeness = 0,
                      unique = 0, stringsAsFactors = FALSE)

######COMPLETENESS#####

###Date field###

#manual check to see if wonky NA values
View(protest %>% group_by(date) %>% summarise(count = n()) %>% arrange(desc(count)))
profile[profile[,"variable"] == "date","completeness"]<- (nrow(protest) - sum(is.na(protest$date)))/nrow(protest)*100

#number of unique values:
profile[profile[,"variable"]== "date","unique"]<-length(unique(protest$date))
max(protest$date) - min(protest$date)

#954 dates in the dataset, but 962 actual days between the start and the end. What
#days did not have any protests?
all_date <- as.Date(min(protest$date):max(protest$date), origin="1970-01-01")
all_date[! all_date %in% unique(protest$date)]
#really? no protests over Christmas or New Years 2017? or are these the dates
#that the protests were entered, not that the protests occurred?

###Location field###
View(protest %>% group_by(location) %>% summarise(count = n()) %>% arrange(desc(count)))
hist(nchar(protest$location))
summary(nchar(protest$location))
#min location length is 5, max is 75, taking a look at extremes:
##the five character location??? was space??? but that's legit, actually
##very long place names are mostly labelled as schools/courthouses/etc on top
##of city/state
unique(protest$location[which(nchar(protest$location) <= 8)])
unique(protest$location[which(nchar(protest$location) == 5)])
unique(protest$location[which(nchar(protest$location) >= 50)])
#every row has a location field:
profile[profile[,"variable"] == "location","completeness"]<- (nrow(protest) - sum(is.na(protest$location)))/nrow(protest)*100
#6,521 unique location values
profile[profile[,"variable"]== "location","unique"]<-length(unique(protest$location))

#are they actually unique? how specific does this get?
unique(protest$location[str_detect(protest$location, "New York") | str_detect(protest$location, "NYC") | str_detect(protest$location, "New York, NY")])
unique(protest$location[str_detect(protest$location, "NYC")])

