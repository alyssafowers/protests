###Data Profiling###
##trying to answer completeness, unique values, validity, 
##look at formatting/frequency of data within variables

library(tidyverse)
library(data.table)
library(lubridate)
library(stringr)
library(muckrakr)

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

#are they actually unique? how specific does this get? what am I in for?
unique(protest$location[str_detect(protest$location, "New York") | str_detect(protest$location, "NYC") | str_detect(protest$location, "New York, NY")])
unique(protest$location[str_detect(protest$location, "NYC")])
View(protest[str_detect(protest$location, coll("New York")),] %>% group_by(location) %>% summarize(count = n()) %>% mutate(perc = count/sum(count))%>% arrange(desc(count)))
##there are 88 different locations that boil down to "new york city," but most of
##them are extremely specific neighborhoods within NYC, and about half are written
##as simply New York, NY

#what about Los Angeles? not AS many, but still lots of specific locations. look
#like they're always divided by commas (specific, city, state) so that might be a way to
#get at this. None that are shortened to LA, none that are lower-cased
View(protest[str_detect(protest$location, coll("Los Angeles")),] %>% group_by(location) %>% summarize(count = n()) %>% mutate(perc = count/sum(count))%>% arrange(desc(count)))
View(protest[str_detect(protest$location, coll("LA")),] %>% group_by(location) %>% summarize(count = n()) %>% mutate(perc = count/sum(count))%>% arrange(desc(count)))
View(protest[str_detect(protest$location, coll("Los Ang")),] %>% group_by(location) %>% summarize(count = n()) %>% mutate(perc = count/sum(count))%>% arrange(desc(count)))

#are there any that do NOT end in two-letter state name? (I know there are, 
#because of SPACE PROTEST, but I just want to see what they are and what they 
#look like.) got this code from https://gist.github.com/PhazeonPhoenix/2008449

state <- "(?-i:A[LKSZRAEP]|C[AOT]|D[EC]|F[LM]|G[AU]|HI|I[ADLN]|K[SY]|LA|M[ADEHINOPST]|N[CDEHJMVY]|O[HKR]|P[ARW]|RI|S[CD]|T[NX]|UT|V[AIT]|W[AIVY])$"
unique(protest$location[!str_detect(protest$location,state)])

#mostly Guam, Puerto Rico, space, UK. Three examples are within US states with messed up
#capitalization (Hi, Fl, Mi), one doesn't have a state listed ("Fredon Township")

unique(str_extract_all(protest$location,state, simplify = TRUE))

#there are 53 "states" included here. PR and DC are included as states, and then there is a blank ("")
#need to investigate what that blank could be

protest$location[which(str_extract_all(protest$location,state, simplify = TRUE) == "")]

#ok so the blank is just, strings that don't match. 52 states and they all look good.


####Attendees
#doesn't appear to have any weird NA-substitutes:
View(protest %>% group_by(attendees) %>% summarise(count = n()) %>% arrange(desc(count)))

#completeness: 64% of protests have an attendee count
profile[profile[,"variable"] == "attendees","completeness"] <- (nrow(protest) - sum(is.na(protest$attendees)))/nrow(protest)*100
#355 unique values for attendees. there is one REALLY LARGE protest that messes up the 
#histogram, but it's the original women's march in DC so, believable.
profile[profile[,"variable"] == "attendees","unique"] <- length(unique(protest$attendees))
hist(protest$attendees)
max(protest$attendees, na.rm = TRUE)
protest[protest$attendees == 725000,]

####Event (legacy field)
#this is the "topic" but may not have a for/against stance, and only has one category per
#protest. not going to spend too much time on it.
profile[profile[,"variable"] == "event_legacy","completeness"] <- (nrow(protest) - sum(is.na(protest$event_legacy)))/nrow(protest)*100
profile[profile[,"variable"] == "event_legacy","unique"] <- length(unique(protest$event_legacy))


####Tags
#this is the GOOD STUFF. although they only started implementing tags in mid-2018, they
#went back and retroactively applied it to the old data: https://countlove.org/blog/protest-tags-exploration.html
#Tags are separated by semicolons and include topics and stances.
#checking first to make sure that every protest has a tag (they do):
profile[profile[,"variable"] == "tags","completeness"] <- (nrow(protest) - sum(is.na(protest$tags)))/nrow(protest)*100

#for uniqueness, going to break apart the tags and see how many unique ones there are. Untangle
#appends a bunch of columns to the existing dataframe, so I'm going to pull out the old columns first:
ncol(protest)
ncol(tags)
tags <- untangle(protest, "tags", pattern = ";")
tags <- tags[,9:449]
View(tags)

#441 unique tags possible:
profile[profile[,"variable"] == "tags","unique"] <- ncol(tags)

#what are the most common tags?
View(tags %>% 
  lapply(sum) %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>% filter(V1>0) %>%
  arrange(desc(V1)))
#civil rights, guns, immigration, "for greater gun control", "other", "for_compassionate_immigration"
#were the top tags

#how many tags does each protest tend to have? all of them have at least 2, median is 2, maximum is 8.
#looks like each one has at least a category and a position
tag_num <- apply(tags, 1, sum)
summary(tag_num)
hist(tag_num)


###curated
#not sure what "curated" means, but it's completely filled out!
profile[profile[,"variable"] == "curated","completeness"]<- (nrow(protest) - sum(is.na(protest$curated)))/nrow(protest)*100
#there are only two values, yes and no. 5,373 of these are NOT curated
protest %>% group_by(curated) %>% summarise(count = n()) %>% arrange(desc(count))
#the map on their website has a toggle labeled "Show curated protest data for a more compassionate country/Show all protest data"
#so my guess is that 'curated' = liberal to some extent: https://countlove.org/search.html?curated=no
profile[profile[,"variable"] == "curated","unique"]<- length(unique(protest$curated))

###source
#every row has a source, and most of them are unique (16,789 sources for 20,025 protests):
profile[profile[,"variable"] == "source","completeness"]<- (nrow(protest) - sum(is.na(protest$source)))/nrow(protest)*100
profile[profile[,"variable"] == "source","unique"]<- length(unique(protest$source))

#what sources are used more than once?
View(protest %>% group_by(source) %>% summarise(count = n()) %>% filter(count > 1) %>% arrange(desc(count)))
#some sources used up to 20+ times, but in checking the articles, looks like
#those sources describe multiple protests (ie https://www.capecod.com/newscenter/cape-codders-join-reunification-protests/)
#so would be used many times if it's one row per protest. each school listed in
#a school walkout article is a separate protest.

###total_articles
#all protests have a value here:
profile[profile[,"variable"] == "total_articles","completeness"]<- (nrow(protest) - sum(is.na(protest$total_articles)))/nrow(protest)*100
profile[profile[,"variable"] == "total_articles","unique"]<- length(unique(protest$total_articles))

#most protests have one article written about them, with a 3rd quartile of 2, but the
#max is NINE HUNDRED AND THIRTEEN. that was the main March For Our Lives in DC.
#about 20 protests have more than a hundred articles written about them. at a quick
#glance through, they look legit?

hist(protest$total_articles)
summary(protest$total_articles)
protest[protest$total_articles == 913,]
View(protest[protest$total_articles >= 100,])