###Time to figure out how to make them beeswarm charts: find an interesting place

library(ggplot2)
library(gganimate)
library(gifski)
library(janitor)
library(lubridate)
library(tidyverse)
library(sf)
library(nngeo)
library(stringr)
library(muckrakr)
library(ggbeeswarm)
library(ggridges)

loc <- file.path("/Users/alyssafowers/Documents/protest_2/protests/data/original")

usa <- map_data("usa")
protest <- read_csv(file.path(loc, "protest_lat_long.csv"))
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

lower_49 <- c("MD", "TN", "IN", "OH", "CT", "DC", "WA", "FL", "UT", "CA", "VT", "MN", "VA", "WI", "MO", "CO", "GA", "PA", "NY",
              "IL", "MA", "OR", "TX", "IA", "NC", "AZ", "MI", "NE", "NM", "LA", "MT", "ME", "NV", "OK", "KY", "AR", "NJ", "RI", 
              "ID", "NH", "KS", "SD", "AK", "MS", "WV", "SC", "ND", "WY", "DE")

protest_lower_49 <- protest[protest$state %in% lower_49,]

#split up tags for all places in lower 49

tag <- untangle(protest_lower_49, "tags", pattern = ";")
tag <- clean_names(tag)

tag_no_position <- tag[,!(str_detect(colnames(tag), "^for") | str_detect(colnames(tag), "^against"))]

tag_freq <- tag[,14:455] %>% 
       lapply(sum) %>% 
       as.data.frame() %>% 
       t() %>% 
       as.data.frame() %>% 
       tibble::rownames_to_column() %>% filter(V1>0) %>%
       arrange(desc(V1))

topic_freq <- tag_no_position[,14:135] %>% 
  lapply(sum) %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>% filter(V1>0) %>%
  arrange(desc(V1))

###Staten Island looked like it might have some interesting things happening?

filter(tag, short_loc == "Staten Island, NY")[14:455] %>% lapply(sum) %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>% filter(V1>0) %>%
  arrange(desc(V1))

###what about Durham:

filter(tag, short_loc == "Durham, NC")[14:455] %>% lapply(sum) %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>% filter(V1>0) %>%
  arrange(desc(V1))

###what about Sacramento, CA:

filter(tag, short_loc == "Sacramento, CA")[14:455] %>% lapply(sum) %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>% filter(V1>0) %>%
  arrange(desc(V1))

###what about Boulder, CO:

filter(tag, short_loc == "St. Louis, MO")[14:455] %>% lapply(sum) %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>% filter(V1>0) %>%
  arrange(desc(V1))


filter(protest_lower_49, short_loc == "Durham, NC") %>% mutate(month = month(date)) %>% ggplot(aes(x = date, y = 1))+
  geom_jitter(width = .2, height = .1, alpha = .2)+ylim(c(0,2))


loc <- protest_lower_49 %>% group_by(short_loc) %>% summarize(count = n()) %>% arrange(desc(count))

ggplot()+geom_density(data = protest_lower_49, aes(x = date)) + geom_density(data = filter(protest_lower_49, short_loc == "St. Louis, MO"), aes(x = date))

###TRY TO PLOT ST. LOUIS, WITH OVERALL AMERICAN PROTESTS IN BACKGROUND

protest_lower_49 <- mutate(protest_lower_49, week=as.Date(cut(date, breaks="week", starts.on.monday=TRUE)))

ggplot()+
  geom_dotplot(data = filter(protest_lower_49, short_loc == "St. Louis, MO"), aes(x = week), 
  binwidth = 1, dotsize = 5, stackratio = 1.3)+ylim(0,1)+theme_bw()

wbw <- protest_lower_49 %>% group_by(week) %>% summarise(all_count = n())
sl <- protest_lower_49 %>% filter(short_loc == "St. Louis, MO") %>% group_by(week) %>% summarise(sl_count = n())

wbw_sl <- inner_join(wbw, sl)

wbw_sl <- mutate(wbw_sl, sl_perc = sl_count/sum(wbw_sl$sl_count))
wbw_sl <- mutate(wbw_sl, all_perc = all_count/sum(wbw_sl$all_count))

ggplot(data = wbw_sl)+geom_bar(aes(x = week, y = all_perc), stat = "identity")+geom_bar(aes(x = week, y = sl_perc), stat = "identity", color = "red", fill = "red")

ggplot(data = wbw_sl, aes(x = week, y = sl_count))+geom_dotplot(binwidth = 1, dotsize = 5, stackratio = 1.3)

##lollipop chart to the rescue!

ggplot() + geom_vline(xintercept = ymd("2017-05-22"))+
  geom_point(data = wbw_sl, aes(x = week, y = sl_perc), color = "white")+
  geom_segment(data = wbw_sl, aes(x = week, y = 0, xend = week, yend = sl_perc), size = .25, color = "white")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="#707589", color="#707589"))
ggplot(data = wbw_sl)

