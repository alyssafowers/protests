##### Analysis of places that tend to protest a LOT #####

#Please go through protest_location_formatting.R first.

library(tidyverse)
library(janitor)
library(stringr)
library(sf)
library(data.table)
library(muckrakr)

####################################
########### Load in data ###########
####################################

loc <- "data/working"

place <- read_csv(file.path(loc, "all_county_cbsa_summary.csv"))
place <- place %>% clean_names()

######################################
## Calculate protest per 10k people ##
######################################

#place <- mutate(place, protest_per_10k = (protest_count/pop)*10000)

######################################
### Flag places of interest ##########
######################################

place$alaska <- place$state == "AK"
place[is.na(place$alaska) == TRUE, "alaska"] <- FALSE
place[place$location_id %in% c(27940, 28540, 21820, 11260), "alaska"] <- TRUE

place <- mutate(place, alaska_or_capital = alaska == TRUE | include_capital == TRUE)

######################################
########### Initial graphs ###########
######################################

ggplot(place, aes(x = perc_democrat, y = protest_count))+
  geom_point(alpha = .2)+labs(title  = "Protest count by percent democrat", 
  subtitle = "all CBSAs and non-affiliated counties") + theme_classic()
ggplot(place, aes(x = pop, y = protest_count))+geom_point(alpha = .2)+
  labs(title  = "Protest count by population", 
  subtitle = "all CBSAs and non-affiliated counties") + theme_classic()
ggplot(place, aes(x = log(pop), y = protest_count))+geom_point(alpha = .2)+
  labs(title  = "Protest count by log of population", 
       subtitle = "all CBSAs and non-affiliated counties") + theme_classic()
ggplot(place, aes(x = protest_count))+geom_histogram(binwidth = 10)+
  labs(title  = "Distribution of protest count", 
       subtitle = "all CBSAs and non-affiliated counties") + theme_classic()
ggplot(place, aes(x = log(protest_count)))+geom_histogram(binwidth = .1)+
  labs(title  = "Distribution of log of protest count", 
       subtitle = "all CBSAs and non-affiliated counties") + theme_classic()
ggplot(place, aes(x = protest_per_10k))+geom_histogram(binwidth = 1)+
  labs(title  = "Distribution of protest per 10k residents", 
       subtitle = "all CBSAs and non-affiliated counties") + theme_classic()
ggplot(place, aes(x = log(protest_per_10k)))+geom_histogram(binwidth = .1)+
  labs(title  = "Distribution of log of protest per 10k residents", 
       subtitle = "all CBSAs and non-affiliated counties") + theme_classic()
ggplot(place, aes(x = perc_democrat))+geom_histogram(binwidth = .1)+
  labs(title  = "Distribution of percent democrat", 
       subtitle = "all CBSAs and non-affiliated counties") + theme_classic()


ggplot(place, aes(x = protest_count))+geom_histogram(binwidth = 10)+
  labs(title  = "Distribution of protest count", 
       subtitle = "all CBSAs and non-affiliated counties") + theme_classic()

  ggplot(place, aes(x = log(pop), y = protest_count))+geom_point(alpha = .4, color = "white", stroke = 0)+
  labs(title  = "Protest count by log of population", 
       subtitle = "all CBSAs and non-affiliated counties") + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_rect(fill="#707589", color="#707589"))+
    scale_x_continuous(breaks = c(4.6, 6.9, 9.2, 11.5, 13.8, 16.1))
  
  ggplot(place, aes(x = log(pop), y = protest_per_10k))+geom_point(alpha = .4, color = "white", stroke = 0)+
    labs(title  = "Protest rate per 10k people by log of population", 
         subtitle = "all CBSAs and non-affiliated counties") + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_rect(fill="#707589", color="#707589"))+
    scale_x_continuous(breaks = c(4.6, 6.9, 9.2, 11.5, 13.8, 16.1))
  
  ggplot(place, aes(x = log(pop), y = protest_per_10k, color = alaska_or_capital))+geom_point(alpha = .4, stroke = 0)+
    labs(title  = "Protest rate per 10k people by log of population", 
         subtitle = "all CBSAs and non-affiliated counties") + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_rect(fill="#707589", color="#707589"))+
    scale_x_continuous(breaks = c(4.6, 6.9, 9.2, 11.5, 13.8, 16.1))
  

View(place %>% filter(alaska_or_capital == FALSE) %>% arrange(desc(protest_per_10k)))
  
######################################
######### Basic linear model #########
######################################

#have to take out Alaska bc no voting info:

place_na <- filter(place, is.na(perc_democrat) == FALSE)

#with all places:
linear <- lm(protest_per_10k~perc_democrat, place_na)
summary(linear)
place_na$predict <- predict(linear)
place_na$residual <- residuals(linear)

##BIG ole positive skew on residuals
ggplot(place_na, aes(x = predict))+geom_histogram()
ggplot(place_na, aes(x = residual))+geom_histogram()

##BAD heteroscedasticity
ggplot(place_na, aes(x = predict, y = residual))+geom_point()

#ok: might need to just look at places that HAD protests to build model, although really dislike it!

################################################
## What distinguishes places without protests ##
################################################

place$has_protest <- place$protest_count > 0

###tend to be much, MUCH smaller:

ggplot(place, aes(x = has_protest, y = pop)) + geom_jitter(alpha = .2)

summary(place[place$has_protest, "pop"])
summary(place[! place$has_protest, "pop"])

ggplot(place, aes(x = has_protest, y = log(pop))) + geom_jitter(alpha = .2)

###lower % democrat in general, although has higher extreme upper value (makes sense if they're small places)
ggplot(place, aes(x = has_protest, y = perc_democrat)) + geom_jitter(alpha = .2)

summary(place[place$has_protest, "perc_democrat"])
summary(place[! place$has_protest, "perc_democrat"])

#combining population and % democrat--now THAT'S about as clear as can be
ggplot(place, aes(x = log(pop), y = perc_democrat, color = has_protest)) + geom_point(alpha = .2)+
  labs(title = "Percent democrat and log population", subtitle = "Areas with and without protests")+
  theme_classic()

##what are those EXTREMELY democrat-leaning places without protests? seem to be small counties in VERY red states

View(place[place$perc_democrat >=.7 & place$has_protest == FALSE,])

################################################
###### Modelling only places with protests #####
################################################

place_protest <- filter(place, has_protest == TRUE, is.na(perc_democrat) == FALSE)

ggplot(place_protest, aes(x = log(pop), y = protest_per_10k))+
  geom_point(alpha = .2)+labs(title  = "Protest rate by population", 
  subtitle = "CBSAs and counties with protests") + theme_classic()

ggplot(place, aes(x = percent_rank(pop), y = protest_count))+
  geom_point(alpha = .2)+labs(title  = "Protest count by population rank", 
  subtitle = "All areas") + theme_classic()

ggplot(place, aes(x = percent_rank(pop), y = protest_per_10k))+
  geom_point(alpha = .2)+labs(title  = "Protest rate by population rank", 
                              subtitle = "All areas") + theme_classic()


ggplot(place_protest, aes(x = percent_rank(pop), y = protest_per_10k))+
  geom_point(alpha = .2)+labs(title  = "Protest rate by population rank", 
                              subtitle = "County/CBSA with protest") + theme_classic()


#with all places:
linear <- lm(log(protest_per_10k)~perc_democrat, place_protest)
summary(linear)
place_protest$predict <- predict(linear)
place_protest$residual <- residuals(linear)

##predicted is right-skewed but that's ok, residuals look normal-ish:
ggplot(place_protest, aes(x = predict))+geom_histogram()+labs(title = "Predicted protests per 10k residents", 
                   subtitle = "perc democrat as predictor, areas with protests only")+theme_classic()
ggplot(place_protest, aes(x = residual))+geom_histogram()+labs(title = "Residual protests per 10k residents", 
  subtitle = "perc democrat as predictor, areas with protests only")+theme_classic()

##homoscedasticity, yay!
ggplot(place_protest, aes(x = predict, y = residual))+geom_point()+labs(title = "Predicted by residual protests per 10k residents",
                subtitle = "perc democrat as predictor, areas with protest only")+theme_classic()

##taking exponent of predicted to get actual number of protests predicted:
place_protest$predict_rate <- 10^place_protest$predict
place_protest$predict_count <- place_protest$predict_rate*(place_protest$pop/10000)

place_protest$residual_rate <- place_protest$protest_per_10k - place_protest$predict_rate
place_protest$residual_count <-  place_protest$protest_per_10k - place_protest$predict_count

##however, does look like big cities tend to have bigger residuals:

ggplot(place_protest, aes(x = pop, y = abs(residual)))+geom_point()
ggplot(place_protest, aes(x = pop, y = protest_per_10k))+geom_point()


ggplot(place_protest, aes(x = log(protest_per_10k), y = residual))+geom_point()+
  labs(title = "Residual by log of protests per 10k residents", 
       subtitle = "perc democrat as predictor, areas with protests only")+theme_classic()


#what if I looked at population in the model:
linear.pop <- lm(log(protest_per_10k)~perc_democrat+log(pop)+perc_democrat*log(pop), place_protest)
summary(linear.pop)
place_protest$predict.pop <- predict(linear.pop)
place_protest$residual.pop <- residuals(linear.pop)

ggplot(place_protest, aes(x = predict.pop))+geom_histogram()+labs(title = "Predicted log of protests per 10k residents", 
      subtitle = "perc democrat and population as predictors, areas with protests only")+theme_classic()
ggplot(place_protest, aes(x = residual.pop))+geom_histogram()+labs(title = "Residual log of protests per 10k residents", 
      subtitle = "perc democrat and population as predictors, areas with protests only")+theme_classic()

ggplot(place_protest, aes(x = log(protest_per_10k), y = residual.pop))+geom_point()+
  labs(title = "Residual by log of protests per 10k residents", 
  subtitle = "perc democrat and population as predictors, areas with protests only")+theme_classic()



ggplot(place_protest, aes(x = log(pop), y = protest_per_10k, color = perc_democrat))+
  geom_point()+scale_color_distiller()+labs(title = "Log population by protest rate")+theme_classic()


ggplot(place_protest, aes(ntile(x = place_protest$pop, 100), y = ntile(x = protest_per_10k, 100), color = perc_democrat))+
  geom_point()+scale_color_distiller()+labs(title = "Log population by protest rate")+theme_classic()


ggplot(place_protest, aes(x = perc_democrat, y = protest_per_10k, 100, color = log(pop)))+
  geom_point()+scale_color_distiller()+labs(title = "Protest rate by perc democrat")+theme_classic()

ggplot(place_protest, aes(x = log(pop), y = protest_per_10k, color = perc_democrat))+
  geom_point()+scale_color_distiller()+labs(title = "Protest rate by perc democrat")+theme_classic()+
  facet_wrap(ntile(x = place_protest$pop, 4)~.)


place_protest$predict.pop_rate <- 10^place_protest$predict.pop
place_protest$predict.pop_count <- place_protest$predict.pop_rate*(place_protest$pop/10000)

place_protest$residual.pop_rate <- place_protest$protest_per_10k - place_protest$predict.pop_rate
place_protest$residual.pop_count <-  place_protest$protest_count - place_protest$predict.pop_count


####is it even worth it to do ANY modelling, or should I just leave it be?

place_protest %>% arrange(desc(residual.pop_rate)) %>% 
  select(name, state, perc_democrat, pop, protest_count, protest_per_10k, residual.pop_rate)

place_protest %>% arrange(desc(residual_rate)) %>% 
  select(name, state, perc_democrat, pop, protest_count, protest_per_10k, residual_rate)

place %>% arrange(desc(protest_per_10k)) %>% 
  select(name, state, perc_democrat, pop, protest_count, protest_per_10k)


####################################
## Protests in individual places ###
####################################

protest <- read_csv(file.path(loc, "protest_all_states.csv"))
protest <- protest %>% clean_names()


#split up tags

tag <- untangle(protest, "tags", pattern = ";")
#tag <- tag[,33:442]
tag <- clean_names(tag)

tag_no_position <- tag[,!(str_detect(colnames(tag), "^for") | str_detect(colnames(tag), "^against"))]



  tag_count <- function(tag_file){
    tag_file <- tag_file[, 33:474]
    out <- lapply(tag_file, sum) %>% as.data.frame() %>% t() %>% as.data.frame() %>% tibble::rownames_to_column() %>% 
    filter(V1>0) %>%mutate(perc = V1/nrow(tag_file)) %>%arrange(desc(V1))
    out
  }
  
###Barre, VT
barre <- filter(tag, metro_micro_stat_area == 12740)
View(tag_count(barre)[1:10,])
View(barre)


###Cook County, MN
cook <- filter(tag, state_and_county_fips == 27031)
tag_count(cook)[1:10,]
View(cook)

###Frankfort, KY
frankfort <- filter(tag, metro_micro_stat_area == 23180)
tag_count(frankfort)[1:10,]
View(frankfort)
ggplot(frankfort, aes(x = week))+geom_bar()+
  labs(title = "All protests in Frankfort, KY")+theme_classic()
ggplot(filter(frankfort, education == 1), aes(x = week))+geom_bar()+
  labs(title = "Education protests in Frankfort, KY")+theme_classic()

###Lexington City, VA
lexington <- filter(tag, state_and_county_fips == 51678)
tag_count(lexington)[1:10,]
View(lexington)
ggplot(lexington, aes(x = week))+geom_bar()+
  labs(title = "All protests in Lexington City, VA")+theme_classic()
ggplot(filter(lexington, executive == 1), aes(x = week))+geom_bar()+
  labs(title = "Executive protests in Lexington City, VA")+theme_classic()

###Juneau, AK
juneau <- filter(tag, metro_micro_stat_area == 27940)
tag_count(juneau)[1:10,]
View(juneau)
ggplot(juneau, aes(x = week))+geom_bar()+
  labs(title = "All protests in Juneau, AK")+theme_classic()
ggplot(filter(juneau, executive == 1), aes(x = week))+geom_bar()+
  labs(title = "Executive protests in Juneau, AK")+theme_classic()

###Vineyard Haven, MA
vineyard <- filter(tag, metro_micro_stat_area == 47240)
tag_count(vineyard)[1:10,]
View(vineyard)
ggplot(juneau, aes(x = week))+geom_bar()+
  labs(title = "All protests in Juneau, AK")+theme_classic()
ggplot(filter(juneau, executive == 1), aes(x = week))+geom_bar()+
  labs(title = "Executive protests in Juneau, AK")+theme_classic()


###San Juan County, WA
sanjuan <- filter(tag, state_and_county_fips == 53055)
tag_count(sanjuan)[1:10,]
View(sanjuan)
ggplot(lexington, aes(x = week))+geom_bar()+
  labs(title = "All protests in Lexington City, VA")+theme_classic()
ggplot(filter(lexington, executive == 1), aes(x = week))+geom_bar()+
  labs(title = "Executive protests in Lexington City, VA")+theme_classic()

###Jackson, WY-ID
jackson <- filter(tag, metro_micro_stat_area == 27220)
tag_count(jackson)[1:10,]
View(jackson)
ggplot(frankfort, aes(x = week))+geom_bar()+
  labs(title = "All protests in Frankfort, KY")+theme_classic()
ggplot(filter(frankfort, education == 1), aes(x = week))+geom_bar()+
  labs(title = "Education protests in Frankfort, KY")+theme_classic()


###Montezuma County, CO
montezuma <- filter(tag, state_and_county_fips == "08083")
tag_count(montezuma)[1:10,]
View(lexington)
ggplot(lexington, aes(x = week))+geom_bar()+
  labs(title = "All protests in Lexington City, VA")+theme_classic()
ggplot(filter(lexington, executive == 1), aes(x = week))+geom_bar()+
  labs(title = "Executive protests in Lexington City, VA")+theme_classic()

###Yakutat City and Borough, AK
yakutat <- filter(tag, location_id == "02282")
tag_count(yakutat)[1:10,]
View(yakutat)

###Hoonah-Angoon, AK
hoonah <- filter(tag, location_id == "02105")
tag_count(hoonah)[1:10,]
View(hoonah)

###Petersburg Borough, AK
petersburg <- filter(tag, location_id == "02195")
tag_count(petersburg)[1:10,]
View(petersburg)

###Wrangell City, AK
wrangell <- filter(tag, location_id == "02275")
tag_count(wrangell)[1:10,]
View(wrangell)

###Haines Borough, AK
haines <- filter(tag, location_id == "02100")
tag_count(haines)[1:10,]
View(haines)

###Sitka City and Borough, AK
sitka <- filter(tag, location_id == "02220")
tag_count(sitka)[1:10,]
View(sitka)

###Taos, NM
taos <- filter(tag, location_id == "45340")
tag_count(taos)[1:10,]
View(taos)
ggplot(lexington, aes(x = week))+geom_bar()+
  labs(title = "All protests in Lexington City, VA")+theme_classic()
ggplot(filter(lexington, executive == 1), aes(x = week))+geom_bar()+
  labs(title = "Executive protests in Lexington City, VA")+theme_classic()


###what is going on in alaska, in general?
alaska <- clean_names(filter(protest, state == "AK"))

tag_count_ak <- function(tag_file){
  tag_file <- tag_file[, 33:103]
  out <- lapply(tag_file, sum) %>% as.data.frame() %>% t() %>% as.data.frame() %>% tibble::rownames_to_column() %>% 
    filter(V1>0) %>%mutate(perc = V1/nrow(tag_file)) %>%arrange(desc(V1))
  out
}

tag_count_ak(alaska_tag)[1:10,]
ggplot(alaska, aes(x = week))+geom_bar()+
  labs(title = "All protests in Alaska")+theme_classic()
ggplot(filter(alaska_tag, against_state_executive == 1), aes(x = week))+geom_bar()+
  labs(title = "State executive protests in Alaska")+theme_classic()






####################################
## standardizing values somehow ###
####################################


##standardize by sd related to size of population?

hist(place_protest$pop)
quantile(place_protest$pop)

sd(filter(place_protest, pop < 36758)$protest_per_10k)

sd(filter(place_protest, pop > 206722)$protest_per_10k)

place_protest <- mutate(place_protest, pop_quart = ntile(pop, 4))

place_protest$sd_adjust <- as.numeric(0)

for(i in 1:nrow(place_protest)){
  place_quant <- as.numeric(place_protest[i, "pop_quart"])
  place_protest[i, "sd_adjust"] <- sd(unlist(place_protest[place_protest$pop_quart == place_quant,  "protest_per_10k"]))
}

place_protest$protest_rate_standardized <- place_protest$protest_per_10k/sd(place_protest$protest_per_10k)
place_protest$protest_rate_std_adjusted <- place_protest$protest_per_10k/place_protest$sd_adjust

place_protest %>% arrange(desc(protest_rate_std_adjusted)) %>% 
  select(name, state, perc_democrat, pop, protest_count, protest_per_10k, protest_rate_standardized, protest_rate_std_adjusted)

ggplot(place_protest, aes(x = log(pop), y = protest_rate_std_adjusted))+geom_point()

#this turns up places that are larger, but keeps the same top 5ish. 
#means I should also look into Augusta-Waterville, ME, Missoula MT, Burlington-South Burlington VT, Charlottesville VA, and Pittsfield, MA

#Augusta-Waterville, ME
augusta <- filter(tag, metro_micro_stat_area == 12300)
tag_count(augusta)[1:10,]
View(augusta)
ggplot(augusta, aes(x = week))+geom_bar()+
  labs(title = "All protests in Augusta, ME")+theme_classic()
ggplot(filter(frankfort, education == 1), aes(x = week))+geom_bar()+
  labs(title = "Education protests in Frankfort, KY")+theme_classic()

#Missoula, MT
missoula <- filter(tag, metro_micro_stat_area == 33540)
tag_count(missoula)[1:10,]
View(missoula)
ggplot(missoula, aes(x = week))+geom_bar()+
  labs(title = "All protests in Missoula, MT")+theme_classic()


#Burligton-South Burlington VT
burlington <- filter(tag, metro_micro_stat_area == 15540)
tag_count(burlington)[1:10,]
View(burlington)
ggplot(burlington, aes(x = week))+geom_bar()+
  labs(title = "All protests in Burlington-South Burlington, VT")+theme_classic()

#Charlottesville, VA
charlottesville <- filter(tag, metro_micro_stat_area == 16820)
tag_count(charlottesville)[1:10,]
View(charlottesville)
ggplot(charlottesville, aes(x = week))+geom_bar()+
  labs(title = "All protests in Charlottesville, VA")+theme_classic()

#Pittsfield, MA

pittsfield <- filter(tag, metro_micro_stat_area == 38340)
tag_count(pittsfield)[1:10,]
View(pittsfield)
ggplot(pittsfield, aes(x = week))+geom_bar()+
  labs(title = "All protests in Pittsfield, MA")+theme_classic()
ggplot(filter(pittsfield, environment == 1), aes(x = week))+geom_bar()+
  labs(title = "Environmental protests in Pittsfield, MA")+theme_classic()

####trouble is, though, that it privileges places that are right on the border of a quartile. 
#Rolling SD instead, using same # as quartile (220 observations on either side)?

place_protest$sd_roll <- NA

for(i in 1:nrow(place_protest)){
  if(i <= 220){
    place_protest[i,"sd_roll"] <- sd(place_protest[1:220,"protest_per_10k"])
  } else if(nrow(place_protest) - i <= 220){
    place_protest[i, "sd_roll"]
  } else {
    
  }
}
