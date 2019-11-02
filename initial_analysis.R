###Exploration and descriptives
##run this AFTER protest_location_formatting

library(tidyverse)
library(janitor)
library(stringr)
library(tigris)
library(sf)
library(data.table)

####Loading data in

loc <- "data/working"

protest <- read_csv(file.path(loc, "protest_lower_49.csv"))
protest <- protest %>% clean_names()

###Summarizing by county

county <- protest %>% group_by(county_fips, county_name_long, state_fips, 
                state, population_estimate_2017, perc_republican) %>% summarise(protest_count = n())
summary(county$protest_count)

#Distribution of number of protests by county
ggplot(county)+geom_histogram(aes(x = protest_count), binwidth = 1) + 
  labs(title = ("Protests per county"), x = "Number of protests", y = "Number of counties")

#count of protests by 2017 population
ggplot(county)+geom_point(aes(x = protest_count, y = population_estimate_2017, color = perc_republican)) +
  labs(title = "Protests per county by 2017 population estimate", x = "Number of protests",
       y = "2017 population estimate")+scale_color_distiller()

#doing this shows me that there is a "los angeles county, oregon" in the data, 
#which is surely incorrect. fixed it in location_formatting.

#count of protests by % republican vote in 2016--looks liike places that had 
#more-than-a-few protests while also being more-republican just had a lot of people?

ggplot(county)+geom_point(aes(x = protest_count, y = perc_republican, color = population_estimate_2017)) +
  labs(title = "Protests per county by percent Republican vote in 2017 presidential 
       election", x = "Number of protests", y = "Percent Republican vote in 2016 
       presidential election")+ scale_color_distiller(palette = "Spectral")
ggplot(county, aes(x = perc_republican))+geom_histogram()
ggplot(county, aes(x = population_estimate_2017))+geom_histogram()

#just checking--relationship between size and perc republican:

ggplot(county, aes(y=perc_republican, x = log(population_estimate_2017)))+geom_point()

cor(county$perc_republican, county$population_estimate_2017)

###Summarizing by week

week <- protest %>% group_by(week) %>% summarise(protest_count = n())

#protests per week over time
ggplot(data = week, aes(x = week, y = protest_count))+geom_bar(stat = "identity")

#distribution of protests
ggplot(data = week, aes(x = protest_count))+geom_histogram(binwidth = 25)+labs(title = "Distribution of protests per week", x = "Protests", y = "Count of weeks")
summary(week$protest_count)

###Summarizing by county by week

c_w <- protest %>% group_by(week, county_name_long) %>% summarise(protest_count = n())
ggplot(data = c_w, aes(x = protest_count))+geom_histogram(binwidth = 1)+labs(title = "Distribution of protests per week", x = "Protests", y = "Count of weeks")


##HMMMM how many counties have no protests at all?

pop <- read_csv("data/original/county_population_data.csv")
nrow(pop)-nrow(county)

#....1899. More than the number of counties with protests! Maybe should look at a larger unit? (micropolitan/metropolitan statistical area?)

length(unique(protest$metro_micro_stat_area))
sum(is.na(protest$combined_statistical_area))

mm_c <- protest %>% group_by(state_and_county_fips, metro_micro_stat_area) %>% summarise(count = n())

#169 protesting counties do not have an MMSA, compared to 1,086 protesting counties 
#that DO have an MMSA

sum(is.na(mm_c$metro_micro_stat_area))
nrow(filter(mm_c, is.na(metro_micro_stat_area) == FALSE))

mm <- protest %>% filter(is.na(metro_micro_stat_area) == FALSE) %>% group_by(metro_micro_stat_area) %>% summarise(count = n())

#there are 408 protests in without MMSAs that had protests, compared to 19,320 protests
#in counties with MMSAs:

nrow(filter(protest, is.na(metro_micro_stat_area) == TRUE))
nrow(filter(protest, is.na(metro_micro_stat_area) == FALSE))

###OKAY if I'm going to group them by metro/micropolitan statistical areas instead...

#need to update population counts
#need to update percent republican
#keep counties that do NOT have metro/micropolitan as their own entities

#but the CBSAs are too general (ie, put New York City and Newark in the same CBSAs. argh.)


###let's just take a quick look at a regression, shall we

poisson.model <- glm(protest_count~population_estimate_2017+perc_republican,
                     county, family = poisson(link = "log"))
summary(poisson.model)

quasi.poisson.model <- glm(protest_count~population_estimate_2017+perc_republican,
                     county, family = quasipoisson(link = "log"))
summary(quasi.poisson.model)

#well this is bad! doesn't fit at allllll
with(poisson.model, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))





linear.model <- lm(protest_count~population_estimate_2017+perc_republican,
                   county)
summary(linear.model)
county.res <- residuals(linear.model)
county.pred <- residuals(linear.model)
ggplot(as.data.frame(county.res), aes(x = county.res))+geom_histogram(binwidth = 5)

which(county.res > 200)
county[which(county.res > 200),]
county.pred[which(county.res > 200)]

county$lm_residual <- residuals(linear.model)
county$lm_predict <- predict(linear.model)

#what I'm seeing in the linear model is, HUGE positive residuals in core areas, and big negative
#residuals in the counties around those, which supports the CBSA model