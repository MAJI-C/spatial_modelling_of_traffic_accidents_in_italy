options(scipen = 666)
# clear the workspace and console
rm(list = ls())
cat("\014") 

# packages for spatial modelling, generating W and visualising data on maps
library(spdep)
library(maptools)
library(rgdal)
library(RColorBrewer)
library(classInt)
library(dplyr)
library(tidyr)
###
map <- readOGR(".", "NUTS_RG_01M_2013")
map <- spTransform(map, "+proj=longlat")
map@data$NUTS_ID_char <- as.character(map@data$NUTS_ID)
map@data$country <- substr(map@data$NUTS_ID_char, 1, 2) 
map <- map[map@data$country == "IT", ]
#wider borders of voivodships
map_NUTS3 <- map[map@data$STAT_LEVL_ == 3, ]

###

#Import other data
#road accidents
road_accidents <- read.csv("road_accidents.csv", header = TRUE)
road_accidents <-road_accidents %>% filter(NATURAINCIDENTE==9 & GIORNOSETT==9)
road_accidents <-road_accidents[,c(1,2,21)]
road_accidents <-road_accidents[nchar(as.character(road_accidents$ITTER107)) == 5, ]
colnames(road_accidents)[3]<-"accidents"
###population & young
population <- read.csv("population.csv", header = TRUE)
young<-population %>%filter(ETA1  %in% c(  "Y18", "Y19", 
                                         'Y20', "Y21", "Y22", "Y23", "Y24",
                                         "Y25") & Gender=='males' & Marital.status=='total' )
population<-population %>% filter(Gender=='total' & Marital.status=='total')
population<-population %>% group_by(ITTER107, Territory) %>% summarise(population=sum(Value))
population<-population[nchar(as.character(population$ITTER107)) == 5, ]
young<- young[nchar(as.character(young$ITTER107)) == 5, ]
young<-young[, c(1,2,13)]
young<-young %>% group_by(ITTER107, Territory) %>% summarise(young=sum(Value))
road_accidents<-merge(road_accidents, population, by=c("ITTER107", "Territory"))
road_accidents<-merge(road_accidents, young, by=c("ITTER107", "Territory"))
rm(population,young)
road_accidents$accident_per_1000<-road_accidents$accidents/(0.001*road_accidents$population)
road_accidents$young_per<-100*road_accidents$young/road_accidents$population
#fleet
vehicle<-read.csv("vehicle_fleet.csv", header = TRUE)
vehicle<-vehicle %>%filter(TIPO_DATO22=='VEHICFLEET' & CATEGVEICOLI %in% c(1,7) )
vehicle<-vehicle[, c(1,2,6,9)]
vehicle<-vehicle  %>% spread(Vehicle.type, Value)
vehicle<-vehicle[nchar(as.character(vehicle$ITTER107)) == 5, ]
road_accidents<-merge(road_accidents, vehicle, by=c("ITTER107", "Territory"))
rm(vehicle)
road_accidents$car_per_pop<-road_accidents$`motor cars`/(0.001*road_accidents$population)
road_accidents$motocicle_per_pop<-road_accidents$motorcycles/(0.001*road_accidents$population)

#unemployment
unemployment<-read.csv("unemployment.csv", header = TRUE)
unemployment<-unemployment %>%filter(Gender=='total' & ETA1 =="Y_GE15" & TIME==2015)
unemployment<-unemployment[, c(1,17)]
colnames(unemployment)[2]<-"unemployment"
unemployment$ITTER107<- gsub('ITD1', 'ITD10', unemployment$ITTER107)
unemployment$ITTER107<- gsub('ITD2', 'ITD20', unemployment$ITTER107)
unemployment<-unemployment[nchar(as.character(unemployment$ITTER107)) == 5, ]
road_accidents<-merge(road_accidents, unemployment, by="ITTER107")
rm(unemployment)

#death rate
death_rate<-read.csv("death_rate.csv", header = TRUE)
death_rate<-death_rate %>%filter(Gender=='total' & ETA1 =="TOTAL")
death_rate<-death_rate[, c(1,11)]
colnames(death_rate)[2]<-"death_rate"
death_rate<-death_rate[nchar(as.character(death_rate$ITTER107)) == 5, ]
road_accidents<-merge(road_accidents, death_rate, by="ITTER107")
rm(death_rate)

#foreigners
foreigners<-read.csv("foreigners.csv", header = TRUE)
foreigners<-foreigners[, c(1,11)]
colnames(foreigners)[2]<-"foreigners"
foreigners<-foreigners[nchar(as.character(foreigners$ITTER107)) == 5, ]
road_accidents<-merge(road_accidents, foreigners, by="ITTER107")
rm(foreigners)
road_accidents$foreigners_pop<-100*road_accidents$foreigners/road_accidents$population

##
road_accidents$ITTER107<- gsub('ITD', 'ITH', road_accidents$ITTER107)
road_accidents$ITTER107<- gsub('ITE', 'ITI', road_accidents$ITTER107)
road_accidents$ITTER107<- gsub('ITC45', 'ITC4C', road_accidents$ITTER107)
road_accidents$ITTER107<- gsub('IT108', 'ITC4D', road_accidents$ITTER107)
road_accidents$ITTER107<- gsub('ITF41', 'ITF46', road_accidents$ITTER107)
road_accidents$ITTER107<- gsub('ITF42', 'ITF47', road_accidents$ITTER107)
road_accidents$ITTER107<- gsub('IT110', 'ITF48', road_accidents$ITTER107)
road_accidents$ITTER107<- gsub('IT109', 'ITI35', road_accidents$ITTER107)
#density
density<- read.csv("density.tsv", header = TRUE, sep="\t")
density<-density%>%separate(1, c("unit", "ITTER107"), sep = ",")
density$density<-as.numeric(as.character(density$X2015))
density<-density[,c(2,30)]
road_accidents<-merge(road_accidents, density, by="ITTER107")
road_accidents<-road_accidents[,-2]
#Put the spatial and economic databases together, remove the partial databases
spatial_data <- merge(y=road_accidents, x = map, by.y = "ITTER107", by.x = "NUTS_ID_char", sort = FALSE, all.x = F)

rm(map, road_accidents,density, map_NUTS3)


#illustrate variable
colors <- brewer.pal(6, "Blues") 
brks <- classIntervals(spatial_data$accident_per_1000, 6, tyle = "equal")
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(spatial_data$accident_per_1000, brks,all.inside = TRUE)], axes = F)
legend(x = "right", legend = leglabs(round(brks, 2), under = "poniżej", over = "powyżej"), fill = colors, bty = "n", x.intersp = .5, y.intersp = 1)

#illustrate variable
colors <- brewer.pal(6, "Blues") 
brks <- classIntervals(spatial_data$young_per, 6 )
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(spatial_data$young_per, brks,all.inside = TRUE)], axes = F, 
     main = "Odsetek mlodych mężczyzn")
legend(x = "right", legend = leglabs(round(brks, 2), under = "poniżej", over = "powyżej"), fill = colors, bty = "n", x.intersp = .5, y.intersp = 1)

#illustrate variable
colors <- brewer.pal(6, "Blues") 
brks <- classIntervals(spatial_data$car_per_pop, 6)
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(spatial_data$car_per_pop, brks,all.inside = TRUE)], axes = F, 
     main = "Liczba samochodów")
legend(x = "right", legend = leglabs(round(brks, 2), under = "poniżej", over = "powyżej"), fill = colors, bty = "n", x.intersp = .5, y.intersp = 1)

#illustrate variable
colors <- brewer.pal(6, "Blues") 
brks <- classIntervals(spatial_data$motocicle_per_pop, 6)
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(spatial_data$motocicle_per_pop, brks,all.inside = TRUE)], axes = F, 
     main = "Liczba zarejestrowanych motocykli")
legend(x = "right", legend = leglabs(round(brks, 2), under = "poniżej", over = "powyżej"), fill = colors, bty = "n", x.intersp = .5, y.intersp = 1)


#illustrate variable
colors <- brewer.pal(6, "Blues") 
brks <- classIntervals(spatial_data$density, 6)
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(spatial_data$density, brks,all.inside = TRUE)], axes = F, 
     main = "Gęstość zaludnienia")
legend(x = "right", legend = leglabs(round(brks, 2), under = "poniżej", over = "powyżej"), fill = colors, bty = "n", x.intersp = .5, y.intersp = 1)

