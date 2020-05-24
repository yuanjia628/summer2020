#####LOAD PACKAGES#####
library(tidyverse)

#####FUNCTIONS#####
q5 <- function(variable) {as.factor(ntile(variable, 5))}
qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

######LOAD DATA#####
#airports data
airports <- read.csv("D:/aviation2020/airports.csv")
#BTS data
mar2019 <- read.csv("D:/aviation2020/032019.csv")
mar2020 <- read.csv("D:/aviation2020/032020.csv")
all <- read.csv("D:/aviation2020/od_with_na.csv")
#processed data
monthchange_rank <- read.csv("D:/aviation2020/rank_monthchange.csv")


#####data cleaning#####
#2019
mar2019 <- mar2019%>%
  select(YEAR,
         DAY_OF_MONTH,
         DAY_OF_WEEK,
         OP_UNIQUE_CARRIER,
         ORIGIN,
         DEST,
         CANCELLED)
  
mar2019 <- left_join(mar2019, airports, by = c("ORIGIN" = "IATA"))%>%
  select(YEAR,
         DAY_OF_MONTH,
         DAY_OF_WEEK,
         OP_UNIQUE_CARRIER,
         ORIGIN,
         DEST,
         CANCELLED,
         LAT,
         LON)%>%
  rename(CARRIER = OP_UNIQUE_CARRIER,
         LAT_O = LAT,
         LON_O = LON)%>%
  filter(CANCELLED == 0)

mar2019 <- left_join(mar2019, airports, by = c("DEST" = "IATA"))%>%
  select(YEAR,
         DAY_OF_MONTH,
         DAY_OF_WEEK,
         CARRIER,
         ORIGIN,
         DEST,
         CANCELLED,
         LAT_O,
         LON_O,
         LAT,
         LON)%>%
  rename(LAT_D = LAT,
         LON_D = LON)
  
#mar2019$OD <- paste(mar2019$ORIGIN,"-", mar2019$DEST)
#mar2019$DO <- paste(mar2019$DEST,"-", mar2019$ORIGIN)


#monthly data by airline and OD
airline201903 <- mar2019%>%
  group_by(YEAR,
           CARRIER,
           ORIGIN,
           DEST,
           LAT_O,
           LON_O,
           LAT_D,
           LON_D)%>%
  summarise(n=n())
#monthly data by only airline
airlineonly201903 <- mar2019%>%
  group_by(YEAR,
           CARRIER)%>%
  summarise(n=n())

#2020
mar2020 <- mar2020%>%
  select(YEAR,
         DAY_OF_MONTH,
         DAY_OF_WEEK,
         OP_UNIQUE_CARRIER,
         ORIGIN,
         DEST,
         CANCELLED)

mar2020 <- left_join(mar2020, airports, by = c("ORIGIN" = "IATA"))%>%
  select(YEAR,
         DAY_OF_MONTH,
         DAY_OF_WEEK,
         OP_UNIQUE_CARRIER,
         ORIGIN,
         DEST,
         CANCELLED,
         LAT,
         LON)%>%
  rename(CARRIER = OP_UNIQUE_CARRIER,
         LAT_O = LAT,
         LON_O = LON)%>%
  filter(CANCELLED == 0)

mar2020 <- left_join(mar2020, airports, by = c("DEST" = "IATA"))%>%
  select(YEAR,
         DAY_OF_MONTH,
         DAY_OF_WEEK,
         CARRIER,
         ORIGIN,
         DEST,
         CANCELLED,
         LAT_O,
         LON_O,
         LAT,
         LON)%>%
  rename(LAT_D = LAT,
         LON_D = LON)

#mar2020$OD <- paste(mar2020$ORIGIN,"-", mar2020$DEST)

#monthly data by airline and OD
airline202003 <- mar2020%>%
  group_by(YEAR,
           CARRIER,
           ORIGIN,
           DEST,
           LAT_O,
           LON_O,
           LAT_D,
           LON_D)%>%
  summarise(n=n())

#monthly data by only airline
airlineonly202003 <- mar2020%>%
  group_by(YEAR,
           CARRIER)%>%
  summarise(n=n())

#merge monthly data by airlines and ODs
mar <- merge(airline201903, airline202003, by = c("CARRIER", "ORIGIN", "DEST"), all = T)%>%
  select(CARRIER,
         ORIGIN,
         DEST,
         n.x,
         n.y)%>%
  replace(is.na(.),0)%>%
  mutate(diff = n.y - n.x)


mar_des <- mar[order(mar$diff, decreasing = TRUE),]

mar_des <- mar_des%>%
  group_by(CARRIER, ORIGIN = pmin(ORIGIN, DEST), DEST = pmax(ORIGIN, DEST)) %>%
  summarise(diff = sum(diff))
  
mar_des <- subset(mar_des, !(mar_des$ORIGIN == mar_des$DEST))
mar_asc <- mar_des[order(mar_des$diff, decreasing = FALSE),]
#mar_test <- subset(mar_des, mar_des$ORIGIN == mar_des$DEST)

#mar_sample <- head(mar_des,5)
#dput <- dput(mar_sample)

#merge monthly data by airlines and ODs
mar_airline <- merge(airlineonly201903, airlineonly202003, by = c("CARRIER"), all = T)%>%
  mutate(diff = n.y - n.x)

mar_airline_des <- mar_airline[order(mar_airline$diff, decreasing = TRUE),]

mar_airline_des <- mar_airline_des%>%
  mutate(pct_change = (diff / n.x) *100)

mar_airline_pct_des <- mar_airline_des[order(mar_airline_des$pct_change, decreasing = TRUE),]


######BUCKET CHANGE#####
target_routes <- monthchange_rank%>%
  filter(!quan_change ==0)

target_routes <- target_routes%>%
  mutate(ODpairs = OD)%>%
  separate(OD, c('AP1', 'AP2'), sep = '\\s*-\\s*')

flight19_AP1 <- target_routes%>%
  group_by(AP1)%>%
  summarise(tot_flights19_AP1 = sum(flights19))

flight19_AP2 <- target_routes%>%
  group_by(AP2)%>%
  summarise(tot_flights19_AP2 = sum(flights19))

flight20_AP1 <- target_routes%>%
  group_by(AP1)%>%
  summarise(tot_flights20_AP1 = sum(flights20))

flight20_AP2 <- target_routes%>%
  group_by(AP2)%>%
  summarise(tot_flights20_AP2= sum(flights20))

flight19_AP <- left_join(flight19_AP1, flight19_AP2, by = c("AP1" = "AP2"))

flight19_AP <- flight19_AP%>%
  replace(is.na(.), 0)%>%
  mutate(tot_flights = tot_flights19_AP1+tot_flights19_AP2)

flight20_AP <- left_join(flight20_AP1, flight20_AP2, by = c("AP1" = "AP2"))

flight20_AP <- flight20_AP%>%
  replace(is.na(.), 0)%>%
  mutate(tot_flights = tot_flights20_AP1+tot_flights20_AP2)

target_routes <- left_join(target_routes, flight19_AP, by = c("AP1" ))%>%
  select(-tot_flights19_AP1,
         -tot_flights19_AP2)
target_routes <- target_routes%>%
  rename(tot_flights19_AP1 = tot_flights)

target_routes <- left_join(target_routes, flight20_AP, by = c("AP1" ))%>%
  select(-tot_flights20_AP1,
         -tot_flights20_AP2)
target_routes <- target_routes%>%
  rename(tot_flights20_AP1 = tot_flights)

#JOIN AND GET DISTANCE
target_routes <- left_join(target_routes,all,by = c("ODpairs" = "OD"))

#####monthly change#####
monthchange_rank <- monthchange_rank%>%
  mutate(ODpairs = OD)%>%
  separate(OD, c('AP1', 'AP2'), sep = '\\s*-\\s*')

all_flight19_AP1 <- monthchange_rank%>%
  group_by(AP1)%>%
  summarise(tot_flights19_AP1 = sum(flights19))

all_flight19_AP2 <- monthchange_rank%>%
  group_by(AP2)%>%
  summarise(tot_flights19_AP2 = sum(flights19))

all_flight20_AP1 <- monthchange_rank%>%
  group_by(AP1)%>%
  summarise(tot_flights20_AP1 = sum(flights20))

all_flight20_AP2 <- monthchange_rank%>%
  group_by(AP2)%>%
  summarise(tot_flights20_AP2= sum(flights20))

all_flight19_AP <- merge(all_flight19_AP1, all_flight19_AP2, by.x = "AP1", by.y = "AP2", all = T)

all_flight19_AP <- all_flight19_AP%>%
  replace(is.na(.), 0)%>%
  mutate(tot_flights = tot_flights19_AP1+tot_flights19_AP2)

all_flight20_AP <- merge(all_flight20_AP1, all_flight20_AP2, by.x = "AP1", by.y = "AP2", all=T)

all_flight20_AP <- all_flight20_AP%>%
  replace(is.na(.), 0)%>%
  mutate(tot_flights = tot_flights20_AP1+tot_flights20_AP2)

monthchange_rank <- left_join(monthchange_rank, all_flight19_AP, by = c("AP1" ))
monthchange_rank <- monthchange_rank%>%
  #select(-tot_flights19_AP1,
   #      -tot_flights19_AP2)%>%
  rename(tot_flights19_AP1 = tot_flights)

monthchange_rank <- left_join(monthchange_rank, all_flight20_AP, by = c("AP1" ))
monthchange_rank <- monthchange_rank%>%  
select(-tot_flights20_AP1,
         -tot_flights20_AP2)%>%
  rename(tot_flights20_AP1 = tot_flights)

monthchange_rank <- left_join(monthchange_rank, all_flight19_AP, by = c("AP2" = "AP1" ))
monthchange_rank <- monthchange_rank%>%
  #select(-tot_flights19_AP1,
   #      -tot_flights19_AP2)%>%
  rename(tot_flights19_AP2 = tot_flights)

monthchange_rank <- left_join(monthchange_rank, all_flight20_AP, by = c("AP2" = "AP1" ))
monthchange_rank <- monthchange_rank%>%
  #select(-tot_flights20_AP1,
   #      -tot_flights20_AP2)%>%
  rename(tot_flights20_AP2 = tot_flights)
