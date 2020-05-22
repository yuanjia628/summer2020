#####LOAD PACKAGES#####
library(tidyverse)

######LOAD DATA#####
#airports data
airports <- read.csv("D:/aviation2020/airports.csv")
#BTS data
mar2019 <- read.csv("D:/aviation2020/032019.csv")
mar2020 <- read.csv("D:/aviation2020/032020.csv")


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
  rename(CARRIER = OP_UNIQUE_CARRIER)%>%
  filter(CANCELLED == 0)
  
mar2019$OD <- paste(mar2019$ORIGIN,"-", mar2019$DEST)

#monthly data by airline and OD
airline201903 <- mar2019%>%
  group_by(YEAR,
           CARRIER,
           OD,
           LAT,
           LON)%>%
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
  rename(CARRIER = OP_UNIQUE_CARRIER)%>%
  filter(CANCELLED == 0)

mar2020$OD <- paste(mar2020$ORIGIN,"-", mar2020$DEST)

#monthly data by airline and OD
airline202003 <- mar2020%>%
  group_by(YEAR,
           CARRIER,
           OD,
           LAT,
           LON)%>%
  summarise(n=n())

#merge
mar <- merge(airline201903, airline202003, by.x = c("CARRIER", "OD"), by.y = c("CARRIER","OD"), all = T)

mar$YEAR.x[is.na(mar$YEAR.x)] <- 2019
mar$YEAR.y[is.na(mar$YEAR.y)] <- 2020
mar$n.x[is.na(mar$n.x)] <- 0
mar$n.y[is.na(mar$n.y)] <- 0
