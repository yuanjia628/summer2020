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

#merge
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

mar_sample <- head(mar_des,5)
dput <- dput(mar_sample)
