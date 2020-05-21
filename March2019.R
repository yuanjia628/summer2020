library(tidyverse)
library(dplyr)

### Replace 2019 to get 2020
Mar <- read.csv("2019_03.csv")

od <- dplyr::select(Mar,ORIGIN,DEST,CANCELLED,DAY_OF_MONTH)
air <- read.csv("airport.csv")
names(air)[7] <- "lat"
names(air)[8] <- "long"
names(air)[2] <- "airportname"
air <- dplyr::select(air,GKA,lat,long,airportname)
air$ORIGIN <- air$GKA
air$DEST <- air$GKA



odpair <- merge(od,air,by="ORIGIN",all.x=T)
names(odpair)[2] <- "DEST"
odpair$DEST.y <- NULL
names(odpair)[6:8] <- c("lat_o","long_o","name_o")
odpair$GKA <- NULL
odpair <- merge(odpair,air,by="DEST",all.x=T)
odpair$GKA <- NULL
odpair$ORIGIN.y <- NULL
names(odpair)[2] <- "ORIGIN"
names(odpair)[8:10] <- c("lat_d","long_d","name_d")


odpair[odpair$ORIGIN=="EAR",]$lat_o <- 40.7251
odpair[odpair$ORIGIN=="EAR",]$long_o <- -99.0090
odpair[odpair$DEST=="EAR",]$lat_d <- 40.7251
odpair[odpair$DEST=="EAR",]$long_d <- -99.0090
odpair[odpair$DEST=="XWA",]$lat_d <- 48.2562
odpair[odpair$DEST=="XWA",]$long_d <- -103.7424
odpair[odpair$ORIGIN=="XWA",]$lat_o <- 48.2562
odpair[odpair$ORIGIN=="XWA",]$long_o <- -103.7424

odpairfly <- odpair %>% filter(CANCELLED==0)
odpaircancel <- odpair %>% filter(CANCELLED==1)


odpairfly$od <- (odpairfly$ORIGIN:odpairfly$DEST)
odpairfly$cnt <- 1

March2019 <- odpairfly %>% 
  group_by(od) %>% 
  summarise(flights=sum(cnt),lat_o=mean(lat_o),long_o=mean(long_o),lat_d=mean(lat_d),long_d=mean(long_d))

March2019 <- March2019[order(-March2019$flights),]


odpair_last10 <- odpairfly[odpairfly$DAY_OF_MONTH>20,]

March2019_last10 <- odpair_last10 %>% 
  group_by(od) %>% 
  summarise(flights=sum(cnt),lat_o=mean(lat_o),long_o=mean(long_o),lat_d=mean(lat_d),long_d=mean(long_d))

March2019_last10 <- March2019_last10[order(-March2019_last10$flights),]


##write.csv(March2019,"march2019.csv")
##write.csv(March2019_last10,"march2019_last10.csv")

##Replace 2019 to get 2020

