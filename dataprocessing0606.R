#####LOAD PACKAGES#####
library(tidyverse)
library(ggplot2)
library(reshape2)
library(directlabels)
library(nnet)

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

#"not include" function
'%ni%' <- Negate('%in%')

######LOAD DATA#####
#airports and metro data
airports_metro <- read.csv("airport_metro.csv") %>% 
  dplyr::select(-X,-(flights19:rankchange))
  
#regions <- read.csv("regions.csv")
categories <- read.csv("airportsize2019.csv")

#BTS data <- *****INSERT RAW DATA HERE*****
mkt2019 <- read.csv("032019_new.csv")
mkt2020 <- read.csv("032020_new.csv")

#ASPM data for historic operations data
ops2019 <- read.csv("D:/aviation2020/032019_Ops.csv")
ops2020 <- read.csv("D:/aviation2020/032020_Ops.csv")

#HI,PR,AS airports
eliminated_AP <- c("ADQ", "AKN", "ANC", "BET", "BQN", "ENA", "FAI",
                   "HNL", "ITO", "JNU", "JRF", 'KOA', "LIH", "MKK",
                   "MRI", "OGG", "SIG", "SJU","PSE")


#####2019 data cleaning#####
#as.character THIS IS NOT NEEDED!
mkt2019$ORIGIN <- as.character(mkt2019$ORIGIN)
mkt2019$DEST <- as.character(mkt2019$DEST)

#Flip the order of Orgins and Destinations based on alphabetical order
mar2019 <- mkt2019%>%
  mutate(AP1 = ifelse(pmin(as.character(ORIGIN),as.character(DEST)) == ORIGIN, ORIGIN, DEST),
         AP2 = ifelse(pmax(as.character(ORIGIN),as.character(DEST)) == ORIGIN, ORIGIN, DEST))
  
#select necessary variables and get lat and lon for AP1
mar2019 <- left_join(mar2019, airports_metro, by = c("AP1" = "code"))%>%
  dplyr::select(YEAR,
         DAY_OF_MONTH,
         DAY_OF_WEEK,
         MKT_CARRIER,
         OP_CARRIER,
         AP1,
         AP2,
         CANCELLED,
         CANCELLATION_CODE,
         DISTANCE,
         lat,
         long)%>%
  filter(AP1 %ni% eliminated_AP,
         AP2 %ni% eliminated_AP,
         MKT_CARRIER != "HA",
         CANCELLED == 0)%>%
  rename(CARRIER = MKT_CARRIER,
         LAT_O = lat,
         LON_O = long)

#get lat and lon for AP2
mar2019 <- left_join(mar2019, airports_metro, by = c("AP2" = "code"))%>%
  dplyr::select(YEAR,
         DAY_OF_MONTH,
         DAY_OF_WEEK,
         CARRIER,
         OP_CARRIER,
         AP1,
         AP2,
         CANCELLED,
         CANCELLATION_CODE,
         DISTANCE,
         LAT_O,
         LON_O,
         lat,
         long)%>%
  rename(LAT_D = lat,
         LON_D = long)

#create OD pair column
mar2019$OD <- paste(mar2019$AP1,"-",mar2019$AP2)

#Routes info in 2019 with distance and counts
flight19 <- mar2019%>%
  group_by(OD, AP1, AP2, DISTANCE)%>%
  summarise(flight19 = n())

######2020 data cleaning#####
#as.character
mkt2020$ORIGIN <- as.character(mkt2020$ORIGIN)
mkt2020$DEST <- as.character(mkt2020$DEST)

#Flip the order of Orgins and Destinations based on alphabetical order
mar2020 <- mkt2020%>%
  mutate(AP1 = ifelse(pmin(as.character(ORIGIN),as.character(DEST)) == ORIGIN, ORIGIN, DEST),
         AP2 = ifelse(pmax(as.character(ORIGIN),as.character(DEST)) == ORIGIN, ORIGIN, DEST))

#select necesarry variables and get lat and lon for AP1
mar2020 <- left_join(mar2020, airports_metro, by = c("ORIGIN" = "code"))%>%
  dplyr::select(YEAR,
         DAY_OF_MONTH,
         DAY_OF_WEEK,
         MKT_CARRIER,
         OP_CARRIER,
         AP1,
         AP2,
         CANCELLED,
         CANCELLATION_CODE,
         DISTANCE,
         lat,
         long)%>%
  filter(AP1 %ni% eliminated_AP,
         AP2 %ni% eliminated_AP,
         MKT_CARRIER != "HA",
         CANCELLED == 0)%>%
  rename(CARRIER = MKT_CARRIER,
         LAT_O = lat,
         LON_O = long)

#get lat and lon for AP2
mar2020 <- left_join(mar2020, airports_metro, by = c("AP2" = "code"))%>%
  dplyr::select(YEAR,
         DAY_OF_MONTH,
         DAY_OF_WEEK,
         CARRIER,
         OP_CARRIER,
         AP1,
         AP2,
         CANCELLED,
         CANCELLATION_CODE,
         DISTANCE,
         LAT_O,
         LON_O,
         lat,
         long)%>%
  rename(LAT_D = lat,
         LON_D = long)

#create OD pair column
mar2020$OD <- paste(mar2020$AP1,"-",mar2020$AP2)

#Routes info in 2020 with distance and counts
flight20 <- mar2020%>%
  group_by(OD, AP1, AP2, DISTANCE)%>%
  summarise(flight20 = n())

#merge 2019 and 2020 routes info
RTdata <- merge(flight19, flight20, by = "OD", all = TRUE)%>%
  dplyr::select(OD, flight19, flight20)


#summarize weekly 2019 by routes
wk1_19_od <- mar2019%>%
  filter(DAY_OF_MONTH >= 3 & DAY_OF_MONTH <= 9) %>% 
  group_by(OD)%>%
  summarise(wk1_19=n())

wk2_19_od <- mar2019%>%
  filter(DAY_OF_MONTH >= 10 & DAY_OF_MONTH <= 16) %>% 
  group_by(OD)%>%
  summarise(wk2_19=n())

wk3_19_od <- mar2019%>%
  filter(DAY_OF_MONTH >= 17 & DAY_OF_MONTH <= 23) %>% 
  group_by(OD)%>%
  summarise(wk3_19=n())

wk4_19_od <- mar2019%>%
  filter(DAY_OF_MONTH >= 24 & DAY_OF_MONTH <= 30) %>% 
  group_by(OD)%>%
  summarise(wk4_19=n())


#summarize weekly 2020 by routes
wk1_20_od <- mar2020%>%
  filter(DAY_OF_MONTH >= 1 & DAY_OF_MONTH <= 7) %>% 
  group_by(OD)%>%
  summarise(wk1_20=n())

wk2_20_od <- mar2020%>%
  filter(DAY_OF_MONTH >= 8 & DAY_OF_MONTH <= 14) %>% 
  group_by(OD)%>%
  summarise(wk2_20=n())

wk3_20_od <- mar2020%>%
  filter(DAY_OF_MONTH >= 15 & DAY_OF_MONTH <= 21) %>% 
  group_by(OD)%>%
  summarise(wk3_20=n())

wk4_20_od <- mar2020%>%
  filter(DAY_OF_MONTH >= 22 & DAY_OF_MONTH <= 28) %>% 
  group_by(OD)%>%
  summarise(wk4_20=n())

#merge weekly data to RTdata
RTdata <- merge(RTdata, wk1_19_od, by = "OD", all = T)
RTdata <- merge(RTdata, wk1_20_od, by = "OD", all = T)
RTdata <- merge(RTdata, wk2_19_od, by = "OD", all = T)
RTdata <- merge(RTdata, wk2_20_od, by = "OD", all = T)
RTdata <- merge(RTdata, wk3_19_od, by = "OD", all = T)
RTdata <- merge(RTdata, wk3_20_od, by = "OD", all = T)
RTdata <- merge(RTdata, wk4_19_od, by = "OD", all = T)
RTdata <- merge(RTdata, wk4_20_od, by = "OD", all = T)

#turn NA into 0s
RTdata[is.na(RTdata)] <- 0

#Not to calculate change here, 0,Inf and NaN issue
# RTdata <- RTdata%>%
#   filter(flight19 != 0)%>%
#   mutate(monthly_chg = (flight20 - flight19)/flight19*100,
#          wk1_chg = (wk1_20 - wk1_19)/wk1_19*100,
#          wk2_chg = (wk2_20 - wk2_19)/wk2_19*100,
#          wk3_chg = (wk3_20 - wk3_19)/wk3_19*100,
#          wk4_chg = (wk4_20 - wk4_19)/wk4_19*100)

#break OD data into single airport data
RTdata <- RTdata%>%
  mutate(ODpairs = OD)%>%
  separate(OD, c('AP1', 'AP2'), sep = '\\s*-\\s*')

AP1_tmp <- RTdata%>%
  group_by(AP1)%>%
  summarise(flight19 = sum(flight19),
            flight20 = sum(flight20),
            wk1_19 = sum(wk1_19),
            wk2_19 = sum(wk2_19),
            wk3_19 = sum(wk3_19),
            wk4_19 = sum(wk4_19),
            wk1_20 = sum(wk1_20),
            wk2_20 = sum(wk2_20),
            wk3_20 = sum(wk3_20),
            wk4_20 = sum(wk4_20))

AP2_tmp <- RTdata%>%
  group_by(AP2)%>%
  summarise(flight19 = sum(flight19),
            flight20 = sum(flight20),
            wk1_19 = sum(wk1_19),
            wk2_19 = sum(wk2_19),
            wk3_19 = sum(wk3_19),
            wk4_19 = sum(wk4_19),
            wk1_20 = sum(wk1_20),
            wk2_20 = sum(wk2_20),
            wk3_20 = sum(wk3_20),
            wk4_20 = sum(wk4_20))

APdata <- merge(AP1_tmp, AP2_tmp, by.x = "AP1", by.y = "AP2", all = TRUE)
APdata[is.na(APdata)] <- 0

#calculate the final data using airport as the unit of analysis
APdata <- APdata%>%
  mutate(flight19 = flight19.x + flight19.y,
         flight20 = flight20.x + flight20.y,
         wk1_19 = wk1_19.x + wk1_19.y,
         wk2_19 = wk2_19.x + wk2_19.y,
         wk3_19 = wk3_19.x + wk3_19.y,
         wk4_19 = wk4_19.x + wk4_19.y,
         wk1_20 = wk1_20.x + wk1_20.y,
         wk2_20 = wk2_20.x + wk2_20.y,
         wk3_20 = wk3_20.x + wk3_20.y,
         wk4_20 = wk4_20.x + wk4_20.y)%>%
  dplyr::select(AP1,
         flight19,
         flight20,
         wk1_19,
         wk2_19,
         wk3_19,
         wk4_19,
         wk1_20,
         wk2_20,
         wk3_20,
         wk4_20)%>%
  rename(code = AP1)

APdata <- APdata%>%
  mutate(rank19 = rank(-flight19, ties.method = "min"),
         rank20 = rank(-flight20, ties.method = "min"))

# monthly_chg = (flight20 - flight19)/flight19*100,
# wk1_chg = (wk1_20 - wk1_19)/wk1_19*100,
# wk2_chg = (wk2_20 - wk2_19)/wk2_19*100,
# wk3_chg = (wk3_20 - wk3_19)/wk3_19*100,
# wk4_chg = (wk4_20 - wk4_19)/wk4_19*100,
# rank_chg = rank20 - rank19


######hubs and multi_ap#####
APdata <- APdata%>%
  mutate(hub = ifelse(code %in% c("ANC","ATL","BOS","CLT","CVG","DCA","DEN","DFW","DTW","EWR","IAD","IAH","JFK","LAX",
                                  "LGA","MIA","MSP","ORD","PDX","PHL","PHX","SEA","SFO","SLC"),1,0),
         multi_ap = ifelse(code %in% c("JFK","EWR","LGA","LAX","BUR","LGB","ONT","SNA","PSP","DCA","IAD",
                                       "BWI","HGR","CHO","MDT","IAH","HOU","ORD","MDW","RFD","BMI","SBN",
                                       "GYY","FWA","DAL","DFW"),1,0))
#####metro#####
#delete airports not in metro areas and airports not in mainland
ap_metro <- airports_metro%>%
  filter(!is.na(Metroname),
         code %ni% eliminated_AP)

APdata <- left_join(ap_metro, APdata, by = "code")


######categories#####
APdata <- left_join(APdata, categories, by = "code")


#####Total Ops 2019#####
APdata <- left_join(APdata, ops2019, by = "code")


##Calculate degree of nodes
DNdata <- RTdata
DNdata[,3:12]<- ifelse(DNdata[,3:12]>0,1,0)



DN1_tmp <- DNdata%>%
  group_by(AP1)%>%
  summarise(flight19 = sum(flight19),
            flight20 = sum(flight20),
            wk1_19 = sum(wk1_19),
            wk2_19 = sum(wk2_19),
            wk3_19 = sum(wk3_19),
            wk4_19 = sum(wk4_19),
            wk1_20 = sum(wk1_20),
            wk2_20 = sum(wk2_20),
            wk3_20 = sum(wk3_20),
            wk4_20 = sum(wk4_20))

DN2_tmp <- DNdata%>%
  group_by(AP2)%>%
  summarise(flight19 = sum(flight19),
            flight20 = sum(flight20),
            wk1_19 = sum(wk1_19),
            wk2_19 = sum(wk2_19),
            wk3_19 = sum(wk3_19),
            wk4_19 = sum(wk4_19),
            wk1_20 = sum(wk1_20),
            wk2_20 = sum(wk2_20),
            wk3_20 = sum(wk3_20),
            wk4_20 = sum(wk4_20))

DNdata <- merge(DN1_tmp, DN2_tmp, by.x = "AP1", by.y = "AP2", all = TRUE)
DNdata[is.na(DNdata)] <- 0
DNdata <- DNdata%>%
  mutate(flight19 = flight19.x + flight19.y,
         flight20 = flight20.x + flight20.y,
         wk1_19 = wk1_19.x + wk1_19.y,
         wk2_19 = wk2_19.x + wk2_19.y,
         wk3_19 = wk3_19.x + wk3_19.y,
         wk4_19 = wk4_19.x + wk4_19.y,
         wk1_20 = wk1_20.x + wk1_20.y,
         wk2_20 = wk2_20.x + wk2_20.y,
         wk3_20 = wk3_20.x + wk3_20.y,
         wk4_20 = wk4_20.x + wk4_20.y)%>%
  dplyr::select(AP1,
                flight19,
                flight20,
                wk1_19,
                wk2_19,
                wk3_19,
                wk4_19,
                wk1_20,
                wk2_20,
                wk3_20,
                wk4_20)%>%
  rename(code = AP1,
         degree19=flight19,
         degree20=flight20)


######hubs and multi_ap#####
DNdata <- DNdata%>%
  mutate(hub = ifelse(code %in% c("ANC","ATL","BOS","CLT","CVG","DCA","DEN","DFW","DTW","EWR","IAD","IAH","JFK","LAX",
                                  "LGA","MIA","MSP","ORD","PDX","PHL","PHX","SEA","SFO","SLC"),1,0),
         multi_ap = ifelse(code %in% c("JFK","EWR","LGA","LAX","BUR","LGB","ONT","SNA","PSP","DCA","IAD",
                                       "BWI","HGR","CHO","MDT","IAH","HOU","ORD","MDW","RFD","BMI","SBN",
                                       "GYY","FWA","DAL","DFW"),1,0))
#####metro#####


DNdata <- left_join(ap_metro, DNdata, by = "code")


######categories#####
DNdata <- left_join(DNdata, categories, by = "code")

#####Total Ops 2019#####
DNdata <- left_join(DNdata, ops2019, by = "code")


