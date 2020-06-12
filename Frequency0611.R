#####LOAD PACKAGES#####
library(tidyverse)
library(ggplot2)
library(reshape2)
library(directlabels)
library(nnet)
library(viridis)
library()
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

#selected airports for flights frequency
frequency_AP <- c("SHV", "DFW","DAL","IAH","SAT","AUS","OKC",
                  "SEA","JFK","EWR","LGA")

#select necessary variables and get lat and lon for ORIGIN
mar2019 <- left_join(mkt2019, airports_metro, by = c("ORIGIN" = "code"))%>%
  dplyr::select(YEAR,
                DAY_OF_MONTH,
                DAY_OF_WEEK,
                MKT_CARRIER,
                OP_CARRIER,
                ORIGIN,
                DEST,
                CANCELLED,
                CANCELLATION_CODE,
                DISTANCE,
                lat,
                long)%>%
  filter(ORIGIN %ni% eliminated_AP,
         DEST %ni% eliminated_AP,
         MKT_CARRIER != "HA",
         CANCELLED == 0)%>%
  rename(CARRIER = MKT_CARRIER,
         LAT_O = lat,
         LON_O = long)
#get lat and lon for DEST
mar2019 <- left_join(mar2019, airports_metro, by = c("DEST" = "code"))%>%
  dplyr::select(YEAR,
                DAY_OF_MONTH,
                DAY_OF_WEEK,
                CARRIER,
                OP_CARRIER,
                ORIGIN,
                DEST,
                CANCELLED,
                CANCELLATION_CODE,
                DISTANCE,
                LAT_O,
                LON_O,
                lat,
                long)%>%
  rename(LAT_D = lat,
         LON_D = long)


#Routes info in 2019 with distance and counts
flight19 <- mar2019%>%
  filter(ORIGIN %in% frequency_AP)%>%
  group_by(ORIGIN,DEST, DAY_OF_MONTH, DISTANCE)%>%
  summarise(flight19 = n())
frequency19 <- flight19%>%
  spread(DAY_OF_MONTH,flight19)

#####2020#####
#select necessary variables and get lat and lon for ORIGIN
mar2020 <- left_join(mkt2020, airports_metro, by = c("ORIGIN" = "code"))%>%
  dplyr::select(YEAR,
                DAY_OF_MONTH,
                DAY_OF_WEEK,
                MKT_CARRIER,
                OP_CARRIER,
                ORIGIN,
                DEST,
                CANCELLED,
                CANCELLATION_CODE,
                DISTANCE,
                lat,
                long)%>%
  filter(ORIGIN %ni% eliminated_AP,
         DEST %ni% eliminated_AP,
         MKT_CARRIER != "HA",
         CANCELLED == 0)%>%
  rename(CARRIER = MKT_CARRIER,
         LAT_O = lat,
         LON_O = long)
#get lat and lon for DEST
mar2020 <- left_join(mar2020, airports_metro, by = c("DEST" = "code"))%>%
  dplyr::select(YEAR,
                DAY_OF_MONTH,
                DAY_OF_WEEK,
                CARRIER,
                OP_CARRIER,
                ORIGIN,
                DEST,
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
mar2020$OD <- paste(mar2020$ORIGIN,"-",mar2020$DEST)

#Routes info in 2019 with distance and counts
flight20 <- mar2020%>%
  filter(ORIGIN %in% frequency_AP)%>%
  group_by(ORIGIN,DEST, OD,DAY_OF_MONTH, DISTANCE)%>%
  summarise(flight20 = n())

#spread the data and generate daily columns
frequency20 <- flight20%>%
  spread(DAY_OF_MONTH,flight20)

#replace NA with 0
frequency20[is.na(frequency20)] <- 0

#generate aggregated data by weeks
frequency20$wk1 <- rowSums(frequency20[,c("1","2","3","4","5","6","7")])
frequency20$wk2 <- rowSums(frequency20[,c("8","9","10","11","12","13","14")])
frequency20$wk3 <- rowSums(frequency20[,c("15","16","17","18","19","20","21")])
frequency20$wk4 <- rowSums(frequency20[,c("22","23","24","25","26","27","28")])

#generate total distance
frequency20 <- frequency20%>%
  mutate(wk1_dist = DISTANCE * wk1,
         wk2_dist = DISTANCE * wk2,
         wk3_dist = DISTANCE * wk3,
         wk4_dist = DISTANCE * wk4)

#generate distance category
##Based on quantiles
frequency20$dist_quan <- ntile(frequency20$DISTANCE,5)

##Based on nmi
frequency20$dist_nmi[frequency20$DISTANCE <= 600] <- 1
frequency20$dist_nmi[frequency20$DISTANCE > 600 & frequency20$DISTANCE < 2531] <- 2
frequency20$dist_nmi[frequency20$DISTANCE >= 2531] <- 3

frequency_plot <- frequency20%>%gather(key = Week, value = Frequency, wk1:wk4)
frequency_dist <- frequency20%>%gather(key = Week, value = Total_Distance, wk1_dist:wk4_dist)

#weekly change
frequency20 <- frequency20%>%
  mutate(wk1_2 = (wk2-wk1)/wk1*100,
         wk2_3 = (wk3-wk2)/wk2*100,
         wk3_4 = (wk4-wk3)/wk3*100,
         wk1_3 = (wk3-wk1)/wk1*100,
         wk1_4 = (wk4-wk1)/wk1*100)

#plot
ggplot(frequency_plot%>% group_by(ORIGIN, Week)%>%summarise(FREQUENCY = sum(Frequency)), 
       aes(ORIGIN,FREQUENCY, fill= Week))+
  scale_fill_viridis(discrete = TRUE)+
  geom_bar(stat = "identity", position = "dodge")

ggplot(frequency_dist%>% group_by(ORIGIN, Total_Distance, Week)%>%summarise(DISTANCE = sum(Total_Distance)), 
       aes(ORIGIN,DISTANCE, fill= as.factor(Week)))+
  scale_fill_viridis(name = "Total Distance by Week",discrete = TRUE)+
  geom_bar(stat = "identity", position = "dodge")

ggplot(frequency_plot%>% group_by(ORIGIN, dist_quan, Week)%>%summarise(FREQUENCY = sum(Frequency)), 
       aes(ORIGIN,FREQUENCY, fill= as.factor(dist_quan)))+
  scale_fill_viridis(name = "Distance in Quantile",discrete = TRUE,  labels = c("74-427miles", "427-722miles", "722-987miles", "987-1345miles","1346-2724miles"))+
  facet_wrap(.~Week, nrow=2, ncol=2)+
  geom_bar(stat = "identity", position = "dodge")+
  theme(
    strip.text.x = element_text(size = 12,face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(face = "bold")
  )

ggplot(frequency_plot%>% group_by(ORIGIN, dist_nmi, Week)%>%summarise(FREQUENCY = sum(Frequency)), 
       aes(ORIGIN,FREQUENCY, fill= as.factor(dist_nmi)))+
  scale_fill_viridis(name = "Flights by Short-/\nMedian-/Long-Haul",discrete = TRUE,labels = c("74-599miles", "599-2521miles", "2521-2724miles"))+
  facet_wrap(.~Week, nrow=2, ncol=2)+
  geom_bar(stat = "identity", position = "dodge")+
  theme(
    strip.text.x = element_text(size = 12,face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(face = "bold")
  )


#IAH - DRO wk3_4 inf ???how to deal with inf
frequency20$wk3_4[is.infinite(frequency20$wk3_4)] <- 0

#generate data for % of weekly frequency change
frequency_chg <- frequency20%>%
  group_by(ORIGIN, dist_quan)%>%
  summarise(wk1 = sum(wk1),
            wk2 = sum(wk2),
            wk3 = sum(wk3),
            wk4 = sum(wk4))%>%
  mutate(Wk1_2 = (wk2-wk1)/wk1*100,
         Wk2_3 = (wk3-wk2)/wk2*100, 
         Wk3_4 = (wk4-wk3)/wk3*100)%>%
  gather(key = Week, value = CHANGE, Wk1_2:Wk3_4)

frequency_chg2 <- frequency20%>%
  group_by(ORIGIN, dist_quan)%>%
  summarise(wk1 = sum(wk1),
            wk2 = sum(wk2),
            wk3 = sum(wk3),
            wk4 = sum(wk4))%>%
  mutate(Wk1_2 = (wk2-wk1)/wk1*100,
         Wk1_3 = (wk3-wk1)/wk1*100,
         Wk1_4 = (wk4-wk1)/wk1*100)%>%
  gather(key = Week, value = CHANGE, Wk1_2:Wk1_4)

frequency_chg_nmi <- frequency20%>%
  group_by(ORIGIN, dist_nmi)%>%
  summarise(wk1 = sum(wk1),
            wk2 = sum(wk2),
            wk3 = sum(wk3),
            wk4 = sum(wk4))%>%
  mutate(Wk1_2 = (wk2-wk1)/wk1*100,
         Wk2_3 = (wk3-wk2)/wk2*100, 
         Wk3_4 = (wk4-wk3)/wk3*100)%>%
  gather(key = Week, value = CHANGE, Wk1_2:Wk3_4)

frequency_chg_nmi2 <- frequency20%>%
  group_by(ORIGIN, dist_nmi)%>%
  summarise(wk1 = sum(wk1),
            wk2 = sum(wk2),
            wk3 = sum(wk3),
            wk4 = sum(wk4))%>%
  mutate(Wk1_2 = (wk2-wk1)/wk1*100,
         Wk1_3 = (wk3-wk1)/wk1*100,
         Wk1_4 = (wk4-wk1)/wk1*100)%>%
  gather(key = Week, value = CHANGE, Wk1_2:Wk1_4)


#change facet_wrap label
quan.labs <- c("74-427miles", "427-722miles", "722-987miles", "987-1345miles","1346-2724miles")
names(quan.labs) <- c("1", "2", "3", "4", "5")

nmi.labs <- c("74-599miles", "599-2521miles", "2521-2724miles")
names(nmi.labs) <- c("1", "2", "3")


#Plot % of weekly frequency change
ggplot(frequency_chg, 
       aes(ORIGIN,CHANGE, fill = as.factor(Week)))+
  scale_fill_viridis(name = "% of Frequency Change",discrete = TRUE)+
  facet_wrap(.~dist_quan, labeller = labeller(dist_quan = quan.labs))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(
    strip.text.x = element_text(size = 12,face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(face = "bold")
  )

ggplot(frequency_chg2, 
       aes(ORIGIN,CHANGE, fill = as.factor(Week)))+
  scale_fill_viridis(name = "% of Frequency Change",discrete = TRUE)+
  facet_wrap(.~dist_quan, labeller = labeller(dist_quan = quan.labs))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(
    strip.text.x = element_text(size = 12,face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(face = "bold")
  )

ggplot(frequency_chg_nmi, 
       aes(ORIGIN,CHANGE, fill = as.factor(Week)))+
  scale_fill_viridis(name = "% of Frequency Change",discrete = TRUE)+
  facet_wrap(.~dist_nmi, labeller = labeller(dist_nmi = nmi.labs))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(
    strip.text.x = element_text(size = 12,face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(face = "bold")
  )

ggplot(frequency_chg_nmi2, 
       aes(ORIGIN,CHANGE, fill = as.factor(Week)))+
  scale_fill_viridis(name = "% of Frequency Change",discrete = TRUE)+
  facet_wrap(.~dist_nmi, labeller = labeller(dist_nmi = nmi.labs))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(
    strip.text.x = element_text(size = 12,face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(face = "bold")
  )

IAH <- frequency20%>%
  filter(ORIGIN == "IAH" & dist_quan == 3)

DAL <- frequency20%>%
  filter(ORIGIN == "DAL")

AUS <- frequency20%>%
  filter(ORIGIN == "AUS" )
