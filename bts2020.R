#####LOAD PACKAGES#####
library(tidyverse)
library(ggplot2)
library(reshape2)
library(directlabels)

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
airports <- read.csv("D:/aviation2020/usairports.csv")
airports_metro <- read.csv("D:/aviation2020/airport_metro.csv")
#BTS data
mar2019 <- read.csv("D:/aviation2020/032019.csv")
mar2020 <- read.csv("D:/aviation2020/032020.csv")
all <- read.csv("D:/aviation2020/od_with_na.csv")
mkt2019 <- read.csv("D:/aviation2020/032019_new.csv")
mkt2020 <- read.csv("D:/aviation2020/032020_new.csv")
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

Wk1_19 <- mar2019%>%
  filter(DAY_OF_MONTH == 3 | DAY_OF_MONTH == 4 | DAY_OF_MONTH == 5 | DAY_OF_MONTH == 6 | DAY_OF_MONTH == 7 | DAY_OF_MONTH == 8 | DAY_OF_MONTH == 9)

Wk2_19 <- mar2019%>%
  filter(DAY_OF_MONTH == 10 | DAY_OF_MONTH == 11 | DAY_OF_MONTH == 12 | DAY_OF_MONTH == 13 | DAY_OF_MONTH == 14 | DAY_OF_MONTH == 15 | DAY_OF_MONTH == 16)

Wk3_19 <- mar2019%>%
  filter(DAY_OF_MONTH == 17 | DAY_OF_MONTH == 18 | DAY_OF_MONTH == 19 | DAY_OF_MONTH == 20 | DAY_OF_MONTH == 21 | DAY_OF_MONTH == 22 | DAY_OF_MONTH == 23)

Wk4_19 <- mar2019%>%
  filter(DAY_OF_MONTH == 24 | DAY_OF_MONTH == 25 | DAY_OF_MONTH == 26 | DAY_OF_MONTH == 27 | DAY_OF_MONTH == 28 | DAY_OF_MONTH == 29 | DAY_OF_MONTH == 30)

  
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

write.csv(airlineonly201903, "D:/aviation2020/airline2019.csv")

######2019 weekly data by airlines#####
wk1_19_airline <- Wk1_19%>%
  group_by(CARRIER)%>%
  summarise(n=n())%>%
  rename(wk1_19 = n)

wk2_19_airline <- Wk2_19%>%
  group_by(CARRIER)%>%
  summarise(n=n())%>%
  rename(wk2_19 = n)

wk3_19_airline <- Wk3_19%>%
  group_by(CARRIER)%>%
  summarise(n=n())%>%
  rename(wk3_19 = n)

wk4_19_airline <- Wk4_19%>%
  group_by(CARRIER)%>%
  summarise(n=n())%>%
  rename(wk4_19 = n)

weekly_19_airline <- left_join(weekly_19_airline, wk4_19_airline, by = "CARRIER")
                               
#plot
weekly_19_airline_long <- melt(weekly_19_airline, id.vars = "CARRIER")
ggplot(data = weekly_19_airline_long, aes(x= variable, y = value, group = CARRIER, color = CARRIER))+
  geom_line()+
  ylab("Number of Flights")+
  xlab("")+
  scale_colour_discrete(guide = 'none') +
  scale_x_discrete(expand=c(0, 1)) +
  geom_dl(aes(label = CARRIER), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  plotTheme()

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

Wk1_20 <- mar2020%>%
  filter(DAY_OF_MONTH == 1 | DAY_OF_MONTH == 2 | DAY_OF_MONTH == 3 | DAY_OF_MONTH == 4 | DAY_OF_MONTH == 5 | DAY_OF_MONTH == 6 | DAY_OF_MONTH == 7)

Wk2_20 <- mar2020%>%
  filter(DAY_OF_MONTH == 8 | DAY_OF_MONTH == 9 | DAY_OF_MONTH == 10 | DAY_OF_MONTH == 11 | DAY_OF_MONTH == 12 | DAY_OF_MONTH == 13 | DAY_OF_MONTH == 14)

Wk3_20 <- mar2020%>%
  filter(DAY_OF_MONTH == 15 | DAY_OF_MONTH == 16 | DAY_OF_MONTH == 17 | DAY_OF_MONTH == 18 | DAY_OF_MONTH == 19 | DAY_OF_MONTH == 20 | DAY_OF_MONTH == 21)

Wk4_20 <- mar2020%>%
  filter(DAY_OF_MONTH == 22 | DAY_OF_MONTH == 23 | DAY_OF_MONTH == 24 | DAY_OF_MONTH == 25 | DAY_OF_MONTH == 26 | DAY_OF_MONTH == 27 | DAY_OF_MONTH == 28)

######2020 weekly data by airlines#####
wk1_20_airline <- Wk1_20%>%
  group_by(CARRIER)%>%
  summarise(n=n())%>%
  rename(wk1_20 = n)

wk2_20_airline <- Wk2_20%>%
  group_by(CARRIER)%>%
  summarise(n=n())%>%
  rename(wk2_20 = n)

wk3_20_airline <- Wk3_20%>%
  group_by(CARRIER)%>%
  summarise(n=n())%>%
  rename(wk3_20 = n)

wk4_20_airline <- Wk4_20%>%
  group_by(CARRIER)%>%
  summarise(n=n())%>%
  rename(wk4_20 = n)

weekly_20_airline <- left_join(weekly_20_airline, wk4_20_airline, by = "CARRIER")


#plot
weekly_20_airline_long <- melt(weekly_20_airline, id.vars = "CARRIER")
ggplot(data = weekly_20_airline_long, aes(x= variable, y = value, group = CARRIER, color = CARRIER))+
  geom_line()+
  ylab("Number of Flights")+
  xlab("")+
  scale_colour_discrete(guide = 'none') +
  scale_x_discrete(expand=c(0, 1)) +
  geom_dl(aes(label = CARRIER), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8))+
  plotTheme()


######combine 2019 and 2020 weekly data#####
weekly <- merge(weekly_19_airline, weekly_20_airline, by = "CARRIER")

weekly_chg <- weekly%>%
  mutate(wk1_chg = wk1_20 - wk1_19,
         wk2_chg = wk2_20 - wk2_19,
         wk3_chg = wk3_20 - wk3_19,
         wk4_chg = wk4_20 - wk4_19)%>%
  select(CARRIER,
         wk1_chg,
         wk2_chg,
         wk3_chg,
         wk4_chg)

weekly_pct_chg <- weekly%>%
  mutate(wk1_chg = wk1_20 - wk1_19,
         wk2_chg = wk2_20 - wk2_19,
         wk3_chg = wk3_20 - wk3_19,
         wk4_chg = wk4_20 - wk4_19,
         wk1_pct_chg = wk1_chg / wk1_19,
         wk2_pct_chg = wk2_chg / wk2_19,
         wk3_pct_chg = wk3_chg / wk3_19,
         wk4_pct_chg = wk4_chg / wk4_19)%>%
  select(CARRIER,
         wk1_pct_chg,
         wk2_pct_chg,
         wk3_pct_chg,
         wk4_pct_chg)

#plot
weekly_chg_long <- melt(weekly_chg, id.vars = "CARRIER")
ggplot(data = weekly_chg_long, aes(x= variable, y = value, group = CARRIER, color = CARRIER))+
  geom_line()+
  ylab("Number of Flights")+
  xlab("")+
  scale_colour_discrete(guide = 'none') +
  scale_x_discrete(expand=c(0, 1)) +
  geom_dl(aes(label = CARRIER), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8))+
  plotTheme()

weekly_pct_chg_long <- melt(weekly_pct_chg, id.vars = "CARRIER")
ggplot(data = weekly_pct_chg_long, aes(x= variable, y = value, group = CARRIER, color = CARRIER))+
  geom_line()+
  ylab("Pct of Change")+
  xlab("")+
  scale_colour_discrete(guide = 'none') +
  scale_x_discrete(expand=c(0, 1)) +
  geom_dl(aes(label = CARRIER), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8))+
  plotTheme()

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

write.csv(airlineonly202003, "D:/aviation2020/airline2020.csv")

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

ggplot(mar_airline_pct_des)+
  geom_bar(aes(x = CARRIER, y = pct_change), stat = "identity")+
  plotTheme()
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

write.csv(monthchange_rank, "D:/aviation2020/monthly.csv")
#####DISAPPEARED ROUTES#####
disappeared <- all%>%
  filter(is.na(flights20))

barplot(disappeared$distance20)
barplot(disappeared$flights19)

disappeared <- disappeared%>%
  select(OD,flights19,distance20)%>%
  mutate(ODpairs = OD)%>%
  separate(OD, c('AP1', 'AP2'), sep = '\\s*-\\s*')

disappeared <- left_join(disappeared, airports, by = c("AP1" = "IATA"))%>%
  select(AP1, AP2, flights19, distance20, ODpairs, LAT, LON)%>%
  rename(LAT_1 = LAT,
         LON_1 = LON)

disappeared <- left_join(disappeared, airports, by = c("AP2" = "IATA"))%>%
  select(AP1, AP2, flights19, distance20, ODpairs, LAT_1, LON_1, LAT, LON)%>%
  rename(LAT_2 = LAT,
         LON_2 = LON)
install.packages("maps")
library(maps)
#create basemap
US <- map("world", fill=T, col="grey8", bg="grey15", lwd = 0.05, ylim=c(12.039321, 71.856229), xlim=c(-171.738281, -56.601563))

xlim <- c(-171.738281, -56.601563)
ylim <- c(12.039321, 71.856229)
map("world", col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05, xlim=xlim, ylim=ylim)
#overlay airports
usairports <- filter(airports, County == "United States")

points(airports$long,airports$lat, pch=3, cex=0.1, col="chocolate1")

#map disappeared routes
for (i in (1:dim(disappeared)[1])) { 
  inter <- gcIntermediate(c(disappeared$LON_1[i], disappeared$LAT_1[i]), c(disappeared$LON_2[i], disappeared$LAT_2[i]))
  lines(inter, lwd=0.08, col="turquoise2")    
}

#####NEW ROUTES#####
new <- all%>%
  filter(is.na(flights19))

barplot(new$distance20)
barplot(new$flights20)

new <- new%>%
  select(OD,flights20,distance20)%>%
  mutate(ODpairs = OD)%>%
  separate(OD, c('AP1', 'AP2'), sep = '\\s*-\\s*')

new <- left_join(new, airports, by = c("AP1" = "IATA"))%>%
  select(AP1, AP2, flights20, distance20, ODpairs, LAT, LON)%>%
  rename(LAT_1 = LAT,
         LON_1 = LON)

new <- left_join(new, airports, by = c("AP2" = "IATA"))%>%
  select(AP1, AP2, flights20, distance20, ODpairs, LAT_1, LON_1, LAT, LON)%>%
  rename(LAT_2 = LAT,
         LON_2 = LON)

newAirports <- new%>%
  group_by(AP1)%>%
  summarise(n = n())

newAirports1 <- new%>%
  group_by(AP2)%>%
  summarise(n=n())

newAirports <- merge(newAirports, newAirports1, by.x = "AP1", by.y = "AP2", all = T)

newAirports <- newAirports%>%
  replace(is.na(.),0)%>%
  mutate(n = n.x + n.y)

write.csv(newAirports, "D:/aviation2020/new.csv")

disappearedAirports <- disappeared%>%
  group_by(AP1)%>%
  summarise(n = n())

disappearedAirports1 <- disappeared%>%
  group_by(AP2)%>%
  summarise(n = n())

disappearedAirports <- merge(disappearedAirports, disappearedAirports1, by.x = "AP1", by.y = "AP2", all = T)

disappearedAirports <- disappearedAirports%>%
  replace(is.na(.),0)%>%
  mutate(n = n.x + n.y)

write.csv(disappearedAirports, "D:/aviation2020/disappeared.csv")
#map disappeared routes

for (i in (1:dim(new)[1])) { 
  inter <- gcIntermediate(c(new$LON_1[i], new$LAT_1[i]), c(new$LON_2[i], new$LAT_2[i]))
  lines(inter, lwd=0.1, col="turquoise2")    
}

new[new$AP1=="EAR",]$LAT_1 <- 40.7251
new[new$AP1=="EAR",]$LON_1 <- -99.0090
new[new$AP2=="XWA",]$LAT_2 <- 48.2562
new[new$AP2=="XWA",]$LON_2 <- -103.7424

US <- get_map("United States", zoom = 3)
ggmap(US)

#####metro and merge variables#####
ap_metro <- airports_metro%>%
  filter(!is.na(Metroname))


