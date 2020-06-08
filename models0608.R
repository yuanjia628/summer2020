library(dplyr)
library(tidyr)
library(tidyverse)
library(corrplot)
library(foreign)
library(ggplot2)
library(MASS)
library(Hmisc)
library(reshape2)
library(caret)
library(nnet)


load("Dataprocess.RData")

#Here we are going to run 4 models for different dependant variables of both APdata and DNdata: 
# wk4 change between March 2019 to 2020 wk4_19_20 and its binary wk4_19_20_40pct or wk4_19_20_q4
# wk1 to wk4 change in March 2020 wk1to4_20 and its binary wk1to4_20_40pct or wk1to4_20_q4
# wk1 to wk2,wk3 change same as above.


#Please filter certain variables before regression!!!!
#APdata %>% APdata %>% filter(size=="nonhub"!size=="small")
#APdata %>% APdata %>% filter(size=="large"!size=="medium")


#Generate variables
APdata <- APdata %>% 
  mutate(wk4_19_20=(wk4_20/wk4_19-1)*100,
         wk4_19_20_40pct=cut(wk4_19_20,breaks = quantile(wk4_19_20, probs=c(0,0.4,1), na.rm=TRUE)
                             ,labels = 1:0,include.lowest=T),
         wk4_19_20_q4=as.factor(ntile(wk4_19_20,4)),
         
         wk1to4_20=(wk4_20/wk1_20-1)*100,
         wk1to4_20_40pct=cut(wk1to4_20,breaks = quantile(wk1to4_20, probs=c(0,0.4,1), na.rm=TRUE)
                              ,labels = 1:0,include.lowest=T),
         wk1to4_20_q4=as.factor(ntile(wk1to4_20,4)),
         
         wk1to3_20=(wk3_20/wk1_20-1)*100,
         wk1to3_20_40pct=cut(wk1to3_20,breaks = quantile(wk1to3_20, probs=c(0,0.4,1), na.rm=TRUE)
                             ,labels = 1:0,include.lowest=T),
         wk1to3_20_q4=as.factor(ntile(wk1to3_20,4)),
         
         wk1to2_20=(wk2_20/wk1_20-1)*100,
         wk1to2_20_40pct=cut(wk1to2_20,breaks = quantile(wk1to2_20, probs=c(0,0.4,1), na.rm=TRUE)
                             ,labels = 1:0,include.lowest=T),
         wk1to2_20_q4=as.factor(ntile(wk1to2_20,4))
         )

APdata$veh0_pct <- APdata$veh0_hh/APdata$hh

#Scale the numeric independent varaibles
#Should we scale dependent varabiles too?
APdata[,c(5:26,30,31,34,37,40,43)] <- scale(APdata[,c(5:26,30,31,34,37,40,43)])

M <- cor(APdata %>% select_if(is.numeric) %>% na.omit())
corrplot(M,method = "circle")

#Change the base value category
APdata$size <- as.character(APdata$size)
APdata$size[is.na(APdata$size)] <- "without value"
APdata[APdata$size=="nonhub","size"] <- "a_nonhub"
APdata$size <- as.factor(APdata$size)

#

#1. Linear Regression
lin_19_20 <- lm(wk4_19_20~popdens+medearn+TotalOps+hub+multi_ap+veh0_pct+size,data=APdata)
summary(lin_19_20)

lin_wk1to4 <- lm(wk1to4_20~popdens+medearn+TotalOps+hub+multi_ap+veh0_pct+size,data=APdata)
summary(lin_wk1to4)

lin_wk1to3 <- lm(wk1to3_20~popdens+medearn+TotalOps+hub+multi_ap+veh0_pct+size,data=APdata)
summary(lin_wk1to3)

lin_wk1to2 <- lm(wk1to2_20~popdens+medearn+TotalOps+hub+multi_ap+veh0_pct+size,data=APdata)
summary(lin_wk1to2)


#2.Binary Logit

bino_19_20 <- glm(wk4_19_20_40pct~popdens+medearn+TotalOps+hub+multi_ap+veh0_pct+size,data=APdata,
                  family = "binomial")
summary(bino_19_20)

bino_wk1to4 <- glm(wk1to4_20_40pct~popdens+medearn+TotalOps+hub+multi_ap+veh0_pct+size,data=APdata,
                   family = "binomial")
summary(bino_wk1to4)

bino_wk1to3 <- glm(wk1to3_20_40pct~popdens+medearn+TotalOps+hub+multi_ap+veh0_pct+size,data=APdata,
                   family = "binomial")
summary(bino_wk1to3)

bino_wk1to2 <- glm(wk1to2_20_40pct~popdens+medearn+TotalOps+hub+multi_ap+veh0_pct+size,data=APdata,
                   family = "binomial")
summary(bino_wk1to2)


#Test the binary model here Please change with the code below!
testbino <- data.frame(real=na.omit(APdata)$wk4_19_20_40pct, 
                       prob=predict(bino_19_20,type ="response"))

# testbino <- data.frame(real=na.omit(APdata)$wk1to4_20_40pct, 
#                        prob=predict(bino_wk1to4,type ="response"))
# testbino <- data.frame(real=na.omit(APdata)$wk1to3_20_40pct, 
#                        prob=predict(bino_wk1to3,type ="response"))
# testbino <- data.frame(real=na.omit(APdata)$wk1to2_20_40pct, 
#                        prob=predict(bino_wk1to2,type ="response"))


testbino$prediction<- ifelse(testbino$prob<0.5,0,1) #change the threshold here
table(testbino$prediction==testbino$real)


testbino$prediction <- as.factor(testbino$prediction)
testbino$real<- as.factor(testbino$real)
caret::confusionMatrix(testbino$prediction, testbino$real, 
                       positive = "1")

ggplot(testbino,aes(x=prob,fill=as.factor(real)))+
  geom_density()+
  facet_grid(real~.)+
  scale_fill_manual(values=c("red","yellow"))+
  labs(title = "Probability Density of whether in fastest 40% of decline")
##Test binary ends


#3.Ordinal Logistic
ord_19_20 <- polr(wk4_19_20_q4~popdens+medearn+TotalOps+hub+multi_ap+veh0_pct+size,
            data=APdata,Hess=TRUE)

testord_19_20 <- data.frame(real=na.omit(APdata)$wk4_19_20_q4, prob=predict(ord_19_20,type ="probs"),
                      prediction=predict(ord_19_20,type = "class"))
table(testord_19_20$real==testord_19_20$prediction)


ord_wk1to4 <- polr(wk1to4_20_q4~popdens+medearn+TotalOps+hub+multi_ap+veh0_pct+size,
                  data=APdata,Hess=TRUE)

testord_wk1to4 <- data.frame(real=na.omit(APdata)$wk1to4_20_q4, prob=predict(ord_wk1to4,type ="probs"),
                            prediction=predict(ord_wk1to4,type = "class"))
table(testord_wk1to4$real==testord_wk1to4$prediction)


ord_wk1to3 <- polr(wk1to3_20_q4~popdens+medearn+TotalOps+hub+multi_ap+veh0_pct+size,
                   data=APdata,Hess=TRUE)

testord_wk1to3 <- data.frame(real=na.omit(APdata)$wk1to3_20_q4, prob=predict(ord_wk1to3,type ="probs"),
                             prediction=predict(ord_wk1to3,type = "class"))
table(testord_wk1to3$real==testord_wk1to3$prediction)


ord_wk1to2 <- polr(wk1to2_20_q4~popdens+medearn+TotalOps+hub+multi_ap+veh0_pct+size,
                   data=APdata,Hess=TRUE)
testord_wk1to2 <- data.frame(real=na.omit(APdata)$wk1to2_20_q4, prob=predict(ord_wk1to2,type ="probs"),
                             prediction=predict(ord_wk1to2,type = "class"))
table(testord_wk1to2$real==testord_wk1to2$prediction)




#4. Multinomial Logistic
multi_19_20 <- multinom(wk4_19_20_q4~popdens+medearn+TotalOps+hub+multi_ap+veh0_pct+size,
                  data=APdata)

testmulti_19_20 <- data.frame(real=na.omit(APdata)$wk4_19_20_q4, prob=predict(multi_19_20,type ="probs"),
                            prediction=predict(multi_19_20,type = "class"))
table(testmulti_19_20$real==testmulti_19_20$prediction)


multi_wk1to4 <- multinom(wk1to4_20_q4~popdens+medearn+TotalOps+hub+multi_ap+veh0_pct+size,
                   data=APdata)

testmulti_wk1to4 <- data.frame(real=na.omit(APdata)$wk1to4_20_q4, prob=predict(multi_wk1to4,type ="probs"),
                             prediction=predict(multi_wk1to4,type = "class"))
table(testmulti_wk1to4$real==testmulti_wk1to4$prediction)


multi_wk1to3 <- multinom(wk1to3_20_q4~popdens+medearn+TotalOps+hub+multi_ap+veh0_pct+size,
                   data=APdata)

testmulti_wk1to3 <- data.frame(real=na.omit(APdata)$wk1to3_20_q4, prob=predict(multi_wk1to3,type ="probs"),
                             prediction=predict(multi_wk1to3,type = "class"))
table(testmulti_wk1to3$real==testmulti_wk1to3$prediction)


multi_wk1to2 <- multinom(wk1to2_20_q4~popdens+medearn+TotalOps+hub+multi_ap+veh0_pct+size,
                   data=APdata)
testmulti_wk1to2 <- data.frame(real=na.omit(APdata)$wk1to2_20_q4, prob=predict(multi_wk1to2,type ="probs"),
                             prediction=predict(multi_wk1to2,type = "class"))
table(testmulti_wk1to2$real==testmulti_wk1to2$prediction)






