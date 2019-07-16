#developing a detection function using R
#NB forest plots collected as a transect, while cocoa data collected as points

library(Distance)
library(tidyverse)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")

birds<-read.csv(paste0(getwd(),"/Biodiversity/Kakum_point_counts.csv"))

#add forest/cocoa designation

forest.birds <- birds  %>% filter(str_detect(Plot, "Forest")) %>% mutate(LandCover="Forest") %>% 
  rename(distance=Distance)

cocoa.birds <- birds  %>% filter(str_detect(Plot, "Forest")==FALSE) %>% mutate(LandCover="Cocoa") %>% 
  rename(distance=Distance)

birds2<-bind_rows(forest.birds,cocoa.birds)

#to calculate encounter rate we need number of total visits
#calculating number of visits per plot
forest.birds$TotalEffort<-forest.birds %>% mutate(tot_visit=paste0(Plot,Dist,Date)) %>% select(tot_visit) %>% unique() %>% nrow()
#481

forest.birds %>% mutate(plot_visit=paste0(Plot,Dist)) %>% count(plot_visit, Date, sort = TRUE) 

cocoa.birds$TotalEffort<-cocoa.birds %>% mutate(plot_visit=paste0(Plot,Date)) %>% select(plot_visit) %>% unique() %>% nrow()
#299

#test<-left_join(birds,birds2 %>% select(Plot,Season,Date,LandCover,Species),by=c("Plot","Season","Date","Species"))

#start with forest birds and select by 1 of three habitat structures
forest.birds.canopy<-forest.birds %>% filter(StructCanopy==1) 
forest.birds.mid<-forest.birds %>% filter(StructMidStorey==1)
forest.birds.under<-forest.birds %>% filter(StructUnderStory==1)

#forest.birds.all<-forest.birds %>% filter(StructAllLevels==1) 

#convert meters to km with .001
halfnorm.dc1<-ds(forest.birds.canopy, transect = "point", truncation=40,key="hn", adjustment="cos", convert.units = 0.001)
summary(halfnorm.dc1)

unifcos.dc1 <- ds(forest.birds.canopy, transect = "point",truncation=40, key="unif", adjustment="cos", mono="strict", convert.units = 0.001)
summary(unifcos.dc1)

hazard.dc1 <- ds(forest.birds.canopy, transect = "point", truncation=40, key="hr",  adjustment="poly", convert.units = 0.001)
summary(hazard.dc1)

#comparing AICs
#halfnorm = 12827.59 
#unifcos = 12823.65   
#hazard =  12796.02   *

par(mfrow=c(1,2))
plot(hazard.dc1, main="Forest Canopy Birds, hazard rate detection function")
fit.test <- ddf.gof(hazard.dc1$ddf)

#calculate ER

#generate dataframe of ER per species, name variable ERcanopy
forest.birds.canopy %>% select(Species) %>% unique()

#mid-canopy
#convert meters to km with .001
halfnorm.dm1<-ds(forest.birds.mid, transect = "point",truncation=40, key="hn", adjustment="cos", convert.units = 0.001)
summary(halfnorm.dm1)

unifcos.dm1 <- ds(forest.birds.mid, transect = "point", truncation=40,key="unif", adjustment="cos", mono="strict", convert.units = 0.001)
summary(unifcos.dm1)

hazard.dm1 <- ds(forest.birds.mid, transect = "point", truncation=40,key="hr",  adjustment="poly", convert.units = 0.001)
summary(hazard.dm1)

#comparing AICs
#halfnorm = 8778.53 
#unifcos = 8790.605 
#hazard = 8739.12 *

par(mfrow=c(1,2))
plot(hazard.dm1, main="Forest Mid-Story Birds, hazard rate detection function")
fit.test <- ddf.gof(hazard.dm1$ddf)


#understory
#convert meters to km with .001
halfnorm.du1<-ds(forest.birds.under,transect = "point", truncation=40,key="hn", adjustment="cos", convert.units = 0.001)
summary(halfnorm.du1)

unifcos.du1 <- ds(forest.birds.under,transect = "point",truncation=40, key="unif", adjustment="cos", mono="strict", convert.units = 0.001)
summary(unifcos.du1)

hazard.du1 <- ds(forest.birds.under, transect = "point", truncation=40,key="hr",  adjustment="poly", convert.units = 0.001)
summary(hazard.du1)

#comparing AICs
#halfnorm = 18188.66 
#unifcos = 18597.53 
#hazard = 18076.28 *

par(mfrow=c(1,2))
plot(hazard.du1, main="Forest Under-Story Birds, hazard rate detection function")
fit.test <- ddf.gof(hazard.du1$ddf)

#####Cocoa plots#####
cocoa.birds.canopy<-cocoa.birds %>% filter(StructCanopy==1) 
cocoa.birds.mid<-cocoa.birds %>% filter(StructMidStorey==1)
cocoa.birds.under<-cocoa.birds %>% filter(StructUnderStory==1)

#convert meters to km with .001
halfnorm.cc1<-ds(cocoa.birds.canopy, transect = "point",truncation=40,key="hn", adjustment="cos", convert.units = 0.001)
summary(halfnorm.cc1)

unifcos.cc1 <- ds(cocoa.birds.canopy, transect = "point",truncation=40,key="unif", adjustment="cos", mono="strict", convert.units = 0.001)
summary(unifcos.cc1)

hazard.cc1 <- ds(cocoa.birds.canopy, transect = "point", truncation=40, key="hr",  adjustment="poly", convert.units = 0.001)
summary(hazard.cc1)

#comparing AICs
#halfnorm = 8098.823 
#unifcos = 8120.388  
#hazard = 8084.605 *

par(mfrow=c(1,2))
plot(hazard.cc1, main="Cocoa Canopy Birds, hazard rate detection function")
fit.test <- ddf.gof(hazard.cc1$ddf)

#mid-canopy
#convert meters to km with .001
halfnorm.cm1<-ds(cocoa.birds.mid, transect = "point",truncation=40, key="hn", adjustment="cos", convert.units = 0.001)
summary(halfnorm.cm1)

unifcos.cm1 <- ds(cocoa.birds.mid, transect = "point",truncation=40, key="unif", adjustment="cos", mono="strict", convert.units = 0.001)
summary(unifcos.cm1)

hazard.cm1 <- ds(cocoa.birds.mid, transect = "point",truncation=40, key="hr",  adjustment="poly", convert.units = 0.001)
summary(hazard.cm1)

#comparing AICs
#halfnorm = 4448.74 
#unifcos = 4451.65 
#hazard = 4434.911 *

par(mfrow=c(1,2))
plot(hazard.cm1, main="Cocoa Mid-Story Birds, hazard rate detection function")
fit.test <- ddf.gof(hazard.cm1$ddf)


#understory
#convert meters to km with .001
halfnorm.cu1<-ds(cocoa.birds.under, key="hn", truncation=40, adjustment="cos", convert.units = 0.001)
summary(halfnorm.cu1)

unifcos.cu1 <- ds(cocoa.birds.under, key="unif", truncation=40, adjustment="cos", mono="strict", convert.units = 0.001)
summary(unifcos.cu1)

hazard.cu1 <- ds(cocoa.birds.under, key="hr", truncation=40, adjustment="poly", convert.units = 0.001)
summary(hazard.cu1)

#comparing AICs
#halfnorm = 11484.59 
#unifcos = 18597.53 
#hazard = 18076.28 *

par(mfrow=c(1,2))
plot(hazard.du1, main="Forest Under-Story Birds, hazard rate detection function")
fit.test <- ddf.gof(hazard.du1$ddf)



