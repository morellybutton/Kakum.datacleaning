#creation of dataset for analysis of cocoa ES benefits/dis-benefits
#library(stringr)
#library(corrplot)
#library(plyr)
library(gdata)
library(lubridate)
#library(reshape2)
#library(lattice)
library(tidyverse)

#detach("package:arm", unload=TRUE)
setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")

#load plot names
ns<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/plots.csv")
ns<-data_frame(Plot=gsub(" ","",ns$name3), distance=ns$distance,age=ns$age, distance.cont=ns$distance.1,
               Canopy.gap.dry=ns$Gap_Jan15,Canopy.gap.wet=ns$Gap_Jun15)

#load soil data
ES.soil<-read.csv(paste0(getwd(),"/Nutrients/Soils/Soil_nutrient_data.csv"))
ES.soil<-data_frame(Transect=as.character(ES.soil$Transect),Plot=ES.soil$name1,N.pct=ES.soil$N.pct,C.pct=ES.soil$C.pct,CN.ratio=ES.soil$C.pct/ES.soil$N.pct,
                    pH=ES.soil$pH,Tot.P=ES.soil$Avail.P.ppm,K.meq=ES.soil$K.meq,Ca.meq=ES.soil$Ca.meq,Mg.meq=ES.soil$Mg.meq)

#load yield data [Heavy Crop from Sept to June, Light Crop July & August]
#ES.yield<-read.csv(paste0(getwd(),"/Yield/Plot_Cropestimates.csv"))
ES.yield.tree<-read.csv(paste0(getwd(),"/Yield/PerTree_Cropestimates.csv"))
ES.yield.tree<- data_frame(Plot=gsub(" ","",ES.yield.tree$Plot),TreeNo=ES.yield.tree$TreeNo,DBH=ES.yield.tree$DBH,HeavyCrop=ES.yield.tree$HeavyCrop,HCBlackpod=ES.yield.tree$HC.blackpod,
                           HCCapsid=ES.yield.tree$HC.capsid,LightCrop=ES.yield.tree$LightCrop,LCBlackpod=ES.yield.tree$LC.blackpod,LCCapsid=ES.yield.tree$LC.capsid,
                           season=as.character(ES.yield.tree$season),Distance=ES.yield.tree$distance,Transect=as.character(ES.yield.tree$transect))
ES.yield.tree <- ES.yield.tree %>% mutate(tree_size="large") %>% mutate(tree_size=replace(tree_size,DBH<10,"small"))
ES.yield <- ES.yield.tree %>% group_by(Plot,season,tree_size) %>%
  summarise(HeavyCrop = median(HeavyCrop),LightCrop=median(LightCrop),Distance=mean(Distance),Transect=unique(Transect),no.tree=length(tree_size)) %>% filter(no.tree>=5)
yield.all <- ES.yield.tree %>% group_by(Plot,season) %>% summarise(HeavyCrop=median(HeavyCrop,na.rm=T),LightCrop=median(LightCrop,na.rm=T),tree_size="all",Distance=mean(Distance),Transect=unique(Transect),no.tree=length(Plot))
ES.yield <- bind_rows(ES.yield,yield.all)

#create dataset of monthly measures
month.yield<-read.csv(paste0(getwd(),"/Yield/Monthly_podremovals.csv"))


#load pollinator data
#ES.pollinos<-read.csv(paste0(getwd(),"/Pollination/Pollinator.nos.HC1415.csv"))
#ES.pollinabund<-read.csv(paste0(getwd(),"/Pollination/Pollinator.diversity.HC1415.csv"))
ES.pollination<-read.csv(paste0(getwd(),"/Pollination/Pollination_monthlyvariables.csv"))
#ES.pollination$year<-year(ES.pollination$month)
#ES.pollination$months<-1

#take mean for each plot (for 2014)
ES.pollin14<-ES.pollination %>% filter(year(Date)=="2014") %>% group_by(Plot) %>% summarise(Biomass=mean(Biomass,na.rm=T),Banana=mean(Banana,na.rm=T),Pollin.nos=mean(Pollin.nos,na.rm=T)*length(months),Pollin.div=mean(Pollin.div,na.rm=T),Sciaridae=mean(Sciaridae,na.rm=T)*length(months))
ES.pollin15<-ES.pollination %>% filter(year(Date)=="2015") %>% group_by(Plot) %>% summarise(Biomass=mean(Biomass,na.rm=T),Banana=mean(Banana,na.rm=T),Pollin.nos=mean(Pollin.nos,na.rm=T)*length(months),Pollin.div=mean(Pollin.div,na.rm=T),Sciaridae=mean(Sciaridae,na.rm=T)*length(months))
ES.pollin16<-ES.pollination %>% filter(year(Date)=="2016") %>% group_by(Plot) %>% summarise(Biomass=mean(Biomass,na.rm=T),Banana=mean(Banana,na.rm=T),Pollin.nos=mean(Pollin.nos,na.rm=T)*length(months),Pollin.div=mean(Pollin.div,na.rm=T),Sciaridae=mean(Sciaridae,na.rm=T)*length(months))

#load disease data 
#ES.disease<-read.csv(paste0(getwd(),"/Disease/Seasonal_meanpestincidence.csv"))
#ES.disease<-ES.disease[ES.disease$season.1415==1,]
ES.tree.disease<-read.csv(paste0(getwd(),"/Disease/Disease_monthlytreepests.csv"))
ES.tree.disease <- data_frame(Plot=gsub(" ","",ES.tree.disease$Plot),month=as.Date(ES.tree.disease$month),Creep=ES.tree.disease$Creep,Mist=ES.tree.disease$Mist,Stb=ES.tree.disease$StB)

ES.pod.disease<-read.csv(paste0(getwd(),"/Disease/Disease_monthlypodpests.csv"))
ES.pod.disease <- data_frame(Plot=gsub(" ","",ES.pod.disease$Plot),month=as.Date(ES.pod.disease$month),Total.pods=ES.pod.disease$Total.pods,PropCPB=ES.pod.disease$PropCPB,PropBP=ES.pod.disease$PropBP,iCPB=ES.pod.disease$iCPB,soil.moist=ES.pod.disease$soil.moist)
#ES.ptree.disease<-read.csv(paste0(getwd(),"/Disease/Monthly_pertree.plot.disease.csv"))

#ES.tree.disease$year<-year(ES.tree.disease$month)
#ES.tree.disease$month<-round_date(as.Date(ES.tree.disease$Date),unit="month")
ES.tdisease14 <- ES.tree.disease %>% filter(month<"2015-07-01") %>% group_by(Plot) %>%
  summarise(Creep=mean(Creep,na.rm=T),Mist=mean(Mist,na.rm=T),Stb=mean(Stb,na.rm=T))
  
ES.tdisease15 <- ES.tree.disease %>% filter(month<"2016-07-01"&month>="2015-07-02")%>% group_by(Plot) %>%
  summarise(Creep=mean(Creep,na.rm=T),Mist=mean(Mist,na.rm=T),Stb=mean(Stb,na.rm=T))

ES.tdisease16 <- ES.tree.disease %>% filter(month>="2016-07-02")%>% group_by(Plot) %>%
  summarise(Creep=mean(Creep,na.rm=T),Mist=mean(Mist,na.rm=T),Stb=mean(Stb,na.rm=T))

#ES.pod.disease$year<-year(ES.pod.disease$month)
ES.pdisease14 <- ES.pod.disease %>% filter(month<"2015-07-01") %>% group_by(Plot) %>%
  summarise(Total.pods=mean(Total.pods,na.rm=T),PropCPB=mean(PropCPB,na.rm=T),PropBP=mean(PropBP,na.rm=T),iCPB=mean(iCPB,na.rm=T),
          soil.moist=mean(soil.moist,na.rm=T))
ES.pdisease15 <- ES.pod.disease %>% filter(month<"2016-07-01"&month>="2015-07-02")%>% group_by(Plot) %>%
  summarise(Total.pods=mean(Total.pods,na.rm=T),PropCPB=mean(PropCPB,na.rm=T),PropBP=mean(PropBP,na.rm=T),iCPB=mean(iCPB,na.rm=T),
            soil.moist=mean(soil.moist,na.rm=T))
ES.pdisease16 <- ES.pod.disease %>% filter(month>="2016-07-02")%>% group_by(Plot) %>%
  summarise(Total.pods=mean(Total.pods,na.rm=T),PropCPB=mean(PropCPB,na.rm=T),PropBP=mean(PropBP,na.rm=T),iCPB=mean(iCPB,na.rm=T),
            soil.moist=mean(soil.moist,na.rm=T))

#ES.ptree.disease.14<-ES.ptree.disease[ES.ptree.disease$season=="2014/15",]
#ES.ptree.disease.15<-ES.ptree.disease[ES.ptree.disease$season=="2015/16",]

#load fruitset/pollination data
ES.fruitset<-read.csv(paste0(getwd(),"/Fruitset/Chset_allplots_allmonths.csv"))
ES.fruitset <- data_frame(Plot=gsub(" ","",ES.fruitset$Plot),month=as.Date(ES.fruitset$Month),FBuds=ES.fruitset$F.Buds,Flowers=ES.fruitset$Flowers,Chset=ES.fruitset$Chset,Chlost=ES.fruitset$Chlost,Chnum=ES.fruitset$Chnum,SBT=ES.fruitset$SBT,DBT=ES.fruitset$DBT)
#pull out Ch set for July 2014 to Sept 2014

ES.fruit14<- ES.fruitset %>% filter(month<="2014-12-01") %>% group_by(Plot) %>%
  summarise(FBuds=sum(FBuds,na.rm=T),Flowers=sum(Flowers,na.rm=T),Chset=mean(Chset,na.rm=T),SBT=mean(SBT,na.rm=T),DBT=mean(DBT,na.rm=T))

ES.fruit15<- ES.fruitset %>% filter(month<="2015-12-01"&month>"2014-12-01") %>% group_by(Plot) %>%
  summarise(FBuds=sum(FBuds,na.rm=T),Flowers=sum(Flowers,na.rm=T),Chset=mean(Chset,na.rm=T),SBT=mean(SBT,na.rm=T),DBT=mean(DBT,na.rm=T))

ES.fruit16<- ES.fruitset %>% filter(month<="2016-12-01"&month>"2015-12-01") %>% group_by(Plot) %>%
  summarise(FBuds=sum(FBuds,na.rm=T),Flowers=sum(Flowers,na.rm=T),Chset=mean(Chset,na.rm=T),SBT=mean(SBT,na.rm=T),DBT=mean(DBT,na.rm=T))

#add Standing Banana Tree and Dead Banana Tree from Jan 2015
banana <- ES.fruitset %>% filter(month=="2015-02-01") %>% group_by(Plot) %>%
  summarise(SBT=mean(SBT,na.rm=T),DBT=mean(DBT))

ES.fruit14<-left_join(ES.fruit14 %>% select(Plot,FBuds,Flowers,Chset),banana, by="Plot")

#load diversity data
ES.vegetation<-read.csv(paste0(getwd(),"/AGB/ForestPlots/Tree_plotdata.csv"))

ES.veg14<-data_frame(Plot=gsub(" ","",ES.vegetation$Plot),Cocoa.density=ES.vegetation$Cdens1,Shade.density=ES.vegetation$Sdens1,BALegume=ES.vegetation$BA.Legume1,
                     BADeciduous=ES.vegetation$BA.Deciduous1,BAShade=ES.vegetation$BA.Shade1,BAPioneer=ES.vegetation$BA.Pioneer1,BAFruit=ES.vegetation$BA.Fruit1,
                     Shannoni=ES.vegetation$Shannoni1,Shannone=ES.vegetation$Shannone1,mDBH=ES.vegetation$DBH1)

ES.veg15<-data_frame(Plot=gsub(" ","",ES.vegetation$Plot),Cocoa.density=ES.vegetation$Cdens2,Shade.density=ES.vegetation$Sdens2,BALegume=ES.vegetation$BA.Legume2,
                     BADeciduous=ES.vegetation$BA.Deciduous2,BAShade=ES.vegetation$BA.Shade2,BAPioneer=ES.vegetation$BA.Pioneer2,BAFruit=ES.vegetation$BA.Fruit2,
                     Shannoni=ES.vegetation$Shannoni2,Shannone=ES.vegetation$Shannone2,mDBH=ES.vegetation$DBH2)
                     
ES.veg16<-data_frame(Plot=gsub(" ","",ES.vegetation$Plot),Cocoa.density=ES.vegetation$Cdens3,Shade.density=ES.vegetation$Sdens3,BALegume=ES.vegetation$BA.Legume3,
                     BADeciduous=ES.vegetation$BA.Deciduous3,BAShade=ES.vegetation$BA.Shade3,BAPioneer=ES.vegetation$BA.Pioneer3,BAFruit=ES.vegetation$BA.Fruit3,
                     Shannoni=ES.vegetation$Shannoni3,Shannone=ES.vegetation$Shannone3,mDBH=ES.vegetation$DBH3)
                                          

#add which plots have been experimented with pollinators
treat<-read.csv(paste0(getwd(),"/Pollination/Treatmentplots.csv"))
treat<-data_frame(Plot=gsub(" ","",treat$Plot),treat = treat$Treatment)

ES.fruit14<-left_join(ES.fruit14,treat,by="Plot")
ES.fruit15<-left_join(ES.fruit15,treat,by="Plot")
ES.fruit16<-left_join(ES.fruit16,treat,by="Plot")

#load micro-climate data
ES.metdata<-read.csv(paste0(getwd(),"/MetData/MonthlyStress_estimates.csv"))
ES.metdata<-data_frame(Plot=ES.metdata$Plot,month=as.Date(ES.metdata$month),Tmax=ES.metdata$maxT,Tmin=ES.metdata$minT,Tmean=ES.metdata$meanT,maxVPD=ES.metdata$maxVPD,meanRH=ES.metdata$meanRH,stress.mm=ES.metdata$stress.mm,stress.mm1=ES.metdata$stress.mm.1)

month.metdata<-read.csv(paste0(getwd(),"/MetData/MonthlyStress_estimates.csv"))

month.yield$plot<-gsub(" ","",month.yield$plot)
month.yield$month<-as.Date(paste("01",month.yield$month,month.yield$year,sep="-"),"%d-%m-%Y")
month.metdata<-month.metdata %>% rename(plot=Plot)
month.metdata$month<-as.Date(month.metdata$month)

monthly<-left_join(month.yield %>% select(-X),month.metdata %>% select(-X),by=c("plot","month"))

ES.soil <- ES.soil %>% rename(plot=Plot)
monthly<-left_join(monthly,ES.soil,by="plot")

ps<-ns %>% rename(plot=Plot)
monthly<-left_join(monthly,ps,by="plot")

write_csv(monthly,paste0(getwd(),"/Analysis/ElNino/SeasonalAnalysis/MonthlyTree_Yield_Disease_Climate.csv"))

#get vegetation stress data for each year
ES.metdata14<-ES.metdata %>% filter(month<="2015-06-01") %>% group_by(Plot) %>%
  summarise(Tmax=mean(Tmax,na.rm=T),Tmin=mean(Tmin,na.rm=T),Tmean=mean(Tmean,na.rm=T),maxVPD=mean(maxVPD,na.rm=T),meanRH=mean(meanRH,na.rm=T),stress.mm=mean(stress.mm,na.rm=T),stress.mm1=mean(stress.mm1,na.rm=T))

ES.metdata15<-ES.metdata %>% filter(month<="2016-06-01"&month>"2015-06-01") %>% group_by(Plot) %>%
  summarise(Tmax=mean(Tmax,na.rm=T),Tmin=mean(Tmin,na.rm=T),Tmean=mean(Tmean,na.rm=T),maxVPD=mean(maxVPD,na.rm=T),meanRH=mean(meanRH,na.rm=T),stress.mm=mean(stress.mm,na.rm=T),stress.mm1=mean(stress.mm1,na.rm=T))

ES.metdata16<-ES.metdata %>% filter(month>"2016-06-01") %>% group_by(Plot) %>%
  summarise(Tmax=mean(Tmax,na.rm=T),Tmin=mean(Tmin,na.rm=T),Tmean=mean(Tmean,na.rm=T),maxVPD=mean(maxVPD,na.rm=T),meanRH=mean(meanRH,na.rm=T),stress.mm=mean(stress.mm,na.rm=T),stress.mm1=mean(stress.mm1,na.rm=T))

#ES.flower<-ddply(ES.metdata1,.(Plot),summarise,maxT=mean(maxT),minT=mean(minT),meanT=mean(meanT),maxVPD=mean(maxVPD),stress.mm=sum(stress.mm.1),stress.days=max(stress.daysinarow))
#colnames(ES.flower)<-c(paste0("flower.",colnames(ES.flower)))
#get vegetation stress data for fruiting
#ES.metdata1<-ES.metdata[as.Date(ES.metdata$month)>"2014-06-01"&as.Date(ES.metdata$month)<"2014-10-31",]
#ES.fruit<-ddply(ES.metdata1,.(Plot),summarise,maxT=mean(maxT),minT=mean(minT),meanT=mean(meanT),maxVPD=mean(maxVPD),stress.mm=sum(stress.mm.1),stress.days=max(stress.daysinarow))
#colnames(ES.fruit)<-c(paste0("fruit.",colnames(ES.fruit)))

#load household data
ES.manage<-read.xls(paste0("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/HouseholdData/Household_Data.xlsx"),sheet=1)
ES.manage <-data_frame(Plot=gsub(" ","",ES.manage$Plot),distance.cat=ES.manage$Distance.to.forest_plotholders,
                       Total.area.ha=ES.manage$Total.area..ha., Cocoa.area.ha=ES.manage$Cocoa.area..ha.,Age.of.cocoa=ES.manage$Age.of.cocoa.farm,
                       Cocoa.kg.ha.2015=ES.manage$Cocoa.yield..kg.ha...2015.,Total.cocoa.kg=ES.manage$Total.cocoa.production..kg., Labour.intensity.dhy=ES.manage$Household.labour.intensity..day.ha.yr.,
                       Purchased.labour.intensity.dhy=ES.manage$Purchased.labour.days.intensity..days.ha.,Total.labour.intensity.dhy=ES.manage$Total.Labour.intensity..days.ha.,Compost=ES.manage$Compost..kg.ha.,
                       Fertiliser=ES.manage$Fertiliser..kg.ha.,Herbicides=ES.manage$Herbicides..l.ha.,Fungicides=ES.manage$Fungicides..l.ha.,Pesticide=ES.manage$Pesticides..l.ha.,Black.pod=ES.manage$Black.Pod..0...no..1...yes.,
                       Black.pod.severity=ES.manage$Black.Pod.Severity..1...hardly.noticeable..2...some.crop.lost..3...significant.crop.losses..4...crop.totally.lost.,Capsid=ES.manage$Akati.Mosquito.Myrid.Capsid..0...no..1...yes.,
                       Capsid.Severity=ES.manage$Akati.Mosquito.Myrid.Capsid.Severity..1...hardly.noticeable..2...some.crop.lost..3...significant.crop.losses..4...crop.totally.lost.,Stem.borer=ES.manage$Stem.borer..0...no..1...yes.,
                       Stem.borer.Severity=ES.manage$Stem.borer.Severity..1...hardly.noticeable..2...some.crop.lost..3...significant.crop.losses..4...crop.totally.lost., Mistletoe=ES.manage$Mistletoe..0...no..1...yes.,
                       Mistletoe.Severity=ES.manage$Mistletoe.Severity..1...hardly.noticeable..2...some.crop.lost..3...significant.crop.losses..4...crop.totally.lost.,Climbers=ES.manage$Climbers..0...no..1...yes.,Climbers.Severity=ES.manage$Climbers.Severity..1...hardly.noticeable..2...some.crop.lost..3...significant.crop.losses..4...crop.totally.lost.,
                       CSSV=ES.manage$CSSV..0...no..1...yes.,CSSV.Severity=ES.manage$CSSV.Severity..1...hardly.noticeable..2...some.crop.lost..3...significant.crop.losses..4...crop.totally.lost.,Elephants=ES.manage$Elephants..0...no..1...yes.,
                       Elephants.Severity=ES.manage$Elephants..1...hardly.noticeable..2...some.crop.lost..3...significant.crop.losses..4...crop.totally.lost.)

ES.management<-ES.manage %>% replace_na(list(Cocoa.kg.ha.2015=0,Total.cocoa.kg=0,Compost=0,Fertiliser=0,Herbicides=0,Fungicides=0,Pesticide=0,Black.pod.severity=0,
                                  Capsid.Severity=0,Stem.borer.Severity=0,Mistletoe.Severity=0, Climbers.Severity=0,CSSV.Severity=0,
                                  Elephants.Severity=0))

ES.management<-left_join(ES.management,ns %>% select(Plot,age,distance.cont,Canopy.gap.dry,Canopy.gap.wet),by="Plot")
ES.management <- distinct(ES.management,Plot,.keep_all=T)
#write.csv(ES.management,paste0(getwd(),"/Analysis/ES/Management.variables.csv"))

#create monthly measures dataset
ES.pollination$month<-as.Date(ES.pollination$month)
pollin <- left_join(ES.fruitset,ES.pollination %>% select(-Date,-X),by=c("Plot","month"))
pollin$year<-year(pollin$month)

month_yield<-read_csv(paste0(getwd(),"/Yield/Monthly_podremovals.csv"))
month_yield$Plot<-gsub(" ","",month_yield$plot)
month_yield$month<-as.Date(paste(month_yield$year,month_yield$month,"01",sep="-"))

ES.veg14$year<-2014
ES.veg15$year<-2015
ES.veg16$year<-2016

shade<-bind_rows(ES.veg14,ES.veg15,ES.veg16)

month_yield<-month_yield %>% select(-X1,-plot,-df.TotRemoved,-TotRemoved,-s.crop,-m.crop,-l.crop,
                                              -s.harv,-m.harv,-l.harv,-s.bp,-m.bp,-s.cpb,-m.cpb,-s.mam,-m.mam,-l.mam,
                                              -abs.l.over.,-abs.s.over.,-abs.m.over.,-check,-check2,-date,-treeno.1)
month_yield<-left_join(month_yield,ES.metdata %>% select(-stress.mm1),by=c("Plot","month"))

#write_csv(month_yield,paste0(getwd(),"/Analysis/ElNino/SeasonalAnalysis/MonthlyTree_Yield_Disease_Climate.csv"))

pollin<-left_join(pollin %>% select(-Sciaridae,-Cecidomyiidae,-Indet,-Psychodidae,-Keroplatidae,
                                    -Empidoidea,-Culcidae,-Ceratopogonidae),shade %>% select(-mDBH,-Shannone),
                  by=c("Plot","year"))
write_csv(pollin,paste0(getwd(),"/Analysis/ElNino/SeasonalAnalysis/MonthlyFarm_Fruitset.csv"))

farm <- left_join(ES.management %>% select(-Black.pod,-Black.pod.severity,-Capsid,-Capsid.Severity,-Stem.borer,-Stem.borer.Severity,
                                 -Mistletoe,-Mistletoe.Severity,-Climbers,-Climbers.Severity,-CSSV,-CSSV.Severity),ES.soil,by="Plot")
write_csv(farm,paste0(getwd(),"/Analysis/ElNino/SeasonalAnalysis/Farm_Characteristics.csv"))

#add seasonal yield measures
d.F.14<- ES.yield %>% filter(season=="2014/15")
d.F.15<- ES.yield %>% filter(season=="2015/16")
d.F.16<- ES.yield %>% filter(season=="2016/17")
rm(ES.yield)

d.F.14.tree<- ES.yield.tree %>% filter(season=="2014/15")
d.F.15.tree<- ES.yield.tree %>% filter(season=="2015/16")
d.F.16.tree<- ES.yield.tree %>% filter(season=="2016/17")
rm(ES.yield.tree)

d.F.14 <- left_join(d.F.14,ES.management %>% select(Plot,Age.of.cocoa,Compost,Fertiliser,Herbicides,Pesticide,Fungicides,distance.cont,Canopy.gap.dry,Canopy.gap.wet,Total.labour.intensity.dhy),by="Plot")
d.F.15 <- left_join(d.F.15,ES.management %>% select(Plot,Age.of.cocoa,Compost,Fertiliser,Herbicides,Pesticide,Fungicides,distance.cont,Canopy.gap.dry,Canopy.gap.wet,Total.labour.intensity.dhy),by="Plot")
d.F.16 <- left_join(d.F.16,ES.management %>% select(Plot,Age.of.cocoa,Compost,Fertiliser,Herbicides,Pesticide,Fungicides,distance.cont,Canopy.gap.dry,Canopy.gap.wet,Total.labour.intensity.dhy),by="Plot")

d.F.14.tree <- left_join(d.F.14.tree,ES.management %>% select(Plot,Age.of.cocoa,Compost,Fertiliser,Herbicides,Pesticide,Fungicides,distance.cont,Canopy.gap.dry,Canopy.gap.wet,Total.labour.intensity.dhy),by="Plot")
d.F.15.tree <- left_join(d.F.15.tree,ES.management %>% select(Plot,Age.of.cocoa,Compost,Fertiliser,Herbicides,Pesticide,Fungicides,distance.cont,Canopy.gap.dry,Canopy.gap.wet,Total.labour.intensity.dhy),by="Plot")
d.F.16.tree <- left_join(d.F.16.tree,ES.management %>% select(Plot,Age.of.cocoa,Compost,Fertiliser,Herbicides,Pesticide,Fungicides,distance.cont,Canopy.gap.dry,Canopy.gap.wet,Total.labour.intensity.dhy),by="Plot")

rm(ES.management,ES.manage)

#add soil data
d.F.14<-left_join(d.F.14,ES.soil[,2:ncol(ES.soil)],by="Plot")
d.F.15<-left_join(d.F.15,ES.soil[,2:ncol(ES.soil)],by="Plot")
d.F.16<-left_join(d.F.16,ES.soil[,2:ncol(ES.soil)],by="Plot")

d.F.14.tree<-left_join(d.F.14.tree,ES.soil[,2:ncol(ES.soil)],by="Plot")
d.F.15.tree<-left_join(d.F.15.tree,ES.soil[,2:ncol(ES.soil)],by="Plot")
d.F.16.tree<-left_join(d.F.16.tree,ES.soil[,2:ncol(ES.soil)],by="Plot")

rm(ES.soil)

#add vegetation diversity data
d.F.14<-left_join(d.F.14,ES.veg14 %>% select(-year),by="Plot")
d.F.15<-left_join(d.F.15,ES.veg15 %>% select(-year),by="Plot")
d.F.16<-left_join(d.F.16,ES.veg16 %>% select(-year),by="Plot")

d.F.14.tree<-left_join(d.F.14.tree,ES.veg14,by="Plot")
d.F.15.tree<-left_join(d.F.15.tree,ES.veg15,by="Plot")
d.F.16.tree<-left_join(d.F.16.tree,ES.veg16,by="Plot")

rm(ES.veg14,ES.veg15,ES.veg16,ES.vegetation)

#add cherelle set
d.F.14<-left_join(d.F.14,ES.fruit14,by="Plot")
d.F.15<-left_join(d.F.15,ES.fruit15,by="Plot")
d.F.16<-left_join(d.F.16,ES.fruit16,by="Plot")

d.F.14.tree<-left_join(d.F.14.tree,ES.fruit14,by="Plot")
d.F.15.tree<-left_join(d.F.15.tree,ES.fruit15,by="Plot")
d.F.16.tree<-left_join(d.F.16.tree,ES.fruit16,by="Plot")

rm(ES.fruit14,ES.fruit15,ES.fruit16,ES.fruitset)

#add pollinator data
d.F.14<-left_join(d.F.14,ES.pollin14,by="Plot")
d.F.15<-left_join(d.F.15,ES.pollin15,by="Plot")
d.F.16<-left_join(d.F.16,ES.pollin16,by="Plot")

d.F.14.tree<-left_join(d.F.14.tree,ES.pollin14,by="Plot")
d.F.15.tree<-left_join(d.F.15.tree,ES.pollin15,by="Plot")
d.F.16.tree<-left_join(d.F.16.tree,ES.pollin16,by="Plot")

rm(ES.pollin14,ES.pollin15,ES.pollination)

#add disease data
d.F.14<- left_join(d.F.14,ES.pdisease14,by="Plot")
d.F.15<- left_join(d.F.15,ES.pdisease15,by="Plot")
d.F.16<- left_join(d.F.16,ES.pdisease16,by="Plot")

d.F.14.tree<- left_join(d.F.14.tree,ES.pdisease14,by="Plot")
d.F.15.tree<- left_join(d.F.15.tree,ES.pdisease15,by="Plot")
d.F.16.tree<- left_join(d.F.16.tree,ES.pdisease16,by="Plot")

rm(ES.pod.disease,ES.pdisease14,ES.pdisease15,ES.pdisease16)

d.F.14<- left_join(d.F.14,ES.tdisease14,by="Plot")
d.F.15<- left_join(d.F.15,ES.tdisease15,by="Plot")
d.F.16<- left_join(d.F.16,ES.tdisease16,by="Plot")

d.F.14.tree<- left_join(d.F.14.tree,ES.tdisease14,by="Plot")
d.F.15.tree<- left_join(d.F.15.tree,ES.tdisease15,by="Plot")
d.F.16.tree<- left_join(d.F.16.tree,ES.tdisease16,by="Plot")

rm(ES.tree.disease,ES.tdisease15,ES.tdisease14,ES.tdisease16)

#add metdata
d.F.14 <- left_join(d.F.14,ES.metdata14,by="Plot")
d.F.15 <- left_join(d.F.15,ES.metdata15,by="Plot")
d.F.16 <- left_join(d.F.16,ES.metdata16,by="Plot")

d.F.14.tree <- left_join(d.F.14.tree,ES.metdata14,by="Plot")
d.F.15.tree <- left_join(d.F.15.tree,ES.metdata15,by="Plot")
d.F.16.tree <- left_join(d.F.16.tree,ES.metdata16,by="Plot")

rm(ES.metdata,ES.metdata14,ES.metdata15,ES.metdata16)

#write dataset for reference
write.csv(d.F.14,paste0(getwd(),"/Analysis/ES/ES_analysis_dataset.2014.csv"))
write.csv(d.F.15,paste0(getwd(),"/Analysis/ES/ES_analysis_dataset.2015.csv"))
write.csv(d.F.16,paste0(getwd(),"/Analysis/ES/ES_analysis_dataset.2016.csv"))

write.csv(d.F.14.tree,paste0(getwd(),"/Analysis/ES/ES_analysis_dataset_ptree.2014.csv"))
write.csv(d.F.15.tree,paste0(getwd(),"/Analysis/ES/ES_analysis_dataset_ptree.2015.csv"))
write.csv(d.F.16.tree,paste0(getwd(),"/Analysis/ES/ES_analysis_dataset_ptree.2016.csv"))

#combine final datasets

l.bor<-read_csv(paste0(getwd(),"/HouseholdData/Labour.csv"))
y.ld<-read_csv(paste0(getwd(),"/HouseholdData/PastHarvest.csv"))
f.rt<-read_csv(paste0(getwd(),"/HouseholdData/Fertiliser.csv"))
f.rt$Plot.Number<-f.rt$`Plot Number`
#take average per plot
l.bor1 <- l.bor %>% group_by(plot) %>% summarise(Labour.harvesting=mean(Labour.harvesting),Labour.weeding=mean(Labour.weeding),Harvesting.months=mean(Harvesting.months),Weedings.months=mean(Weedings.months))
y.ld1 <- y.ld %>% group_by(plot) %>% summarise(Yield.cv=mean(as.numeric(as.character(Yield.cv)),na.rm=T)) %>% 
  mutate(Yield.cv=replace(Yield.cv,is.na(Yield.cv),0))
f.rt1 <- f.rt %>% group_by(Plot.Number) %>% summarise(No.applications.yr=mean(No.applications.5yrs,na.rm=T)/5,Applied.1516=mean(Applied.1516,na.rm=T),Solid.N.ha=mean(Solid.N.ha,na.rm=T),
                                                      Solid.P.ha=mean(Solid.P.ha,na.rm=T),Solid.K.ha=mean(Solid.K.ha,na.rm=T),App.rate.espa=mean(App.rate.espa,na.rm=T)) %>%
  mutate(No.applications.yr=replace(No.applications.yr,is.na(No.applications.yr),0),Applied.1516=replace(Applied.1516,is.na(Applied.1516),0),Solid.N.ha=replace(Solid.N.ha,is.na(Solid.N.ha),0),
         Solid.P.ha=replace(Solid.P.ha,is.na(Solid.P.ha),0),Solid.K.ha=replace(Solid.K.ha,is.na(Solid.K.ha),0),App.rate.espa=replace(App.rate.espa,is.na(App.rate.espa),0)) %>%
  rename(plot=Plot.Number)

year<-c("2014","2015","2016")
combo<-list()
for(i in 1:length(year)){
  d.F.plot <- read_csv(paste0(getwd(),"/Analysis/ES/ES_analysis_dataset.",year[i],".csv"))
  d.F.plot <- d.F.plot %>% rename(plot=Plot)
  #d.F.plot <- distinct(d.F.plot, plot, .keep_all = TRUE)
  
  d.F.plot <- left_join(d.F.plot,l.bor1 %>% select(plot,Labour.harvesting,Harvesting.months,Labour.weeding,Weedings.months), by="plot")
  d.F.plot <- left_join(d.F.plot,y.ld1 %>% select(plot,Yield.cv),by="plot")
  d.F.plot <- left_join(d.F.plot,f.rt1,by="plot")
  
  combo[[i]] <- d.F.plot %>% select(plot,Transect,tree_size,season,HeavyCrop,LightCrop,PropCPB,PropBP,Mist,Biomass,Distance,distance.cont,Age.of.cocoa,Cocoa.density,Shade.density,Shannoni,BALegume,Canopy.gap.dry,CN.ratio,Tot.P,K.meq,pH,soil.moist,stress.mm,Tmax,Tmin,maxVPD,Chset,SBT,No.applications.yr)
  
  #make sure binary variables are factors
  d.F.plot$Fertliser.bin<-0
  d.F.plot[d.F.plot$Fertiliser>0,"Fertliser.bin"]<-1
  d.F.plot$Fertliser.bin<-factor(d.F.plot$Fertliser.bin)
  d.F.plot$Compost.bin<-0
  d.F.plot[d.F.plot$Compost>0,"Compost.bin"]<-1
  d.F.plot$Compost.bin<-factor(d.F.plot$Compost.bin)
  d.F.plot$treat<-factor(d.F.plot$treat)
  
  #save dataset for subsequent modeling
  write.csv(d.F.plot,paste0(getwd(),"/Analysis/ES/Yield_dataset.",year[i],".csv"))
}
yield_all<-do.call(rbind.data.frame,combo)

#take anomalies per plot
yield_mean <- yield_all %>% group_by(plot,tree_size) %>% summarise(m.HeavyCrop=mean(HeavyCrop,na.rm=T),m.LightCrop=mean(LightCrop,na.rm=T),
                                                        m.PropCPB=mean(PropCPB,na.rm=T),m.PropBP=mean(PropBP,na.rm=T))

yield_all <- left_join(yield_all,yield_mean,by=c("plot","tree_size"))
yield_all <- yield_all %>% mutate(anom_heavycrop=HeavyCrop-m.HeavyCrop,anom_lightcrop=LightCrop-m.LightCrop,anom_cpb=PropCPB-m.PropCPB,
                                  anom_bp=PropBP-m.PropBP)

write_csv(yield_all,paste0(getwd(),"/Analysis/ES/Yield_anomalies.csv"))
  