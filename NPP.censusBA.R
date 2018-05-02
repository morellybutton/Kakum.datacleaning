#code to calculate basal area, above ground biomass and cubic meters for each species by plot

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/AGB/ForestPlots/")

library(gdata)
#library(plyr)
library(stringr)
#library(ggplot2)
#library(reshape)
library(tidyverse)

#load plotdata
plts<-read.csv(paste0("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/plots.csv"))
f.plts<-as.character(plts$name3[grep("FP",plts$name3)])
c.plts<-as.character(plts$name3[grep("FP",plts$name3,invert=T)])
#load wood density lookup
w.d<-read.csv(paste0(getwd(),"/Species_lookup.csv"))
w.d <- data_frame(Species = as.character(w.d$species), Family = as.character(w.d$family), WoodDensity = w.d$WD,
                  Timber_code = w.d$Timber_code, Legume_code = w.d$Legume_code, Deciduous = w.d$Deciduous,
                  Succession = w.d$Succession, Elephant = w.d$Elephant, Climber = w.d$Climber, Fruit = w.d$Fruit,
                  SoilWater = w.d$SoilWater, SoilFertility = w.d$SoilFertility)

#load cocoa height eqn
c.ht<-read.csv(paste0(getwd(),"/cocoa_height.csv"))
c.ht<-data.frame(cbind(c.ht[1,2],c.ht[2,2]))
colnames(c.ht)<-c("intercept","slope")

#create dataframe to save all calculations too
biomass<-list()

h.est=function(dbh, h){
  l      =lm(h~dbh)
  coeffs = coefficients(l)
  pred.h = coeffs[1] + coeffs[2]*dbh
}

#diax in cm, density in g/cm3 and height in m, output in kg
Chave2014 <- function(diax, density, height) {
  AGB_est <- 0.0673*(density*((diax)^2)*height)^0.976 
  return(AGB_est)
}


t.hghts<-list()
SPP<-list()
#go through forest plots first
#assign plot area of large tree census
area = 1
#assign plot area of small tree census
areaS = 5*0.1*0.1
for(i in 1:length(f.plts)){
  df.ls<-data.frame(read.csv(paste0(getwd(),"/",strsplit(as.character(f.plts[i])," ")[[1]][1],"_forest.csv")),stringsAsFactors = F)
  #df.ss<-data.frame(read.csv(paste0(getwd(),"/",strsplit(as.character(f.plts[i])," ")[[1]][1],"_forestSS.csv")),stringsAsFactors = F)
  
  #create tidy data
  df.l<-data_frame(Subplot = as.numeric(df.ls$T1), Tag = as.numeric(df.ls$Tag), Family = as.character(df.ls$NFam), Species = as.character(df.ls$NSpecies), DBH1 = as.numeric(df.ls$DBH), DBH2 = as.numeric(df.ls$DBH2), DBH3 = as.numeric(df.ls$DBH3),
                   POM = as.numeric(df.ls$POM), Height = as.numeric(df.ls$THeight), Flag1 = as.character(df.ls$Flag1), Flag1.1 = as.character(df.ls$Flag1.1))
  #df.s<-data_frame(Subplot = as.numeric(df.ss$T1), Tag = as.numeric(df.ss$Tag), Family = as.character(df.ss$NFam), Species = as.character(df.ss$NSpecies), DBH1 = as.numeric(df.ss$DBH), DBH2 = as.numeric(df.ss$DBH2), DBH3 = as.numeric(df.ss$DBH3),
  #POM = as.numeric(df.ss$POM), Height = as.numeric(df.ss$THeight), Flag1 = as.character(df.ss$Flag1), Flag1.1 = as.character(df.ss$Flag1.1))
  
  rm(df.ls)
  #remove lianas (code t)
  df.l<-df.l[df.l$Flag1!="t",]
  #replace missing species/families with Indet
  df.l[is.na(df.l$Species),"Species"]<-"Indet indet"
  #df.s[is.na(df.s$Species),"Species"]<-"Indet indet"
  
  #replace mis-spelled species Ceolocaryon oxycarpum
  #df.l[df.l$Species=="Ceolocaryon oxycarpum"&!is.na(df.s$Species),"Species"]<-"Coelocaryon oxycarpum"
  #df.s[df.s$Species=="Ceolocaryon oxycarpum"&!is.na(df.s$Species),"Species"]<-"Coelocaryon oxycarpum"
  #remove NA species
  #df.ls<-df.ls[!is.na(df.ls$NSpecies),]
  #df.ss<-df.ss[!is.na(df.ss$NSpecies),]
  
  #calculate height measures
  hts <- df.l %>% select(DBH1, Height)
  
  hghts<-hts %>% summarise(intercept = coefficients(lm(DBH1/10~Height))[1], slope = coefficients(lm(DBH1/10~Height))[2])
  hghts$transect<-strsplit(as.character(f.plts[i])," ")[[1]][1]
  
  t.hghts[[i]]<-hghts
  
  #replace any missing tree heights
  df.l[is.na(df.l$Height),]<-df.l %>% filter(is.na(Height)) %>%
    mutate(Height = DBH1/10*hghts$slope+hghts$intercept)
  
  #df.s[is.na(df.s$Height),]<-df.s %>% filter(is.na(Height)) %>%
  #mutate(Height = DBH1/10*hghts$slope+hghts$intercept)
  
  #add wood density for biomass calculations
  df.l <- left_join(df.l,w.d %>% select(Species,WoodDensity), by="Species")
  df.l <- df.l %>%  mutate(BM1 = Chave2014(DBH1/10,WoodDensity,Height), BM2 =  Chave2014(DBH2/10,WoodDensity,Height),
                   BM3 =  Chave2014(DBH3/10,WoodDensity,Height) )
  
  #df.s <- left_join(df.s,w.d %>% select(Species,WoodDensity), by="Species")
  #df.s <- df.s %>%  mutate(BM1 = Chave2014(DBH1/10,WoodDensity,Height), BM2 =  Chave2014(DBH2/10,WoodDensity,Height),
  #BM3 =  Chave2014(DBH3/10,WoodDensity,Height) )
  
  #remove trees that are "dead", flag value of "0"
  df.l[df.l$Flag1.1==0,] <- df.l %>% filter(Flag1.1==0) %>% mutate(DBH2=NA,DBH3=NA,BM2=NA, BM3=NA)
  df.l[df.l$Flag1==0,] <- df.l %>% filter(Flag1==0) %>% mutate(DBH3=NA,BM3=NA)
  
  #df.s[df.s$Flag1.1==0,] <- df.s %>% filter(Flag1.1==0) %>% mutate(DBH2=NA,DBH3=NA,BM2=NA, BM3=NA)
  #df.s[df.s$Flag1==0,] <- df.s %>% filter(Flag1==0) %>% mutate(DBH3=NA,BM3=NA)
  
  #####UGH issue, if tree moves from small tree to large tree census, biomass per ha reduces a ton!
  #calculate basal area and biomass, per species for large and small tree census
  spp <- df.l %>% group_by(Species) %>%
    summarise(BA1 = sum(pi*(DBH1/2000)^2,na.rm=T)/area, BA2 = sum(pi*(DBH2/2000)^2,na.rm=T)/area, BA3 = sum(pi*(DBH3/2000)^2,na.rm=T)/area,
              BM1 = sum(BM1, na.rm=T)/1000/area, BM2 = sum(BM2, na.rm=T)/1000/area, BM3 = sum(BM3, na.rm=T)/1000/area,
              N.1 = ceiling(length(DBH1[!is.na(DBH1)])/area), N.2 = ceiling(length(DBH2[!is.na(DBH2)])/area), N.3 = ceiling(length(DBH3[!is.na(DBH3)])/area),
              S.1 = ceiling(length(Height[Height>=12&!is.na(DBH1)])/area), S.2 = ceiling(length(Height[Height>=12&!is.na(DBH2)])/area), S.3 = ceiling(length(Height[Height>=12&!is.na(DBH3)])/area))
  
  #spp.s <- df.s %>% group_by(Species) %>%
  # summarise(BA1 = sum(pi*(DBH1/2000)^2,na.rm=T)/areaS, BA2 = sum(pi*(DBH2/2000)^2,na.rm=T)/areaS, BA3 = sum(pi*(DBH3/2000)^2,na.rm=T)/areaS,
  #BM1 = sum(BM1, na.rm=T)/areaS/1000, BM2 = sum(BM2, na.rm=T)/areaS/1000, BM3 = sum(BM3, na.rm=T)/areaS/1000)
  
  #sum small and large trees
  #tmp <- full_join(spp.l,spp.s, by = "Species")
  
  #spp <- tmp %>% group_by(Species) %>%
  #mutate(BA1 = sum(BA1.x,BA1.y,na.rm=T), BA2 = sum(BA2.x,BA2.y,na.rm=T), BA3 = sum(BA3.x,BA3.y,na.rm=T),
  #BM1 = sum(BM1.x,BM1.y,na.rm=T), BM2 = sum(BM2.x,BM2.y,na.rm=T), BM3 = sum(BM3.x,BM3.y,na.rm=T) ) %>% 
  #select(Species, BA1,BA2,BA3,BM1,BM2,BM3,N.1,N.2,N.3,S.1,S.2,S.3) %>% arrange(Species)
  
  #add species codes
  spp<- left_join(spp, w.d %>% select(Species,Timber_code,Legume_code,Deciduous,Succession,Fruit), by="Species")
  
  #add Plot
  spp$Plot<-paste0(strsplit(as.character(f.plts[i])," ")[[1]][1]," FP")
  SPP[[i]]<-spp
}
heights<-do.call(rbind.data.frame,t.hghts)
#save height relationships
write.csv(heights,paste0(getwd(),"/forest_heights.csv"))

heights<-read.csv(paste0(getwd(),"/forest_heights.csv"))

#pull out mean DBH of cocoa trees (from large tree plots and large/small combined)
#mean.d<-data.frame(Plot=character(),c.l.dbh1=numeric(),c.t.dbh1=numeric(),c.l.dbh2=numeric(),c.t.dbh2=numeric(),stringsAsFactors = F)
mean.d<-list()
#assign plot area of large tree census
area = 0.36
#assign plot area of small tree census
areaS = 5*0.1*0.1

#each plot and load large and small tree measures
for(i in 1:length(c.plts)){
  df.ls<-data.frame(read.csv(paste0(getwd(),"/",gsub(" ","",c.plts[i]),"_LS.csv")),stringsAsFactors = F)
  #df.ss<-data.frame(read.csv(paste0(getwd(),"/",gsub(" ","",c.plts[i]),"_SS.csv")),stringsAsFactors = F)
  
  #extract transect
  transect = strsplit(as.character(c.plts[i])," ")[[1]][1]
  #create tidy data
  df.l<-data_frame(Subplot = as.numeric(df.ls$T1), Tag = as.numeric(df.ls$Tag), Family = as.character(df.ls$NFam), Species = as.character(df.ls$NSpecies), DBH1 = as.numeric(df.ls$DBH), DBH2 = as.numeric(df.ls$DBH2), DBH3 = as.numeric(df.ls$DBH3),
                   POM = as.numeric(df.ls$POM), Height = as.numeric(df.ls$THeight), Flag1 = as.character(df.ls$Flag1))
  #df.s<-data_frame(Subplot = as.numeric(df.ss$T1), Tag = as.numeric(df.ss$Tag), Family = as.character(df.ss$NFam), Species = as.character(df.ss$NSpecies), DBH1 = as.numeric(df.ss$DBH), DBH2 = as.numeric(df.ss$DBH2), DBH3 = as.numeric(df.ss$DBH3),
  #POM = as.numeric(df.ss$POM), Height = NA, Flag1 = as.character(df.ss$Flag1))
  rm(df.ls)
  #remove lianas (code t)
  df.l<-df.l[df.l$Flag1!="t",]
  #replace missing species/families with Indet
  df.l[is.na(df.l$Species),"Species"]<-"Indet indet"
  #df.s[is.na(df.s$Species),"Species"]<-"Indet indet"
  
  #replace any missing tree heights
  df.l[is.na(df.l$Height)&df.l$Species!="Theobroma cacao",]<- df.l %>% filter(is.na(Height)&Species!="Theobroma cacao") %>%
    mutate(Height = DBH1/10*heights[heights$transect==transect,"slope"]+heights[heights$transect==transect,"intercept"])
  
  #df.s[is.na(df.s$Height)&df.s$Species!="Theobroma cacao",] <- df.s %>% filter(is.na(Height)&Species!="Theobroma cacao") %>%
  #mutate(Height = DBH1/10*heights[heights$transect==transect,"slope"]+heights[heights$transect==transect,"intercept"])
  
  #calculate heights for missing trees,cocoa
  df.l[is.na(df.l$Height)&df.l$Species=="Theobroma cacao",]<- df.l %>% filter(is.na(Height)&Species=="Theobroma cacao") %>%
    mutate(Height = DBH1/10*c.ht$slope+c.ht$intercept)
  
  #df.s[is.na(df.s$Height)&df.s$Species=="Theobroma cacao",] <- df.s %>% filter(is.na(Height)&Species=="Theobroma cacao") %>%
  #mutate(Height = DBH1/10*c.ht$slope+c.ht$intercept)
  
  #add wood density for biomass calculations
  df.l <- left_join(df.l,w.d %>% select(Species,WoodDensity), by="Species")
  df.l <- df.l %>%  mutate(BM1 = Chave2014(DBH1/10,WoodDensity,Height), BM2 =  Chave2014(DBH2/10,WoodDensity,Height),
                           BM3 =  Chave2014(DBH3/10,WoodDensity,Height) )
  
  #df.s <- left_join(df.s,w.d %>% select(Species,WoodDensity), by="Species")
  #df.s <- df.s %>%  mutate(BM1 = Chave2014(DBH1/10,WoodDensity,Height), BM2 =  Chave2014(DBH2/10,WoodDensity,Height),
  #BM3 =  Chave2014(DBH3/10,WoodDensity,Height) )
  
  #pull out mean DBH of cocoa trees >10 cm
  tmp <- df.l %>% filter(Species == "Theobroma cacao") %>% summarise(DBH1 = mean(DBH1[DBH1>100], na.rm=T)/10,
                                                                     DBH2 = mean(DBH2[DBH2>100], na.rm=T)/10,
                                                                    DBH3 = mean(DBH3[DBH3>100], na.rm=T)/10)
  tmp$Plot <- as.character(c.plts[i])
  
  mean.d[[i]]<-tmp
  rm(tmp)
  
  #calculate basal area and biomass, per species for large and small tree census
  spp <- df.l %>% group_by(Species) %>%
    summarise(BA1 = sum(pi*(DBH1[DBH1>100]/2000)^2,na.rm=T)/area, BA2 = sum(pi*(DBH2[DBH2>100]/2000)^2,na.rm=T)/area, BA3 = sum(pi*(DBH3[DBH3>100]/2000)^2,na.rm=T)/area,
              BM1 = sum(BM1[DBH1>100], na.rm=T)/1000/area, BM2 = sum(BM2[DBH2>100], na.rm=T)/1000/area, BM3 = sum(BM3[DBH3>100], na.rm=T)/1000/area,
              N.1 = ceiling(length(DBH1[!is.na(DBH1)&DBH1>100])/area), N.2 = ceiling(length(DBH2[!is.na(DBH2)&DBH2>100])/area), N.3 = ceiling(length(DBH3[!is.na(DBH3)&DBH3>100])/area), 
              S.1 = ceiling(length(Height[Height>=12&!is.na(DBH1)])/area), S.2 = ceiling(length(Height[Height>=12&!is.na(DBH2)])/area), S.3 = ceiling(length(Height[Height>=12&!is.na(DBH3)])/area))
  
  #spp.s <- df.s %>% group_by(Species) %>%
  #summarise(BA1 = sum(pi*(DBH1/2000)^2,na.rm=T)/areaS, BA2 = sum(pi*(DBH2/2000)^2,na.rm=T)/areaS, BA3 = sum(pi*(DBH3/2000)^2,na.rm=T)/areaS,
  #BM1 = sum(BM1, na.rm=T)/areaS/1000, BM2 = sum(BM2, na.rm=T)/areaS/1000, BM3 = sum(BM3, na.rm=T)/areaS/1000)
  
  #sum small and large trees
  #tmp <- full_join(spp.l,spp.s, by = "Species")
  
  #spp <- tmp %>% group_by(Species) %>%
  #mutate(BA1 = sum(BA1.x,BA1.y,na.rm=T), BA2 = sum(BA2.x,BA2.y,na.rm=T), BA3 = sum(BA3.x,BA3.y,na.rm=T),
  #BM1 = sum(BM1.x,BM1.y,na.rm=T), BM2 = sum(BM2.x,BM2.y,na.rm=T), BM3 = sum(BM3.x,BM3.y,na.rm=T) ) %>% 
  #select(Species, BA1,BA2,BA3,BM1,BM2,BM3,N.1,N.2,N.3,S.1,S.2,S.3) %>% arrange(Species)
  
  #add species codes
  spp<- left_join(spp, w.d %>% select(Species,Timber_code,Legume_code,Deciduous,Succession,Fruit), by="Species")
  
  #add Plot
  spp$Plot<-as.character(c.plts[i])
  SPP[[i+3]]<-spp
  rm(df.l)
}
rm(c.ht,heights,hghts,hts,w.d,spp)

final<-do.call(rbind.data.frame,SPP)
write.csv(final,paste0(getwd(),"/AllPlots_BiomassBasalArea.csv"))

#calculate TAGC for each plot, convert to carbon = 47.8%
tagc <- final %>% group_by(Plot) %>% summarise(TAGC1 = sum(BM1,na.rm=T)*(1/(2.1097)), TAGC2 = sum(BM2, na.rm=T)*(1/(2.1097)),TAGC3 = sum(BM3, na.rm=T)*(1/(2.1097)))
#save .csv of plot carbon values
write.csv(tagc,paste0(getwd(),"/AllPlots_TAGC_calcs.csv"))

#write cocoa DBH averages
mean.d<-do.call(rbind.data.frame,mean.d)

write.csv(mean.d,paste0(getwd(),"/Cocoa_meanDBH.csv"))
mean.d<-read.csv(paste0(getwd(),"/Cocoa_meanDBH.csv"))

#open calculations
d.f<-data.frame(read.csv(paste0(getwd(),"/AllPlots_BiomassBasalArea.csv")))

d.f<-data_frame(Plot=as.character(d.f$Plot),Species=as.character(d.f$Species),BA1=d.f$BA1,BA2=d.f$BA2,BA3=d.f$BA3,BM1=d.f$BM1,BM2=d.f$BM2,BM3=d.f$BM3,
                N.1=d.f$N.1,N.2=d.f$N.2,N.3=d.f$N.3,S.1=d.f$S.1,S.2=d.f$S.2,S.3=d.f$S.3,Timber=d.f$Timber_code,Legume=d.f$Legume_code,
                Deciduous=d.f$Deciduous,Succession=d.f$Succession,Fruit=d.f$Fruit,Transect=cbind(str_split_fixed(as.character(d.f$Plot)," ",2))[,1])

b.a<-d.f[grep("FP",d.f$Plot,invert=T),] %>% group_by(Transect,Species) %>%
  summarise(BA1 = sum(BA1, na.rm=T),BA2 = sum(BA2,na.rm=T),BA3 = sum(BA3,na.rm=T)) %>%
  arrange(Transect,-BA1)

write.csv(b.a,paste0(getwd(),"/Basalarea_spp.csv"))
#rm(b.a,d.c,x1)

#create .xlsx file
#write.xlsx(b.a[b.a$trans=="AB",],paste0(getwd(),"/TimberAnalysis.v4.xlsx"),sheetName="Aboabo Species")
#write.xlsx(b.a[b.a$trans=="HM",],paste0(getwd(),"/TimberAnalysis.v4.xlsx"),sheetName="Ahomaho Species",append=T)
#write.xlsx(b.a[b.a$trans=="KA",],paste0(getwd(),"/TimberAnalysis.v4.xlsx"),sheetName="Kwameamoebang Species",append=T)

#calculate diversity of plot by basal area, using Shannon's index given by: -sum(ln(propi)^propi) and evenness given by index/ln(spp)
#where propi is the proportional abundance of a species and spp is number of species
dump <- d.f[grep("FP",d.f$Plot,invert=T),]  %>% group_by(Plot) %>% 
  summarise( Cdens1 = N.1[Species=="Theobroma cacao"],Cdens2 = N.2[Species=="Theobroma cacao"],Cdens3 = N.3[Species=="Theobroma cacao"],
             Sdens1 = sum(S.1), Sdens2 = sum(S.2), Sdens3 = sum(S.3),BA.Legume1 = sum(BA1[Legume==1],na.rm=T),BA.Legume2 = sum(BA2[Legume==1],na.rm=T),BA.Legume3 = sum(BA3[Legume==1],na.rm=T),
             BA.Deciduous1 = sum(BA1[Deciduous==1],na.rm=T),BA.Deciduous2 = sum(BA2[Deciduous==1],na.rm=T),BA.Deciduous3 = sum(BA3[Deciduous==1],na.rm=T),
             BA.Shade1 = sum(BA1[Succession=="Shade"],na.rm=T),BA.Shade2 = sum(BA2[Succession=="Shade"],na.rm=T),BA.Shade3 = sum(BA3[Succession=="Shade"],na.rm=T),
             BA.Pioneer1 = sum(BA1[Succession=="Pioneer"],na.rm=T),BA.Pioneer2 = sum(BA2[Succession=="Pioneer"],na.rm=T),BA.Pioneer3 = sum(BA3[Succession=="Pioneer"],na.rm=T),
             BA.Fruit1 = sum(BA1[Fruit==1],na.rm=T),BA.Fruit2 = sum(BA2[Fruit==1],na.rm=T),BA.Fruit3 = sum(BA3[Fruit==1],na.rm=T))

tmp <- d.f[grep("FP",d.f$Plot,invert=T),]  %>% group_by(Plot) %>% filter(Species != "Theobroma cacao") %>%
  summarise(No.Sp1 = length(unique(Species[BA1>0])),No.Sp2 = length(unique(Species[BA2>0])),No.Sp3 = length(unique(Species[BA3>0])),
            Shannoni1 = -sum((BA1/sum(BA1,na.rm=T))*log(BA1/sum(BA1,na.rm=T)),na.rm=T), 
             Shannoni2 = -sum((BA2/sum(BA2,na.rm=T))*log(BA2/sum(BA2,na.rm=T)),na.rm=T),
             Shannoni3 = -sum((BA3/sum(BA3,na.rm=T))*log(BA3/sum(BA3,na.rm=T)),na.rm=T))
tmp<-tmp %>% group_by(Plot) %>%  
  mutate(Shannone1 = Shannoni1/log(No.Sp1),
         Shannone2 = Shannoni2/log(No.Sp2),
         Shannone3 = Shannoni3/log(No.Sp3))

dump<-full_join(dump,tmp,by="Plot")
rm(tmp)

dump<-left_join(dump,mean.d[,2:5],by="Plot")

#save vegetation data dump
write.csv(dump,paste0(getwd(),"/Tree_plotdata.csv"))
rm(dump)


