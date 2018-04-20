#averages of soil nutrients

library(gdata)
#library(ggplot2)
#library(plyr)
library(gridExtra)
library(corrplot)
library(stringr)
library(tidyverse)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Nutrients/Soils")

#load plot values
plts<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/plots.csv")
plts$name1<-gsub(" ","",plts$name3)

#load household data
h.hold<-read.xls(paste0("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/HouseholdData/Household_Data.xlsx"),sheet=1)
h.hold$name1<-gsub(" ","",h.hold$Plot)

#nutrient limits
s.limits<-read_csv(paste0(getwd(),"/SoilLimits.csv"))
colnames(s.limits)<-c("C","N","Avail.P","pH.1","p.H.2","Ca.meq.100g","K.meq.100g","Mg.meq.100g","Cu.mg.kg","Fe.mg.kg","Zn.mg.kg")

#load soil nutrient values
d.F<-read.xls(paste0(getwd(),"/Combined_soildata.xlsx"),sheet=1)
#reorder depth values
d.F$Depth<-factor(d.F$Depth,levels=rev(levels(d.F$Depth)))
d.F$Plot.Number<-gsub("FOREST","FP",d.F$Plot.Number)
#add distance and age values
d.F$Distance<-plts[match(d.F$Plot.Number,plts$name1),"distance"]
d.F$Age<-plts[match(d.F$Plot.Number,plts$name1),"age"]

#load bulk density and texture values
txt<-data.frame(read.xls(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Data/Kakum/Nutrients/Soils/Soil_ BD.xlsx"),sheet="TextureDefinitions"),stringsAsFactors = F)
txt <- txt %>% rename(Texture=Correct)
#load infiltration rates
inf<-read.xls(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Analysis/MetData/infiltration_rates.xlsx"),sheet="cms")
inf<-data.frame(inf[2:nrow(inf),1:2],stringsAsFactors = F)
colnames(inf)<-c("Texture","Infiltrate")
trns<-c("AB","HM","KA")

B.D<-list()
for(i in 1:length(trns)){
  b.D<-read.xls(paste0(getwd(),"/Soil_ BD.xlsx"),sheet=paste0(trns[i]," BD"))
  #take averages per plot and depth
  ps<-as.character(unique(b.D$Plot.ID))
  b.d1<-list()
  for(j in 1:length(ps)){
    #BD<-data.frame(Plot=character(),Depth=character(),BulkD<-numeric(),Texture=character(),Sand=numeric(),Silt=numeric(),Clay=numeric(),stringsAsFactors = F)
    b.d<-b.D[b.D$Plot.ID==ps[j],]
    #remove (numbers) and take averages of bulk density by depth
    b.d$Depth<-str_split_fixed(as.character(b.d$Depth)," ",2)[,1]
    #replace 10-20 and 20-30 with 10-30
    b.d[b.d$Depth=="10-20"|b.d$Depth=="20-30","Depth"]<-"10-30"
    BD<-b.d %>% group_by(Depth) %>% summarise(bulk.d=mean(Redo_BD))
    BD$Texture<-as.character(txt[match(as.character(b.d[match(BD$Depth,b.d$Depth),"Texture"]),txt$Category),"Texture"])
    BD <- left_join(BD,txt %>% select(-Category),by="Texture")
    #remove duplicates
    BD<-distinct(BD,Depth,.keep_all=T)
    BD$Plot<-ps[j]
   
    b.d1[[j]]<-BD
  }
  B.D[[i]]<-do.call(rbind.data.frame, b.d1)
}
b.d<-do.call(rbind.data.frame, B.D)

#add plot name to match with plots ignoring forest plots
b.d <- b.d %>% rename(name=Plot,Bulk_density=bulk.d,Sand=X..Sand,Silt=X.Silt,Clay=X..Clay)
b.d<- left_join(b.d,plts %>% select(name,name1), by="name")
b.d <- b.d %>% mutate(name1=replace(name1,is.na(name1),name[is.na(name1)]))
b.d$name1<-gsub(" ","",b.d$name1)

#add to main soil data
d.F <- d.F %>% rename(name1=Plot.Number)
d.F <- left_join(d.F,b.d %>% select(-name),by=c("name1","Depth"))

#if <LOD assign minimum value?
lod<-read.xls(paste0(getwd(),"/Combined_soildata.xlsx"),sheet=2)
d.F$Total.P.mg.kg<-as.numeric(as.character(d.F$Total.P.mg.kg))
d.F[is.na(d.F$Total.P.mg.kg),"Total.P.mg.kg"]<-max(lod[grep("P",lod[,1]),2])
d.F$Al...mg.kg.<-as.numeric(as.character(d.F$Al...mg.kg))
d.F[is.na(d.F$Al...mg.kg.),"Al...mg.kg."]<-lod[grep("Al",lod[,1]),2]

d.F$Na..mg.kg.<-as.numeric(as.character(d.F$Na..mg.kg.))
d.F[is.na(d.F$Na..mg.kg.),"Na..mg.kg."]<-lod[grep("Na",lod[,1]),2]

d.F$Cu..mg.kg.<-as.numeric(as.character(d.F$Cu..mg.kg.))
d.F[is.na(d.F$Cu..mg.kg.),"Cu..mg.kg."]<-max(lod[grep("Cu",lod[,1]),2])

d.F$Avail.Cu..mg.kg.<-as.numeric(as.character(d.F$Avail.Cu..mg.kg.))
d.F[is.na(d.F$Avail.Cu..mg.kg.),"Avail.Cu..mg.kg."]<-min(lod[grep("Cu",lod[,1]),2])

d.F$Avail.Zn..mg.kg.<-as.numeric(as.character(d.F$Avail.Zn..mg.kg.))
d.F[is.na(d.F$Avail.Zn..mg.kg.),"Avail.Zn..mg.kg."]<-min(lod[grep("Zn",lod[,1]),2])

d.F$weight<-NA
d.F[d.F$Depth=="0-10","weight"]<-0.1/0.3
d.F[d.F$Depth=="10-30","weight"]<-0.2/0.3

d.F.2 <- d.F %>% filter(!is.na(d.F$weight))  %>% group_by(Transect,name1) %>% 
  summarise(N.pct=sum(N.*weight,na.rm=T),C.pct=sum(C.*weight,na.rm=T),pH=sum(pH..H2O.*weight,na.rm=T),Avail.P.ppm=sum(Avail.P..mg.kg.*weight,na.rm=T),Ca.meq=sum(Ca2..*weight,na.rm=T),K.meq=sum(K..*weight,na.rm=T),Mg.meq=sum(Mg.2.*weight,na.rm=T),Fe.ppm=sum(Fe..mg.kg.*weight,na.rm=T))

#add proportion of limits for each nutrient measure
d.F.2$N.prop<-d.F.2$N.pct/s.limits$N
d.F.2$C.prop<-d.F.2$C.pct/s.limits$C
d.F.2$pH.diff<-(d.F.2$pH-s.limits$pH.1)/s.limits$pH.1
d.F.2$P.prop<-d.F.2$Avail.P.ppm/s.limits$Avail.P
d.F.2$Ca.prop<-d.F.2$Ca.meq/s.limits$Ca.meq.100g
d.F.2$K.prop<-d.F.2$K.meq/s.limits$K.meq.100g
d.F.2$Mg.prop<-d.F.2$Mg.meq/s.limits$Mg.meq.100g

write.csv(d.F.2,paste0(getwd(),"/Soil_nutrient_data.csv"))

