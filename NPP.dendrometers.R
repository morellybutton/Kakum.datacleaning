# Written by: C?cile Girardin September 2014

## Preparation of dendrometer data for NPP analysis

# dendrometers measured every 3 months
# assumes all trees and lianas over 10 cm measured at 1.3 meters
# added measured increased dendrometer growth to DBH for each quarterly measure (CHECK!!)

# requires two .csv files: 
# census   <- read.csv() 
# dendrometer <- read.csv() 

library(gdata)
library(lubridate)
library(tidyverse)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/")
site="Kakum"
trans=c("HM","KA")
#load plotdata
plts<-read.csv(paste0(getwd(),"/",site,"/plots.csv"))
#load cocoa height data
co.ht<-read.csv(paste0(getwd(),"/",site,"/AGB/ForestPlots/cocoaheights.csv"))
co.ht$plotname<-"XXX"
co.ht$Species<-"Theobroma cacao"
co.ht<-data.frame(cbind(co.ht$plotname,co.ht$Tag,as.character(co.ht$Species),co.ht$dbh,co.ht$THeight),stringsAsFactors=F)
names(co.ht)<-c("plotname","TagNo","Species","dbh","height_m")
#if dbh in dendro in cm, multiply by 10 to be mm
cf=10
#open census dates
d.ts<-read.csv(paste0(getwd(),"/",site,"/AGB/ForestPlots/census.dates.csv"))

#load wood density values
w.dens<-read.xls("/Volumes/ELDS/ECOLIMITS/R_codes/GlobalWoodDensityDatabase.xls",sheet="Data")
w.dens<-data_frame(family=as.character(w.dens$Family),Binomial=as.character(w.dens$Binomial),wood_density_g_m2=as.numeric(w.dens$Wood.density..g.cm.3...oven.dry.mass.fresh.volume),Region=as.character(w.dens$Region))
w.dens$genus <- str_split_fixed(w.dens$Binomial, " ",2)[,1]

## Correct for missing tree heights
# missing height function
h.est=function(dbh, h){
  l      =lm(h~dbh)
  coeffs = coefficients(l)
  pred.h = coeffs[1] + coeffs[2]*dbh
}

for(p in 1:length(trans)){
  Dataall <- read.xls(paste0(getwd(),"/",site,"/NPP/Dendrometers/Dendrometers.xlsx"),sheet=trans[p])
  #find all plots on datasheet
  ind<-grep("Plot",Dataall[,1])
  #add last row
  ind[length(ind)+1]<-nrow(Dataall)
  final<-list()
  allo<-list()
  for(k in 1:(length(ind)-1)){
    plotname<-strsplit(as.character(Dataall[ind[k],1]),":")[[1]][2]
    plotcode<-as.character(plts[grep(plotname,plts$name3),"PlotCode"])
    dF<-data.frame(lapply(Dataall[ind[k]:(ind[k+1]-2),], as.character),stringsAsFactors=F)
    names(dF)<-as.character(dF[2,])
    #remove header lines
    dF<-dF[3:nrow(dF),]
    #replace NGs with 0
    dF[dF=="NG"]=0
    #convert all values to number
    dF[,8:(ncol(dF)-1)]<-lapply(dF[,8:(ncol(dF)-1)], as.numeric)
    dF$plotname<-plotname
    dF$plot_code<-plotcode
    
    #DF<-data.frame(plotname=character(),plot_code=character(),subplot=character(),tag=numeric(),dbh=numeric(),dendrometer_reading_mm=numeric(),year=numeric(),month=numeric(),day=numeric())
    DF<-data.frame(cbind(dF$plotname,dF$plot_code,dF$Subplot))
    DF[,4:5]<-cbind(dF$Tag,dF$Diameter*cf)
    DF[,6:7]<-c(0,0)
    #pull out dates of dendro readings
    ind2<-grep("Growth",colnames(dF))
    ds<-unlist(strsplit(names(dF[,ind2]),"Growth"))
    #remove blanks
    ds<-gsub(" ","",ds[ds!=""])
    
    d1<-as.Date(as.character(d.ts[d.ts$plotname==plotname,"date"]),format="%Y-%m-%d")
    
    #load census data to get height and DBH values
    if(length(grep("FP",plotname))==0) census<-read.csv(paste0(getwd(),"/",site,"/AGB/ForestPlots/",gsub(" ","",plotname),"_LS.csv")) else census<-read.csv(paste0(getwd(),"/",site,"/AGB/ForestPlots/",trans[p],"_forest.csv"))
    #colnames(census)<-c(colnames(census[,1:8]),"DBH0","DBH1",colnames(census[,11:ncol(census)]))
    DF[,8]<-year(d1[1])
    DF[,9]<-month(d1[1])
    DF[,10]<-day(d1[1])
    allo[[k]]<-cbind(as.character(plotname),as.character(census$Tag),as.character(census$NSpecies),census[,grep("DBH",colnames(census))]/10,as.numeric(census$THeight))
    names(DF)<-c("plotname","plot_code","sub_plot","tree_tag","baseline_dbh_mm","dendrometer_reading_mm_cum","dendrometer_reading_mm","year","month","day")
    #remove NA DBH
    census<-census[!is.na(census$DBH),]
    for(j in 1:(length(ds))){
      #d[j,"date"]<-as.Date(substr(ds[j],2,9),format="%d/%m/%y")
      d<-as.Date(substr(ds[j],2,9),format="%d/%m/%y")
      DF[(j*nrow(dF)+1):((j+1)*nrow(dF)),1:3]<-cbind(dF$plotname,dF$plot_code,dF$Subplot)
      DF[(j*nrow(dF)+1):((j+1)*nrow(dF)),4]<-dF$Tag
      DF[(j*nrow(dF)+1):((j+1)*nrow(dF)),5]<-as.numeric(DF[((j-1)*nrow(dF)+1):(j*nrow(dF)),"baseline_dbh_mm"])
      DF[(j*nrow(dF)+1):((j+1)*nrow(dF)),6]<-as.numeric(dF[,ind2[j]])*10
      if(j==1) DF[(j*nrow(dF)+1):((j+1)*nrow(dF)),7]<-as.numeric(dF[,ind2[j]])*10 else DF[(j*nrow(dF)+1):((j+1)*nrow(dF)),7]<-(as.numeric(dF[,ind2[j]])-as.numeric(dF[,ind2[j-1]]))*10
      DF[(j*nrow(dF)+1):((j+1)*nrow(dF)),8]<-year(d)
      DF[(j*nrow(dF)+1):((j+1)*nrow(dF)),9]<-month(d)
      DF[(j*nrow(dF)+1):((j+1)*nrow(dF)),10]<-day(d)
    }
    #colnames(dF[,(ncol(dF)-j+1):ncol(dF)])=c(paste0("date",1:j))
    #colnames(dF)<-c(colnames(dF[,1:(ncol(dF)-j)]),paste0("date",1:j))
    #add original census year
    DF$baseline_dbh_year<-year(min(as.Date(d.ts[d.ts$plotcode==plotcode,"date"])))
    #add census data for each tree
    DF$tree_height_m<-census[match(DF$tree_tag,census$Tag),"THeight"]
    DF$tree_height_year<-year(min(as.Date(d.ts[d.ts$plotcode==plotcode,"date"])))
    DF$pom_height_m<-census[match(DF$tree_tag,census$Tag),"POM"]/1000
    #add living code (1=alive, 0=dead)
    DF$mortality_code_alive_dead<-1
    tmp<-census[census$Flag1.1==0,"Tag"]
    if(length(tmp)>0) {for(g in 1:length(tmp)){
      DF[DF$year==year(as.Date(d.ts[d.ts$plotcode==plotcode,"date"]))[2]&DF$tree_tag==tmp[g],"mortality_code_alive_dead"]<-0
    }}
    tmp<-census[census$Flag1==0,"Tag"]
    if(length(tmp)>0) {for(g in 1:length(tmp)){
      DF[DF$year==year(as.Date(d.ts[d.ts$plotcode==plotcode,"date"]))[3]&DF$tree_tag==tmp[g],"mortality_code_alive_dead"]<-0
    }}
    DF$family<-census[match(DF$tree_tag,census$Tag),"NFam"]
    DF$genus<-str_split_fixed(as.character(census[match(DF$tree_tag,census$Tag),"NSpecies"])," ",2)[,1]
    DF$species<-str_split_fixed(as.character(census[match(DF$tree_tag,census$Tag),"NSpecies"])," ",2)[,2]
    DF$date<-as.Date(paste(DF$year,DF$month,"01",sep="-"),format="%Y-%m-%d")
    DF$Binomial<-paste(DF$genus,DF$species)
    DF$ID<-1:nrow(DF)
    
    #match wood density for species
    wds<-left_join(DF %>% select(family,Binomial),w.dens %>% filter(Region=="Africa (tropical)") %>% select(-family),by="Binomial")
    wdens<-wds %>% group_by(family,Binomial) %>% summarise(wood_density_g_m2=mean(wood_density_g_m2,na.rm=T)) %>% ungroup()
    #pull out genera from missing species
    wdens$genus <- str_split_fixed(wdens$Binomial," ",2)[,1]
    wdg<-left_join(wdens %>% select(genus),w.dens %>% select(family,genus,wood_density_g_m2),by="genus")
    wdg <- wdg %>% group_by(genus) %>% summarise(gen.wood_density = mean(wood_density_g_m2,na.rm=T))
    wdens <- left_join(wdens,wdg,by="genus")
    wdens <- wdens %>% mutate(wood_density_g_m2=replace(wood_density_g_m2,is.na(wood_density_g_m2),gen.wood_density[is.na(wood_density_g_m2)]))
    #pull out missing family
    wdf <- wdens %>% filter(is.na(wood_density_g_m2))
    wdf <- left_join(wdf %>% select(family),w.dens %>% select(family,wood_density_g_m2),by="family")
    wdf <- wdf %>% group_by(family) %>% summarise(fam.wood_density = mean(wood_density_g_m2,na.rm=T)) %>% ungroup()
    wdens <- left_join(wdens,wdf,by="family")
    wdens <- wdens %>% mutate(wood_density_g_m2=replace(wood_density_g_m2,is.na(wood_density_g_m2),fam.wood_density[is.na(wood_density_g_m2)])) %>%
      select(Binomial,wood_density_g_m2)
    
    DF <- left_join(DF,wdens,by="Binomial")
    
    final[[k]]<-DF
  }
  final.1<-do.call(rbind.data.frame,final)
  write.csv(final.1,paste0(getwd(),"/",site,"/NPP/Dendrometers/",trans[p],"_dendroAll_clean.csv"))
  
  #generate calculated heights for non-cocoa trees
  allob<-do.call(rbind.data.frame,allo)
  names(allob)<-c("plotname","TagNo","Species","dbh","dbh2","dbh3","height_m")
  allob$dbh<-as.numeric(as.character(allob$dbh))
  allob$height_m<-as.numeric(as.character(allob$height_m))
  allob<-allob[!is.na(allob$dbh),]
  allob<-allob[!is.na(allob$Species),]
  
  alloc<-allob[allob$Species!="Theobroma cacao",]
  #alloc<-alloc[!is.na(alloc$dbh),]
  w <- which(is.na(alloc$height_m))
  h.pred <- h.est(alloc$dbh, alloc$height_m)
  alloc$height_m[w] <- h.pred[w]
  
  allob[allob$Species!="Theobroma cacao","height_m"]<-alloc$height_m
  
  #do again for cocoa
  alloc<-allob[allob$Species=="Theobroma cacao",]
  alloc<-cbind(alloc[,1:4],alloc$height_m)
  colnames(alloc)<-c(colnames(alloc[,1:4]),"height_m")
  alloc<-rbind(alloc[,1:5],co.ht)
  alloc$dbh<-as.numeric(as.character(alloc$dbh))
  alloc$height_m<-as.numeric(as.character(alloc$height_m))
  w <- which(is.na(alloc$height_m))
  h.pred<-h.est(alloc$dbh, alloc$height_m)
  alloc$height_m[w] <- h.pred[w]
  alloc<-alloc[alloc$plotname!="XXX",]
  allob[allob$Species=="Theobroma cacao","height_m"]<-alloc$height_m
  write.csv(allob,paste0(getwd(),"/",site,"/NPP/Dendrometers/",trans[p],"_treeheights.csv"))
}

#add missing height values
for(p in 1:length(trans)){
  h.ts<-read.csv(paste0(getwd(),"/",site,"/NPP/Dendrometers/",trans[p],"_treeheights.csv"))
  d.nds<-read.csv(paste0(getwd(),"/",site,"/NPP/Dendrometers/",trans[p],"_dendroAll_clean.csv"))
  
  d.nds$tree_height_m<-h.ts[match(d.nds$tree_tag,h.ts$TagNo),"height_m"]
  
  write.csv(d.nds,paste0(getwd(),"/",site,"/NPP/Dendrometers/",trans[p],"_dendroAll_clean.csv"))
}
