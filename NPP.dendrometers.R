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

#open census dates
d.ts<-read.csv(paste0(getwd(),"/",site,"/AGB/ForestPlots/census.dates.csv"))

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
    DF[,4:5]<-cbind(dF$Tag,dF$Diameter)
    DF[,6:7]<-c(0,0)
    #pull out dates of dendro readings
    ind2<-grep("Growth",colnames(dF))
    ds<-unlist(strsplit(names(dF[,ind2]),"Growth"))
    #remove blanks
    ds<-gsub(" ","",ds[ds!=""])
    
    d1<-as.Date(as.character(d.ts[d.ts$plotname==plotname,"date"]),format="%d/%m/%y")
    
    #load census data to get height and DBH values
    if(length(grep("FP",plotname))==0) census<-read.csv(paste0(getwd(),"/",site,"/AGB/ForestPlots/",gsub(" ","",plotname),"_LS.csv")) else census<-read.csv(paste0(getwd(),"/",site,"/AGB/ForestPlots/",trans[p],"_forest.csv"))
    #colnames(census)<-c(colnames(census[,1:8]),"DBH0","DBH1",colnames(census[,11:ncol(census)]))
    DF[,8]<-year(d1[1])
    DF[,9]<-month(d1[1])
    DF[,10]<-day(d1[1])
    allo[[k]]<-cbind(as.character(plotname),as.character(census$Tag),as.character(census$NSpecies),census[,grep("DBH",colnames(census))]/10,as.numeric(census$THeight))
    names(DF)<-c("plotname","plot_code","subplot","tree_tag","dbh","dendrometer_reading_mm_cum","dendrometer_reading_mm","year","month","day")
     #remove NA DBH
    census<-census[!is.na(census$DBH),]
    for(j in 1:(length(ds))){
      #d[j,"date"]<-as.Date(substr(ds[j],2,9),format="%d/%m/%y")
      d<-as.Date(substr(ds[j],2,9),format="%d/%m/%y")
      DF[(j*nrow(dF)+1):((j+1)*nrow(dF)),1:3]<-cbind(dF$plotname,dF$plot_code,dF$Subplot)
      DF[(j*nrow(dF)+1):((j+1)*nrow(dF)),4]<-dF$Tag
      DF[(j*nrow(dF)+1):((j+1)*nrow(dF)),5]<-as.numeric(DF[((j-1)*nrow(dF)+1):(j*nrow(dF)),"dbh"])
      DF[(j*nrow(dF)+1):((j+1)*nrow(dF)),6]<-as.numeric(dF[,ind2[j]])*10
      if(j==1) DF[(j*nrow(dF)+1):((j+1)*nrow(dF)),7]<-as.numeric(dF[,ind2[j]])*10 else DF[(j*nrow(dF)+1):((j+1)*nrow(dF)),7]<-(as.numeric(dF[,ind2[j]])-as.numeric(dF[,ind2[j-1]]))*10
      DF[(j*nrow(dF)+1):((j+1)*nrow(dF)),8]<-year(d)
      DF[(j*nrow(dF)+1):((j+1)*nrow(dF)),9]<-month(d)
      DF[(j*nrow(dF)+1):((j+1)*nrow(dF)),10]<-day(d)
    }
    #colnames(dF[,(ncol(dF)-j+1):ncol(dF)])=c(paste0("date",1:j))
    #colnames(dF)<-c(colnames(dF[,1:(ncol(dF)-j)]),paste0("date",1:j))
    
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


