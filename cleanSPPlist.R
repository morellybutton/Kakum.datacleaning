#code to check all latin names on tree census datasheet and produce correction sheet

library(stringr)
require(data.table)
library(taxize)
library(plyr)
library(gdata)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/")
site="Kakum"
#load plot data
plts<-read.csv(paste0(getwd(),"/",site,"/plots.csv"))
#names of transects to loop through
snames=c("AB","HM","KA")

#identify NPP plots
npp<-as.character(plts[plts$metstation==1,7])
#remove FP plots
#npp<-npp[grep("FP",npp,invert=T)]

#collect all species name from plots to correct
SPP<-list()
for (i in 1:length(snames)){
  data<-read.xls(paste0(getwd(),"/",site,"/AGB/ForestPlots_LS.xlsx"), sheet=snames[i])
  data.c<-read.xls(paste0(getwd(),"/",site,"/AGB/ForestPlots_SS.xlsx"), sheet=snames[i])
  
  #remove factors
  data<-data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
  data.c<-data.frame(lapply(data.c, as.character), stringsAsFactors=FALSE)
  #add column names
  colnames(data)<-as.character(data[data[,1]=="Subplot",])
  colnames(data.c)<-as.character(data.c[data.c[,1]=="Sub-plot",])
  #remove heading rows
  data<-data[(which(data[,1]=="Subplot")+1):nrow(data),]
  data.c<-data.c[(which(data.c[,1]=="Sub-plot")+1):nrow(data.c),]
  
  p<-data
  p2<-data.c
  
  #combine two columns for species
  spp<-data.frame(do.call(paste, p[,6:7]),stringsAsFactors=FALSE)
  colnames(spp)<-"OrigSpecies"
  spp$OrigFamily<-p$Fam
  spp[(nrow(spp)+1):(nrow(spp)+nrow(p2)),1:2]<-cbind(do.call(paste,p2[,6:7]),p2$Fam)
  
  #find unique species & families
  spp.1<-data.frame(unique(spp[,1]))
  spp.1$Fam<-spp[match(spp.1[,1],spp$OrigSpecies),"OrigFamily"]
  colnames(spp.1)<-c("Species","Family")
  #remove blanks
  spp.1<-spp.1[spp.1$Species!=" ",]
  SPP[[i]]<-spp.1
}

#combine all species
SPP.1<-do.call(rbind.data.frame,SPP)

#do again for cocoa species
#get plot names
xs<-as.character(plts$name3)
xs<-xs[grep("FP",xs,invert=T)]
SPP<-list()
for (i in 1:length(xs)){
  data<-read.xls(paste0(getwd(),"/",site,"/AGB/CocoaPlots_LS.xlsx"), sheet=xs[i])
  data.c<-read.xls(paste0(getwd(),"/",site,"/AGB/CocoaPlots_SS.xlsx"), sheet=xs[i])
  
  #remove factors
  data<-data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
  data.c<-data.frame(lapply(data.c, as.character), stringsAsFactors=FALSE)
  
  #add column names
  colnames(data)<-as.character(data[data[,1]=="Subplot",])
  colnames(data.c)<-as.character(data.c[data.c[,1]=="Sub-plot",])
  #remove heading rows
  data<-data[(which(data[,1]=="Subplot")+1):nrow(data),]
  data.c<-data.c[(which(data.c[,1]=="Sub-plot")+1):nrow(data.c),]
  
  p<-data
  p2<-data.c
  
  #combine two columns for species
  spp<-data.frame(do.call(paste, p[,6:7]),stringsAsFactors=FALSE)
  colnames(spp)<-"OrigSpecies"
  spp$OrigFamily<-p$Fam
  
  #find unique species & families
  spp.1<-data.frame(unique(spp[,1]))
  spp.1$Fam<-spp[match(spp.1[,1],spp$OrigSpecies),"OrigFamily"]
  colnames(spp.1)<-c("Species","Family")
  #remove blanks
  spp.1<-spp.1[spp.1$Species!=" ",]
  SPP[[i]]<-spp.1
}

#combine all species
SPP.2<-do.call(rbind.data.frame,SPP)

SPP<-rbind(SPP.1,SPP.2)
#find unique species & families
spp<-data.frame(unique(SPP[,1]))
spp$Fam<-SPP[match(spp[,1],SPP$Species),"Family"]
colnames(spp)<-c("Species","Family")
spp$acceptedname<-NA

#find family & accepted name
for(i in 1:nrow(spp)){
  tmp1<-tnrs(query=as.character(spp[i,1]), source="iPlant_TNRS")[ , -c(5:7)]
  if(length(tmp1$acceptedname)==0) tmp1<-tnrs(query=strsplit(SPP[i,2]," ")[[1]][1],source="iPlant_TNRS")[ , -c(5:7)]
  spp[i,"acceptedname"]<-tmp1$acceptedname
  tmp<-tax_name(query=SPP[i,1], db = "ncbi", get="family")
  if(is.na(tmp$family)) tmp<-tax_name(query=tmp1$acceptedname, db = "ncbi", get="family")
  if(is.na(tmp$family)) spp[i,"check"]<-"DNE" else spp[i,"AccFamily"]<-tmp$family
  if(is.na(tmp$family)) spp[i,"NewSpp"]<- paste(strsplit(spp[i,1]," ")[[1]][1],"spp.")
  if(is.na(tmp$family)) tmp<-tax_name(query=strsplit(as.character(spp[i,1])," ")[[1]][1], db = "ncbi", get="family")
  spp[i,"AccName"]<-tmp1$acceptedname
}

#write csv to correct
write.csv(spp,paste0(getwd(),"/",site,"/AGB/spp.correction.csv"))