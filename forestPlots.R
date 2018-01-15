#clean and export forest plot datasheets
library(stringr)
#require(data.table)
#library(plyr)
library(gdata)
library(tidyverse)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/")
site="Kakum"
#site="Yayu"
#load plotdata
plts<-read.csv(paste0(getwd(),"/",site,"/plots.csv"))
plts1<-plts[grep("FP",plts$name3,invert=T),]
#names of transects to loop through
#snames<-unique(plts$name)
snames=c("AB","HM","KA")
#what number census is this
censusnum<-3

#identify NPP plots
npp<-as.character(plts[plts$metstation==1,7])
#remove FP plots
npp<-npp[grep("FP",npp,invert=T)]

#load "correction" sheet
ctn<-data.frame(read.csv(paste0(getwd(),"/",site,"/AGB/spp.correction.csv")),stringsAsFactors=FALSE)
#remove NAs
ctn<-ctn[!is.na(ctn[,4]),]

#final<-list()
dates<-list()
for (i in 1:length(snames)){
  #load plot dump (if applicable) to get tree IDs
  pcode<-as.character(plts[plts$name2==paste0(snames[i]," FP"),"PlotCode"])
  pcode<-gsub("-","_",pcode)
  dump<-read.xls(paste0(getwd(),"/",site,"/AGB/ForestPlots/",pcode,"_PlotDump.xlsx"), sheet="Plot Dump")
  
  data<-read.xls(paste0(getwd(),"/",site,"/AGB/ForestPlots_LS.xlsx"), sheet=snames[i])
  #remove factors
  data<-data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
  #add column names
  colnames(data)<-as.character(data[data[,1]=="Subplot",])
  #remove heading rows
  data<-data[(which(data[,1]=="Subplot")+1):nrow(data),]
  
  p<-data.frame(cbind(data[,1:5],paste(data[,6],data[,7])))
  colnames(p)<-c("T1","x-coord","y-coord","Tag","OrigFam","OrigSpecies")
  #add TreeID (if applicable)
  p$TreeID<-dump[match(p$Tag,dump$Tag.No),"Tree.ID"]
  p[,"NFam"]<-as.character(ctn[match(p$OrigSpecies, as.character(ctn$Species)),"AccFamily"])
  p[,"NSpecies"]<-as.character(ctn[match(p$OrigSpecies, as.character(ctn$Species)),"AccName"])
  
  tmp<-colnames(data[,grep("Diameter",colnames(data))])
  p[,"DBH"]<-as.numeric(data[,grep(tmp[1],colnames(data))])*10
  p$POM<-as.numeric(data$POM)*1000
  for(k in 2:length(tmp)){
    #p[,paste0("census",k)]<-as.Date(gsub("Diameter ","",tmp[k]),format="%d/%m/%y")
    p[,paste0("DBH",k)]<-as.numeric(data[,grep(tmp[k],colnames(data))])*10
  }
  dates[[i]]<-cbind(gsub("Diameter ", "",tmp),as.character(plts[plts$name2==paste0(snames[i]," FP"),"PlotCode"]),paste0(snames[i]," FP"))
  #p$PlotNum<-paste0(snames[i]," FP")
  #add character for blank rows
  data[data$Height==""&!is.na(data$Height),"Height"]<-NA
  h<-do.call(rbind,strsplit(data$Height,","))
  if(ncol(h)==2) p$THeight<-as.numeric(gsub(" ","",h[,2])) else p$THeight<-as.numeric(h)
  
  #pull out flag values for all censuses
  flag.all<-data[,grep("Tree codes",colnames(data))]
  flag<-flag.all[,str_split_fixed(colnames(data[,grep("Tree codes",colnames(data))]),"Tree codes",2)[,2]==censusnum]
  
  #Flag 1 Alive status, if dead add "0" do for max Tree codes number
  p$Flag1<-as.character(data[,max(grep("Tree codes",colnames(data)))])
  #Flag 2 Mode of Death, if alive enter "1"
  p$Flag2<-1
  #add in dead trees
  tmp1<-grep("0",p$Flag1)
  p[tmp1,"Flag1"]<-0
  #extract cause of death
  p[tmp1,"Flag2"]<-do.call(rbind,strsplit(flag[tmp1],","))[,2]
  p[p$Flag2=="???","Flag2"]<-"m"
  #add Flag 3, measurement technique
  p$Flag3<-0
  #add ladder measurements
  tmp1<-grep(",4",flag)
  p[tmp1,"Flag3"]<-4
  #add Flag 4, Data Manipulation, 0 means normal measurement
  if(is.null(data$Flag4)) p$Flag4<-0 else p$Flag4<-data$Flag4
  #remove NA
  p<-p[,!is.na(p[1,])]
  #add Flag5 for mode of height measurement, 4 refers to laser hypsometer
  p[!is.na(p$THeight),"Flag5"]<-4
  #add Flag1.1 if tree died in previous census
  p$Flag1.1 <-as.character(data[,max(grep("Tree codes",colnames(data)))-1])
  #add in dead trees
  tmp1<-grep("0",p$Flag1.1)
  p[tmp1,"Flag1.1"]<-0
  
  p$CensusNotes<-data$Notes
  
  #write csv of cleaned data
  write.csv(p,paste0(getwd(),"/",site,"/AGB/ForestPlots/",snames[i],"_forest.csv"))
  #write.xls(p,paste0(getwd(),"/",site,"/AGB/ForestPlots/",snames[i],"FP_c1.",censusnum,".xlsx"),sheetName = paste0("census",censusnum),col.names=T,append=F)
  rm(p,h,data)
}

#do again for small stems
for (i in 1:length(snames)){
  data<-read.xls(paste0(getwd(),"/",site,"/AGB/ForestPlots_SS.xlsx"), sheet=snames[i])
  #remove factors
  data<-data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
  #add column names
  colnames(data)<-as.character(data[data[,1]=="Sub-plot",])
  #remove heading rows
  data<-data[(which(data[,1]=="Sub-plot")+1):nrow(data),]
  
  p<-data.frame(cbind(data[,1:5],paste(data[,6],data[,7])))
  colnames(p)<-c("T1","x-coord","y-coord","Tag","OrigFam","OrigSpecies")
  p[,"NFam"]<-as.character(ctn[match(p$OrigSpecies, as.character(ctn$Species)),"AccFamily"])
  p[,"NSpecies"]<-as.character(ctn[match(p$OrigSpecies, as.character(ctn$Species)),"AccName"])
  
  tmp<-colnames(data[,grep("Diam",colnames(data))])
  p[,"DBH"]<-as.numeric(data[,grep(tmp[1],colnames(data))])*10
  p$POM<-as.numeric(data$POM)*1000
  for(k in 2:length(tmp)){
    #p[,paste0("census",k)]<-as.Date(gsub("Diameter ","",tmp[k]),format="%d/%m/%y")
    p[,paste0("DBH",k)]<-as.numeric(data[,grep(tmp[k],colnames(data))])*10
  }
  #dates[[i]]<-cbind(gsub("Diameter ", "",tmp),as.character(plts[plts$name2==paste0(snames[i]," FP"),"PlotCode"]),paste0(snames[i]," FP"))
  #p$PlotNum<-paste0(snames[i]," FP")
  #add character for blank rows
  data[data$Height==""&!is.na(data$Height),"Height"]<-NA
  h<-do.call(rbind,strsplit(data$Height,","))
  if(ncol(h)==2) p$THeight<-as.numeric(gsub(" ","",h[,2])) else p$THeight<-as.numeric(h)
  #pull out flag values for all censuses
  flag.all<-data[,grep("Tree codes",colnames(data))]
  flag<-flag.all[,str_split_fixed(colnames(data[,grep("Tree codes",colnames(data))]),"Tree codes",2)[,2]==censusnum]
  
  #Flag 1 Alive status, if dead add "0" do for max Tree codes number
  p$Flag1<-as.character(data[,max(grep("Tree codes",colnames(data)))])
  #Flag 2 Mode of Death, if alive enter "1"
  p$Flag2<-1
  #add in dead trees
  tmp1<-grep("0",p$Flag1)
  p[tmp1,"Flag1"]<-0
  #extract cause of death
  p[tmp1,"Flag2"]<-do.call(rbind,strsplit(flag[tmp1],","))[,2]
  p[p$Flag2=="???","Flag2"]<-"m"
  #add Flag 3, measurement technique
  p$Flag3<-0
  #add ladder measurements
  tmp1<-grep(",4",flag)
  p[tmp1,"Flag3"]<-4
  #add Flag 4, Data Manipulation, 0 means normal measurement
  if(is.null(data$Flag4)) p$Flag4<-0 else p$Flag4<-data$Flag4
  #add Flag1.1 if tree died in previous census
  p$Flag1.1 <-as.character(data[,max(grep("Tree codes",colnames(data)))-1])
  #add in dead trees
  tmp1<-grep("0",p$Flag1.1)
  p[tmp1,"Flag1.1"]<-0
  
  #remove NA
  #p<-p[,!is.na(p[1,])]
  
  p$CensusNotes<-data$Notes
  #write csv of cleaned data
  write.csv(p,paste0(getwd(),"/",site,"/AGB/ForestPlots/",snames[i],"_forestSS.csv"))
  rm(h,p,data)
}

#do for Cocoa trees
heights<-list()
for (i in 1:nrow(plts1)){
  data<-read.xls(paste0(getwd(),"/",site,"/AGB/CocoaPlots_LS.xlsx"), sheet=plts1$name3[i])
  #pcode<-as.character(plts[plts$name2==paste0(snames[i]," FP"),"PlotCode"])
  #pcode<-gsub("-","_",pcode)
  #dump<-read.xlsx(paste0(getwd(),"/",site,"/AGB/ForestPlots/",pcode,"_PlotDump.xlsx"), sheetName="Plot Dump", header=TRUE,startRow=2)
  
  data<-data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
  #add column names
  colnames(data)<-as.character(data[data[,1]=="Subplot",])
  #remove heading rows
  data<-data[(which(data[,1]=="Subplot")+1):nrow(data),]
  
  p<-data.frame(cbind(data[,1:5],paste(data[,6],data[,7])))
  colnames(p)<-c("T1","x-coord","y-coord","Tag","OrigFam","OrigSpecies")
  #add TreeID (if applicable)
  #p$TreeID<-dump[match(p$Tag,dump$Tag.No),"Tree.ID"]
  p[,"NFam"]<-as.character(ctn[match(p$OrigSpecies, as.character(ctn$Species)),"AccFamily"])
  p[,"NSpecies"]<-as.character(ctn[match(p$OrigSpecies, as.character(ctn$Species)),"AccName"])
  
  tmp<-colnames(data[,grep("Diameter",colnames(data))])
  p[,"DBH"]<-as.numeric(data[,grep(tmp[1],colnames(data))])*10
  p$POM<-as.numeric(data$POM)*1000
  for(k in 2:length(tmp)){
    #p[,paste0("census",k)]<-as.Date(gsub("Diameter ","",tmp[k]),format="%d/%m/%y")
    p[,paste0("DBH",k)]<-as.numeric(data[,grep(tmp[k],colnames(data))])*10
  }
  dates[[i+3]]<-cbind(gsub("Diameter ", "",tmp),as.character(plts1[plts1$name3==plts1$name3[i],"PlotCode"]),as.character(plts1$name3[i]))
  #p$PlotNum<-paste0(snames[i]," FP")
  #add character for blank rows
  data[data$Height==""&!is.na(data$Height),"Height"]<-NA
  h<-do.call(rbind,strsplit(data$Height,","))
  if(ncol(h)==2) p$THeight<-as.numeric(gsub(" ","",h[,2])) else p$THeight<-as.numeric(h)
  
  #pull out flag values for latest census
  flag.all<-data[,grep("Tree codes",colnames(data))]
  flag<-flag.all[,str_split_fixed(colnames(data[,grep("Tree codes",colnames(data))]),"Tree codes",2)[,2]==censusnum]
  
  #Flag 1 Alive status, if dead add "0" do for max Tree codes number
  p$Flag1<-as.character(data[,max(grep("Tree codes",colnames(data)))])
  #Flag 2 Mode of Death, if alive enter "1"
  p$Flag2<-1
  #add in dead trees
  tmp1<-grep("0",p$Flag1)
  p[tmp1,"Flag1"]<-0
  #extract cause of death
  p[tmp1,"Flag2"]<-do.call(rbind,strsplit(flag[tmp1],","))[,2]
  p[p$Flag2=="???","Flag2"]<-"m"
  #add Flag 3, measurement technique
  p$Flag3<-0
  #add ladder measurements
  tmp1<-grep(",4",flag)
  p[tmp1,"Flag3"]<-4
  #add Flag 4, Data Manipulation, 0 means normal measurement
  if(is.null(data$Flag4)) p$Flag4<-0 else p$Flag4<-data$Flag4
  #remove NA
  #p<-p[,!is.na(p[1,])]
  #add Flag5 for mode of height measurement, 4 refers to laser hypsometer
  p[!is.na(p$THeight),"Flag5"]<-4
  #add Flag1.1 if tree died in previous census
  p$Flag1.1 <-as.character(data[,max(grep("Tree codes",colnames(data)))-1])
  #add in dead trees
  tmp1<-grep("0",p$Flag1.1)
  p[tmp1,"Flag1.1"]<-0
  
  p$CensusNotes<-data$Notes
  
  #pull out measured heights and DBH for cocoa
  h<-data.frame(cbind(as.numeric(p[p$NSpecies=="Theobroma cacao","DBH"]),p[p$NSpecies=="Theobroma cacao","THeight"]))
  heights[[i]]<-h[!is.na(h$X2)&!is.na(h$X1),]
  #remove NA
  #p<-p[,!is.na(p[1,])]
 
  #write csv of cleaned data
  write.csv(p,paste0(getwd(),"/",site,"/AGB/ForestPlots/",gsub(" ","",plts1$name3[i]),"_LS.csv")) 
  #write.xlsx(p,paste0(getwd(),"/",site,"/AGB/ForestPlots/",gsub(" ","",plts1$name3[i]),"_c1.",censusnum,".xlsx"),sheetName = paste0("census",censusnum),col.names=T,append=F)
  #write.csv(test,paste0(getwd(),"/",site,"/AGB/test_spp2.csv")) 
}

ht<-do.call(rbind.data.frame,heights)
colnames(ht)<-c("dbh","h")
ht$dbh<-as.numeric(as.character(ht$dbh))
ht$h<-as.numeric(as.character(ht$h))
ht<-ht[!is.na(ht$h),]
#calculate height relationship for cocoa
h.ght<-coefficients(lm(ht$h~ht$dbh))
#save relationship
write.csv(h.ght,paste0(getwd(),"/",site,"/AGB/ForestPlots/cocoa_height.csv"))
#save dates
d.ts<-do.call(rbind.data.frame,dates)
#d.ts<-d.ts[,2:4]
colnames(d.ts)<-c("date","plotcode","plotname")
d.ts$date<-as.Date(d.ts$date,format="%d/%m/%y")
write.csv(d.ts,paste0(getwd(),"/",site,"/AGB/ForestPlots/census.dates.csv"))

#do for Cocoa census of small trees
for(i in 1:nrow(plts1)){
  data<-read.xls(paste0(getwd(),"/",site,"/AGB/CocoaPlots_SS.xlsx"), sheet=plts1$name3[i])
  #remove factors
  data<-data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
  #add column names
  colnames(data)<-as.character(data[data[,1]=="Sub-plot",])
  #remove heading rows
  data<-data[(which(data[,1]=="Sub-plot")+1):nrow(data),]
  
  p<-data.frame(cbind(data[,1:5],paste(data[,6],data[,7])))
  colnames(p)<-c("T1","x-coord","y-coord","Tag","OrigFam","OrigSpecies")
  p[,"NFam"]<-as.character(ctn[match(p$OrigSpecies, as.character(ctn$Species)),"AccFamily"])
  p[,"NSpecies"]<-as.character(ctn[match(p$OrigSpecies, as.character(ctn$Species)),"AccName"])
  
  tmp<-colnames(data[,grep("Diam",colnames(data))])
  p[,"DBH"]<-as.numeric(data[,grep(tmp[1],colnames(data))])*10
  p$POM<-as.numeric(data$POM)*1000
  for(k in 2:length(tmp)){
    #p[,paste0("census",k)]<-as.Date(gsub("Diameter ","",tmp[k]),format="%d/%m/%y")
    p[,paste0("DBH",k)]<-as.numeric(data[,grep(tmp[k],colnames(data))])*10
  }
  #dates[[i]]<-cbind(gsub("Diameter ", "",tmp),as.character(plts[plts$name2==paste0(snames[i]," FP"),"PlotCode"]),paste0(snames[i]," FP"))
  #p$PlotNum<-paste0(snames[i]," FP")
  #add character for blank rows
  data[data$Height==""&!is.na(data$Height),"Height"]<-NA
  h<-do.call(rbind,strsplit(data$Height,","))
  if(ncol(h)==2) p$THeight<-as.numeric(gsub(" ","",h[,2])) else p$THeight<-as.numeric(h)
  
  #pull out flag values for latest census
  flag.all<-data[,grep("Tree codes",colnames(data))]
  flag<-flag.all[,str_split_fixed(colnames(data[,grep("Tree codes",colnames(data))]),"Tree codes",2)[,2]==censusnum]
  
  #Flag 1 Alive status, if dead add "0" do for max Tree codes number
  p$Flag1<-as.character(data[,max(grep("Tree codes",colnames(data)))])
  #Flag 2 Mode of Death, if alive enter "1"
  p$Flag2<-1
  #add in dead trees
  tmp1<-grep("0",p$Flag1)
  p[tmp1,"Flag1"]<-0
  #extract cause of death
  p[tmp1,"Flag2"]<-do.call(rbind,strsplit(flag[tmp1],","))[,2]
  p[p$Flag2=="???","Flag2"]<-"m"
  #add Flag 3, measurement technique
  p$Flag3<-0
  #add ladder measurements
  tmp1<-grep(",4",flag)
  p[tmp1,"Flag3"]<-4
  #add Flag 4, Data Manipulation, 0 means normal measurement
  if(is.null(data$Flag4)) p$Flag4<-0 else p$Flag4<-data$Flag4
  
  #add Flag1.1 if tree died in previous census
  p$Flag1.1 <-as.character(data[,max(grep("Tree codes",colnames(data)))-1])
  #add in dead trees
  tmp1<-grep("0",p$Flag1.1)
  p[tmp1,"Flag1.1"]<-0
  
  #remove NA
  #p<-p[,!is.na(p[1,])]
  
  p$CensusNotes<-data$Notes
  #write csv of cleaned data
  write.csv(p,paste0(getwd(),"/",site,"/AGB/ForestPlots/",gsub(" ","",plts1$name3[i]),"_SS.csv"))
  rm(h,p,data)
}

