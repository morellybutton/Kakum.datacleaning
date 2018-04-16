#create figures of microclimate data
library(gdata)
#library(stringr)
library(gridExtra)
#library(Rmisc)
#library(ggplot2)
library(zoo)
library(tidyverse)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/MetData")

names<-data.frame(read.csv(paste0(getwd(),"/MS_names.csv")), stringsAsFactors=FALSE)
ns<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/plots.csv")
dnames<-data.frame(read.csv(paste0(getwd(),"/DL_names.csv")), stringsAsFactors=FALSE)

#canopy measures
canopy<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/NPP/LAI/Combine_gap_analysis.csv")
#remove NAs
canopy<-canopy[!is.na(canopy$Plot),]
#create monthly sums and averages
#figure ppt
summ<-read.csv(paste0(getwd(),"/",as.character(names[as.character(names$PlotNum)=="LargeMetStation","Mstation"]),"_summary.csv"))
#remove NAs
summ<-summ[!is.na(summ$day),]
#organize by month
summ$month<-as.Date(cut(as.Date(summ$day),breaks="month"))
#check for missing dates
daycount <- table(summ$day) 

# generate vector of all dates[taken from http://www.r-bloggers.com/fix-missing-dates-with-r/]
alldays <- seq(as.Date(min(summ$month,na.rm=T)),length=as.numeric(as.Date(summ[!is.na(summ$day),"day"][length(summ[!is.na(summ$day),1])])-as.Date(min(summ$month,na.rm=T))+1),by="+1 day")  
allcount <- table(alldays) # create table object from alldays.
actindex <- match(names(allcount),names(daycount),nomatch = 0)  
# create "active" index: vector of length(allcount), i.e. all days. 
# on days with no activity (i.e. a missing day in daycount), this has value 0 (nomatch = 0). 
# For days with activity, actindex holds the index of the matching position in daycount.
# function to get entries of daycount corresponding to actindex
# indexing is a bit tricky. i loops over all days. get correct date by
# substracting all "zero-activity" days accumulated so far.
days <- function(actindex,daycount){
  n <- length(actindex)
  x <- rep(NA,times=n)
  zero <- 0
  for (i in 1:n){
    if (actindex[i]==0) {
      zero <- zero +1
      x[i] <- 0
    } else {
      x[i] <- daycount[i-zero]
    }			
  }
  return(x)
}

alldaycount <- data.frame(days(actindex,daycount))   # construct vector with number of hits per day
alldaycount[,2] <- names(allcount)           # name entries by consecutive dates.

#for all days where ppt data is missing put daily average for month
ds<-data.frame(alldaycount[alldaycount[,1]==0,2])
colnames(ds)<-"day"
#add month
ds$month<-as.Date(cut(as.Date(ds[,1]),breaks="month"))
m<-unique(ds$month)
for(i in 1:length(m)){
  ds[ds$month==m[i],"avg"]<-mean(summ[summ$month==m[i],"Tppt"],na.rm=T)
}
#add average ppt for missing points with daily ppt measures
alldaycount[match(as.Date(summ$day),as.Date(alldaycount$V2),nomatch =0),"Tppt"]<-summ$Tppt
alldaycount[match(ds$day,alldaycount$V2),"Tppt"]<-ds$avg
#write daily ppt
write.csv(alldaycount,paste0(getwd(),"/LargeMetstation_dailyppt.csv"))
rm(ds)
#take monthly sums
alldaycount$month<-as.Date(cut(as.Date(alldaycount$V2),breaks="month"))
#m<-data.frame(unique(alldaycount$month))

m<-data.frame(aggregate(alldaycount$Tppt,list(alldaycount$month),FUN="sum"))
colnames(m)<-c("month","Tppt")
n<-data.frame(aggregate(summ[,2:68],list(summ$month),FUN="mean",na.action="na.omit"))
#m[,3:69]<-n[,2:68]
#create figure of monthly ppt
grid1<-ggplot(m,aes(m$month,m$Tppt))+geom_line()+xlab("Month")+ylab("Monthly Precipitation (cm)")+ggtitle("Monthly Precipitation")+theme(
  plot.background = element_blank()
  ,panel.background = element_blank()
  ,panel.grid.major = element_blank()
  ,panel.grid.minor = element_blank()
  ,panel.border = element_blank()
  ,axis.line.x = element_line(color = 'black')
  ,axis.line.y = element_line(color = 'black')
  ,text = element_text(size = 14)
  ,axis.text.x=element_text(angle = 45,hjust=1))
#ggplot(m,aes(m$month,m$Tppt))+geom_bar(stat="identity")+xlab("Month")+ylab("Monthly Precipitation (cm)")+ggtitle("Monthly Precipitation")
ggsave(paste0(getwd(),"/Figures/LargeMetstation_ppt.pdf"),grid1)
#save corrected ppt
write.csv(m,paste0(getwd(),"/LargeMetstation_ppt.csv"))

#do with running mean
ggplot(m,aes(month,Tppt))+geom_line(color="grey",linetype="dashed")+geom_line(aes(y=rollmean(Tppt, 3, na.pad=TRUE)))+xlab("Month")+ylab("Monthly Precipitation (cm)")+ggtitle("Monthly Precipitation")+theme(
  plot.background = element_blank()
  ,panel.background = element_blank()
  ,panel.grid.major = element_blank()
  ,panel.grid.minor = element_blank()
  ,panel.border = element_blank()
  ,axis.line.x = element_line(color = 'black')
  ,axis.line.y = element_line(color = 'black')
  ,text = element_text(size = 14)
  ,axis.text.x=element_text(angle = 45,hjust=1))
#ggplot(m,aes(m$month,m$Tppt))+geom_bar(stat="identity")+xlab("Month")+ylab("Monthly Precipitation (cm)")+ggtitle("Monthly Precipitation")
ggsave(paste0(getwd(),"/Figures/LargeMetstation_ppt_3monthlymeans.pdf"))

#create dataframes for each variable
AH<-list() #absolute humidity
VPD<-list() #vapour pressure deficit
mVPD<-list() #max vapour pressure deficit
VWC<-list() #volumetric water content, soil
ST<-list() #soil temperature
mxST<-list() #max soil temperature
mnST<-list() #min soil temperature
TMAX<-list() #max temp
TMIN<-list() #min temp
TAVG<-list() #avg temp

#for metstations
for(i in 1:(length(names[,1])-1)){
  dF<-read.csv(paste0(getwd(),"/",names[i,"Mstation"],"_summary.csv"))
  #remove NAs
  dF<-dF[!is.na(dF$day),]
  #replace -Inf and & Inf
  dF[dF=="-Inf"&!is.na(dF)]<-NA
  dF[dF=="Inf"&!is.na(dF)]<-NA
  #only take day averages (t3,t4,t5,t6)
  #check if have t2soilT.1
  if(length(dF[,grep("t2soilT.1",colnames(dF))])>0) dF.2<-cbind(dF$Tmax,dF$Tmin,dF$SoilTmax,dF$SoilTmin,dF$VPDmax,dF$day,dF[,grep("t3",colnames(dF))],dF[,grep("t2soilT.1",colnames(dF))],dF[,grep("t4",colnames(dF))],dF[,grep("t5",colnames(dF))],dF[,grep("t6",colnames(dF))]) else dF.2<-cbind(dF$Tmax,dF$Tmin,dF$Soiltmax,dF$Soiltmin,dF$VPDmax,dF$day,dF[,grep("t3",colnames(dF))],dF[,grep("t4",colnames(dF))],dF[,grep("t5",colnames(dF))],dF[,grep("t6",colnames(dF))])
  
  #add month
  dF.2$month<-as.Date(cut(as.Date(dF.2[,6]),breaks="month"))
  
  dF.3<-aggregate(dF.2[,grep("ah",colnames(dF.2))],list(dF.2$month),FUN="mean",na.rm=T)
  dF.3$avg<-rowMeans(dF.3[,2:5],na.rm=T)
  dF.3$name<-names[i,"Mstation"]
  dF.3$PlotCode<-as.character(ns[ns$name2==as.character(names[i,"Plot.1"]),"PlotCode"])
  if(length(ns[ns$name2==as.character(names[i,"Plot.1"]),"distance"])==0) dF.3$distance<-ns[ns$name==as.character(names[i,"Plot.1"]),"distance"] else dF.3$distance<-ns[ns$name2==as.character(names[i,"Plot.1"]),"distance"]
  AH[[i]]<-cbind(dF.3$Group.1,dF.3[,6:9])
  rm(dF.3)
  dF.3<-aggregate(dF.2[,grep("vpd",colnames(dF.2))],list(dF.2$month),FUN="mean",na.rm=T)
  dF.3$avg<-rowMeans(dF.3[,2:5],na.rm=T)
  dF.3$name<-names[i,"Mstation"]
  dF.3$PlotCode<-as.character(ns[ns$name2==as.character(names[i,"Plot.1"]),"PlotCode"])
  if(length(ns[ns$name2==as.character(names[i,"Plot.1"]),"distance"])==0) dF.3$distance<-ns[ns$name==as.character(names[i,"Plot.1"]),"distance"] else dF.3$distance<-ns[ns$name2==as.character(names[i,"Plot.1"]),"distance"]
  VPD[[i]]<-cbind(dF.3$Group.1,dF.3[,6:9])
  rm(dF.3)
  dF.3<-aggregate(dF.2[,5],list(dF.2$month),FUN="max")
  dF.3$name<-names[i,"Mstation"]
  dF.3$PlotCode<-as.character(ns[ns$name2==as.character(names[i,"Plot.1"]),"PlotCode"])
  if(length(ns[ns$name2==as.character(names[i,"Plot.1"]),"distance"])==0) dF.3$distance<-ns[ns$name==as.character(names[i,"Plot.1"]),"distance"] else dF.3$distance<-ns[ns$name2==as.character(names[i,"Plot.1"]),"distance"]
  mVPD[[i]]<-cbind(dF.3$Group.1,dF.3[,2:5])
  dF.3<-aggregate(dF.2[,grep("vwc",colnames(dF.2))],list(dF.2$month),FUN="mean",na.rm=T)
  dF.3$avg<-rowMeans(dF.3[,2:5],na.rm=T)
  dF.3$name<-names[i,"Mstation"]
  dF.3$PlotCode<-as.character(ns[ns$name2==as.character(names[i,"Plot.1"]),"PlotCode"])
  if(length(ns[ns$name2==as.character(names[i,"Plot.1"]),"distance"])==0) dF.3$distance<-ns[ns$name==as.character(names[i,"Plot.1"]),"distance"] else dF.3$distance<-ns[ns$name2==as.character(names[i,"Plot.1"]),"distance"]
  VWC[[i]]<-cbind(dF.3$Group.1,dF.3[,6:9])
  rm(dF.3)
  dF.3<-aggregate(dF.2[,grep("soilT",colnames(dF.2))],list(dF.2$month),FUN="mean",na.rm=T)
  dF.3$avg<-rowMeans(dF.3[,2:5],na.rm=T)
  dF.3$name<-names[i,"Mstation"]
  dF.3$PlotCode<-as.character(ns[ns$name2==as.character(names[i,"Plot.1"]),"PlotCode"])
  if(length(ns[ns$name2==as.character(names[i,"Plot.1"]),"distance"])==0) dF.3$distance<-ns[ns$name==as.character(names[i,"Plot.1"]),"distance"] else dF.3$distance<-ns[ns$name2==as.character(names[i,"Plot.1"]),"distance"]
  ST[[i]]<-cbind(dF.3$Group.1,dF.3[,6:9])   
  rm(dF.3)
  dF.3<-aggregate(dF.2[,3],list(dF.2$month),FUN="max",na.rm=T)
  dF.3$name<-names[i,"Mstation"]
  dF.3$PlotCode<-as.character(ns[ns$name2==as.character(names[i,"Plot.1"]),"PlotCode"])
  if(length(ns[ns$name2==as.character(names[i,"Plot.1"]),"distance"])==0) dF.3$distance<-ns[ns$name==as.character(names[i,"Plot.1"]),"distance"] else dF.3$distance<-ns[ns$name2==as.character(names[i,"Plot.1"]),"distance"]
  mxST[[i]]<-cbind(dF.3$Group.1,dF.3[,2:5])
  rm(dF.3)
  dF.3<-aggregate(dF.2[,4],list(dF.2$month),FUN="min",na.rm=T)
  dF.3$name<-names[i,"Mstation"]
  dF.3$PlotCode<-as.character(ns[ns$name2==as.character(names[i,"Plot.1"]),"PlotCode"])
  if(length(ns[ns$name2==as.character(names[i,"Plot.1"]),"distance"])==0) dF.3$distance<-ns[ns$name==as.character(names[i,"Plot.1"]),"distance"] else dF.3$distance<-ns[ns$name2==as.character(names[i,"Plot.1"]),"distance"]
  mnST[[i]]<-cbind(dF.3$Group.1,dF.3[,2:5])
  rm(dF.3)
  dF.3<-aggregate(dF.2[,grep("Tmax",colnames(dF.2))],list(dF.2$month),FUN="mean",na.rm=T)
  dF.3$name<-names[i,"Mstation"]
  dF.3$PlotCode<-as.character(ns[ns$name2==as.character(names[i,"Plot.1"]),"PlotCode"])
  if(length(ns[ns$name2==as.character(names[i,"Plot.1"]),"distance"])==0) dF.3$distance<-ns[ns$name==as.character(names[i,"Plot.1"]),"distance"] else dF.3$distance<-ns[ns$name2==as.character(names[i,"Plot.1"]),"distance"]
  TMAX[[i]]<-cbind(dF.3$Group.1,dF.3[,2:5])
  rm(dF.3)
  dF.3<-aggregate(dF.2[,grep("Tmin",colnames(dF.2))],list(dF.2$month),FUN="mean")
  dF.3$name<-names[i,"Mstation"]
  dF.3$PlotCode<-as.character(ns[ns$name2==as.character(names[i,"Plot.1"]),"PlotCode"])
  if(length(ns[ns$name2==as.character(names[i,"Plot.1"]),"distance"])==0) dF.3$distance<-ns[ns$name==as.character(names[i,"Plot.1"]),"distance"] else dF.3$distance<-ns[ns$name2==as.character(names[i,"Plot.1"]),"distance"]
  TMIN[[i]]<-cbind(dF.3$Group.1,dF.3[,2:5])
  dF.3<-aggregate(dF.2[,grep("Tavg",colnames(dF.2))],list(dF.2$month),FUN="mean",na.rm=T)
  dF.3$avg<-rowMeans(dF.3[,2:5],na.rm=T)
  dF.3$name<-names[i,"Mstation"]
  dF.3$PlotCode<-as.character(ns[ns$name2==as.character(names[i,"Plot.1"]),"PlotCode"])
  if(length(ns[ns$name2==as.character(names[i,"Plot.1"]),"distance"])==0) dF.3$distance<-ns[ns$name==as.character(names[i,"Plot.1"]),"distance"] else dF.3$distance<-ns[ns$name2==as.character(names[i,"Plot.1"]),"distance"]
  TAVG[[i]]<-cbind(dF.3$Group.1,dF.3[,6:9])    
}

#add dataloggers
#remove dataloggers that do not have reliable data (__NCRC)
#dnames<-dnames[grep("__NCRC",as.character(dnames$Datalogger.id),invert=T),]
for(i in 1:(length(dnames[,1]))){
  dF<-read.csv(paste0(getwd(),"/dataloggers/summary/",dnames[i,"Datalogger.id"],"_summary.csv"))
  #remove NAs
  dF<-dF[!is.na(dF$day),]
  
  #replace -Inf and & Inf
  dF[dF=="-Inf"&!is.na(dF)]<-NA
  dF[dF=="Inf"&!is.na(dF)]<-NA
  
  #only take day averages (t3,t4,t5,t6)
  #check if have t2soilT.1
  #if(length(dF[,grep("t2soilT.1",colnames(dF))])>0) dF.2<-cbind(dF$Tmax,dF$Tmin,dF$day,dF[,grep("t3",colnames(dF))],dF[,grep("t2soilT.1",colnames(dF))],dF[,grep("t4",colnames(dF))],dF[,grep("t5",colnames(dF))],dF[,grep("t6",colnames(dF))]) else 
  dF.2<-cbind(dF$Tmax,dF$Tmin,dF$VPDmax,dF$day,dF[,grep("t3",colnames(dF))],dF[,grep("t4",colnames(dF))],dF[,grep("t5",colnames(dF))],dF[,grep("t6",colnames(dF))])
  
  #add month
  dF.2$month<-as.Date(cut(as.Date(dF.2[,4]),breaks="month"))
  dF.3<-aggregate(dF.2[,grep("ah",colnames(dF.2))],list(dF.2$month),FUN="mean",na.rm=T)
  dF.3$avg<-rowMeans(dF.3[,2:5],na.rm=T)
  dF.3$name<-dnames[i,"Datalogger.id"]
  dF.3$PlotCode<-as.character(ns[ns$name2==as.character(dnames[i,"Plot.1"]),"PlotCode"])
  if(length(ns[ns$name2==as.character(dnames[i,"Plot.1"]),"distance"])==0) dF.3$distance<-ns[ns$name==as.character(dnames[i,"Plot.1"]),"distance"] else dF.3$distance<-ns[ns$name2==as.character(dnames[i,"Plot.1"]),"distance"]
  AH[[i+10]]<-cbind(dF.3$Group.1,dF.3[,6:9]) 
  rm(dF.3)
  
  dF.3<-aggregate(dF.2[,grep("vpd",colnames(dF.2))],list(dF.2$month),FUN="mean",na.rm=T)
  dF.3$avg<-rowMeans(dF.3[,2:5],na.rm=T)
  dF.3$name<-dnames[i,"Datalogger.id"]
  dF.3$PlotCode<-as.character(ns[ns$name2==as.character(dnames[i,"Plot.1"]),"PlotCode"])
  if(length(ns[ns$name2==as.character(dnames[i,"Plot.1"]),"distance"])==0) dF.3$distance<-ns[ns$name==as.character(dnames[i,"Plot.1"]),"distance"] else dF.3$distance<-ns[ns$name2==as.character(dnames[i,"Plot.1"]),"distance"]
  VPD[[i+10]]<-cbind(dF.3$Group.1,dF.3[,6:9])  
  rm(dF.3)
  
  dF.3<-aggregate(dF.2[,3],list(dF.2$month),FUN="max",na.rm=T)
  dF.3$name<-dnames[i,"Datalogger.id"]
  dF.3$PlotCode<-as.character(ns[ns$name2==as.character(dnames[i,"Plot.1"]),"PlotCode"])
  if(length(ns[ns$name2==as.character(dnames[i,"Plot.1"]),"distance"])==0) dF.3$distance<-ns[ns$name==as.character(dnames[i,"Plot.1"]),"distance"] else dF.3$distance<-ns[ns$name2==as.character(dnames[i,"Plot.1"]),"distance"]
  mVPD[[i+10]]<-cbind(dF.3$Group.1,dF.3[,2:5])
  rm(dF.3)
  
  dF.3<-aggregate(dF.2[,grep("Tmax",colnames(dF.2))],list(dF.2$month),FUN="mean",na.rm=T)
  dF.3$name<-dnames[i,"Datalogger.id"]
  dF.3$PlotCode<-as.character(ns[ns$name2==as.character(dnames[i,"Plot.1"]),"PlotCode"])
  if(length(ns[ns$name2==as.character(dnames[i,"Plot.1"]),"distance"])==0) dF.3$distance<-ns[ns$name==as.character(dnames[i,"Plot.1"]),"distance"] else dF.3$distance<-ns[ns$name2==as.character(dnames[i,"Plot.1"]),"distance"]
  TMAX[[i+10]]<-cbind(dF.3$Group.1,dF.3[,2:5])  
  rm(dF.3)
  
  dF.3<-aggregate(dF.2[,grep("Tmin",colnames(dF.2))],list(dF.2$month),FUN="mean",na.rm=T)
  dF.3$name<-dnames[i,"Datalogger.id"]
  dF.3$PlotCode<-as.character(ns[ns$name2==as.character(dnames[i,"Plot.1"]),"PlotCode"])
  if(length(ns[ns$name2==as.character(dnames[i,"Plot.1"]),"distance"])==0) dF.3$distance<-ns[ns$name==as.character(dnames[i,"Plot.1"]),"distance"] else dF.3$distance<-ns[ns$name2==as.character(dnames[i,"Plot.1"]),"distance"]
  TMIN[[i+10]]<-cbind(dF.3$Group.1,dF.3[,2:5])  
  rm(dF.3)
  
  dF.3<-aggregate(dF.2[,grep("Tavg",colnames(dF.2))],list(dF.2$month),FUN="mean",na.rm=T)
  dF.3$avg<-rowMeans(dF.3[,2:5],na.rm=T)
  dF.3$name<-dnames[i,"Datalogger.id"]
  dF.3$PlotCode<-as.character(ns[ns$name2==as.character(dnames[i,"Plot.1"]),"PlotCode"])
  if(length(ns[ns$name2==as.character(dnames[i,"Plot.1"]),"distance"])==0) dF.3$distance<-ns[ns$name==as.character(dnames[i,"Plot.1"]),"distance"] else dF.3$distance<-ns[ns$name2==as.character(dnames[i,"Plot.1"]),"distance"]
  TAVG[[i+10]]<-cbind(dF.3$Group.1,dF.3[,6:9])    
  rm(dF,dF.2,dF.3)
}

ah<-do.call(rbind.data.frame,AH)
colnames(ah)<-c("month","avg","name","plotcode","dist")
ah[ah$avg<=0&!is.na(ah$avg),"avg"]<-NA
#ahc <- summarySE(ah, measurevar="avg", groupvars=c("month","dist"),na.rm=T)
#ahc$var<-"AH"

#vpd<-do.call(rbind.data.frame,VPD)
#colnames(vpd)<-c("month","avg","name","dist")
#vpc <- summarySE(vpd, measurevar="avg", groupvars=c("month","dist"),na.rm=T)
#vpc$var<-"VPD"

mvpd<-do.call(rbind.data.frame,mVPD)
colnames(mvpd)<-c("month","mvpd","name","plotcode","dist")
#remove -Inf
mvpd[mvpd$mvpd==-Inf&!is.na(mvpd$mvpd),"mvpd"]<-NA
#remove super high value for now
mvpd[mvpd$mvpd>80&!is.na(mvpd$mvpd),"mvpd"]<-NA

#vpc <- summarySE(mvpd, measurevar="max", groupvars=c("month","dist"),na.rm=T)
#vpc$var<-"VPD"

vwc<-do.call(rbind.data.frame,VWC)
colnames(vwc)<-c("month","vwc","name","plotcode","dist")
#vwc <- summarySE(vwc, measurevar="avg", groupvars=c("month","dist"),na.rm=T)
#vwc$var<-"VWC"

#combo<-rbind(ahc,vpc,vwc)
#ggplot(combo, aes(x=month, y=avg, colour=factor(dist))) + 
  #geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=.1) +
  #geom_line() +
  #geom_point()+facet_grid(var~.)
#ahc$Distance<-factor(ahc$dist)
#grid2<-ggplot(ahc, aes(x=month, y=avg, colour=Distance)) + 
  #geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=.1) +
#geom_line()+geom_point()+xlab("Month")+ylab("Absolute Humidity (g/m3)")+ggtitle("Monthly Absolute Humidity\nby Distance From Forest")+
#theme_classic()
#ggsave(paste0(getwd(),"/Figures/AbsoluteHumidity.pdf"),grid2)

#vpc$Distance<-factor(vpc$dist)
#grid3<-ggplot(vpc, aes(x=month, y=max, colour=Distance)) + 
#geom_line() +
#geom_point()+xlab("Month")+ylab("Vapour Pressure Deficit (hPa)")+ggtitle("Monthly Max Vapour Pressure Deficit\nby Distance From Forest")+
#geom_errorbar(aes(ymin=max-se, ymax=max+se), width=.1)+theme_classic()
#ggsave(paste0(getwd(),"/Figures/VapourPressureDeficit.pdf"),grid3)

#vwc$Distance<-factor(vwc$dist)
#grid4<-ggplot(vwc, aes(x=month, y=avg, colour=Distance)) + 
  #geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=.1) +
#geom_line() +
#geom_point()+xlab("Month")+ylab("Volumetric Water Content (Fraction)")+ggtitle(paste0("Monthly Volumetric Water Content \nby Distance From Forest"))+
#theme_classic()
#ggsave(paste0(getwd(),"/Figures/VolumetricWaterContent.pdf"),grid4)

#pdf("Humidity_Figures.pdf",paper="special",width=11,height=6)
#grid.arrange(grid2,grid3,ncol=2)
#dev.off()

tmax<-do.call(rbind.data.frame,TMAX)
colnames(tmax)<-c("month","tmax","name","plotcode","dist")
tmax[tmax$tmax<0&!is.na(tmax$tmax),"tmax"]<-NA
#txc <- summarySE(tmax, measurevar="max", groupvars=c("month","dist"),na.rm=T)
#ahc$var<-"AH"

stmax<-do.call(rbind.data.frame,mxST)
colnames(stmax)<-c("month","stmax","name","plotcode","dist")
#stxc <- summarySE(stmax, measurevar="max", groupvars=c("month","dist"),na.rm=T)

tmin<-do.call(rbind.data.frame,TMIN)
colnames(tmin)<-c("month","tmin","name","plotcode","dist")
tmin[tmin$tmin<0&!is.na(tmin$tmin),"tmin"]<-NA
#tmc <- summarySE(tmin, measurevar="min", groupvars=c("month","dist"),na.rm=T)
#vpc$var<-"VPD"

stmin<-do.call(rbind.data.frame,mnST)
colnames(stmin)<-c("month","stmin","name","plotcode","dist")
#stmc <- summarySE(stmin, measurevar="min", groupvars=c("month","dist"),na.rm=T)

tavg<-do.call(rbind.data.frame,TAVG)
colnames(tavg)<-c("month","tavg","name","plotcode","dist")
tavg[tavg$tavg<0&!is.na(tavg$tavg),"tavg"]<-NA
#tac <- summarySE(tavg, measurevar="avg", groupvars=c("month","dist"),na.rm=T)
#vwc$var<-"VWC"

stavg<-do.call(rbind.data.frame,ST)
colnames(stavg)<-c("month","stavg","name","plotcode","dist")
#stac <- summarySE(stavg, measurevar="avg", groupvars=c("month","dist"),na.rm=T)

#txc$Distance<-factor(txc$dist)
#grid1<-ggplot(txc, aes(x=month, y=max, colour=Distance)) + 
#geom_errorbar(aes(ymin=max-se, ymax=max+se), width=.1) +
#geom_line() +
#geom_point()+xlab("Month")+ylab("Maximum Temperature (C)")+ggtitle("Monthly Maximum Temperature by Distance From Forest")+
#theme_classic()


#tmc$Distance<-factor(tmc$dist)
#grid2<-ggplot(tmc, aes(x=month, y=min, colour=factor(dist))) + 
#geom_errorbar(aes(ymin=min-se, ymax=min+se), width=.1) +
#geom_line() +
#geom_point()+xlab("Month")+ylab("Minimum Temperature (C)")+ggtitle("Monthly Minimum Temperature by Distance From Forest")+
#theme_classic()

#tac$Distance<-factor(tac$dist)
#grid3<-ggplot(tac[tac$avg>20&!is.na(tac$avg),], aes(x=month, y=avg, colour=Distance)) + 
#geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=.1) +
# geom_line() +
# geom_point()+xlab("Month")+ylab("Average Temperature (C)")+ggtitle(paste0("Monthly Average Temperature by Distance From Forest"))+
# theme_classic()
#ggsave(paste0(getwd(),"/Figures/AverageTemperature.pdf"),grid3)

#pdf("Temperature_Figures.pdf",paper="special",width=11,height=8)
#grid.arrange(grid1,grid2,grid3,ncol=2)
#dev.off()

#do soilT figures
#grid1<-ggplot(stxc, aes(x=month, y=max, colour=factor(dist))) + 
#geom_errorbar(aes(ymin=max-se, ymax=max+se), width=.1) +
#  geom_line() +
#  geom_point()+xlab("Month")+ylab("Maximum Soil Temperature (C)")+ggtitle("Monthly Maximum Temperature by Distance From Forest")


#grid2<-ggplot(stmc, aes(x=month, y=min, colour=factor(dist))) + 
#  geom_errorbar(aes(ymin=min-se, ymax=min+se), width=.1) +
#  geom_line() +
#  geom_point()+xlab("Month")+ylab("Minimum Soil Temperature (C)")+ggtitle("Monthly Minimum Temperature by Distance From Forest")

#stac$Distance<-factor(stac$dist)
#grid3<-ggplot(stac, aes(x=month, y=avg, colour=Distance)) + 
#  geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=.1) +
# geom_line() +
#  geom_point()+xlab("Month")+ylab("Average Soil Temperature (C)")+ggtitle(paste0("Monthly Average Temperature by Distance From Forest"))+
#  theme_classic()
#ggsave(paste0(getwd(),"/Figures/SoilAverageTemperature.pdf"),grid3)


#pdf("SoilTemperature_Figures.pdf",paper="special",width=11,height=8)
#grid.arrange(grid1,grid2,grid3,ncol=2)
#dev.off()

#combine metdata measures
dF<-cbind(ah,tavg$tavg,tmax$tmax,tmin$tmin,mvpd$mvpd)
#canopy gap and microclimate measures
m<-as.character(unique(ah[,3]))
for(i in 1:length(m)){
  #find plot number
  if(length(grep("ESPA_G",m[i]))>0)  p<-gsub(" ","",as.character(names[names$Mstation==m[i],"Plot.1"])) else p<-gsub(" ","",as.character(dnames[dnames$Datalogger.id==m[i],"Plot.1"]))
  #assign "5" for forest measures
  if(length(grep("FP",p))>0) dF[dF$name==m[i],"Cavg"]<-5
  if(length(grep("FP",p))>0) next
  #find canopy measures
  can<-canopy[canopy$Plot==p,"Synth"]
  if(length(can)>1) dF[dF$name==m[i],"Jan"]<-can[1] else dF[dF$name==m[i],strsplit(as.character(canopy[canopy$Plot==p,"Month"]),"-")[[1]][1]]<-can
  if(length(can)>1) dF[dF$name==m[i],"Jun"]<-can[2]
  dF[dF$name==m[i],"Cavg"]<-mean(can)
  
}
colnames(dF)<-c("month","ah","name","plotcode","dist","tavg","tmax","tmin","mvpd","Jan","Jun","Cavg")

#add soilT and vwc
dF <- left_join(dF,vwc %>% select(month,vwc,plotcode),by=c("month","plotcode"))
dF <- left_join(dF,stavg %>% select(month,stavg ,plotcode),by=c("month","plotcode"))
dF <- left_join(dF,stmax %>% select(month,stmax ,plotcode),by=c("month","plotcode"))
dF <- left_join(dF,stmin %>% select(month,stmin ,plotcode),by=c("month","plotcode"))

#write datasheet to csv
write.csv(dF,paste0(getwd(),"/Monthly_metdata_withcanopygap.csv"))
#plot relationship between canopy gap and microclimate by month
#df<-data.frame(cbind(dF$Cavg,dF$ah,dF$month))
#colnames(df)<-c("x","y","month")
#ggplot(df)+geom_point(aes(x=x,y=y,color=factor(month)))+ geom_smooth(aes(x=x,y=y), method="lm")+facet_wrap(~ month,ncol=4)+
  #geom_text(x = 25, y = 10, label = lm_eqn(df), parse = TRUE)

ggplot(dF)+geom_point(aes(x=Cavg,y=ah,color=factor(month)))+ geom_smooth(aes(x=Cavg,y=ah), method="lm")+facet_wrap(~ month,ncol=4)+
  xlab("Canopy Gap")+ylab("Absolute Humidity (g/m3)")+ggtitle(paste0("Monthly Absolute Humidity by Canopy Gap"))+theme(
  legend.position="none"
  )
ggsave(paste0(getwd(),"/Figures/CanopyGap_AH.pdf"), width = 12, height = 8)

ggplot(dF)+geom_point(aes(x=Cavg,y=tavg,color=factor(month)))+ geom_smooth(aes(x=Cavg,y=tavg), method="lm")+facet_wrap(~ month,ncol=4)+
  xlab("Canopy Gap")+ylab("Temperature (C)")+ggtitle(paste0("Monthly Average Temperature by Canopy Gap"))+theme(
    legend.position="none"
  )
ggsave(paste0(getwd(),"/Figures/CanopyGap_AvgT.pdf"), width = 12, height = 8)

ggplot(dF)+geom_point(aes(x=Cavg,y=tmax,color=factor(month)))+ geom_smooth(aes(x=Cavg,y=tmax), method="lm")+facet_wrap(~ month,ncol=4)+
  xlab("Canopy Gap")+ylab("Temperature (C)")+ggtitle(paste0("Monthly Maximum Temperature by Canopy Gap"))+theme(
    legend.position="none"
  )
ggsave(paste0(getwd(),"/Figures/CanopyGap_MaxT.pdf"), width = 12, height = 8)

ggplot(dF)+geom_point(aes(x=Cavg,y=tmin,color=factor(month)))+ geom_smooth(aes(x=Cavg,y=tmin), method="lm")+facet_wrap(~ month,ncol=4)+
  xlab("Canopy Gap")+ylab("Temperature (C)")+ggtitle(paste0("Monthly Minimum Temperature by Canopy Gap"))+theme(
    legend.position="none"
  )
ggsave(paste0(getwd(),"/Figures/CanopyGap_MinT.pdf"), width = 12, height = 8)

ggplot(dF)+geom_point(aes(x=Cavg,y=mvpd,color=factor(month)))+ geom_smooth(aes(x=Cavg,y=mvpd), method="lm")+facet_wrap(~ month,ncol=4)+
  xlab("Canopy Gap")+ylab("Vapour Pressure Deficit (hPa)")+ggtitle(paste0("Monthly Max Vapour Pressure Deficit by Canopy Gap"))+theme(
    legend.position="none"
  )
ggsave(paste0(getwd(),"/Figures/CanopyGap_VPD.pdf"), width = 12, height = 8)

#plot relationship between canopy gap and microclimate for each month
m<-as.Date(unique(dF$month))
months<-c("Jan","Feb","March","April","May","June","July","August","Sept","Oct","Nov","Dec")
for(i in 1:length(m)){
  grid1<-ggplot(dF[dF$month==as.Date(m[i]),])+geom_point(aes(x=Cavg,y=ah))+ geom_smooth(aes(x=Cavg,y=ah), method="lm")+
    xlab("Canopy Gap")+ylab("Absolute Humidity (g/m3)")+ggtitle(paste0("Absolute Humidity by Canopy Gap for\n",m[i]))
  #month.abb(as.Date(m[i]))
  grid2<-ggplot(dF[dF$month==as.Date(m[i]),])+geom_point(aes(x=Cavg,y=tavg))+ geom_smooth(aes(x=Cavg,y=tavg), method="lm")+
    xlab("Canopy Gap")+ylab("Temperature (C)")+ggtitle(paste0("Average Temperature by Canopy Gap for\n",m[i]))
  
  grid3<-ggplot(dF[dF$month==as.Date(m[i]),])+geom_point(aes(x=Cavg,y=tmax))+ geom_smooth(aes(x=Cavg,y=tmax), method="lm")+
    xlab("Canopy Gap")+ylab("Temperature (C)")+ggtitle(paste0("Maximum Temperature by Canopy Gap for\n",m[i]))
  
  grid4<-ggplot(dF[dF$month==as.Date(m[i]),])+geom_point(aes(x=Cavg,y=tmin))+ geom_smooth(aes(x=Cavg,y=tmin), method="lm")+
    xlab("Canopy Gap")+ylab("Temperature (C)")+ggtitle(paste0("Minimum Temperature by Canopy Gap for\n",m[i]))
  
  grid5<-ggplot(dF[dF$month==as.Date(m[i]),])+geom_point(aes(x=Cavg,y=mvpd))+ geom_smooth(aes(x=Cavg,y=mvpd), method="lm")+
    xlab("Canopy Gap")+ylab("VPD (hPa)")+ggtitle(paste0("Max Vapour Pressure Deficit by Canopy Gap for\n",m[i]))
  
  d1 <- as.numeric(format(m[i], "%m"))
  d2 <- as.numeric(format(m[i], "%y"))
  pdf(paste0(getwd(),"/Figures/",months[d1],d2,"_CanopyGap.pdf"),paper="special",width=11,height=8)
  grid.arrange(grid1,grid5,grid2,grid3,grid4,ncol=2)
  dev.off()

}


