#code for analyzing raw datasheets of pollinators, averaging measures across 3 trees per plot
library(gdata)
library(stringr)
library(plyr)
library(lubridate)
library(reshape)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Pollination")
dates.1=c("June","July","Aug")
dates.2=c("Jan","Feb","March","April","May","June","July","Aug")
dates.3=c("June","July","Aug","Sept")
#dates=c("Jan")
topic="Pollinator"
year<-c("2014","2015","2016")

ID.s<-read.csv(paste0(getwd(),"/SampleIDs.csv"))
  
dataP<-read.xls(paste0(getwd(),"/Pollinators_revised ",year[1],".xlsx"), sheet=paste0(topic," ",dates.1[1]," ",year[1]))
#find each plot
p<-unique(as.character((dataP[,2])))
#remove "Plot" and ""
p<-p[p!="Plot"]
p<-p[p!=""]

final.1<-list()
for(j in 1:length(p)){
  final<-data.frame(Date=as.Date(character()),Plot=character(),Tree=character(),Blue=numeric(),Yellow=numeric(),White=numeric(),Biomass=numeric(),Banana=numeric(),Blank=character(),stringsAsFactors=FALSE) 
  final[1:3,1]<-as.Date(dataP[grep(p[j],as.character(dataP[,2])),1])
  final[1:3,2:(length(dataP[1,])-1)]<-data.frame(lapply(dataP[grep(p[j],as.character(dataP[,2])),2:(length(dataP[1,])-1)], as.character), stringsAsFactors=FALSE)
  
  for(k in 2:length(dates.1)){
    dataP.1<-read.xls(paste0(getwd(),"/Pollinators_revised ",year[1],".xlsx"), sheet=paste0(topic," ",dates.1[k]," ",year[1]))
    final[((k-1)*3+1):(k*3),1]<-as.Date(dataP.1[grep(p[j],as.character(dataP.1[,2])),1])
    final[((k-1)*3+1):(k*3),2:(length(dataP.1[1,])-1)]<-data.frame(lapply(dataP.1[grep(p[j],as.character(dataP.1[,2])),2:(length(dataP.1[1,])-1)], as.character), stringsAsFactors=FALSE)
  }
  
  final.2<-data.frame(Date=as.Date(character()),Plot=character(),Tree=character(),Blue=numeric(),Yellow=numeric(),White=numeric(),Biomass=numeric(),Banana=numeric(),No_Banana=numeric(),stringsAsFactors=FALSE)
  dataP.2<-read.xls(paste0(getwd(),"/Pollinators_revised ",year[2],".xlsx"), sheet=paste0(dates.2[1]," ",year[2]))
 
  final.2[1:3,1]<-as.Date(dataP.2[grep(p[j],as.character(dataP.2[,2])),1])
  final.2[1:3,2:(length(dataP.2[1,])-1)]<-data.frame(lapply(dataP.2[grep(p[j],as.character(dataP.2[,2])),2:(length(dataP.2[1,])-1)], as.character), stringsAsFactors=FALSE)
  
  #if(length(w[as.character(w[1:length(w[,1]),8])!="DNS",8])==0) final[k,8]<-0
  for(k in 2:length(dates.2)){
    dataP.2<-read.xls(paste0(getwd(),"/Pollinators_revised ",year[2],".xlsx"), sheet=paste0(dates.2[k]," ",year[2]))
    final.2[((k-1)*3+1):(k*3),1]<-as.Date(dataP.2[grep(p[j],as.character(dataP.2[,2])),1])
    final.2[((k-1)*3+1):(k*3),2:(length(dataP.2[1,])-1)]<-data.frame(lapply(dataP.2[grep(p[j],as.character(dataP.2[,2])),2:(length(dataP.2[1,])-1)], as.character), stringsAsFactors=FALSE)
    }
  #combine final and final.2
  final[,9]<-0
  colnames(final)<-colnames(final.2)
  final.1[[j]]<-rbind(final,final.2)
}
final<-do.call(rbind.data.frame,final.1)
#replace DNS or "out of range"
final[final$Biomass=="DNS","Biomass"]<-NA
final[final$Banana=="DNS","Banana"]<-NA

write.csv(final,paste0(getwd(),"/Pollinator_",year[1],"_",year[2],".csv"))
rm(dataP,dataP.1,dataP.2,final,final.2,final.1)

#######waiting for final pollinator numbers######
#analyze pollinator counts
ms<-c(paste(dates.1,"14",sep="."),paste(dates.2,"15",sep="."))

#get diversity measures
dataP.div<-data.frame(read.xls(paste0(getwd(),"/Counts_ pollinator.cont.xlsx"), sheet="Colors"),stringsAsFactors = F)
final.div<-data.frame(Plot=character(),Jun14=numeric(),Jul14=numeric(),Aug14=numeric(),Jan15=numeric(),Feb15=numeric(),Mar15=numeric(),Apr15=numeric(),May15=numeric(),Jun15=numeric(),Jul15=numeric(),Aug15=numeric(),stringsAsFactors = F)
final.pol<-data.frame(Plot=character(),Family=character(),Jun14=numeric(),Jul14=numeric(),Aug14=numeric(),Jan15=numeric(),Feb15=numeric(),Mar15=numeric(),Apr15=numeric(),May15=numeric(),Jun15=numeric(),Jul15=numeric(),Aug15=numeric(),stringsAsFactors = F)

for(i in 0:(length(p)-1)){
  P.div<-dataP.div[dataP.div$Plot==p[i+1],]
  final.div[i+1,1]<-p[i+1]
  final.pol[(i*9+1):(i*9+9),1]<-p[i+1]
  final.pol[(i*9+1):(i*9+9),2]<-c(as.character(unique(ID.s$Family)),"Total")
  for(j in 1:length(ms)){
    x1<-data.frame(colSums(P.div[,grep(ms[j],colnames(P.div))],na.rm=T))
    rownames(x1)<-str_split_fixed(rownames(x1),paste0(ms[j],".."),2)[,2]
    #identify families represented
    y1<-ID.s[,1:2]
    y1$Count<-x1[match(gsub(" ",".",y1$Label),rownames(x1)),1]
    y.1<-ddply(y1,.(Family),summarise,Count=sum(Count,na.rm=T))
    final.div[i+1,j+1]<-sum(y.1$Count>0,na.rm=T)
    final.pol[(i*9+1):(i*9+8),j+2]<-y.1[match(final.pol[(i*9+1):(i*9+8),2],y.1$Family),"Count"]
    final.pol[i*9+9,j+2]<-sum(y.1$Count,na.rm=T)
  }
}
write.csv(final.div,paste0(getwd(),"/Pollinator.diversity.",year[1],"_",year[2],".csv"))
write.csv(final.pol,paste0(getwd(),"/Pollinator.nos.",year[1],"_",year[2],".csv"))

#dataP.pol<-data.frame(read.xls(paste0(getwd(),"/Counts_ pollinator.cont.xlsx"), sheet="cumul_pollinators"),stringsAsFactors = F)
#
#create monthly comparative datasets
tmp<-read.csv(paste0(getwd(),"/Pollinator_",year[1],"_",year[2],".csv"))
#remove * from Plot names
tmp$Plot<-gsub("[*]","",tmp$Plot)
tmp$Biomass<-as.numeric(as.character(tmp$Biomass))
#replace NA as "100 m" (close to infinite)
tmp[is.na(tmp$Biomass),"Biomass"]<-100
tmp[is.na(tmp$Banana),"Banana"]<-100
#take mean biomass distance for each plot
tmp.1<-ddply(tmp,.(Plot,Date),summarise,Biomass=mean(Biomass),Banana=mean(Banana),No_Banana=mean(No_Banana))
#add month value instead of specific date
dts<-data.frame(unique(as.Date(tmp.1$Date)),stringsAsFactors = F)
colnames(dts)<-"date"
dts$month<-dts$date
for(i in 1:nrow(dts)){
  if(day(dts[i,1])>25) dts[i,"month"]<-as.Date(ceiling_date(dts[i,1],unit="month")) else dts[i,"month"]<-as.Date(floor_date(dts[i,1],unit="month"))
  if(year(dts[i,"month"])=="2014") dts[i,"month"]<-as.Date(paste(year(dts[i,"month"]),month(dts[i,"month"])-1,"01",sep="-"))
}
tmp.1$month<-dts[match(as.Date(tmp.1$Date),dts$date),"month"]

#create monthly measures for pollinator diversity
tmp1<-read.csv(paste0(getwd(),"/Pollinator.diversity.",year[1],"_",year[2],".csv"))

tmp1.2<-melt(tmp1[,2:ncol(tmp1)])
colnames(tmp1.2)<-c("Plot","variable","Pollin.div")
#replace "variable" with actual date
tmp1.2$date<-as.Date(paste(1,substr(as.character(tmp1.2$variable),1,3),paste0(20,gsub("[^0-9]","",tmp1.2$variable)),sep="-"),format="%d-%b-%Y")

#create monthly measures for pollinator numbers
tmp2<-read.csv(paste0(getwd(),"/Pollinator.nos.",year[1],"_",year[2],".csv"))
tmp2.1<-tmp2[,2:ncol(tmp2)]
tmp2.2<-melt(tmp2.1)
#tmp2.2<-data.frame(cbind(as.character(tmp2.2[,1]),as.character(tmp2.2$variable),tmp2.2$value),stringsAsFactors = F)
colnames(tmp2.2)<-c("Plot","family","variable","Pollin.nos")
tmp2.2$date<-as.Date(paste(1,substr(as.character(tmp2.2$variable),1,3),paste0(20,gsub("[^0-9]","",tmp2.2$variable)),sep="-"),format="%d-%b-%Y")
tmp2.3<-tmp2.2[tmp2.2$family=="Total",]
#combine monthly measures
tmp.1$Pollin.div<-tmp1.2[match(interaction(tmp.1$Plot,as.Date(tmp.1$month)),interaction(as.character(tmp1.2$Plot),tmp1.2$date)),"Pollin.div"]
tmp.1$Pollin.nos<-tmp2.3[match(interaction(tmp.1$Plot,tmp.1$month),interaction(tmp2.3$Plot,tmp2.3$date)),"Pollin.nos"]
ids<-as.character(unique(ID.s$Family))
tmp2.3<-tmp2.2[tmp2.2$family==paste0(ids[1]),]
tmp.1[,paste0(ids[1])]<-tmp2.3[match(interaction(tmp.1$Plot,tmp.1$month),interaction(tmp2.3$Plot,tmp2.3$date)),"Pollin.nos"]

tmp2.3<-tmp2.2[tmp2.2$family==paste0(ids[2]),]
tmp.1[,paste0(ids[2])]<-tmp2.3[match(interaction(tmp.1$Plot,tmp.1$month),interaction(tmp2.3$Plot,tmp2.3$date)),"Pollin.nos"]

tmp2.3<-tmp2.2[tmp2.2$family==paste0(ids[3]),]
tmp.1[,paste0(ids[3])]<-tmp2.3[match(interaction(tmp.1$Plot,tmp.1$month),interaction(tmp2.3$Plot,tmp2.3$date)),"Pollin.nos"]

tmp2.3<-tmp2.2[tmp2.2$family==paste0(ids[4]),]
tmp.1[,paste0(ids[4])]<-tmp2.3[match(interaction(tmp.1$Plot,tmp.1$month),interaction(tmp2.3$Plot,tmp2.3$date)),"Pollin.nos"]

tmp2.3<-tmp2.2[tmp2.2$family==paste0(ids[5]),]
tmp.1[,paste0(ids[5])]<-tmp2.3[match(interaction(tmp.1$Plot,tmp.1$month),interaction(tmp2.3$Plot,tmp2.3$date)),"Pollin.nos"]

tmp2.3<-tmp2.2[tmp2.2$family==paste0(ids[6]),]
tmp.1[,paste0(ids[6])]<-tmp2.3[match(interaction(tmp.1$Plot,tmp.1$month),interaction(tmp2.3$Plot,tmp2.3$date)),"Pollin.nos"]

tmp2.3<-tmp2.2[tmp2.2$family==paste0(ids[7]),]
tmp.1[,paste0(ids[7])]<-tmp2.3[match(interaction(tmp.1$Plot,tmp.1$month),interaction(tmp2.3$Plot,tmp2.3$date)),"Pollin.nos"]

tmp2.3<-tmp2.2[tmp2.2$family==paste0(ids[8]),]
tmp.1[,paste0(ids[8])]<-tmp2.3[match(interaction(tmp.1$Plot,tmp.1$month),interaction(tmp2.3$Plot,tmp2.3$date)),"Pollin.nos"]

#write dataset
write.csv(tmp.1,paste0(getwd(),"/Pollination_monthlyvariables.csv"))