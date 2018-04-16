#code to combine and organize disease survey data for cocoa from raw data sheets
library(stringr)
#library(gridBase)
library(gdata)
#library(ggplot2)
library(lubridate)
#library(plyr)
library(reshape2)
library(tidyverse)
#library(xts)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Disease")

dates=c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sept","Oct","Nov","Dec")
#dates=c("June","Aug","Sept","Oct","Nov")
#dates=c("Jan","Feb","Mar","Apr","May","June")
topics=c("(tree)","(CPD)")
year<-"2016"
trans<-c("AB","HM","KA")
ns<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/plots.csv")


final.2<-list()
#final.2<-list()
for( i in 1:length(dates)){
  final.1<-list()
  #open tree sheet
  dataS<-read.xls(paste0(getwd(),"/Disease_rawdata ",year,"_revised.xlsx"), sheet=paste0("Disease ",topics[1]," ",dates[i]," ",year))
  dataB<-read.xls(paste0(getwd(),"/Disease_rawdata ",year,"_revised.xlsx"), sheet=paste0("Disease ",topics[2]," ",dates[i]," ",year))

  #identify all data sheets
  d<-agrep("Date: ",as.character(dataS[,6]))
  DT<-7
  if(length(d)==0) DT<-8
  if(length(d)==0) d<-agrep("Date: ",as.character(dataS[,7]))
 
  #add length of datasheet
  d[length(d)+1]<-length(dataS[,1])
  for(j in 1:(length(d)-1)){
    dS<-dataS[d[j]:(d[j+1]),]
    #find date
    dt<-as.Date(dataS[d[j],DT],format="%d/%m/%Y")
    if(is.na(dt))  dt<-as.Date(dataS[d[j],DT])
    #find start of datasheet
    s<-grep("Plot",as.character(dS[,1]))
    #find end of datasheet
    e<-grep("Disease",as.character(dS[,1]))
    if(length(e)==0) dataS.1<-data.frame(dS[(s+1):length(dS[,1]),],stringsAsFactors=FALSE) else dataS.1<-data.frame(dS[(s+1):(e-1),],stringsAsFactors=FALSE)
    rm(s,e,dS)
    #remove random blank rows and columns
    dataS.1[dataS.1[,1]=="",]<-NA
    dataS.1<-dataS.1[, colSums(is.na(dataS.1)) != nrow(dataS.1)]
    dataS.1<-na.omit(dataS.1)
   
    if(year=="2014") colnames(dataS.1)<-c("Plot","TreeNo","Subplot","TsPods","OsPods","TmPods","OmPods","TlPods","OlPods","NoCPB","NoBP","Creep","Mist","StB","Ants","SM","Notes") else colnames(dataS.1)<-c("Plot","TreeNo","Subplot","TsPods","OsPods","TmPods","OmPods","TlPods","OlPods","NoCPB","NoBP","Mammal","Creep","Mist","StB","Ants","SM","Notes")
    if(year=="2014") dataS.1[,"Mammal"]=0
    
    #add mortality event
    dataS.1$Mortality<-0
    dataS.1[grep("Dead",dataS.1$Notes),"Mortality"]<-1
    #find unique plotnumbers
    p<-unique(as.character((dataS.1$Plot)))
    #add CPB intensity column
    dataS.1[,"iCPB"]=0
    
    for(k in 1:length(p)){
      #pull out trees for each plot
      w<-dataS.1[grep(p[k],as.character(dataS.1$Plot)),]
      #find all CPDs
      id<-data.frame(as.character(w[as.numeric(as.character(w$NoCPB))>0,"TreeNo"]))
      rm(w)
      #remove blank rows
      id<-na.omit(id)
      if(length(id[,1])==0) dataS.1[,"iCPB"]=0
      if(length(id[,1])==0) next
      cpd<-dataB[grep(p[k],as.character(dataB[,1])),]
      x<-unique(as.character(cpd[,2]))
      X<-str_split_fixed(x, " ", 2)
      #separate CPD tree tag numbers from cocoa tree numbers
      pods<-str_split_fixed(cpd$X, " ", 2)
      iD<-str_split_fixed(id[,1], " ", 2)
      #check if have same number of cpd in tree and CPD sheet
      #if mistake is in CPD sheet
      if(length(x)<length(id[,1])) dataS.1[as.numeric(iD[!(X[,1] %in% iD[,1]),1]),"iCPB"]=NA
      #if mistake is in tree sheet
      #if(length(x)>length(id[,1])) dataS.1[as.numeric(X[!(X[,1] %in% iD[,1]),1]),10]=NA
      #check if missing all CPD data
      if(length(x)==0) dataS.1[as.numeric(iD[,1]),"iCPB"]=NA
      if(length(x)==0) next
      rm(x,X)
        for(l in 1:length(id[,1])){
          #get tree number
          #tr<-strsplit(as.character(id[l,])," ")
          CPD<-pods[,1]==iD[l,1]
          #reassign number values for measures: 1=1-2;2=3-4,3=5-9,4=>10
          pd<-as.character(cpd[CPD,5])
          pd[pd==2]=1
          pd[pd %in% 3:4]=2
          pd[pd %in% 5:9]=3
          pd[pd==">10"]=4
          #take average of CPB intensity measures & assign to tree
          #mean(as.numeric(pd))
          dataS.1[(as.numeric(iD[l,1])+18*(k-1)),"iCPB"]=mean(as.numeric(pd))
          rm(pd,CPD)
      }
      rm(pods,id,iD,cpd)
    }
    #identify incidence of mammal attack
    #dataS.1$X.10<-0
    dataS.1[grep("rodent",dataS.1$Notes),"Mammal"]=1
    dataS.1[grep("mammal",dataS.1$Notes),"Mammal"]=1
    
    #add Date
    dataS.1[,"Date"]<-dt
    #change Ys and Ns to 1s and 0s
    dataS.1$Creep<-as.character(dataS.1$Creep)
    dataS.1[dataS.1$Creep=="N","Creep"]<-0
    dataS.1[dataS.1$Creep=="Y","Creep"]<-1
    
    dataS.1$Mist<-as.character(dataS.1$Mist)
    dataS.1[dataS.1$Mist=="N","Mist"]<-0
    dataS.1[dataS.1$Mist=="Y","Mist"]<-1
    
    dataS.1$StB<-as.character(dataS.1$StB)
    dataS.1[dataS.1$StB=="N","StB"]<-0
    dataS.1[dataS.1$StB=="Y","StB"]<-1
    rm(p)
    final.1[[j]]<-dataS.1
  }
  final<-do.call(rbind.data.frame,final.1)
  #colnames(final)<-c("Plot","Tree Number","Subplot No","No. Small Pods (Total)","No. Small Pods (overipe)","No. Med Pods (Total)","No Med Pods (overipe)","No Large Pods (Total)", "No Large Pods (overipe)","No Pods with Capsids","No. Black Pods","No. Pods with Mammal Attack","Creepers","Mistletoe","Stem Borer","No. Ant Tents","Soil Moisture (%)","Date","CPB")
  #sort data sheet by plot number
  final.2[[i]]<-final[do.call(order,final),]
}
final<-do.call(rbind.data.frame,final.2)
#save datasheet for each month
write.csv(final,paste0(getwd(),"/Disease_",year,"_summary.csv"))
rm(final,d,final.1,final.2,dataB,dataS,dataS.1)

#generate monthly measure metrics for disease incidence by plot
#cocoa pod production
pdw<-read.table(paste0(getwd(),"/Disease_2014_summary.csv"),sep=",",header=T)
pdw$Date<-as.Date(as.character(pdw$Date),format="%Y-%m-%d")
pdw1<-read.table(paste0(getwd(),"/Disease_2015_summary.csv"),sep=",",header=T)
pdw1$Date<-as.Date(as.character(pdw1$Date),format="%d/%m/%Y")
pdw2<-read.table(paste0(getwd(),"/Disease_2016_summary.csv"),sep=",",header=T)
pdw2$Date<-as.Date(as.character(pdw2$Date),format="%Y-%m-%d")
pdw3<-read.table(paste0(getwd(),"/Disease_2017_summary.csv"),sep=",",header=T)
pdw3$Date<-as.Date(as.character(pdw3$Date),format="%Y-%m-%d")
#avg<-read.table(paste0(getwd(),"/cocoaYield.csv"),sep=",",header=T)
pdw<-data.frame(rbind(pdw,pdw1,pdw2,pdw3),stringsAsFactors = F)
rm(pdw1,pdw2,pdw3)
pdw<-pdw[grep("Entered by:",pdw$Plot,invert=T),]

#pdw$season <- NA

#identify period of pod maturation of interest for 2014/15 heavy crop
pdw[year(pdw$Date)=="2015"&month(pdw$Date)<7|year(pdw$Date)=="2014","season"]<-"2014/15"
pdw[year(pdw$Date)=="2016"&month(pdw$Date)<7|year(pdw$Date)=="2015"&month(pdw$Date)>8,"season"]<-"2015/16"
pdw[year(pdw$Date)=="2017"&month(pdw$Date)<7|year(pdw$Date)=="2016"&month(pdw$Date)>8,"season"]<-"2016/17"

#plot proportion of trees with stem borer, mistletoe and creeper (mean or max) for whole season (July-May)
pdw$Creep<-as.numeric(as.character(pdw$Creep))
pdw$Mist<-as.numeric(as.character(pdw$Mist))
pdw$StB<-as.numeric(as.character(pdw$StB))
pdw$Ants<-as.numeric(as.character(pdw$Ants))
pdw$SM<-as.numeric(pdw$SM)
pdw$TmPods<-as.numeric(as.character(pdw$TmPods))
pdw$TsPods<-as.numeric(as.character(pdw$TsPods))
pdw$TlPods<-as.numeric(as.character(pdw$TlPods))
pdw$NoCPB<-as.numeric(as.character(pdw$NoCPB))
pdw$NoBP<-as.numeric(as.character(pdw$NoBP))
pdw$Mammal<-as.numeric(as.character(pdw$Mammal))

#change tree number
no<-as.character(pdw$TreeNo)
pdw$treeno<-str_split_fixed(no, " ", 2)[,1]
pdw$month<-as.Date(round_date(as.Date(pdw$Date),unit="month"))

#pull out disease measures for each season
#pdw.14<-pdw[pdw$month<="2015-03-01",]
#look at frequency of pods on trees
pdw$Tpods<-rowSums(cbind(pdw$TsPods,pdw$TmPods,pdw$TlPods),na.rm=T)
#calculate average number of pods per tree
#test<-ddply(pdw.14,.(Plot,treeno),summarise,Topds.m<-mean(Tpods))
#quantile(test$..1)

#write per tree data sheet
write.csv(pdw,paste0(getwd(),"/Monthly_disease_pertree.csv"))

#pull out disease monitored trees for subplot 5 (to overlap with fruitset monitoring)
pdw2<-pdw[pdw$Subplot==5,]
write.csv(pdw2,paste0(getwd(),"/Monthly_disease.SP5_pertree.csv"))

#calculate per tree seasonal incidence
pdw$months<-1
pertree.pset<-ddply(pdw[!is.na(pdw$season),],.(Plot,treeno,season),summarise,Creep=mean(Creep,na.rm=T),Mist=mean(Mist,na.rm=T),StB=mean(StB,na.rm=T),Tpods=sum(Tpods,na.rm=T)/sum(months),PropCPB=sum(NoCPB,na.rm=T)/sum(Tpods,na.rm=T),PropBP=sum(NoBP,na.rm=T)/sum(Tpods,na.rm=T),PropMammal=sum(Mammal,na.rm=T)/sum(Tpods,na.rm=T),SM=mean(SM,na.rm=T))
write.csv(pertree.pset,paste0(getwd(),"/Seasonal_pertree.plot.disease.csv"))

#calculate per plot monthly incidence
tree.p<-ddply(pdw %>% filter(month>"2014-01-01"),.(Plot,month),summarise,Creep=mean(Creep,na.rm=T),Mist=mean(Mist,na.rm=T),StB=mean(StB,na.rm=T))
#add month variable
#tree.p$month<-as.Date(round_date(as.Date(tree.p$Date),unit="month"))
#plot incidence over season
#tree.p[year(tree.p$Date)=="2015"&month(tree.p$Date)<6|year(tree.p$Date)=="2014","season.1415"]<-1
#tree.p[is.na(tree.p$season.1415),"season.1415"]<-0

#tree.p1<-tree.p[tree.p$season.1415==1,]
tree.p2<-melt(tree.p[,1:5],id.vars=c("Plot","month"))
ggplot(tree.p2,aes(as.Date(month),value,color=factor(variable)))+geom_line()+facet_wrap(~Plot)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=10))
ggsave(paste0(getwd(),"/PerTree_MonthlyProportion.pdf"),width=11,height=8)

#save tree level pests
write.csv(tree.p,paste0(getwd(),"/Disease_monthlytreepests.csv"))

pdw$TsPods<-as.numeric(as.character(pdw$TsPods))
pdw$TmPods<-as.numeric(as.character(pdw$TmPods))
pdw$TlPods<-as.numeric(as.character(pdw$TlPods))
pdw$NoCPB<-as.numeric(as.character(pdw$NoCPB))
pdw$NoBP<-as.numeric(as.character(pdw$NoBP))
pdw$Mammal<-as.numeric(as.character(pdw$Mammal))

#pull out disease measures for HC14/15 (August-Feb)
#pdw.14<-pdw[pdw$month<="2015-03-01",]

#calculate per tree pod pest incidence
#pod.p14<-ddply(pdw.14,.(Plot,treeno),summarise,Total.pods=sum(TsPods,TmPods,TlPods,na.rm=T),PropCPB=sum(NoCPB,na.rm=T)/sum(TsPods,TmPods,TlPods,na.rm=T),PropBP=sum(NoBP,na.rm=T)/sum(TsPods,TmPods,TlPods,na.rm=T),PropMammal=sum(Mammal,na.rm=T)/sum(TsPods,TmPods,TlPods,na.rm=T),iCPB=sum(iCPB,na.rm=T)/sum(NoCPB,na.rm=T),soil.moist=mean(SM,na.rm=T))
#pod.p14$soil.moist.flower<-ddply(pdw.14[as.Date(pdw.14$month)<="2014-09-01",],.(Plot,treeno),summarise,soil.moist.flower=mean(SM,na.rm=T))[,3]
#pod.p14$soil.moist.fruit<-ddply(pdw.14[as.Date(pdw.14$month)>"2014-09-01",],.(Plot,treeno),summarise, soil.moist.fruit=mean(SM,na.rm=T))[,3]

#write.csv(pod.p14,paste0(getwd(),"/Disease_HC1415podpests.csv"))

#for pod pests, plot incidence over month
pod.p<-ddply(pdw %>% filter(month>"2014-01-01"),.(Plot,month),summarise,Total.pods=sum(TsPods,TmPods,TlPods,na.rm=T),PropCPB=sum(NoCPB,na.rm=T)/sum(TsPods,TmPods,TlPods,na.rm=T),PropBP=sum(NoBP,na.rm=T)/sum(TsPods,TmPods,TlPods,na.rm=T),PropMammal=sum(Mammal,na.rm=T)/sum(TsPods,TmPods,TlPods,na.rm=T),iCPB=sum(iCPB*NoCPB,na.rm=T)/sum(NoCPB,na.rm=T),soil.moist=mean(SM,na.rm=T))
#add month
#pod.p$month<-as.Date(round_date(as.Date(pod.p$month),unit="month"))
#save dataset
write.csv(pod.p,paste0(getwd(),"/Disease_monthlypodpests.csv"))

#combine tree and pod measures
tree.p[,6:10]<-pod.p[match(interaction(tree.p$Plot,tree.p$month),interaction(pod.p$Plot,pod.p$month)),4:8]

#write monthly disease measures
write.csv(tree.p,paste0(getwd(),"/Disease_monthlymeasures.csv"))
