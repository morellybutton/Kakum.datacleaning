#calculation of yield from standing crop/disease survey measures and comparison with other yield measures

#library(ggplot2)
require(grid)
library(reshape2)
library(lubridate)
library(gdata)
#library(plyr)
library(stringr)
library(tidyverse)

plots<-read.csv(paste0("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/plots.csv"))
plts<-as.character(plots$name3)
#rm(plots)
#remove forest plots
plts<-plts[grep("FP",plts,invert=T)]

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")

#cocoa pod production
pdw<-read.table(paste0(getwd(),"/Disease/Disease_2014_summary.csv"),sep=",",header=T)
pdw$Date<-as.Date(as.character(pdw$Date),format="%Y-%m-%d")
pdw1<-read.table(paste0(getwd(),"/Disease/Disease_2015_summary.csv"),sep=",",header=T)
pdw1$Date<-as.Date(as.character(pdw1$Date),format="%d/%m/%Y")
pdw2<-read.table(paste0(getwd(),"/Disease/Disease_2016_summary.csv"),sep=",",header=T)
pdw2$Date<-as.Date(as.character(pdw2$Date),format="%Y-%m-%d")
pdw3<-read.table(paste0(getwd(),"/Disease/Disease_2017_summary.csv"),sep=",",header=T)
pdw3$Date<-as.Date(as.character(pdw3$Date),format="%Y-%m-%d")
#avg<-read.table(paste0(getwd(),"/cocoaYield.csv"),sep=",",header=T)
pdw<-data.frame(rbind(pdw,pdw1,pdw2,pdw3),stringsAsFactors = F)
rm(pdw1,pdw2,pdw3)
pdw<-pdw[grep("Entered by:",pdw$Plot,invert=T),]

pdw$NoBP<-as.character(pdw$NoBP)
pdw$Ants<-as.character(pdw$Ants)
pdw$Creep<-as.character(pdw$Creep)
#replace Black pod "N"
pdw[pdw$NoBP=="N"|is.na(pdw$NoBP)|pdw$NoBP=="","NoBP"]<-0
pdw[pdw$Ants=="N"|is.na(pdw$Ants)|pdw$Ants=="","Ants"]<-0
pdw[pdw$Ants=="Y","Ants"]<-1
pdw[pdw$Creep=="N "|is.na(pdw$Creep)|pdw$Creep=="","Creep"]<-0
pdw[pdw$Creep=="y","Creep"]<-1
pdw[is.na(pdw$NoCPB),"NoCPB"]<-0

pdw[,5:17]<-lapply(pdw[,5:17],as.numeric)
pdw[,19:20]<-lapply(pdw[,19:20],as.numeric)


#open correction factors
c.f<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Yield/Pod Measurments/correction.factors.csv")

#calculate per tree yields ignoring first month
final.1<-list()
for(i in 1:length(plts)){
  total<-data.frame(plot=character(),treeno=numeric(),month=character(),year=character(),date=as.Date(character()),Spod=numeric(),Sopod=numeric(),Mpod=numeric(),Mopod=numeric(),Lpod=numeric(),Lopod=numeric(),BP=numeric(),CPB=numeric(),Mam=numeric(),TotRemoved=numeric(),stringsAsFactors=F)
  p <- pdw %>% filter(Plot==plts[i]) %>% mutate(treeno=str_split_fixed(TreeNo," ",2)[,1])
  #replace mortality events with NAs
  #p <- p %>% mutate(TsPods=replace(TsPods,Mortality==1,NA),OsPods=replace(OsPods,Mortality==1,NA),TmPods=replace(TmPods,Mortality==1,NA),OmPods=replace(OmPods,Mortality==1,NA),TlPods=replace(TlPods,Mortality==1,NA),OlPods=replace(OlPods,Mortality==1,NA),
  #                  NoCPB=replace(NoCPB,Mortality==1,NA),NoBP=replace(NoBP,Mortality==1,NA))
  
  #replace NAs with 0s
  #p[is.na(p[,5]),5]<-0
  #p[is.na(p[,6]),6]<-0
  #plotcode<-as.character(plots[plots$name3==plts[i],"PlotCode"])
  #p$Date<-as.Date(as.character(p$Date))
  #find unique dates
  d <- p %>% pull(Date) %>% unique() %>% sort()
  
  #report first month without change
  new.data <- p %>% filter(Date==d[1]) %>% select(TsPods,OsPods,TmPods,OmPods,TlPods,OlPods,NoBP,NoCPB,Mammal)
  #new.data<-cbind(p[p$Date==d[1],"TsPods"],p[p$Date==d[1],"OsPods"],as.numeric(p[p$Date==d[1],"TmPods"]),as.numeric(p[p$Date==d[1],"OmPods"]),as.numeric(p[p$Date==d[1],"TlPods"]),as.numeric(p[p$Date==d[1],"OlPods"]),as.numeric(as.character(p[p$Date==d[1],"NoBP"])),as.numeric(as.character(p[p$Date==d[1],"NoCPB"])),as.numeric(as.character(p[p$Date==d[1],"Mammal"])),0)
  total[1:nrow(new.data),6:15]<-new.data
  total$plot<-plts[i]
  total$treeno<-p %>% filter(Date==d[1]) %>% pull(treeno)
  total$month<-month(round_date(d[1],unit="month"))
  total$year<-year(round_date(d[1],unit="month"))
  total$date<-d[1]
  #add other disease measures
  total[,16:21]<- p %>% filter(Date==d[1]) %>% select(Creep,Mist,StB,Ants,SM,iCPB)
  total$SM<-as.numeric(total$SM) 
  #colnames(total)<-c("plot","treeno","month","year","date","Spod","Sopod","Mpod","Mopod","Lpod","Lopod","BP","CPB","Mam","TotRemoved","Creep","Mist","StB","Ants","SM","iCPB")
  
  #calculate proportion of pods that are BP or CPB
  total<- total %>% group_by(treeno) %>% mutate(PropBP=BP/sum(Spod,Sopod,Mpod,Mopod,Lpod,Lopod,na.rm=T),PropCPB=CPB/sum(Spod,Sopod,Mpod,Mopod,Lpod,Lopod,na.rm=T)) %>%
    mutate(PropBP=replace(PropBP,is.na(PropBP),0),PropCPB=replace(PropCPB,is.na(PropCPB),0)) %>% ungroup()
  
  #add number of BP pods for back calculating (if needed)
  total$NoBP<- p %>% filter(Date==d[1]) %>% pull(NoBP)
  total$NoCPB<-p %>% filter(Date==d[1]) %>% pull(NoCPB)
  
  #calculate difference in diseased pods from first month
  tmp<-left_join(p %>% filter(Date==d[2]) %>% select(treeno,NoBP),p %>% filter(Date==d[1]) %>% select(treeno,NoBP),by="treeno")
  bp <- tmp %>% pull(NoBP.x) - tmp %>% pull(NoBP.y)
  #negative black pod values are considered to have been removed by farmer and positive values are additional infected pods
  bp.new<-bp
  bp.new[bp.new<0]<-0
  bp.rem<-bp
  bp.rem[bp.new>0]<-0
  
  tmp<-left_join(p %>% filter(Date==d[2]) %>% select(treeno,NoCPB),p %>% filter(Date==d[1]) %>% select(treeno,NoCPB),by="treeno")
  cpb<-tmp %>% pull(NoCPB.x) - tmp %>% pull(NoCPB.y)
  #negative black pod values are considered to have been removed by farmer and positive values are additional infected pods
  cpb.new<-cpb
  cpb.new[cpb.new<0]<-0
  cpb.rem<-cpb
  cpb.rem[cpb.new>0]<-0
  
  tmp<-left_join(p %>% filter(Date==d[2]) %>% select(treeno,Mammal),p %>% filter(Date==d[1]) %>% select(treeno,Mammal),by="treeno")
  mam<- tmp %>% pull(Mammal.x) - tmp %>% pull(Mammal.y)
  mam.new<-mam
  mam.new[mam.new<0]<-0
  mam.rem<-mam
  mam.rem[mam.new>0]<-0
  #calculate total pods lost to disease
  tot.rem<-rowSums(cbind(bp.rem,cpb.rem,mam.rem))
  
  #establish which pods from previous month are now overripe by size class
  tmp<-left_join(p %>% filter(Date==d[2]) %>% select(treeno,OsPods),p %>% filter(Date==d[1]) %>% select(treeno,OsPods),by="treeno")
  pso <- tmp %>% pull(OsPods.x) - tmp %>% pull(OsPods.y)
  #pso[pso<0]<-0
  tmp<-left_join(p %>% filter(Date==d[2]) %>% select(treeno,OmPods),p %>% filter(Date==d[1]) %>% select(treeno,OmPods),by="treeno")
  pmo<-tmp %>% pull(OmPods.x) - tmp %>% pull(OmPods.y)
  #pmo[pmo<0]<-0
  tmp<-left_join(p %>% filter(Date==d[2]) %>% select(treeno,OlPods),p %>% filter(Date==d[1]) %>% select(treeno,OlPods),by="treeno")
  plo<-tmp %>% pull(OlPods.x) - tmp %>% pull(OlPods.y)
  #plo[plo<0]<-0
  
  tmp<-left_join(p %>% filter(Date==d[2]) %>% select(treeno,TsPods),p %>% filter(Date==d[1]) %>% select(treeno,TsPods),by="treeno")
  s1<-tmp %>% pull(TsPods.x) - tmp %>% pull(TsPods.y)
  tmp<-left_join(p %>% filter(Date==d[2]) %>% select(treeno,TmPods),p %>% filter(Date==d[1]) %>% select(treeno,TmPods),by="treeno")
  m1<-tmp %>% pull(TmPods.x) - tmp %>% pull(TmPods.y)
  tmp<-left_join(p %>% filter(Date==d[2]) %>% select(treeno,TlPods),p %>% filter(Date==d[1]) %>% select(treeno,TlPods),by="treeno")
  l1<-tmp %>% pull(TlPods.x) - tmp %>% pull(TlPods.y)
  
  #combine all pre tree measures
  new.data<-cbind(s1,pso,m1,pmo,l1,plo,bp,cpb,mam,tot.rem)
  
  #calculate harvested beans
  total[(nrow(new.data)+1):(2*nrow(new.data)),6:15]<-new.data
  total[(nrow(new.data)+1):(2*nrow(new.data)),"plot"]<-plts[i]
  total[(nrow(new.data)+1):(2*nrow(new.data)),"treeno"]<-p %>% filter(Date==d[2]) %>% pull(treeno)
  total[(nrow(new.data)+1):(2*nrow(new.data)),"month"]<-month(round_date(d[2],unit="month"))
  total[(nrow(new.data)+1):(2*nrow(new.data)),"year"]<-year(round_date(d[2],unit="month"))
  total[(nrow(new.data)+1):(2*nrow(new.data)),"date"]<-d[2]
  #total[1,"tree_kg"]<-sum(s1*c.f[c.f$SizeClass=="Small","BeanBiomass"]/1000,m1*c.f[c.f$SizeClass=="Medium"&c.f$Disease=="Normal","BeanBiomass"]/1000,l1*c.f[c.f$SizeClass=="Large"&c.f$Disease=="Normal","BeanBiomass"]/1000)/18
  #total[1,"farm_kg"]<-cen*total[1,"tree_kg"]
  
  #add other disease measures
  total[(nrow(new.data)+1):(2*nrow(new.data)),16:21]<- p %>% filter(Date==d[2]) %>% select(Creep,Mist,StB,Ants,SM,iCPB)
  
  #calculate proportion of pods that are BP or CPB
  total[(nrow(new.data)+1):(2*nrow(new.data)),"PropBP"] <- p %>% filter(Date==d[2]) %>% group_by(treeno) %>% mutate(PropBP=NoBP/sum(TsPods,OsPods,TmPods,OmPods,TlPods,OlPods,na.rm=T))  %>% 
    mutate(PropBP=replace(PropBP,is.na(PropBP),0)) %>% pull(PropBP)
  total[(nrow(new.data)+1):(2*nrow(new.data)),"PropCPB"] <- p %>% filter(Date==d[2]) %>% group_by(treeno) %>% mutate(PropCPB=NoCPB/sum(TsPods,OsPods,TmPods,OmPods,TlPods,OlPods,na.rm=T))  %>% 
    mutate(PropCPB=replace(PropCPB,is.na(PropCPB),0)) %>% pull(PropCPB)
  
  #add number of BP pods for back calculating (if needed)
  total[(nrow(new.data)+1):(2*nrow(new.data)),"NoBP"]<-p %>% filter(Date==d[2]) %>% pull(NoBP)
  total[(nrow(new.data)+1):(2*nrow(new.data)),"NoCPB"]<-p %>% filter(Date==d[2]) %>% pull(NoCPB)
  
  #if mortality event "0" out all change metrics
  mort <- p %>% filter(Date==d[2]&Mortality==1) %>% pull(treeno)
  if(length(mort)>0) total <- total %>% mutate(Spod=replace(Spod,treeno==mort,0),Sopod=replace(Sopod,treeno==mort,0),Mpod=replace(Mpod,treeno==mort,0),Mopod=replace(Mopod,treeno==mort,0),Lpod=replace(Lpod,treeno==mort,0),Lopod=replace(Lopod,treeno==mort,0),
                                      BP=replace(BP,treeno==mort,0),CPB=replace(CPB,treeno==mort,0))
  for(j in 3:length(d)){
    tmp<-left_join(p %>% filter(Date==d[j]) %>% select(treeno,NoBP),p %>% filter(Date==d[j-1]) %>% select(treeno,NoBP),by="treeno")
    bp <- tmp %>% pull(NoBP.x) - tmp %>% pull(NoBP.y)
    #negative black pod values are considered to have been removed by farmer and positive values are additional infected pods
    bp.new<-bp
    bp.new[bp.new<0]<-0
    bp.rem<-bp
    bp.rem[bp.new>0]<-0
    
    tmp<-left_join(p %>% filter(Date==d[j]) %>% select(treeno,NoCPB),p %>% filter(Date==d[j-1]) %>% select(treeno,NoCPB),by="treeno")
    cpb<-tmp %>% pull(NoCPB.x) - tmp %>% pull(NoCPB.y)
    #negative black pod values are considered to have been removed by farmer and positive values are additional infected pods
    cpb.new<-cpb
    cpb.new[cpb.new<0]<-0
    cpb.rem<-cpb
    cpb.rem[cpb.new>0]<-0
    
    tmp<-left_join(p %>% filter(Date==d[j]) %>% select(treeno,Mammal),p %>% filter(Date==d[j-1]) %>% select(treeno,Mammal),by="treeno")
    mam<- tmp %>% pull(Mammal.x) - tmp %>% pull(Mammal.y)
    mam.new<-mam
    mam.new[mam.new<0]<-0
    mam.rem<-mam
    mam.rem[mam.new>0]<-0
    #calculate total pods lost to disease
    tot.rem<-rowSums(cbind(bp.rem,cpb.rem,mam.rem),na.rm=T)
    
    #establish which pods from previous month are now overripe by size class
    tmp<-left_join(p %>% filter(Date==d[j]) %>% select(treeno,OsPods),p %>% filter(Date==d[j-1]) %>% select(treeno,OsPods),by="treeno")
    pso <- tmp %>% pull(OsPods.x) - tmp %>% pull(OsPods.y)
    #pso[pso<0]<-0
    tmp<-left_join(p %>% filter(Date==d[j]) %>% select(treeno,OmPods),p %>% filter(Date==d[j-1]) %>% select(treeno,OmPods),by="treeno")
    pmo<-tmp %>% pull(OmPods.x) - tmp %>% pull(OmPods.y)
    #pmo[pmo<0]<-0
    tmp<-left_join(p %>% filter(Date==d[j]) %>% select(treeno,OlPods),p %>% filter(Date==d[j-1]) %>% select(treeno,OlPods),by="treeno")
    plo<-tmp %>% pull(OlPods.x) - tmp %>% pull(OlPods.y)
    
    tmp<-left_join(p %>% filter(Date==d[j]) %>% select(treeno,TsPods),p %>% filter(Date==d[j-1]) %>% select(treeno,TsPods),by="treeno")
    s1<-tmp %>% pull(TsPods.x) - tmp %>% pull(TsPods.y)
    tmp<-left_join(p %>% filter(Date==d[j]) %>% select(treeno,TmPods),p %>% filter(Date==d[j-1]) %>% select(treeno,TmPods),by="treeno")
    m1<-tmp %>% pull(TmPods.x) - tmp %>% pull(TmPods.y)
    tmp<-left_join(p %>% filter(Date==d[j]) %>% select(treeno,TlPods),p %>% filter(Date==d[j-1]) %>% select(treeno,TlPods),by="treeno")
    l1<-tmp %>% pull(TlPods.x) - tmp %>% pull(TlPods.y)
    
    #combine all pre tree measures
    new.data<-cbind(s1,pso,m1,pmo,l1,plo,bp,cpb,mam,tot.rem)
 
    #calculate harvested beans
    total[((j-1)*nrow(new.data)+1):(j*nrow(new.data)),6:15]<-new.data
    total[((j-1)*nrow(new.data)+1):(j*nrow(new.data)),"plot"]<-plts[i]
    total[((j-1)*nrow(new.data)+1):(j*nrow(new.data)),"treeno"]<-p %>% filter(Date==d[j]) %>% pull(treeno)
    meas<-month(round_date(d[j],unit="month"))
    if(meas==month(round_date(d[j-1],unit="month"))) meas<-meas+1
    total[((j-1)*nrow(new.data)+1):(j*nrow(new.data)),"month"]<-meas
    total[((j-1)*nrow(new.data)+1):(j*nrow(new.data)),"year"]<-year(round_date(d[j],unit="month"))
    total[((j-1)*nrow(new.data)+1):(j*nrow(new.data)),"date"]<-d[j]
    #total[1,"tree_kg"]<-sum(s1*c.f[c.f$SizeClass=="Small","BeanBiomass"]/1000,m1*c.f[c.f$SizeClass=="Medium"&c.f$Disease=="Normal","BeanBiomass"]/1000,l1*c.f[c.f$SizeClass=="Large"&c.f$Disease=="Normal","BeanBiomass"]/1000)/18
    #total[1,"farm_kg"]<-cen*total[1,"tree_kg"]
    
    #add other disease measures
    total[((j-1)*nrow(new.data)+1):(j*nrow(new.data)),16:21]<-p %>% filter(Date==d[j]) %>% select(Creep,Mist,StB,Ants,SM,iCPB)
    
    #calculate proportion of pods that are BP or CPB
    total[((j-1)*nrow(new.data)+1):(j*nrow(new.data)),"PropBP"]<-p %>% filter(Date==d[j]) %>% group_by(treeno) %>% mutate(PropBP=NoBP/sum(TsPods,OsPods,TmPods,OmPods,TlPods,OlPods,na.rm=T))  %>% 
      mutate(PropBP=replace(PropBP,is.na(PropBP),0)) %>% pull(PropBP)

    total[((j-1)*nrow(new.data)+1):(j*nrow(new.data)),"PropCPB"]<-p %>% filter(Date==d[j]) %>% group_by(treeno) %>% mutate(PropCPB=NoCPB/sum(TsPods,OsPods,TmPods,OmPods,TlPods,OlPods,na.rm=T))  %>% 
      mutate(PropCPB=replace(PropCPB,is.na(PropCPB),0)) %>% pull(PropCPB)
    
    #add number of BP pods for back calculating (if needed)
    total[((j-1)*nrow(new.data)+1):(j*nrow(new.data)),"NoBP"]<-p %>% filter(Date==d[j]) %>% pull(NoBP)
    total[((j-1)*nrow(new.data)+1):(j*nrow(new.data)),"NoCPB"]<-p %>% filter(Date==d[j]) %>% pull(NoCPB)
   
     #if mortality event "0" out all change metrics
    mort <- p %>% filter(Date==d[j-1]&Mortality==1|Date==d[j]&Mortality==1) %>% pull(treeno)
    if(length(mort)>0) total <- total %>% ungroup() %>% mutate(Spod=replace(Spod,treeno==mort&date==d[j],0),Sopod=replace(Sopod,treeno==mort&date==d[j],0),Mpod=replace(Mpod,treeno==mort&date==d[j],0),Mopod=replace(Mopod,treeno==mort&date==d[j],0),
                                                 Lpod=replace(Lpod,treeno==mort&date==d[j],0),Lopod=replace(Lopod,treeno==mort&date==d[j],0),BP=replace(BP,treeno==mort&date==d[j],0),CPB=replace(CPB,treeno==mort&date==d[j],0),Mam=replace(Mam,treeno==mort&date==d[j],0),
                                                 TotRemoved=replace(TotRemoved,treeno==mort&date==d[j],0),PropBP=replace(PropBP,treeno==mort&date==d[j],0),PropCPB=replace(PropCPB,treeno==mort&date==d[j],0),NoBP=replace(NoBP,treeno==mort&date==d[j],0),NoCPB=replace(NoCPB,treeno==mort&date==d[j],0))
    
  }
  if(nrow(total[is.na(total$plot),])>0) break
  final.1[[i]]<-total
}
final<-do.call(rbind.data.frame,final.1)
#final<- final[!is.na(final$plot),]
write.csv(final,paste0(getwd(),"/Yield/Monthly_standingcrop_pertree.csv"))
rm(new.data,final,total,p,pdw,plo,pmo,pso,s1,m1,l1,bp,bp.new,bp.rem,cpb,cpb.new,cpb.rem,mam,mam.new,mam.rem)

dataF<-read.csv(paste0(getwd(),"/Yield/Monthly_standingcrop_pertree.csv"))
#dataF<-dataF[!is.na(dataF$plot),]
final.1<-list()
for(i in 1:length(plts)){
  dF<-dataF[dataF$plot==plts[i],]
  dF<-dF[19:nrow(dF),]
  #need to add up pods per month and estimate amount of beans lost to disease or harvested
  #separate by months
  dts<-sort(unique(as.Date(dF$date)))
  dts<-dts[dts<"2017-07-01"]
  final.2<-list()
  for(j in 1:length(dts)){
    #start with L pods and L overripe pods, 
    df<-dF[as.Date(dF$date)==dts[j],]
    #replace all NAs with 0s
    df[is.na(df)]<-0
    #if L pods negative and overripe 0, pod has been removed (harvested or diseased)
    plo<-df$Lopod
    l.over<-plo
    l.over[l.over>0]<-0 #this gets you number of pods removed due to "overripeness"
    plo[plo<0]<-0 #this gets you number of large pods becoming "overipe" from last month
    l1<-cbind(df$Lpod,plo)
    l1[l1[,1]<0,1]<-rowSums(cbind(df[df$Lpod<0,"Lpod"],plo[df$Lpod<0]),na.rm=T)
    l.harv<-l1[,1]
    l.harv[l.harv>0]<-0
    l.harv<-abs(l.harv)
    
    #calculate pods still standing
    l1<-l1[,1]
    l1[l1<0]<-0
    #if L pods positive, check if any M pods are negative, after checking M to overripe (only positive values)
    pmo<-df$Mopod
    m.over<-pmo
    m.over[m.over>0]<-0 #this gets you number of pods removed due to "overripeness"
    pmo[pmo<0]<-0 #this gets you number of medium pods becoming "overipe" from last month
    m1<-cbind(df$Mpod,pmo)
    m1[m1[,1]<0,1]<-rowSums(cbind(df[df$Mpod<0,"Mpod"],pmo[df$Mpod<0]),na.rm=T) #only combine negative medium pods with overripe pods
    #m1<-rowSums(cbind(df$Mpod,pmo),na.rm=T) 
    m1<-m1[,1]
    m1<-cbind(m1,l1)
    m1[m1[,1]<0,1]<-rowSums(cbind(m1[m1[,1]<0,1],m1[m1[,1]<0,2]),na.rm=T) #only combine negative medium pods with large pods
    m.harv<-m1[,1]
    m.harv[m.harv>0]<-0
    m.harv<-abs(m.harv)
    #calculate ripe pods still standing
    m1<-m1[,1]
    m1[m1<0]<-0
    #if M pods positive, check if any S pods are negative after checking S to overripe (only positive values)
    pso<-df$Sopod
    s.over<-pso
    s.over[s.over>0]<-0 #this gets you number of pods removed due to "overripeness"
    pso[pso<0]<-0 #this gets you number of medium pods becoming "overipe" from last month
    s1<-cbind(df$Spod,pso)
    s1[s1[,1]<0,1]<-rowSums(cbind(s1[s1[,1]<0,1],s1[s1[,1]<0,2]),na.rm=T) #only combine negative small pods with overripe pods
    s1<-cbind(s1[,1],m1)  #compare with remaining medium pods
    s1[s1[,1]<0,1]<-rowSums(cbind(s1[s1[,1]<0,1],s1[s1[,1]<0,2]),na.rm=T) #only combine negative small pods with medium pods
    s.harv<-s1[,1] #don't think I should assume these are harvested? more likely due to disease
    s.harv[s.harv>0]<-0
    s.harv<-abs(s.harv)
    #calculate ripe pods still standing
    s1<-s1[,1]
    s1[s1<0]<-0
    #from harvested estimates calculate what percentage may have been diseased, black pod and/or capsids
    cpb<-df$CPB
    cpb[cpb>0]<-0
    cpb[is.na(cpb)]<-0
    bp<-df$BP
    bp[bp>0]<-0
    bp[is.na(bp)]<-0
    mam<-df$Mam
    mam[mam>0]<-0
    mam[is.na(mam)]<-0
    c1<-data.frame(cbind(s1,m1,l1,s.harv,m.harv,l.harv,bp,cpb,mam))
    c1$treeno<-as.character(df$treeno)
    rm(s.harv,m.harv,l.harv,bp,cpb,s1,m1,l1)
    #go through each tree, starting from large to small pod and mammal to black pod to capsid
    d.pods<-data.frame(treeno=character(),s.crop=numeric(),m.crop=numeric(),l.crop=numeric(),s.harv=numeric(),m.harv=numeric(),l.harv=numeric(),s.bp=numeric(),m.bp=numeric(),s.cpb=numeric(),m.cpb=numeric(),s.mam=numeric(),m.mam=numeric(),l.mam=numeric(),stringsAsFactors=F)
    for(m in 1:nrow(c1)){
      #check if any of the diseases are reported
      if(c1[m,"bp"]==0&&c1[m,"cpb"]==0&&c1[m,"mam"]==0) { d.pods[m,2:7]<-cbind(c1[m,"s1"],c1[m,"m1"],c1[m,"l1"],c1[m,"s.harv"],c1[m,"m.harv"],c1[m,"l.harv"]);
      d.pods[m,8:ncol(d.pods)]<-0 ; d.pods[m,"treeno"]<-as.character(c1[m,"treeno"]); 
      next}
      
      #if(c1[m,"s.harv"]==0&&c1[m,"m.harv"]==0&&c1[m,"l.harv"]==0) { d.pods[m,]<-0 ; 
      #d.pods[m,"treeno"]<-as.character(c1[m,"treeno"]); 
      #next}
      
      #start with mammal b/c more likely for pod to be removed
      if(c1[m,"l.harv"]==0) l.mam<-0 else { 
        l.mam<-sum(c1[m,"l.harv"],c1[m,"mam"]); if(l.mam<0) l.mam=c1[m,"l.harv"]
      }
      mam<-sum(l.mam,c1[m,"mam"])
      if(l.mam==0&&mam<0){ m.mam<-sum(c1[m,"m.harv"],mam); if(m.mam<0) m.mam=c1[m,"m.harv"]
      }else m.mam<-0
      mam<-sum(m.mam,mam)
      
      #if there remain mammal pods with no size, we assume they are small pods
      if(mam<0) s.mam<-abs(mam) else s.mam<-0
      
      #now remove m.mam from m.harv and l.harv
      m.harv<-c1[m,"m.harv"]-m.mam
      l.harv<-c1[m,"l.harv"]-l.mam
      s.harv<-c1[m,"s.harv"]-s.mam
      rm(mam)
      if(s.harv<0) s.harv=0
      #then go to black pod and only consider medium and small pods, it is likely large pods would have been removed before
      #if(l.harv==0) l.bp<-0 else {l.bp<-l.harv-sum(l.harv,c1[m,"bp"]);l.harv<-l.harv-l.bp}
      #if there are more black pods than harvested large pods, you need to carry those to the medium pod comparison
      #if(l.bp>l.harv) { m.bp<-m.harv; m.harv<-0} else m.harv<-m.harv-m.bp
      bp<-c1[m,"bp"]
      if(bp<0&s.harv>0){s.bp<-sum(s.harv,bp); if(s.bp<0) s.bp=s.harv else s.bp=abs(bp)} else s.bp<-0
      #if there are more black pods than harvested small pods, you need to carry those to the small pod comparison
#      if(s.bp>s.harv) {
      #        s.bp<-s.harv; s.harv<-0
      #      } else {
      #        if(m.harv==0) {
      #         s.bp=0
          #        } else {
      #          s.harv<-s.harv-s.bp
      #        }
      #      }
      
      bp<-sum(s.bp,bp)
      
      if(bp<0&m.harv>0){m.bp<-sum(m.harv,bp); if(m.bp<0) m.bp=m.harv else m.bp=abs(bp)} else m.bp<-0
      #if(m.bp>m.harv) {
      #        m.harv<-0;m.bp<-m.harv
      #      } else {
      #        if(m.harv==0) {
      #          m.bp<-0
      #        } else {
      #          m.harv<-m.harv-m.bp
      #        }
      #      }
      bp<-sum(m.bp,bp)
      #if there remain black pods with no size, we assume they are small pods
      if(bp<0) s.bp<-s.bp-bp 
      m.harv<- m.harv-m.bp
      s.harv<- s.harv-s.bp
      rm(bp)
      
      if(s.harv<0) s.harv=0
      
      #do the same again for capsids, although only considering small or medium pods (large pods with capsids will still be harvested)
      #if(l.harv==0) l.cpb<-0 else {l.cpb<-l.harv-sum(l.harv,c1[m,"cpb"]);l.harv<-l.harv-l.cpb}
      cpb<-c1[m,"cpb"]
      if(cpb<0&s.harv>0){s.cpb<-sum(s.harv,cpb); if(s.cpb<0) s.cpb=s.harv else s.cpb=abs(cpb)} else s.cpb<-0
      
      #if(cpb<0&&m.harv==0&&s.harv==0&&l.harv>0) { l.cpb<-l.harv-sum(l.harv,cpb); cpb<-sum(l.cpb,cpb) } else l.cpb<-0
      #if(cpb<0) s.cpb<-s.harv-sum(s.harv,cpb) else s.cpb<-0
      #if there are more black pods than harvested small pods, you need to carry those to the medium pod comparison
      #      if(s.cpb>s.harv&&s.harv>0) { 
      #       s.cpb<-s.harv; s.harv<-0
      #     } else {
      #        if(s.harv==0) {
      #          s.cpb<-0
      #        } else {
      #          s.harv<-s.harv-s.cpb
      #        }
      #      }
      cpb<-sum(s.cpb,cpb)
      
      if(cpb<0&m.harv>0){m.cpb<-sum(m.harv,cpb); if(m.cpb<0) m.cpb=m.harv else m.cpb=abs(cpb)} else m.cpb<-0
      
      #     if(cpb<0) m.cpb<-m.harv-sum(m.harv,cpb) else m.cpb<-0
      #if there are more black pods than harvested medium pods, you need to carry those to the small pod comparison
      #     if(m.cpb>m.harv&&m.harv>0) { 
      #        m.cpb<-m.harv; m.harv<-0
      #      } else {
      #        if(m.harv==0) {
      #         m.cpb<-0
      #       } else {
      #        m.harv<-m.harv-m.cpb
      #       }
      #     }
      cpb<-sum(m.cpb,cpb)
      #if there remain capsid pods with no size, we assume they are small pods
      if(cpb<0) s.cpb<-s.cpb-cpb 
      m.harv<- m.harv-m.cpb
      s.harv<- s.harv-s.cpb
      rm(cpb)
      if(s.harv<0) s.harv=0
      
      #if(cpb<0&&s.harv>0) s.cpb<-s.cpb-cpb else {s.cpb<-abs(s.harv); s.harv<-0}
      #    if(cpb<0){ s.cpb<-s.cpb-cpb 
      #    } else {
      #     if(s.harv==0&&s.cpb>0){ 
      #       s.cpb<-sum(s.cpb,abs(cpb))
      #   } else {
      #     if(s.harv<0) {
      #     s.cpb<-abs(s.harv); s.harv<-0 
      #   }
      #  }
      # }
      
      #combine diseased and harvested pods
      d.pods[m,"treeno"]<-as.character(c1[m,"treeno"])
      d.pods[m,2:ncol(d.pods)]<-cbind(c1[m,"s1"],c1[m,"m1"],c1[m,"l1"],s.harv,m.harv,l.harv,s.bp,m.bp,s.cpb,m.cpb,s.mam,m.mam,l.mam)
      rm(s.harv,m.harv,l.harv,s.bp,m.bp,s.cpb,m.cpb,s.mam,m.mam,l.mam)
    }
    c.pods<-data.frame(cbind(df[,2:5]),df$TotRemoved,df[,16:25],d.pods,abs(s.over),abs(m.over),abs(l.over))
    #create a check that I am not overestimating number of diseased pods
    c.pods$check<-rowSums(cbind(c.pods$df.TotRemoved,c.pods$s.bp,c.pods$m.bp,c.pods$l.bp,c.pods$s.mam,c.pods$m.mam,c.pods$l.mam,c.pods$s.cpb,c.pods$m.cpb,c.pods$l.cpb))
    #create a second check in case it is a miscount from previous month, with no other changes in standing crop
    c.pods$check2<-c.pods$check*rowSums(cbind(c.pods$s.bp,c.pods$m.bp,c.pods$l.bp,c.pods$s.mam,c.pods$m.mam,c.pods$l.mam,c.pods$s.cpb,c.pods$m.cpb,c.pods$l.cpb))
    c.pods$date<-dts[j]
    final.2[[j]]<-c.pods
  }
  final.1[[i]]<-do.call(rbind.data.frame,final.2)
}
final<-do.call(rbind.data.frame,final.1)
test2<-final[final$check<0,]  
#calculate bean weight for each category, though not include small pods?
#final$harvest_kg<-rowSums(cbind(c.f[c.f$SizeClass=="Medium"&c.f$Disease=="Normal","BeanBiomass"]/1000*final$m.harv,c.f[c.f$SizeClass=="Large"&c.f$Disease=="Normal","BeanBiomass"]/1000*final$l.harv),na.rm=T)
#final$spods_kg<-c.f[c.f$SizeClass=="Small","BeanBiomass"]/1000*as.numeric(final$s.harv)
final$harvest_kg<-rowSums(cbind(c.f[c.f$SizeClass=="Small","BeanBiomass"]/1000*as.numeric(final$s.harv),c.f[c.f$SizeClass=="Medium"&c.f$Disease=="Normal","BeanBiomass"]/1000*final$m.harv,c.f[c.f$SizeClass=="Large"&c.f$Disease=="Normal","BeanBiomass"]/1000*final$l.harv,c.f[c.f$SizeClass=="Large"&c.f$Disease=="Normal","BeanBiomass"]/1000*final$l.cpb),na.rm=T)
test<-final[final$harvest_kg<0,]
final$bp_kg<-rowSums(cbind(c.f[c.f$SizeClass=="Small","BeanBiomass"]/1000*as.numeric(final$s.bp),c.f[c.f$SizeClass=="Medium"&c.f$Disease=="Normal","BeanBiomass"]/1000*final$m.bp),na.rm=T)
final$cpb_kg<-rowSums(cbind(c.f[c.f$SizeClass=="Small","BeanBiomass"]/1000*as.numeric(final$s.cpb),c.f[c.f$SizeClass=="Medium"&c.f$Disease=="Normal","BeanBiomass"]/1000*final$m.cpb),na.rm=T) #assume Large pods with capsids will still be harvested
final$mam_kg<-rowSums(cbind(c.f[c.f$SizeClass=="Small","BeanBiomass"]/1000*as.numeric(final$s.mam),c.f[c.f$SizeClass=="Medium"&c.f$Disease=="Normal","BeanBiomass"]/1000*final$m.mam,c.f[c.f$SizeClass=="Large"&c.f$Disease=="Normal","BeanBiomass"]/1000*final$l.mam),na.rm=T) 

final<-final[!is.na(final$plot),]
#write to csv
write.csv(final,paste0(getwd(),"/Yield/Monthly_podremovals.csv"))
#compare per tree measures from 2014 and 2015
tree.c<-read.csv(paste0(getwd(),"/Yield/Monthly_podremovals.csv"))
no<-as.character(tree.c$treeno)
tree.c$treenum<-str_split_fixed(no," ", 2)[,1]

#add date
tree.c$date<-as.Date(paste(1,month(tree.c$month,label=T),tree.c$year,sep="-"),format="%d-%b-%Y")
#pull out subplot 5 measures for comparison with pollination experiment
tmp<-tree.c[tree.c$treenum=="9" | tree.c$treenum=="10",]
write.csv(tmp,paste0(getwd(),"/Yield/Monthly_podremovals_SP5.csv"))

#calculate per tree 2014/15 heavy crop values (Sept-June) 
#calculate monthly harvest as well as seasonal
#tree.C<-tree.c[as.Date(tree.c$date)<="2015-02-01",]
#yield.t<-ddply(tree.C,.(plot,treeno),summarise,harvest=sum(harvest_kg),bp=sum(bp_kg),cpb=sum(cpb_kg),mam=sum(mam_kg))

year<-c("2014","2015","2016","2017")
season<-1:3
months<-unique(tree.c$date)
y4<-list()
y5<-list()
plots$name1<-gsub(" ","",plots$name3)
for(i in 1:length(plts)){
  #x1<-stand.c[stand.c$plot==plts[i],]
  #get transect and census data
  
  x1<-tree.c[tree.c$plot==plts[i],]
  
  census   <- data.frame(lapply(read.csv(paste0(getwd(),"/AGB/ForestPlots/",gsub(" ","",plts[i]),"_LS.csv")) , as.character),stringsAsFactors=F)
  #colnames(census)<-c("X","Subplot","x.coord","y.coord","Tag","Fam","Genus", "Species","DBH1", "DBH2","POM","Height","TreeCodes","Notes", "PlotCode", "PlotNum","OrigSpecies","NSpecies","NFam","THeight")         
  #no.cocoa1 <- nrow(census[census$NSpecies=="Theobroma cacao"&as.numeric(census$DBH2)>10&!is.na(census$DBH2),])/0.36
  #no.cocoa2 <- nrow(census[census$NSpecies=="Theobroma cacao"&as.numeric(census$DBH3)>10&!is.na(census$DBH3),])/0.36
  #tmp<-census[grep(plts[i],census[,1]):nrow(census),]
  #ind<-grep("Plot",tmp[,1])
  #if(length(ind)>1) tmp<-tmp[3:(ind[2]-1),] else tmp<-tmp[3:nrow(tmp),]
  #pull out cocoa trees per plot
   
  y2<-data.frame(Plot=character(),Month=character(),No.cocoatrees=numeric(),Prop.tree=numeric(),Monthly.harvest.tree=numeric(),Monthly.harvest.blackpod.tree=numeric(),Monthly.harvest.capsid.tree=numeric(),Monthly.harvest.ha=numeric(),Monthly.harvest.blackpod.ha=numeric(),Monthly.harvest.capsid.ha=numeric(),Monthly.med.harvest.ha=numeric(),Monthly.med.harvest.blackpod.ha=numeric(),Monthly.med.harvest.capsid.ha=numeric(),stringsAsFactors=F)
  for(k in 1:length(months)){
    #pull out live cocoa trees for yield estimate
    n<-census[grep("Theobroma",census$NSpecies),]
    #remove NAs
    #n<-n[!is.na(n)]
    #pull out DMT to get DBH
    d<-n[grep("DMT",n$CensusNotes),]
    d$DMT<-str_split_fixed(d$CensusNotes, " ", 2)[,2]
    d$DMT<-str_split_fixed(d$DMT,",",2)[,1]
    if(year(months[k])<2016) n[is.na(as.numeric(n$DBH)),"Flag1"]<-0 else n$DBH<-n$DBH3
    n<-n[!is.na(as.numeric(n$DBH)),]
    n<-n[as.numeric(n$DBH)>10,]
    n.1<-nrow(n[grep("0",n$Flag1,invert=T),])
    rm(n)
    
    #for(j in 1:length(no)){
    #No<-str_split_fixed(x1$treeno," ",2)
    x2<-x1[x1$date==months[k],]
    n<-n.1/0.36 
    
    #find proportion of trees being harvested
    harv_tree <- x2 %>% filter(harvest_kg>0) %>% nrow()/18
    
    #take mean per tree harvest, bp and cpb loss
    m.harv<-mean(x2$harvest_kg,na.rm=T)
    m.bp<-mean(x2$bp_kg,na.rm=T)
    m.cpb<-mean(x2$cpb_kg,na.rm=T)
   
    #take median per tree harvest, bp and cpb loss (in case of skew)
    med.harv<-median(x2$harvest_kg,na.rm=T)
    med.bp<-median(x2$bp_kg,na.rm=T)
    med.cpb<-median(x2$cpb_kg,na.rm=T)
    
    y2[k,1:2]<-cbind(as.character(plts[i]),as.character(months[k]))
    y2[k,3:7] <-cbind(n,harv_tree,m.harv,m.bp,m.cpb)
    #calculate per ha yield, depending on year
    y2[k,8:10]<-cbind(m.harv*n,m.bp*n,m.cpb*n)
    y2[k,11:13]<-cbind(med.harv*n,med.bp*n,med.cpb*n)
    #}
    
  }
  y2$Month<-as.Date(y2$Month)
  y5[[i]]<-y2
   
 #sum each tree separately
  no<-as.character(unique(x1$treeno))
  No<-str_split_fixed(no, " ", 2)
 
  no<-paste0(as.character(unique(x1$treenum))," ")
  x1$treenum<-paste0(x1$treenum," ")
  y1<-list()
  y2<-data.frame(Plot=character(),TreeNo=character(),DBH=numeric(),No.1=numeric(),HeavyCrop=numeric(),HC.blackpod=numeric(),HC.capsid=numeric(),LightCrop=numeric(),LC.blackpod=numeric(),LC.capsid=numeric(),stringsAsFactors=F)
  for(k in 1:length(season)){
    for(j in 1:length(no)){
      #No<-str_split_fixed(x1$treeno," ",2)
      x2<-x1[x1$treenum==no[j],]
      hc.1<- x2[x2$date>as.Date(paste(year[k],"09","01",sep="-"))&x2$date<as.Date(paste(year[k+1],"07","01",sep="-")),]
      #hc1.5<-x2[x2$year==year[1]|x2$year<=year[2]&x2$month<3,]
      lc.1<-x2[x2$date>as.Date(paste(year[k+1],"06","01",sep="-"))&x2$date<as.Date(paste(year[k+1],"10","01",sep="-")),]
      #hc.2<-x2[x2$year==year[k+1]&x2$month>=10|x2$year==year[3]&x2$month<7,]
      dbh<-as.numeric(d[d$DMT==gsub(" ","",no[j]),"DBH2"])/10
      #if no DBH put in NA
      if(length(dbh)==0) dbh<-mean(as.numeric(d$DBH2),na.rm=T)/10
      y2[j,1:4]<-cbind(as.character(plts[i]),as.character(no[j]),dbh,n.1)
      #calculate per tree yield
      y2[j,5:10]<-cbind(sum(as.numeric(hc.1$harvest_kg),na.rm=T),sum(as.numeric(hc.1$bp_kg),na.rm=T),sum(as.numeric(hc.1$cpb_kg),na.rm=T),sum(lc.1$harvest_kg,na.rm=T),sum(as.numeric(lc.1$bp_kg),na.rm=T),sum(as.numeric(lc.1$cpb_kg),na.rm=T))
      rm(hc.1,lc.1,dbh,x2,No)
    }
    y2$season<-paste0(year[k],"/",gsub("20","",year[k+1]))
    y1[[k]]<-y2
  }
  y4[[i]]<-do.call(rbind.data.frame,y1)
}
y6<-do.call(rbind.data.frame,y5)
y6$name1<-gsub(" ","",y6$Plot)
#plots$name1<-gsub(" ","",plots$name3)
y6$distance<-plots[match(as.character(y6$name1),plots$name1),"distance"]
y6$transect<-plots[match(as.character(y6$name1),plots$name1),"transect"]
#report monthly per tree and per ha yield
write.csv(y6,paste0(getwd(),"/Yield/Monthly_HarvestEstimates.csv"))

y3<-do.call(rbind.data.frame,y4)
y3$name1<-gsub(" ","",y3$Plot)
#plots$name1<-gsub(" ","",plots$name3)
y3$distance<-plots[match(as.character(y3$name1),plots$name1),"distance"]
y3$transect<-plots[match(as.character(y3$name1),plots$name1),"transect"]
#report DBH and season yields
write.csv(y3,paste0(getwd(),"/Yield/PerTree_CropEstimates.csv"))
rm(y1,y2,y3,y4,x1)

y3<-read.csv(paste0(getwd(),"/Yield/PerTree_CropEstimates.csv"))
#create figure of seasonal yields for each transect
ggplot(y3, aes(season,HeavyCrop))+geom_boxplot(aes(color=transect))+facet_wrap(~transect,ncol=3)+
  theme_classic() + ylab("Heavy Crop [kg/tree]")
  ggsave(paste0(getwd(),"/Analysis/ElNino/Yield_pertree_transect.seasons.comparison.pdf"))

ggplot(y3, aes(season,HC.blackpod))+geom_boxplot(aes(color=transect))+facet_wrap(~transect,ncol=3)+
    theme_classic() + ylab("Heavy Crop Lost to Black Pod [kg/tree]")
  ggsave(paste0(getwd(),"/Analysis/ElNino/Yield.bp_pertree_transect.seasons.comparison.pdf"))

ggplot(y3, aes(season,HC.capsid))+geom_boxplot(aes(color=transect))+facet_wrap(~transect,ncol=3)+
    theme_classic() + ylab("Heavy Crop Lost to Capsid [kg/tree]")
  ggsave(paste0(getwd(),"/Analysis/ElNino/Yield.capsid_pertree_transect.seasons.comparison.pdf"))
  
#create figure of seasonal yields for each transect
ggplot(y3, aes(season,LightCrop))+geom_boxplot(aes(color=transect))+facet_wrap(~transect,ncol=3)+
  theme_classic() + ylab("Light Crop [kg/tree]")
ggsave(paste0(getwd(),"/Analysis/ElNino/Yield.lc_pertree_transect.seasons.comparison.pdf"))

ggplot(y3, aes(season,LC.blackpod))+geom_boxplot(aes(color=transect))+facet_wrap(~transect,ncol=3)+
  theme_classic() + ylab("Light Crop Lost to Black Pod [kg/tree]")
ggsave(paste0(getwd(),"/Analysis/ElNino/Yield.lc.bp_pertree_transect.seasons.comparison.pdf"))

ggplot(y3, aes(season,LC.capsid))+geom_boxplot(aes(color=transect))+facet_wrap(~transect,ncol=3)+
  theme_classic() + ylab("Light Crop Lost to Capsid [kg/tree]")
ggsave(paste0(getwd(),"/Analysis/ElNino/Yield.lc.capsid_pertree_transect.seasons.comparison.pdf"))

#create figures for each plot
ggplot(y3, aes(season,HeavyCrop))+geom_boxplot(aes(color=transect))+facet_wrap(~Plot,ncol=6)+
  theme_classic() + ylab("Heavy Crop [kg/tree]")
ggsave(paste0(getwd(),"/Analysis/ElNino/Yield_pertree_plot.seasons.comparison.pdf"))

ggplot(y6, aes(Month,Monthly.harvest.tree))+geom_line(aes(color=Plot))+facet_wrap(~transect,ncol=1)+
  theme_classic() +ylab("Harvest [kg/tree]")
ggsave(paste0(getwd(),"/Analysis/ElNino/Yield_pertree_transect.monthly.comparison.pdf"))

ggplot(y3, aes(season,HeavyCrop))+geom_boxplot()+facet_wrap(~distance,ncol=2)+theme_classic()
ggsave(paste0(getwd(),"/Analysis/ElNino/Yield_pertree_distance.seasons.comparison.pdf"),height=6, width = 7)

#cocoa pod carbon production
pdw<-read.table(paste0(getwd(),"/Yield/Monthly_podremovals.csv"),sep=",",header=T)
#avg<-read.table(paste0(getwd(),"/cocoaYield.csv"),sep=",",header=T)
#add month
pdw$date<-as.Date(paste(pdw$year,pdw$month,"01",sep="-"))
c.f <- read.csv(paste0(getwd(),"/Yield/Pod Measurments/correction.factors.csv"))
cf <- c.f %>% filter(Disease=="Normal")
#sum all rows by pod size
pdw <- pdw %>% group_by(plot,treeno,date) %>% mutate(Tlpods=sum(l.harv,l.mam,abs.l.over.,na.rm=T),Tmpods=sum(m.harv,m.bp,m.cpb,m.mam,abs.m.over.,na.rm=T),
                                                     Tspods=sum(s.harv,s.bp,s.cpb,s.mam,abs.s.over.,na.rm=T))
#pdw$Tpods<-rowSums(pdw[,23:33])
plts<-c("KA 100 F3","KA 100 F1","KA 500 F3","KA 1K F3","HM 5K F2","HM 500 F3","HM 100 F3","HM 500 F2")
plots<-read.csv(paste0(getwd(),"/plots.csv"))

for(i in 1:length(plts)){
  #total<-data.frame(month=character(),year=character(),shell=numeric(),beans=numeric(),stringsAsFactors=F)
  p<-pdw %>% filter(plot==plts[i]) %>% ungroup()
  
  plotcode<- plots %>% filter(name3==plts[i]) %>% pull(PlotCode) %>% as.character()
  
  #p$Date<-as.Date(as.character(p$Date),format="%d/%m/%Y")
  #find unique dates
  d <- p %>% pull(date) %>% as.Date() %>% unique() %>% sort()
  d<-d[d>"2014-08-01"]
  
  summ <- p %>% group_by(plot,date,treeno) %>% summarise(TShell_kgC=sum(Tlpods*cf[cf$SizeClass=="Large","ShellCarbon"],Tmpods*cf[cf$SizeClass=="Medium","ShellCarbon"],Tspods*cf[cf$SizeClass=="Small","ShellCarbon"],na.rm=T)/1000,
                                                  TBean_kgC=sum(Tlpods*cf[cf$SizeClass=="Large","BeanCarbon"],Tmpods*cf[cf$SizeClass=="Medium","BeanCarbon"],Tspods*cf[cf$SizeClass=="Small","BeanCarbon"],na.rm=T)/1000)
  total <- summ %>% group_by(plot,date) %>% summarise(TShell_kgC=mean(TShell_kgC,na.rm=T),TBean_kgC=mean(TBean_kgC,na.rm=T)) 
  total1 <- summ %>% group_by(plot,date) %>% summarise(TShell_kgCse=sd(TShell_kgC,na.rm=T)/sqrt(18),TBean_kgCse=sd(TBean_kgC,na.rm=T)/sqrt(18))
  
  total <- left_join(total,total1,by=c("plot","date"))
  #total<-ddply(p,.(plot,date),summarise,TShell_kgC=sum(Tpods)*cf[1,1]/2.1097,TBean_kgC=sum(Tpods)*cf[1,2]/2.1097/1000/18)
  #multiply by number of cocoa trees, per census 
  census   <- data.frame(lapply(read.csv(paste0(getwd(),"/AGB/ForestPlots/",gsub(" ","",plts[i]),"_LS.csv")) , as.character),stringsAsFactors=F)
  #colnames(census)<-c("X","Subplot","x.coord","y.coord","Tag","Fam","Genus", "Species","DBH1", "DBH2","POM","Height","TreeCodes","Notes", "PlotCode", "PlotNum","OrigSpecies","NSpecies","NFam","THeight")         
  no.cocoa1 <- round(nrow(census[census$NSpecies=="Theobroma cacao"&as.numeric(census$DBH2)>10&!is.na(census$DBH),])/0.36)
  no.cocoa2 <- round(nrow(census[census$NSpecies=="Theobroma cacao"&as.numeric(census$DBH3)>10&!is.na(census$DBH3),])/0.36)
  
  total1 <- total %>% filter(date<"2015-11-01") %>% mutate(no.trees=no.cocoa1,TShell_MgCperha=TShell_kgC*no.cocoa1/1000,TBean_MgCperha=TBean_kgC*no.cocoa1/1000,
                                                           TShell_MgCperhase=TShell_kgCse*no.cocoa1/1000,TBean_MgCperhase=TBean_kgCse*no.cocoa1/1000)
  total2 <- total %>% filter(date>="2015-11-01") %>% mutate(no.trees=no.cocoa2,TShell_MgCperha=TShell_kgC*no.cocoa2/1000,TBean_MgCperha=TBean_kgC*no.cocoa2/1000,
                                                            TShell_MgCperhase=TShell_kgCse*no.cocoa1/1000,TBean_MgCperhase=TBean_kgCse*no.cocoa1/1000)
  
  total <- bind_rows(total1,total2)
  
  write.csv(total,paste0(getwd(),"/NPP/Total/",gsub(" ","",plts[i]),"_NPPcocoapods.csv"))
}

