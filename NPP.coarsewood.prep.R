# Coarse wood NPP

# This program prepares raw datasheets for later analysis with coarsewoodNPP_espa.R

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/NPP/CWD")

#list plots to loop through
plts<-c("HM FP","KA FP","KA 100 F3","KA 100 F1","KA 500 F3","KA 1K F3","HM 5K F2","HM 500 F3","HM 100 F3","HM 500 F2","AB 100 F1","AB 500 F2","AB 1K F2","AB 5K F2","AB FP")

#months of sampling
#m<-c("Sept 14","Jan 15","Apr 15","July 15","Oct 15","Jan 16", "Apr 16")
#assign transect numbers
#(1) 0,0 to 0,60, (2) 0,60 to 60,60, (3) 60,60 to 60,0, (4) 60,0 to 0,0
#assign diameter classes
#2-5 (s), 5-10 (m), >10 (l)

#subtract bag size (in g)
bs<-50

require(gdata)
require(lubridate)
library(tidyverse)

for(i in 1:length(plts)){
  dataw<-read.xls(paste0(getwd(),"/Combo_plotlevel.xlsx"),sheet=plts[i])
  dataw<-dataw[dataw$X!=""&dataw$X!="Transect",]
  dataw<-data_frame(no=c(1:nrow(dataw)),date=as.Date(dataw$Ground.Coarse.Litter.Mass....2.cm.Diameter.),transect=as.character(dataw$X),d1=as.numeric(as.character(dataw$X.1)),d2=as.numeric(as.character(dataw$X.2)),d3=as.numeric(as.character(dataw$X.3)),avd=as.numeric(as.character(dataw$X.4)),
                    length=as.numeric(as.character(dataw$X.5)),weight=as.character(dataw$X.6),decomp_cat=as.character(dataw$X.7),diam_cat=as.character(dataw$X.8))

  #change entries to transect number
  dataw<-dataw %>% mutate(transect= replace(transect, transect=="0,0-60,0",4),transect= replace(transect, transect=="60,0-0,0",4),transect = replace(transect,transect=="60,60-60,0",3),transect = replace(transect,transect=="0,60-60,60",2),
                        transect = replace(transect,transect=="60,0-60,60",3),transect = replace(transect,transect=="0,0-0,60",1),transect = replace(transect,transect=="60,60-0,60",2),transect = replace(transect,transect=="0,60-0,0",1))
  #replace diameter classes with letters
  dataw <- dataw %>% mutate(diam_cat=replace(diam_cat,diam_cat==">10","l"),diam_cat=replace(diam_cat,diam_cat=="5-10","m"),diam_cat=replace(diam_cat,diam_cat=="2-5","s"))
  
  #find month and year
  dataw <- dataw %>% mutate(month=paste(month(date,label=T),year(date),sep="-"))
  
  w<- dataw %>% filter(weight!="") %>% mutate(decomp_cat=do.call(rbind.data.frame,strsplit(as.character(weight),") "))[,1],weight.kg=do.call(rbind.data.frame,strsplit(as.character(weight),") "))[,2])
  w<- w %>% mutate(weight.kg=as.numeric(as.character(gsub("kg","",weight.kg)))*1000-bs,diam_cat=as.character(do.call(rbind.data.frame,strsplit(as.character(decomp_cat),"[(]"))[,1]),decomp_cat=as.character(do.call(rbind.data.frame,strsplit(as.character(decomp_cat),"[(]"))[,2]))
  w<-w %>% mutate(diam_cat=replace(diam_cat,diam_cat==">10","l"),diam_cat=replace(diam_cat,diam_cat=="5-10","m"),diam_cat=replace(diam_cat,diam_cat=="2-5","s"))
  
  #find/calculate density from lab worksheet
  lab<-data.frame(read.xls(paste0(getwd(),"/CWD_lab.xlsx"),sheet= plts[i]))
  lab<-data_frame(month=as.character(lab$Date),diam_cat=gsub(" ","",as.character(do.call(rbind.data.frame,strsplit(as.character(lab$Sample.ID),"[(]"))[,1])),
                  decomp_cat = gsub(")","",as.character(do.call(rbind.data.frame,strsplit(as.character(lab$Sample.ID),"[(]"))[,2])),
                  fw=lab$Fresh.Weight,fw1=lab$Fresh.Weight.1,fw2=lab$Fresh.Weight.2,fw3=lab$Fresh.Weight.3,fw4=lab$Fresh.Weight.4,fw5=lab$Fresh.Weight.5,
                  fw6=lab$Fresh.Weight.6,fw7=lab$Fresh.Weight.7,fw8=lab$Fresh.Weight.8,fw9=lab$Fresh.Weight.9,dw=lab$Dry.Weight,dw1=lab$Dry.Weight.1,dw2=lab$Dry.Weight.2,
                  dw3=lab$Dry.Weight.3,dw4=lab$Dry.Weight.4,dw5=lab$Dry.Weight.5,dw6=lab$Dry.Weight.6,dw7=lab$Dry.Weight.7,dw8=lab$Dry.Weight.8,dw9=lab$Dry.Weight.9,final=lab$Final.Vol,
                  final1=lab$Final.Vol.1,final2=lab$Final.Vol.2,final3=lab$Final.Vol.3,final4=lab$Final.Vol.4,final5=lab$Final.Vol.5,final6=lab$Final.Vol.6,final7=lab$Final.Vol.7,
                  final8=lab$Final.Vol.8,final9=lab$Final.Vol.9)
 
  lab <- lab %>% mutate(month=paste0(do.call(rbind.data.frame,strsplit(month,"-"))[,1],"-20",do.call(rbind.data.frame,strsplit(month,"-"))[,2]))
  lab<-lab %>% mutate(diam_cat=replace(diam_cat,diam_cat==">10","l"),diam_cat=replace(diam_cat,diam_cat=="5-10","m"),diam_cat=replace(diam_cat,diam_cat=="2-5","s"))
  
  #calculate density from lab worksheet (g/cm3) [dry weight/volume-350 ml]
  tmp<-lab %>% select(diam_cat,decomp_cat,month,grep("dw",colnames(lab))) %>% gather(key="category",value="dryweight",c(-diam_cat,-decomp_cat,-month))
  tmp1<-lab %>% select(diam_cat,decomp_cat,month,grep("final",colnames(lab))) %>% gather(key="category",value="volume",c(-diam_cat,-decomp_cat,-month))
  
  tmp<-tmp %>% group_by(month,diam_cat,decomp_cat) %>% summarise(dryweight=sum(dryweight,na.rm=T))
  tmp1<-tmp1 %>% group_by(month,diam_cat,decomp_cat) %>% summarise(volume=sum(volume-350,na.rm=T))
  
  tmp<-left_join(tmp,tmp1,by=c("diam_cat","decomp_cat","month"))
  tmp<-tmp %>% group_by(month,diam_cat,decomp_cat) %>% mutate(density=dryweight/(volume)) %>%
    ungroup()

  w<-left_join(w,tmp %>% select(diam_cat,decomp_cat,month,density),by=c("diam_cat","decomp_cat","month"))
  
  #check if the month is off by one
  tmp<- tmp %>% mutate(month=paste0(month(month(as.Date(paste("01-",month,sep=""),format="%d-%b-%Y"))+1,label=T),"-",year(as.Date(paste("01-",month,sep=""),format="%d-%b-%Y"))))
  w<-left_join(w,tmp %>% select(diam_cat,decomp_cat,month,density), by=c("diam_cat","decomp_cat","month"))
  w<- w %>% group_by(date,decomp_cat,diam_cat) %>% mutate(density=sum(density.x,density.y,na.rm=T))
  
  w2<- w %>% group_by(decomp_cat,diam_cat) %>% summarise(m.density=mean(density,na.rm=T))
  w3 <- w2 %>% group_by(decomp_cat)  %>% summarise(m.density=mean(m.density,na.rm=T))
  
  w<-left_join(w,w2, by=c("decomp_cat","diam_cat"))
  w<-w %>% group_by(date,decomp_cat,diam_cat) %>% mutate(density=replace(density,density==0,m.density[density==0])) %>% 
    ungroup()

  #how to join WITHOUT adding rows....?!!!!
  dataw <- left_join(dataw,w %>% select(weight,weight.kg),by="weight")
  dataw <- left_join(dataw,w %>% select(date,decomp_cat,diam_cat,density), by=c("date","decomp_cat","diam_cat"))
  #
  #fill in missing density values
  dataw<-left_join(dataw,w2,by=c("decomp_cat","diam_cat"))
  dataw <- dataw %>% mutate(density=replace(density,is.na(density),m.density[is.na(density)]))
  
  #fill in missing density values, if still missing by diam_cat
  dataw<-left_join(dataw,w3,by=c("decomp_cat"))
  dataw <- dataw %>% mutate(density=replace(density,is.na(density),m.density.y[is.na(density)]))
  dataw <- dataw %>% mutate(density=replace(density,density==0,m.density.y[density==0]))
  
  if(nrow(dataw %>% filter(is.na(density))) >0) break
  if(nrow(dataw %>% filter(density==0))>0) break
  
  #remove duplicate rows
  dataw<- dataw %>% distinct(no,.keep_all = TRUE)
  
  final<- data_frame(year=year(dataw$date),month=month(dataw$date),day=day(dataw$date),plot=plts[i],transect=dataw$transect,diameter_class=dataw$diam_cat,decomposition_class=dataw$decomp_cat,density=dataw$density,dry_weight=dataw$weight.kg,width_1=dataw$d1,width_2=dataw$d2,width_3=dataw$d3,length_1=dataw$length)
   
  write.csv(final,paste0(getwd(),"/",gsub(" ","",plts[i]),"_CWD.csv"))
  rm(w,w2,tmp,tmp1,lab,dataw,final)
}
