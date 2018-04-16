#code for analyzing raw datasheets of fruit set and comparison with collected yield ata
library(gdata)
library(stringr)
library(ggplot2)
library(lubridate)
library(plyr)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Yield/yield data")
trans<-c("AB","HM","KA")

ns<-read.csv("//Volumes/ELDS/ECOLIMITS/Ghana/Kakum/plots.csv")

#generation of buying company weights
final<-list()
for(i in 1:length(trans)){
  dataF<-read.xls(paste0(getwd(),"/Combined_yield-Revised.xlsx"), sheet=trans[i])
  totals<-data.frame(Farm=character(),FarmSize=numeric(),Month=numeric(),DaysDry=numeric(),TotalKg=numeric(),ClerksKg=numeric(),Payment=numeric(),stringsAsFactors=F)
  #find unique farmer/plots using farmsize
  plt<-as.character(unique(dataF$Plot.Number))
  for(m in 1:length(plt)){
    Fm<-dataF[dataF$Plot.Number==plt[m],]
    fm<-unique(Fm$Farm.Size)
    for(j in 1:length(fm)){
      Fm.1<-Fm[Fm$Farm.Size==fm[j],]
      #find number of months reported
      mo<-unique(Fm.1$Date.of.harvest)
      if(as.character(mo)=="") totals[nrow(totals)+1,]<-c(as.character(Fm.1$Plot.Number),as.numeric(Fm.1$Farm.Size),"Total"," ",0,0,0)
      if(as.character(mo)=="") next
      for(k in 1:length(mo)){
        x<-Fm.1[Fm.1$Date.of.harvest==mo[k],]
        totals[nrow(totals)+1,]<-c(as.character(x$Plot.Number),as.numeric(x$Farm.Size),month(unique(as.Date(x$Date.of.harvest))),as.numeric(unique(as.Date(x$Date.of.weighing)-as.Date(x$Date.of.harvest))),sum(x$Total.weight...Kg..ESPA.point,na.rm=T),sum(x$Clerk.s.Weight..Kg.),sum(x$Clerk.s.Weight..Kg./64*x$Price.per.bag.GHS-x$Amount.owed.to.Clerk))
      }
      totals[nrow(totals)+1,]<-c(as.character(x$Plot.Number),as.numeric(x$Farm.Size),"Total"," ",sum(as.numeric(totals[totals$FarmSize==x$Farm.Size,"TotalKg"])),sum(as.numeric(totals[totals$FarmSize==x$Farm.Size,"ClerksKg"])),sum(as.numeric(totals[totals$FarmSize==x$Farm.Size,"Payment"])))
    }
    totals$Payment<-round(as.numeric(totals$Payment),2)
    final[[i]]<-totals
  }
}
dF<-do.call(rbind.data.frame,final)
#calculate plot yield and reported yield
dF[dF$Month=="Total","ActualYield"]<-as.numeric(dF[dF$Month=="Total","TotalKg"])/as.numeric(dF[dF$Month=="Total","FarmSize"])
dF[dF$Month=="Total","ReportedYield"]<-as.numeric(dF[dF$Month=="Total","ClerksKg"])/as.numeric(dF[dF$Month=="Total","FarmSize"])

#write out combined data
write.csv(dF,paste0(getwd(),"/CombinedYield.csv"))
rm(dF,Fm,Fm.1,totals,x,final,dataF,plt,fm)

#generation of correction factors by pod sizes
setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Yield/Pod Measurments")

dataF<-read.csv(paste0(getwd(),"/pod.measures.csv"))
dataF<-dataF[,1:13]
#sizes of pods small (5-10 cm), medium (10-20cm), large (>20 cm)
s<-dataF[as.numeric(dataF$Length)<=11,]
m<-dataF[as.numeric(dataF$Length)>11&as.numeric(dataF$Length)<20,]
l<-dataF[as.numeric(dataF$Length)>=20,]
#remove NA for widths
#s<-s[!is.na(s$Middle),]
#m<-m[!is.na(m$Middle),]
#l<-l[!is.na(l$Middle),]

#remove mistakes *
s<-s[!is.na(s$TotalWeight),]
m<-m[!is.na(m$TotalWeight),]
l<-l[!is.na(l$TotalWeight),]

#calculate volume of pods
s$Volume<-s$Middle/2/sqrt(s$Length/2)*((s$Length^2)/2)
m$Volume<-m$Middle/2/sqrt(m$Length/2)*((m$Length^2)/2)
l$Volume<-l$Middle/2/sqrt(l$Length/2)*((l$Length^2)/2)

#calculate beans weight
s$BeanWeight<-as.numeric(as.character(s$TotalWeight))-s$ShellWeight-s$Discardedbeans
s$Bweight<-s$BeanWeight/s$NoBeans
s$BeanBiomass<-s$BeanWeight*(1-.6)
s$ShellBiomass<-s$ShellWeight*(1-.141) #derived from Daud et al (2013)
s$BeanCarbon<-s$BeanWeight*(1-.6)/2.1097
s$ShellCarbon<-s$ShellWeight*(1-.141)/2.1097

m$BeanWeight<-as.numeric(as.character(m$TotalWeight))-m$ShellWeight-m$Discardedbeans
m$Bweight<-m$BeanWeight/m$NoBeans
m$BeanBiomass<-m$BeanWeight*(1-.6)
m$ShellBiomass<-m$ShellWeight*(1-.141)
m$BeanCarbon<-m$BeanWeight*(1-.6)/2.1097
m$ShellCarbon<-m$ShellWeight*(1-.141)/2.1097

l$BeanWeight<-as.numeric(as.character(l$TotalWeight))-l$ShellWeight-l$Discardedbeans
l$Bweight<-l$BeanWeight/l$NoBeans
l$BeanBiomass<-l$BeanWeight*(1-.6)
l$ShellBiomass<-l$ShellWeight*(1-.141)
l$BeanCarbon<-l$BeanWeight*(1-.6)/2.1097
l$ShellCarbon<-l$ShellWeight*(1-.141)/2.1097

#remove any negative shell weight values
m<-m[m$BeanWeight>0,]

#separate diseased pods
m0<-m[m$BlackPod=="Y",]
m1<-m[m$Capsid==">10",]
m<-m[m$BlackPod!="Y"&m$Capsid==0,]

l0<-l[l$BlackPod=="Y",]
l1<-l[l$Capsid==">10",]
l<-l[l$BlackPod!="Y"&l$Capsid==0,]

#calculate averages for each size class 
final<-data.frame(t(colMeans(s[,14:20])))
final$SizeClass<-"Small"
final$Disease<-"Normal"

#<-c(mean(s$Volume),mean(s$BeanWeight),mean(s$Bweight),mean(s$BeanCarbon),mean(s$ShellCarbon),mean(s$density))
final[2,1:7]<-colMeans(m[,14:20],na.rm=T)
final[2,8:9]<-c("Medium","Normal")
final[3,1:7]<-colMeans(m0[,14:20],na.rm=T)
final[3,8:9]<-c("Medium","BlackPod")
final[4,1:7]<-colMeans(m1[,14:20],na.rm=T)
final[4,8:9]<-c("Medium","Capsid")
final[5,1:7]<-colMeans(l[,14:20],na.rm=T)
final[5,8:9]<-c("Large","Normal")
final[6,1:7]<-colMeans(l0[,14:20],na.rm=T)
final[6,8:9]<-c("Large","BlackPod")
final[7,1:7]<-colMeans(l1[,14:20],na.rm=T)
final[7,8:9]<-c("Large","Capsid")

#write average bean and shell weights per size class
write.csv(final,paste0(getwd(),"/correction.factors.csv"))

