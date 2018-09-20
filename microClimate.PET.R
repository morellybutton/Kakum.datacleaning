#calculating radiation for each plot using Yoder et al (2005), Meza and Varas (2000) & Allen (1993) FAO 56
#with input from Nico Raab [can look more at Campbell and Norm (1998)--Intro to Environmental Biophysics]

#will use Turc 1961 equation
#ET0=aT*0.013*meanT/(meanT+15)*(23.8856*Rs+50)/lambda
#latent heat vaporization, lambda = 2.45 [MJ/kg]
lambda<-2.45
#meanT=mean daily temperature [(Tmax+Tmin)/2]
#aT[for RHmean=50%]=1.0 or aT[for RHmean<50%]=1.0+(50-RHmean)/70
#Rs solar radiation at atmosphere

#calculating Rs [MJ/m2/day] from Ra [extra-terrestrial radiation], Meza and Varas (2000)
#Ra=86400*1360/pi*(dm/d)^2*[Hs*sin(phi)*sin(gamma)+cos(phi)*cos(gamma)*sin(Hs)]
#where:
#d=distance from sun to earth (km)
#dm=mean distance sun-earth (km)
#or dr=dm/d given by, given by eqn: 1+0.033*cos(2*pi/365*JulianDay)
#phi=latitude (radians), multiply lattitude (in degrees) by pi/180
#gamma=solar declination (radians), given by eqn: 0.409*sin(2*pi/365*JulianDay-1.39)
#Hs=solar angle at sunrise/sunset (radians), given by eqn: acos(-tan(phi)*tan(gamma))

#or FAO equation, which is
#Ra=24*60/pi*Gsc*dr*(Hs*sin(phi)*sin(gamma)+cos(phi)*cos(gamma)*sin(Hs))
#where Gsc is the solar constant = 0.082 MJ/m2/min
Gsc<-0.082
#Rs is a subset of Ra given by eqn: Rs/Ra=a + b*n/N Angstrom model (1924)
#where a=0.25, b=0.5, n=actual sunshine hours (or time period with solar radiation > 120 W/m2) and N=theoretical sunshine hours, by the eqn: N=24/pi*Hs
#whereby Rs=Ra*(a+b*n/N)
a<-0.25
b<-0.5

#calculate infiltration factor using Kostiakov equation F=alpha*kappa*t^(alpha-1), alpha and kappa taken from tables

library(lubridate)
#library(plyr)
library(ggplot2)
library(gridExtra)
library(reshape)
library(gtools)
library(tidyverse)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/MetData")
names<-data.frame(read.csv(paste0(getwd(),"/MS_names.csv")), stringsAsFactors=FALSE)
ns<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/plots.csv")
dnames<-data.frame(read.csv(paste0(getwd(),"/DL_names.csv")), stringsAsFactors=FALSE)
latlon<-data.frame(read.csv(paste0("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/kakumplots_latlon.csv")),stringsAsFactors = F)
latlon.ms<-data.frame(read.csv(paste0("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Kakum_metstation.csv")),stringsAsFactors = F)
ppt<-data.frame(read.csv(paste0(getwd(),"/LargeMetstation_dailyppt.csv")))
soil<-data.frame(read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Nutrients/Soils/Soil_variables.csv"))
  
#open met station data and sum all hours per day greater than .12 kw
met<-data.frame(read.csv(paste0(getwd(),"/",names[11,"Mstation"],"_Table1.csv"), stringsAsFactors=FALSE))
met[,1]<-as.POSIXct(met[,1],format="%d/%m/%Y %H:%M")
colnames(met)<-c("date","no","voltage","ptemp","temp","rh","slrkw","slrmj","wsms","wind.dir","ppt")

#remove extraneous rows and get hourly radiation
met<-na.omit(met)

#identify days
dates<-unique(met$day)

#create dataframe of mj values
met$day<-as.Date(cut(met[,1],breaks="day"))
met.mj<-data.frame(as.Date(met$day),met$date,stringsAsFactors = F)
colnames(met.mj)<-c("day","date")

met.mj$slrmj<-met$slrmj
met.mj$hour<-hour(cut(met[,1],breaks="hour"))

met.rad<-met.mj %>% group_by(day,hour) %>% summarise(radiation=sum(as.numeric(slrmj),na.rm=T))
nas<-which(is.na(met.rad$radiation))

#fill in missing values
for(i in 1:length(nas)){
  met.rad[nas[i],"radiation"]<-mean(met.rad[(nas[i]-1):(nas[i]+1),"radiation"],na.rm=T)
}

#sum hours of sunshine per day
met.rad$sunshine<-met.rad$radiation>.01
met.rad$sunshine<-met.rad$sunshine+0
met.rad$month<-as.Date(cut(as.Date(met.rad$day),breaks="month"))

met.kw<-data.frame(as.Date(met$day),met$date)
colnames(met.kw)<-c("day","date")
met.kw$slrkw<-met$slrkw
#sum hours of sunshine per day
met.kw$sunshine<-met.kw$slrkw>.12
met.kw$sunshine<-met.kw$sunshine+0

sunshine<-met.kw %>% group_by(day) %>% summarise(daylight=sum(sunshine)/2)

sunshine$month<-as.Date(cut(as.Date(sunshine$day),breaks="month"))
daycount <- table(sunshine$day) 

# generate vector of all dates[taken from http://www.r-bloggers.com/fix-missing-dates-with-r/]
alldays <- seq(as.Date(min(sunshine$day,na.rm=T)),length=as.numeric(sunshine[nrow(sunshine[!is.na(sunshine$day),]),"day"]-as.numeric(min(sunshine$day,na.rm=T))+1),by="+1 day")  
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

#create hourly dataset for inserting filled data
allhours <-data.frame(seq(from = as.POSIXct(paste0(min(met.rad$day)+1," 01:00")), 
                          to = as.POSIXct(paste0(max(met.rad$day)-1," 23:00")), by = "hour"),stringsAsFactors = F)
colnames(allhours)<-c("time")
allhours$day<-as.Date(allhours$time)
allhours$hour<-hour(allhours$time)
allhours$radiation<-met.rad[match(interaction(allhours$day,allhours$hour),interaction(met.rad$day,met.rad$hour)),"radiation"]

#for all days where daylight data is missing put diurnal average for month
ds.1<-data.frame(day=as.Date(character()),month=as.Date(character()),stringsAsFactors = F)
ds.1[1:nrow(alldaycount[alldaycount[,1]==0,]),1]<-alldaycount[alldaycount[,1]==0,2]

#add month
ds.1$month<-as.Date(cut(as.Date(ds.1[,1]),breaks="month"))
m<-unique(ds.1$month)
d.s.1<-list()
for(i in 1:length(m)){
  d.s.1[[i]]<-met.rad %>% filter(month==m[i]) %>% group_by(month,hour) %>% summarise(radiation.hr=mean(radiation,na.rm=T))
}
d.s.1<-do.call(rbind.data.frame,d.s.1)

for(i in 1:nrow(ds.1)){
  tmp<-d.s.1[d.s.1$month==ds.1[i,"month"],]
  allhours[allhours$day==ds.1$day[i],"radiation"]<-tmp[match(allhours[allhours$day==ds.1$day[i],"hour"],tmp$hour),"radiation.hr"]
}

#for all days where daylight data is missing put daily average for month
ds<-data.frame(alldaycount[alldaycount[,1]==0,2])
colnames(ds)<-"day"
#add month
ds$month<-as.Date(cut(as.Date(ds[,1]),breaks="month"))
m<-unique(ds$month)
for(i in 1:length(m)){
  ds[ds$month==m[i],"avg"]<-mean(sunshine[sunshine$month==m[i],"daylight"],na.rm=T)
}

#add average ppt for missing points with daily ppt measures
alldaycount[match(as.Date(sunshine$day),as.Date(alldaycount$V2),nomatch=0),"daylight"]<-sunshine$daylight
alldaycount[match(ds$day,alldaycount$V2),"daylight"]<-ds$avg

alldaycount$JulianDay<-yday(alldaycount$V2)
#calculate latitude in radians
phi<-latlon.ms$Y*pi/180
alldaycount$gamma<-0.409*sin(2*pi/365*alldaycount$JulianDay-1.39)
alldaycount$Hs<-acos(-tan(phi)*tan(alldaycount$gamma))
alldaycount$n_N<-alldaycount$daylight/(24/pi*alldaycount$Hs)

#sum hourly radiation levels to get daily radiation
alldaycount.1<-allhours %>% group_by(day) %>% summarise(Rs=sum(radiation,na.rm=T)*2)

alldaycount.1$month<-as.Date(cut(as.Date(alldaycount.1$day),breaks="month"))

#identify days with missing hours of measurement and supply average monthly value
alldaycount.1[alldaycount.1$Rs<1,"Rs"]<-NA
miss<-alldaycount.1[is.na(alldaycount.1$Rs),]

for(i in 1:nrow(miss)){
  alldaycount.1[alldaycount.1$day==miss[i,"day"],"Rs"]<-mean(alldaycount.1[alldaycount.1$month==miss[i,"month"],"Rs"],na.rm=T)
}

PET<-list()
MET<-list()
for(i in 1:(length(names[,1])-1)){
  dF<-data.frame(read.csv(paste0(getwd(),"/",names[i,"Mstation"],"_summary.csv")),stringsAsFactors = F)
  dF<-dF[!is.na(dF$day),]
  tmp<-dF
  tmp$Plot<-as.character(names[i,"Plot.2"])
  MET[[i]]<-tmp
  rm(tmp)
  #insert avg for missing RH
  dF[is.na(dF$RHmean),"RHmean"]<-mean(dF$RHmean,na.rm=T)
  #calculate meanT from max and min T
  dF$meanT<-(dF$Tmax+dF$Tmin)/2
  #convert dates to Julian Day
  dF$JulianDay<-yday(dF$day)
  #calculate dr
  dF$dr<-1+0.033*cos(2*pi/365*dF$JulianDay)
  #calculate latitude in radians
  phi<-latlon[match(names[i,"Plot.2"],latlon$name2),"Y"]*pi/180
  #calculate gamma and Hs
  dF$gamma<-0.409*sin(2*pi/365*dF$JulianDay-1.39)
  dF$Hs<-acos(-tan(phi)*tan(dF$gamma))
  #calculate daily Ra
  #dF$Ra<-86400*1360/pi*(dF$dr)^2*(dF$Hs*sin(phi)*sin(dF$gamma)+cos(phi)*cos(dF$gamma)*sin(dF$Hs))
  dF$Ra<-24*60/pi*Gsc*dF$dr*(dF$Hs*sin(phi)*sin(dF$gamma)+cos(phi)*cos(dF$gamma)*sin(dF$Hs))
  #convert Ra to Rs
  dF$Rs<-dF$Ra*(a+b*alldaycount[match(dF$day,alldaycount$V2),"n_N"])
  #compare with measured radiation
  dF$Rs.1<-alldaycount.1[match(as.Date(dF$day),alldaycount.1$day),"Rs"]
  #assign aT depending RH
  dF[dF$RHmean>=50,"aT"]<-1
  dF[dF$RHmean<50,"aT"]<-1.0+(50-dF[dF$RHmean<50,"RHmean"])/70
  #calculate evapotranspiration
  dF$ET0<-dF$aT*0.013*dF$meanT/(dF$meanT+15)*(23.8856*dF$Rs+50)/lambda
  dF$ET0.1<-dF$aT*0.013*dF$meanT/(dF$meanT+15)*(23.8856*dF$Rs.1+50)/lambda
  #remove ET0 NAs
  dF<-dF[!is.na(dF$ET0)&!is.na(dF$ET0.1),]
  pet<-cbind(dF[,2:3],dF[,6:7],dF$day,dF$ET0,dF$ET0.1)
  colnames(pet)<-c(colnames(pet[,1:4]),"day","ET0","ET0.1")
  pet$Plot<-names[i,"Plot.2"]
  PET[[i]]<-pet
}

MET.1<-list()
#do again for dataloggers
for(i in 1:(length(dnames[,1]))){
  dF<-read.csv(paste0(getwd(),"/dataloggers/summary/",dnames[i,"Datalogger.id"],"_summary.csv"))
  dF<-dF[!is.na(dF$day),]
  tmp<-dF
  tmp$Plot<-as.character(dnames[i,"Plot.2"])
  MET.1[[i]]<-tmp
  #insert avg for missing RH
  dF[is.na(dF$RHmean),"RHmean"]<-mean(dF$RHmean,na.rm=T)
  #calculate meanT from max and min T
  dF$meanT<-(dF$Tmax+dF$Tmin)/2
  #convert dates to Julian Day
  dF$JulianDay<-yday(dF$day)
  #calculate dr
  dF$dr<-1+0.033*cos(2*pi/365*dF$JulianDay)
  #calculate latitude in radians
  phi<-latlon[match(dnames[i,"Plot.2"],latlon$name2),"Y"]*pi/180
  #calculate gamma and Hs
  dF$gamma<-0.409*sin(2*pi/365*dF$JulianDay-1.39)
  dF$Hs<-acos(-tan(phi)*tan(dF$gamma))
  #calculate daily Ra
  #dF$Ra<-86400*1360/pi*(dF$dr)^2*(dF$Hs*sin(phi)*sin(dF$gamma)+cos(phi)*cos(dF$gamma)*sin(dF$Hs))
  dF$Ra<-24*60/pi*Gsc*dF$dr*(dF$Hs*sin(phi)*sin(dF$gamma)+cos(phi)*cos(dF$gamma)*sin(dF$Hs))
  #convert Ra to Rs
  dF$Rs<-dF$Ra*(a+b*alldaycount[match(dF$day,alldaycount$V2),"n_N"])
  #compare with measured radiation
  dF$Rs.1<-alldaycount.1[match(as.Date(dF$day),alldaycount.1$day),"Rs"]
  #assign aT depending RH
  dF[dF$RHmean>=50,"aT"]<-1
  dF[dF$RHmean<50,"aT"]<-1.0+(50-dF[dF$RHmean<50,"RHmean"])/70
  #calculate evapotranspiration
  dF$ET0<-dF$aT*0.013*dF$meanT/(dF$meanT+15)*(23.8856*dF$Rs+50)/lambda
  dF$ET0.1<-dF$aT*0.013*dF$meanT/(dF$meanT+15)*(23.8856*dF$Rs.1+50)/lambda
  #remove ET0 NAs
  dF<-dF[!is.na(dF$ET0)&!is.na(dF$ET0.1),]
  pet<-cbind(dF[,2:5],dF$day,dF$ET0,dF$ET0.1)
  colnames(pet)<-c(colnames(pet[,1:4]),"day","ET0","ET0.1")
  pet$Plot<-dnames[i,"Plot.2"]
  PET[[i+10]]<-pet
}
met<-do.call(rbind.data.frame,MET)
met.1<-do.call(rbind.data.frame,MET.1)
final<-smartbind(met,met.1)

write.csv(final,paste0(getwd(),"/summary/All_plots_summary.csv"))
rm(met,met.1,final)

et.0<-do.call(rbind.data.frame,PET)
#add month
et.0$month<-as.Date(cut(as.Date(et.0$day),breaks="months"))

#calculate daily ppt available for infiltration (left after runoff)
#met data
met<-data.frame(read.csv(paste0("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/MetData/ESPA_G11_Table1.csv"), stringsAsFactors=FALSE))
met[,1]<-as.POSIXct(met[,1],format="%d/%m/%Y %H:%M")

colnames(met)<-c("date","no","voltage","ptemp","temp","rh","slrkw","slrmj","wsms","wind.dir","ppt")

#identify days
met$day<-as.Date(cut(met[,1],breaks="day"))
#identify hours
met$hour<-cut(met[,1],breaks="hour")
#remove extraneous rows
met<-na.omit(met)

dates<-unique(met$day)
#pull out unique textures
txt.1<-data.frame(as.character(unique(soil$Texture)),stringsAsFactors = F)
txt.1$irate<-soil[match(txt.1$as.character.unique.soil.Texture..,soil$Texture),"Infiltrate"]

#create tidy data
met<-data_frame(date = as.Date(met$date),no = as.numeric(met$no), voltage = as.numeric(met$voltage), ptemp = as.numeric(met$ptemp), temp = as.numeric(met$temp), 
                rh = as.numeric(met$rh), slrkw = as.numeric(met$slrkw), slrmj = as.numeric(met$slrmj), wsms = as.numeric(met$wsms), wind.dir=as.numeric(met$wind.dir), ppt = as.numeric(met$ppt),
                day = met$day, hour = met$hour)

et.0<-data_frame(Tmax=et.0$Tmax,Tmin=et.0$Tmin,VPDmax=et.0$VPDmax,RHmean=et.0$RHmean,day.x=as.Date(et.0$day),ET0=et.0$ET0,ET0.1=et.0$ET0.1,Plot=gsub(" ","",et.0$Plot),month=et.0$month)
soil<-data_frame(Plot=as.character(soil$Plot),Texture=gsub(" ","",soil$Texture))

#Calculate hourly ppt
#ppt.h<-data.frame(Day=as.Date(character()),Hour=numeric(),ppt=numeric())
ppt.h <- met %>% group_by(hour) %>% summarise(ppt=sum(ppt), day=mean(date))

#calculate run-off per texture, negative values are run-off and positive values refer to infiltration potential remaining
irate<-ppt.h %>% filter(ppt!=0) %>% mutate(Sandyloam=txt.1[6,2]-ppt,Siltyclay=txt.1[3,2]-ppt,Siltloam=txt.1[2,2]-ppt,Loamysand=txt.1[1,2]-ppt,FineSand=txt.1[4,2]-ppt,Sandyclay=txt.1[5,2]-ppt)
#add to main ppt dataframe
ppt.h<-left_join(ppt.h,irate,by="hour")

#from hourly rates calculate daily runoff
#runoff<-ddply(ppt.h,.(Day),summarise,Sandyloam=sum(Sandyloam[Sandyloam<0]),Siltyclay=sum(Siltyclay[Siltyclay<0]),Siltloam=sum(Siltloam[Siltloam<0]),Loamysand=sum(Loamysand[Loamysand<0]),FineSand=sum(FineSand[FineSand<0]),Sandyclay=sum(Sandyclay[Sandyclay>0]),Clay=sum(Clay[Clay>0]))
runoff<-ppt.h %>% group_by(day.x) %>% summarise(Sandyloam=sum(Sandyloam[Sandyloam<0],na.rm=T),Siltyclay=sum(Siltyclay[Siltyclay<0],na.rm=T),Siltloam=sum(Siltloam[Siltloam<0],na.rm=T),Loamysand=sum(Loamysand[Loamysand<0],na.rm=T),FineSand=sum(FineSand[FineSand<0],na.rm=T),Sandyclay=sum(Sandyclay[Sandyclay>0],na.rm=T))

#calculate daily infiltration rates and add to precip data
ppt<-data_frame(daycount=ppt$days.actindex..daycount.,day.x=as.Date(ppt$V2),Tppt=ppt$Tppt)
ppt.d <- left_join(ppt,runoff,by="day.x") %>% mutate( month = as.Date(cut(day.x,breaks="month")))

#for days with missing data, calculate average number of hours and average amount of rain a day (by month)
correct <- ppt.h %>% group_by(day.x) %>% summarise(no.hrs=length(ppt.x[ppt.x>0&!is.na(ppt.x)]),mm.hrs=mean(ppt.x[ppt.x>0&!is.na(ppt.x)])) %>% 
  mutate(month = as.Date(cut(day.x,breaks="month")))
m.correct <- correct %>% group_by(month) %>% summarise(no.hrs=mean(no.hrs),mm.hrs=mean(mm.hrs,na.rm=T))

ppt.d <- left_join(ppt.d,m.correct, by="month")

correct <- ppt.d %>%  filter(is.na(Sandyloam)) %>% group_by(month) %>%
  mutate(Tppt = mean(no.hrs*mm.hrs),Sandyloam=(txt.1[6,2]-mm.hrs)*no.hrs,Siltyclay=(txt.1[3,2]-mm.hrs)*no.hrs,Siltloam=(txt.1[2,2]-mm.hrs)*no.hrs,Loamysand=(txt.1[1,2]-mm.hrs)*no.hrs,FineSand=(txt.1[4,2]-mm.hrs)*no.hrs,Sandyclay=(txt.1[5,2]-mm.hrs)*no.hrs)

ppt.d <- bind_rows(ppt.d,correct) %>%
  filter(!is.na(Sandyloam)) %>% arrange(day.x)

#match daily ppt to et.0

et.0<-left_join(et.0, ppt.d %>% select(day.x,Tppt),by="day.x")
et.0<-left_join(et.0,soil,by="Plot")

wide <- data_frame(day.x=as.Date(ppt.d$day.x),Sandyloam=ppt.d$Sandyloam,Siltyclay=ppt.d$Siltyclay,Siltloam=ppt.d$Siltloam,Loamysand=ppt.d$Loamysand,FineSand=ppt.d$FineSand,Sandyclay=ppt.d$Sandyclay)
long <- wide %>% gather(key="Texture",value="runoff",-day.x)

et.0<-left_join(et.0,long)

#calculate infiltration by I=P+(-R)
et.0$infil<-et.0$Tppt+et.0$runoff

#calculate water stress by subtracting evapotranspiration from "infiltrated water" and taking cumulative sum per plot per month
et.0$diff<-et.0$infil-et.0$ET0
et.0$diff.1<-et.0$infil-et.0$ET0.1

#count number of days "diff" is negative
et.0[et.0$diff>0,"stress"]<-0
et.0[et.0$diff<0,"stress"]<-1

et.0[et.0$diff.1>0,"stress.1"]<-0
et.0[et.0$diff.1<0,"stress.1"]<-1

#save ET0 data
write.csv(et.0,paste0(getwd(),"/ET0_allplots.csv"))

#et.0<-data.frame(read.csv(paste0(getwd(),"/ET0_allplots.csv")),stringsAsFactors = F)
tmp<- et.0 %>% group_by(Plot,month) %>% summarise(maxT=mean(Tmax),minT=mean(Tmin),meanT=mean((Tmax+Tmin)/2),maxVPD=mean(VPDmax),meanRH=mean(RHmean),stress.nodays=sum(stress),
                                                  stress.nodays.1=sum(stress.1),stress.mm=sum(diff),stress.mm.1=sum(diff.1))

#calculate monthly maxT, minT, meanT, maxVPD, stress incidence (number of negative days), sum infiltration and ETO differences 
tmp<-et.0 %>% group_by(Plot,month) %>% summarise(maxT=mean(Tmax),minT=mean(Tmin),meanT=mean((Tmax+Tmin)/2),maxVPD=mean(VPDmax),meanRH=mean(RHmean),stress.nodays=sum(stress),stress.nodays.1=sum(stress.1),stress.mm=sum(diff),stress.mm.1=sum(diff.1))

#write out stress indices per month
write.csv(tmp,paste0(getwd(),"/MonthlyStress_estimates.csv"))

#create figure of monthly average microclimate values
library(ggpubr)

clim<-read_csv(paste0(getwd(),"/MonthlyStress_estimates.csv"))
#remove negative values
clim <- clim %>% mutate(maxT=replace(maxT,maxT<0,NA),maxVPD=replace(maxVPD,maxVPD<0,NA))
#pull out metstations
m_stations<-data_frame(gsub(" ","",as.character(names[1:10,"Plot.2"])))
colnames(m_stations)<-"Plot"
m_stations$one<-1

sub_clim<-left_join(clim,m_stations,by="Plot") %>% filter(one==1) %>%
  mutate(LandCover="Cocoa") %>% mutate(LandCover=replace(LandCover,Plot=="HMFP"|Plot=="KAFP","Forest")) %>%
  mutate(LandCover=replace(LandCover,Plot=="HM5KF2","High Shade Cocoa"))

sub_clim$no_month<-month.abb[month(sub_clim$month)]
sub_clim$no_month<-factor(sub_clim$no_month,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=T)
sub_clim$LandCover<-factor(sub_clim$LandCover,levels=c("Forest","High Shade Cocoa","Cocoa"),ordered=T)

sum_clim<-sub_clim %>% group_by(no_month,LandCover) %>% 
  summarise(maxT=mean(maxT,na.rm=T),maxVPD=mean(maxVPD,na.rm=T))

sum_clim.se<-sub_clim %>% group_by(no_month,LandCover) %>% 
  summarise(maxT.se=sd(maxT,na.rm=T)/sqrt(length(unique(Plot))),no_length=length(unique(Plot)),maxVPD.se=sd(maxVPD,na.rm=T)/sqrt(length(unique(Plot))))

sum_clim<-left_join(sum_clim,sum_clim.se,by=c("no_month","LandCover"))

#g1<-
ggplot(sum_clim,aes(no_month,maxT,group=factor(LandCover))) + geom_line(aes(linetype=factor(LandCover))) +
  geom_errorbar(aes(ymin=maxT-maxT.se,ymax=maxT+maxT.se),width=0.1) +
  theme_classic() + xlab("Month") + ylab("Monthly Maximum Temperature [C]") + 
  theme(legend.title=element_blank(),legend.position="bottom", text=element_text(size=20),plot.background = element_blank())+
  ylim(20,34)
ggsave(paste0(getwd(),"/MaxTemp.monthly_Cocoa.v.Forest.pdf"))

#g2<-ggplot(sum_clim,aes(no_month,maxVPD,group=factor(LandCover))) + geom_line(aes(colour=factor(LandCover))) +
#  geom_errorbar(aes(ymin=maxVPD-maxVPD.se,ymax=maxVPD+maxVPD.se,colour=factor(LandCover)),width=0.1) +
#  theme_classic() + xlab("Month") + ylab("Monthly Maximum Vapour Pressure Deficit [hPa]") + theme(legend.title=element_blank(),legend.position="bottom")

#ggarrange(g1,g2,ncol=2,nrow=1,common.legend=T)