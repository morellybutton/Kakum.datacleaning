# Function/Script: fine root biomsss v3
## This function calculates Root biomass (fine root productivity)
## option = 1: this means the time steps 10,20,30,40 minutes are chosen
## option 2: this means the time steps 5, 10, 15 minutes are chosen

library(gdata)
library(lubridate)
### Read test data:

## open Boldata:
#library(R.matlab)
#setwd("C:/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/Original-matlab-code/Ken_Iq-Tang")
#setwd("D:/Dokumente/My Dropbox/Carbon_Use_Efficieny_R/Original-matlab-code/Ken_Iq-Tang")
#dir()

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/")
site="Kakum"

#envelope weights
le<-8.2
se<-2.8
sea<-6.7
seb<-3
sec<-3.2
## adjust options:
plotname = c("HM FP","KA FP","KA 100 F3","KA 100 F1","KA 500 F3","KA 1K F3","HM 5K F2","HM 500 F3","HM 100 F3","HM 500 F2")
option = 1
logtransform = T
fine_root_cor <- "Default" # Toby wanted to add this as a variable
tubed = 0.06  ## radius of tube, diameter = 12 cm
num.ic= 4  ##number of ingrowth cores measured per plot

#setwd("D:/Dokumente/My Dropbox/Carbon_Use_Efficieny_R/R-functions(v3)")
for(p in 1:length(plotname)){
  Dataall <- read.xls(paste0(getwd(),"/",site,"/NPP/Roots/Combo.IC_rawdata.xlsx"),sheet=plotname[p])
  data.IC<-Dataall[(grep("Core",Dataall[,1])+1):nrow(Dataall),]
  names(data.IC) <- c("core","time step","time","temp","humid","rts_less2mm","rts_great2mm","date")
  
  #add month and year
  data.IC$month<-as.Date(cut(as.Date(data.IC$date),breaks="month"))
  data.IC$year<-as.Date(cut(as.Date(data.IC$date),breaks="year"))
  
  #correct data by subtracting envelope values
  data.IC[grep("le",data.IC$rts_less2mm),"rts_less2mm2"]<-as.numeric(gsub("le","",data.IC[grep("le",data.IC$rts_less2mm),"rts_less2mm"]))-le
  data.IC[grep("sea",data.IC$rts_less2mm),"rts_less2mm2"]<-as.numeric(gsub("sea","",data.IC[grep("sea",data.IC$rts_less2mm),"rts_less2mm"]))-sea
  data.IC[grep("seb",data.IC$rts_less2mm),"rts_less2mm2"]<-as.numeric(gsub("seb","",data.IC[grep("seb",data.IC$rts_less2mm),"rts_less2mm"]))-seb
  data.IC[grep("sec",data.IC$rts_less2mm),"rts_less2mm2"]<-as.numeric(gsub("sec","",data.IC[grep("sec",data.IC$rts_less2mm),"rts_less2mm"]))-sec
  data.IC[!is.na(data.IC$rts_less2mm)&is.na(data.IC$rts_less2mm2),"rts_less2mm2"]<-as.numeric(as.character(data.IC[!is.na(data.IC$rts_less2mm)&is.na(data.IC$rts_less2mm2),"rts_less2mm"]))-se
  data.IC[is.na(data.IC$rts_less2mm),"rts_less2mm2"]<-""
  
  data.IC[grep("le",data.IC$rts_great2mm),"rts_great2mm2"]<-as.numeric(gsub("le","",data.IC[grep("le",data.IC$rts_great2mm),"rts_great2mm"]))-le
  data.IC[grep("sea",data.IC$rts_great2mm),"rts_great2mm2"]<-as.numeric(gsub("sea","",data.IC[grep("sea",data.IC$rts_great2mm),"rts_great2mm"]))-sea
  data.IC[grep("seb",data.IC$rts_great2mm),"rts_great2mm2"]<-as.numeric(gsub("seb","",data.IC[grep("seb",data.IC$rts_great2mm),"rts_great2mm"]))-seb
  data.IC[grep("sec",data.IC$rts_great2mm),"rts_great2mm2"]<-as.numeric(gsub("sec","",data.IC[grep("sec",data.IC$rts_great2mm),"rts_great2mm"]))-sec
  data.IC[!is.na(data.IC$rts_great2mm)&is.na(data.IC$rts_great2mm2),"rts_great2mm2"]<-as.numeric(as.character(data.IC[!is.na(data.IC$rts_great2mm)&is.na(data.IC$rts_great2mm2),"rts_great2mm"]))-se
  data.IC[is.na(data.IC$rts_great2mm2),"rts_great2mm2"]<-""
  
  # root biomass:
  data.ic<-data.frame(cbind(plotname[p],year(data.IC$year),month(data.IC$month),day(data.IC$date),as.character(data.IC$core)))
  data.ic[,6:11]<-cbind(as.numeric(as.character(data.IC$humid)),as.numeric(as.character(data.IC$temp)),as.character(data.IC[,"time step"]),10,as.numeric(as.character(data.IC$rts_less2mm2)),as.numeric(as.character(data.IC$rts_great2mm2)))
  colnames(data.ic) <-c("plot_code","year","month","day","ingrowth_core_num","soil_humidity_pcnt","soil_temperature_c","time_step","time_step_minutes", "ml_under_2mm_g","ml_2to3_mm_g")
  write.csv(data.ic,paste0(getwd(),"/",site,"/NPP/Roots/RTS_",gsub(" ","",plotname[p]),"_",min(as.numeric(as.character(data.ic$year))),"_",max(as.numeric(as.character(data.ic$year))),".csv"))
}
