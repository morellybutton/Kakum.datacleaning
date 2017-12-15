### Function fine litter fall: 
# This function uses data to calculate NPP from fine litterfall for ESPA.

## Required Data:
#% year  
#% month  
#% plot	
#% Ponto	--- Point
#% Folhas	--- Shade Leaves
#% Folhas  --- Crop Leaves
#% Galhos	--- Branches
#% Flores	--- Flowers
#% Frutos	--- Fruits
#% Sementes	--- Seeds
#% Nao identif. --- Other

library(gdata)
library(lubridate)
#setwd("C:/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/R-functions(v3)")

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/")
site="Kakum"

#plot names
#plts<-c("HM FP","KA FP","KA 100M F3","HM 5K F2","HM 500M F3")

# adjust options:
plotname = c("HM FP","KA FP","KA 100 F3","KA 100 F1","KA 500 F3","KA 1K F3","HM 5K F2","HM 500 F3","HM 100 F3","HM 500 F2","AB 100 F1","AB 500 F2","AB 1K F2","AB 5K F2","AB FP")
plotsize=0.36  ### VARIABLE PLOTSIZE IS NOT YET INCLUDED: DISCUSS WITH CECILE, HOW TO INCLUDE IT...
num.lf<-9

#envelope weights
le<-8.2
se<-2.8

final=list()
for(p in 1:length(plotname)){
  Dataall<-read.xls(paste0(getwd(),"/",site,"/NPP/FineLitterFall/Combo_plotlevel.xlsx"),sheet=plotname[p])
  #find listed dates
  dts<-c(grep("Date",Dataall[,1])+1,nrow(Dataall))
  final<-list()
  for(k in 1:(length(dts)-1)){
    if(k<(length(dts)-1)) data.FLF<-Dataall[dts[k]:(dts[k+1]-4),] else data.FLF<-Dataall[dts[k]:(dts[k+1]),]
    #remove column titles
    # fine litterfall data:
    names(data.FLF) <- c("date","plot","fresh_area","date2","crop_leaves","shade_leaves","fruits","seeds","branches","not_identified","flowers")
    #add month and year
    data.FLF$month<-as.Date(cut(as.Date(data.FLF$date),breaks="month"))
    data.FLF$year<-as.Date(cut(as.Date(data.FLF$date),breaks="year"))
    #correct data by subtracting envelope values
    if(k<7) {data.FLF[agrep("se",data.FLF$crop_leaves),"crop_leaves2"]<-as.numeric(gsub("se","",data.FLF[agrep("se",data.FLF$crop_leaves),"crop_leaves"]))-se
    data.FLF[grep("se",data.FLF$crop_leaves,invert=T),"crop_leaves2"]<-as.numeric(gsub("se","",data.FLF[grep("se",data.FLF$crop_leaves,invert=T),"crop_leaves"]))-le}
    
    if(k>6) {data.FLF[agrep("a",data.FLF$crop_leaves),"crop_leaves2"]<-as.numeric(gsub("a","",data.FLF[agrep("a",data.FLF$crop_leaves),"crop_leaves"]))-se
    data.FLF[grep("a",data.FLF$crop_leaves,invert=T),"crop_leaves2"]<-as.numeric(gsub("b","",data.FLF[grep("a",data.FLF$crop_leaves,invert=T),"crop_leaves"]))-le}
    
    data.FLF[is.na(data.FLF$crop_leaves2),"crop_leaves2"]<-0
    
    if(k<7) {data.FLF[agrep("se",data.FLF$shade_leaves),"shade_leaves2"]<-as.numeric(gsub("se","",data.FLF[agrep("se",data.FLF$shade_leaves),"shade_leaves"]))-se
    data.FLF[grep("se",data.FLF$shade_leaves,invert=T),"shade_leaves2"]<-as.numeric(gsub("se","",data.FLF[grep("se",data.FLF$shade_leaves,invert=T),"shade_leaves"]))-le}
    
    if(k>6) {data.FLF[agrep("a",data.FLF$shade_leaves),"shade_leaves2"]<-as.numeric(gsub("a","",data.FLF[agrep("a",data.FLF$shade_leaves),"shade_leaves"]))-se
    data.FLF[grep("a",data.FLF$shade_leaves,invert=T),"shade_leaves2"]<-as.numeric(gsub("b","",data.FLF[grep("a",data.FLF$shade_leaves,invert=T),"shade_leaves"]))-le}
    
    data.FLF[is.na(data.FLF$shade_leaves2),"shade_leaves2"]<-0
    
    if(k<7) {data.FLF[agrep("le",data.FLF$fruits),"fruits2"]<-as.numeric(gsub("le","",data.FLF[agrep("le",data.FLF$fruits),"fruits"]))-le
    data.FLF[grep("le",data.FLF$fruits,invert=T),"fruits2"]<-as.numeric(gsub("le","",data.FLF[grep("le",data.FLF$fruits,invert=T),"fruits"]))-se}
    
    if(k>6) {data.FLF[agrep("a",data.FLF$fruits),"fruits2"]<-as.numeric(gsub("a","",data.FLF[agrep("a",data.FLF$fruits),"fruits"]))-se
    data.FLF[grep("a",data.FLF$fruits,invert=T),"fruits2"]<-as.numeric(gsub("b","",data.FLF[grep("a",data.FLF$fruits,invert=T),"fruits"]))-le}
    
    data.FLF[is.na(data.FLF$fruits2),"fruits2"]<-0
    
    if(k<7) {data.FLF[agrep("le",data.FLF$seeds),"seeds2"]<-as.numeric(gsub("le","",data.FLF[agrep("le",data.FLF$seeds),"seeds"]))-le
    data.FLF[grep("le",data.FLF$seeds,invert=T),"seeds2"]<-as.numeric(gsub("le","",data.FLF[grep("le",data.FLF$seeds,invert=T),"seeds"]))-se}
    
    if(k>6) {data.FLF[agrep("a",data.FLF$seeds),"seeds2"]<-as.numeric(gsub("a","",data.FLF[agrep("a",data.FLF$seeds),"seeds"]))-se
    data.FLF[grep("a",data.FLF$seeds,invert=T),"seeds2"]<-as.numeric(gsub("b","",data.FLF[grep("a",data.FLF$seeds,invert=T),"seeds"]))-le}
      
    data.FLF[is.na(data.FLF$seeds2),"seeds2"]<-0
    
    if(k<7) {data.FLF[agrep("le",data.FLF$branches),"branches2"]<-as.numeric(gsub("le","",data.FLF[agrep("le",data.FLF$branches),"branches"]))-le
    data.FLF[grep("le",data.FLF$branches,invert=T),"branches2"]<-as.numeric(gsub("le","",data.FLF[grep("le",data.FLF$branches,invert=T),"branches"]))-se}
    
    if(k>6) {data.FLF[agrep("a",data.FLF$branches),"branches2"]<-as.numeric(gsub("a","",data.FLF[agrep("a",data.FLF$branches),"branches"]))-se
    data.FLF[grep("a",data.FLF$branches,invert=T),"branches2"]<-as.numeric(gsub("b","",data.FLF[grep("a",data.FLF$branches,invert=T),"branches"]))-le}
      
    data.FLF[is.na(data.FLF$branches2),"branches2"]<-0
    
    if(k<7) {data.FLF[agrep("le",data.FLF$not_identified),"not_identified2"]<-as.numeric(gsub("le","",data.FLF[agrep("le",data.FLF$not_identified),"not_identified"]))-le
    data.FLF[grep("le",data.FLF$not_identified,invert=T),"not_identified2"]<-as.numeric(gsub("le","",data.FLF[grep("le",data.FLF$not_identified,invert=T),"not_identified"]))-se}
    
    if(k>6) {data.FLF[agrep("a",data.FLF$not_identified),"not_identified2"]<-as.numeric(gsub("a","",data.FLF[agrep("a",data.FLF$not_identified),"not_identified"]))-se
    data.FLF[grep("a",data.FLF$not_identified,invert=T),"not_identified2"]<-as.numeric(gsub("b","",data.FLF[grep("a",data.FLF$not_identified,invert=T),"not_identified"]))-le}
    
    data.FLF[is.na(data.FLF$not_identified2),"not_identified2"]<-0
    
    if(k<7) {data.FLF[agrep("le",data.FLF$flowers),"flowers2"]<-as.numeric(gsub("le","",data.FLF[agrep("le",data.FLF$flowers),"flowers"]))-le
    data.FLF[grep("le",data.FLF$flowers,invert=T),"flowers2"]<-as.numeric(gsub("le","",data.FLF[grep("le",data.FLF$flowers,invert=T),"flowers"]))-se}
    
    if(k>6) {data.FLF[agrep("a",data.FLF$flowers),"flowers2"]<-as.numeric(gsub("a","",data.FLF[agrep("a",data.FLF$flowers),"flowers"]))-se
    data.FLF[grep("a",data.FLF$flowers,invert=T),"flowers2"]<-as.numeric(gsub("b","",data.FLF[grep("a",data.FLF$flowers,invert=T),"flowers"]))-le}
    
    data.FLF[is.na(data.FLF$flowers2),"flowers2"]<-0
    
    data.flf<-data.frame(cbind(as.character(plotname[p]),year(as.Date(data.FLF$year)),month(as.Date(data.FLF$month)),day(as.Date(data.FLF[,1])),as.character(data.FLF[,2])))
    data.flf[,6]<-0.5*0.5
    data.flf[,7:13]<-cbind(as.numeric(data.FLF$shade_leaves2),as.numeric(data.FLF$crop_leaves2),as.numeric(data.FLF$branches2),as.numeric(data.FLF$flowers2),as.numeric(data.FLF$fruits2),as.numeric(data.FLF$seeds2),as.numeric(data.FLF$not_identified2))
    
    names(data.flf) <- c("plot_code", "year","month", "day","litterfall_trap_num", "litterfall_trap_size_m2","leaves_g_per_trap","crop_leaves_g_per_trap","twigs_g_per_trap","flowers_g_per_trap","fruits_g_per_trap","seeds_g_per_trap","other_g_per_trap")
    final[[k]]<-data.flf
    #names(data.flf)<-c("date","plot","subplot","month","year","crop_leaves","shade_leaves","fruits","seeds","branches","not_identified","flowers")
    #rm(data.FLF)
      
  }
  final.1<-do.call(rbind.data.frame, final)
  write.csv(final.1,paste0(getwd(),"/",site,"/NPP/FineLitterFall/FLFQ_",gsub(" ","",plotname[p]),"_",min(as.numeric(as.character(final.1$year))),"_",max(as.numeric(as.character(final.1$year))),".csv"))
  
}

