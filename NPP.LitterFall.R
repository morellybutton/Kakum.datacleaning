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
plotname = c("HM FP","KA FP","KA 100 F3","KA 100 F1","KA 500 F3","KA 1K F3","HM 5K F2","HM 500 F3","HM 100 F3","HM 500 F2")
plotsize=0.36  ### VARIABLE PLOTSIZE IS NOT YET INCLUDED: DISCUSS WITH CECILE, HOW TO INCLUDE IT...
num.lf<-9

#envelope weights
le<-8.2
se<-2.8
#a is 2.8 and b is 8.2

final=list()
for(p in 1:length(plotname)){
  Dataall<-read.xls(paste0(getwd(),"/",site,"/NPP/Litter Data/Combo_plotlevel.v2.xlsx"),sheet=plotname[p])
  #remove column titles
  # fine litterfall data:
  data.FLF<-Dataall[(grep("Date",Dataall[,1])+1):nrow(Dataall),]
  names(data.FLF) <- c("date","plot","crop_leaves","shade_leaves","fruits","seeds","branches","not_identified","flowers")
   #add month and year
  data.FLF$month<-as.Date(cut(as.Date(data.FLF$date),breaks="month"))
  data.FLF$year<-as.Date(cut(as.Date(data.FLF$date),breaks="year"))
  
  #remove factors
  data.FLF<-data.frame(lapply(data.FLF, as.character), stringsAsFactors=FALSE)
  
  #correct data by subtracting envelope values
  #replace a with se and delete b
  data.FLF[grep("b",data.FLF$crop_leaves),"crop_leaves"]<-gsub("b","",data.FLF[grep("b",data.FLF$crop_leaves),"crop_leaves"])
  data.FLF[grep("a",data.FLF$crop_leaves),"crop_leaves"]<-gsub("a","se",data.FLF[grep("a",data.FLF$crop_leaves),"crop_leaves"])
  
  data.FLF[agrep("se",data.FLF$crop_leaves),"crop_leaves2"]<-as.numeric(gsub("se","",data.FLF[agrep("se",data.FLF$crop_leaves),"crop_leaves"]))-se
  data.FLF[grep("se",data.FLF$crop_leaves,invert=T),"crop_leaves2"]<-as.numeric(gsub("se","",data.FLF[grep("se",data.FLF$crop_leaves,invert=T),"crop_leaves"]))-le
  data.FLF[is.na(data.FLF$crop_leaves2),"crop_leaves2"]<-0
  
  data.FLF[grep("b",data.FLF$shade_leaves),"shade_leaves"]<-gsub("b","",data.FLF[grep("b",data.FLF$shade_leaves),"shade_leaves"])
  data.FLF[grep("a",data.FLF$shade_leaves),"shade_leaves"]<-gsub("a","se",data.FLF[grep("a",data.FLF$shade_leaves),"shade_leaves"])
  
  data.FLF[agrep("se",data.FLF$shade_leaves),"shade_leaves2"]<-as.numeric(gsub("se","",data.FLF[agrep("se",data.FLF$shade_leaves),"shade_leaves"]))-se
  data.FLF[grep("se",data.FLF$shade_leaves,invert=T),"shade_leaves2"]<-as.numeric(gsub("se","",data.FLF[grep("se",data.FLF$shade_leaves,invert=T),"shade_leaves"]))-le
  data.FLF[is.na(data.FLF$shade_leaves2),"shade_leaves2"]<-0
  
  data.FLF[grep("b",data.FLF$fruits),"fruits"]<-gsub("b","le",data.FLF[grep("b",data.FLF$fruits),"fruits"])
  data.FLF[grep("a",data.FLF$fruits),"fruits"]<-gsub("a","",data.FLF[grep("a",data.FLF$fruits),"fruits"])
  
  data.FLF[agrep("le",data.FLF$fruits),"fruits2"]<-as.numeric(gsub("le","",data.FLF[agrep("le",data.FLF$fruits),"fruits"]))-le
  data.FLF[grep("le",data.FLF$fruits,invert=T),"fruits2"]<-as.numeric(gsub("le","",data.FLF[grep("le",data.FLF$fruits,invert=T),"fruits"]))-se
  data.FLF[is.na(data.FLF$fruits2),"fruits2"]<-0
  
  data.FLF[grep("b",data.FLF$seeds),"seeds"]<-gsub("b","le",data.FLF[grep("b",data.FLF$seeds),"seeds"])
  data.FLF[grep("a",data.FLF$seeds),"seeds"]<-gsub("a","",data.FLF[grep("a",data.FLF$seeds),"seeds"])
  
  data.FLF[agrep("le",data.FLF$seeds),"seeds2"]<-as.numeric(gsub("le","",data.FLF[agrep("le",data.FLF$seeds),"seeds"]))-le
  data.FLF[grep("le",data.FLF$seeds,invert=T),"seeds2"]<-as.numeric(gsub("le","",data.FLF[grep("le",data.FLF$seeds,invert=T),"seeds"]))-se
  data.FLF[is.na(data.FLF$seeds2),"seeds2"]<-0
  
  data.FLF[grep("b",data.FLF$branches),"branches"]<-gsub("b","le",data.FLF[grep("b",data.FLF$branches),"branches"])
  data.FLF[grep("a",data.FLF$branches),"branches"]<-gsub("a","",data.FLF[grep("a",data.FLF$branches),"branches"])
  
  data.FLF[agrep("le",data.FLF$branches),"branches2"]<-as.numeric(gsub("le","",data.FLF[agrep("le",data.FLF$branches),"branches"]))-le
  data.FLF[grep("le",data.FLF$branches,invert=T),"branches2"]<-as.numeric(gsub("le","",data.FLF[grep("le",data.FLF$branches,invert=T),"branches"]))-se
  data.FLF[is.na(data.FLF$branches2),"branches2"]<-0
  
  data.FLF[grep("b",data.FLF$not_identified),"not_identified"]<-gsub("b","le",data.FLF[grep("b",data.FLF$not_identified),"not_identified"])
  data.FLF[grep("a",data.FLF$not_identified),"not_identified"]<-gsub("a","",data.FLF[grep("a",data.FLF$not_identified),"not_identified"])
  
  data.FLF[agrep("le",data.FLF$not_identified),"not_identified2"]<-as.numeric(gsub("le","",data.FLF[agrep("le",data.FLF$not_identified),"not_identified"]))-le
  data.FLF[grep("le",data.FLF$not_identified,invert=T),"not_identified2"]<-as.numeric(gsub("le","",data.FLF[grep("le",data.FLF$not_identified,invert=T),"not_identified"]))-se
  data.FLF[is.na(data.FLF$not_identified2),"not_identified2"]<-0
  
  data.FLF[grep("b",data.FLF$flowers),"flowers"]<-gsub("b","le",data.FLF[grep("b",data.FLF$flowers),"flowers"])
  data.FLF[grep("a",data.FLF$flowers),"flowers"]<-gsub("a","",data.FLF[grep("a",data.FLF$flowers),"flowers"])
  
  data.FLF[agrep("le",data.FLF$flowers),"flowers2"]<-as.numeric(gsub("le","",data.FLF[agrep("le",data.FLF$flowers),"flowers"]))-le
  data.FLF[grep("le",data.FLF$flowers,invert=T),"flowers2"]<-as.numeric(gsub("le","",data.FLF[grep("le",data.FLF$flowers,invert=T),"flowers"]))-se
  data.FLF[is.na(data.FLF$flowers2),"flowers2"]<-0
  
  data.flf<-data.frame(cbind(as.character(plotname[p]),year(as.Date(data.FLF$year)),month(as.Date(data.FLF$month)),day(as.Date(data.FLF[,1])),as.character(data.FLF[,2])))
  data.flf[,6]<-0.5*0.5
  data.flf[,7:13]<-cbind(as.numeric(data.FLF$shade_leaves2),as.numeric(data.FLF$crop_leaves2),as.numeric(data.FLF$branches2),as.numeric(data.FLF$flowers2),as.numeric(data.FLF$fruits2),as.numeric(data.FLF$seeds2),as.numeric(data.FLF$not_identified2))
  
  names(data.flf) <- c("plot_code", "year","month", "day","litterfall_trap_num", "litterfall_trap_size_m2","leaves_g_per_trap","crop_leaves_g_per_trap","twigs_g_per_trap","flowers_g_per_trap","fruits_g_per_trap","seeds_g_per_trap","other_g_per_trap")
                       
  #names(data.flf)<-c("date","plot","subplot","month","year","crop_leaves","shade_leaves","fruits","seeds","branches","not_identified","flowers")
  #rm(data.FLF)
  write.csv(data.flf,paste0(getwd(),"/",site,"/NPP/Litter Data/FLF_",gsub(" ","",plotname[p]),"_",min(as.numeric(as.character(data.flf$year))),"_",max(as.numeric(as.character(data.flf$year))),".csv"))
  #final[[p]]<-data.flf
}
#data.flf<-do.call(rbind.data.frame,final)
  # flf <- function(data.flf, plotname, ret="monthly.means.ts", plotit=F) {   # plotsize=1                                                                                     
#write.csv(data.flf,paste0(getwd(),"/",site,"/NPP/Litter Data/FLF_",min(data.flf$year),"_",max(data.flf$year),".csv"))

