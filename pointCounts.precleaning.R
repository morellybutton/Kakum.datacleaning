#code for identifying all unique species ID'd across all transects/plots
library(gdata)
library(stringr)
setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")
#site="Kakum"
#names of transects to loop through
snames=c("AB","HM","KA")
year=c("2014 Wet season","2015 Dry season","2016 Wet season","2017 Dry season")

#ref<-read.csv(paste0(getwd(),"/",site,"/Biodiversity/bird_reflist.csv"))


for(j in 1:length(year)){
  final<-list()
  for (i in 1:length(snames)){
    #data<-read.csv(paste0(getwd(),"/",site,"/Biodiversity/PointCounts_rawdata_",snames[i],".csv"))
    dF<-read.xls(paste0(getwd(),"/Biodiversity/PointCounts_rawdata_",year[j],".xlsx"),sheet=snames[i])
    #remove factors
    dF<-data.frame(lapply(dF, as.character), stringsAsFactors=FALSE)
    #extract all common names
    spp<-unique(dF[!dF[,1]>0,2])
    #remove non bird names
    spp<-spp[spp!="Data Recording Form"]
    spp<-spp[spp!=""]
    final[[i]]<-spp
  }
  final.1<-unique(unlist(final))
  final.1<-final.1[order(final.1)]
  write.csv(final.1,paste0(getwd(),"/Biodiversity/Pointcount_spplist ",year[j],".csv"))
}


for(j in 1:length(year)){
  comp<-read.csv(paste0(getwd(),"/Biodiversity/Pointcount_spplist ",year[j],".csv"))
  colnames(comp)<-c("no","orig","correct")
  comp<-data.frame(lapply(comp,as.character),stringsAsFactors = F)
  #replace misspelled names with "correct names" in cheat sheet.
  for (i in 1:length(snames)){
    #data<-read.csv(paste0(getwd(),"/",site,"/Biodiversity/PointCounts_rawdata_",snames[i],".csv"))
    dF<-read.xls(paste0(getwd(),"/Biodiversity/PointCounts_rawdata_",year[j],".xlsx"),sheet=snames[i])
    #remove factors
    dF<-data.frame(lapply(dF, as.character), stringsAsFactors=FALSE)
    #replace bird names
    dF$corrected<-comp[match(dF$Bird.Point.Counts,comp$orig),"correct"]
    dF[!is.na(dF$corrected),"Bird.Point.Counts"]<-as.character(dF[!is.na(dF$corrected),"corrected"])
    dF<-dF[,1:7]
    write.csv(dF,paste0(getwd(),"/Biodiversity/Pointcount_rawdata_",snames[i],".",gsub(" ",".",year[j]),".csv"))
  }
  
}


#check changes
check<-list()
for(j in 1:length(year)){
  for (i in 1:length(snames)){
    #data<-read.csv(paste0(getwd(),"/",site,"/Biodiversity/PointCounts_rawdata_",snames[i],".csv"))
    dF<-read.csv(paste0(getwd(),"/Biodiversity/Pointcount_rawdata_",snames[i],".",gsub(" ",".",year[j]),".csv"))
    #remove factors
    dF<-data.frame(lapply(dF, as.character), stringsAsFactors=FALSE)
    #extract all common names
    spp<-unique(dF[dF$Bird.Point.Counts!=0,3])
    #remove non bird names
    spp<-spp[spp!="Data Recording Form"]
    spp<-spp[spp!=""]
    spp<-spp[order(spp)]
    check[[i]]<-data.frame(spp)
  }
  final<-do.call(rbind.data.frame,check)
  final<-unique(final)
  final<-final[order(final[,1]),]
  break
}

