#create datasheet of point counts for analysis
library(gdata)
library(stringr)
library(lubridate)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")

#names of transects to loop through
snames=c("AB","HM","KA")
#data collection name
#season=c("Wet season2014","Wet season2015","Dry season2015")
#plot numbers
ns<-read.csv(paste0(getwd(),"/plots.csv"))

#identify files
fs<-list.files(path=paste0(getwd(),"/Biodiversity/"), pattern="Pointcount_cleaned_")
#remove .xlsx
fs<-fs[grep(".xlsx",fs,invert=T)]


#create dataframe
final.3<-list()
for(i in 1:length(fs)){
  #final.3<-list()
  #for(j in 1:length(season)){
    #load datasheet 
    dF<-read.csv(paste0(getwd(),"/Biodiversity/",fs[i]))
    #extract transect name
    sname<-str_split_fixed(str_split_fixed(fs[i],"Pointcount_cleaned_",2)[[2]],".20",2)[[1]]
    #extract season
    season<-str_split_fixed(str_split_fixed(fs[i],paste0(sname,"."),2)[[2]],".csv",2)[[1]]
    #load LAI
    lai<-read.csv(paste0(getwd(),"/NPP/LAI/Analysis/4.5m/",sname,"_gap_analysis.csv"))
    #disease
    dS<-read.csv(paste0(getwd(),"/Disease/Disease_monthlymeasures.csv"))
    
    #find each pointcount
    index<-which(!is.na(dF$X1))
    #add last line
    index<-c(index,nrow(dF))
    
    final.2<-list()
    for(k in 1:(length(index)-1)){
      final.1<-data.frame(Transect=character(),Plot=character(),Dist=numeric(),Season=character(),Date=as.Date(character()),Canopy=numeric(),LAI=numeric(),Capsids=numeric(),Intensity=numeric(),Mistletoe=numeric(),Hour=numeric(),Species=character(),Distance=numeric(),NoIndividuals=numeric(),Seen_Heard=character(),PrimaryForest=numeric(),SecondaryForest=numeric(),ForestEdge=numeric(),Woodland=numeric(),Agriculture=numeric(),Settlements=numeric(),StructCanopy=numeric(),StructMidStorey=numeric(),StructUnderStory=numeric(),StructGround=numeric(),StructAllLevels=numeric(),Frugivore=numeric(),Omnivore=numeric(),Insectivore=numeric(),Orthoptera=numeric(),Hemiptera=numeric(),Coleoptera=numeric(),Lepidoptera=numeric(),Diptera=numeric(),Isoptera=numeric(),Diplopoda=numeric(),Odonata=numeric(),Hymenoptera=numeric(),stringsAsFactors=FALSE)
      pct<-dF[index[k]:(index[k+1]-1),]
      #extract plot number
      p<-as.character(pct[1,5])
      #remove extra spaces
      p<-gsub(" ","",p)
      y<-strsplit(p,"Forest")
      if(length(y[[1]])==1) d<-ns[gsub(" ","",ns$name3)==p,6] else d<--as.numeric(strsplit(as.character(pct[1,4]),"M")[[1]][1])
      final.1[1,1:4]<-c(sname,p,d,season)
      rm(d)
      d<-as.Date(as.character(pct[1,2]),format="%d/%m/%Y")
      if(is.na(d))
        {final.1[1,5]<-as.Date(as.character(pct[1,2]))
        d<-as.Date(as.character(pct[1,2]))} else {final.1[1,5]<-d}
      final.1[1,6]<-strsplit(as.character(pct[1,6]),"Canopy:")[[1]][2]
      x<-lai[as.character(lai$Plot)==p,"Synth"]
      if(length(grep("Dry",season))==1) final.1[1,"LAI"]<-x[1] else final.1[1,"LAI"]<-x[2]
      
      dis<-dS[grep(p,as.character(gsub(" ","",dS$Plot))),]
      #if(s=="Wet ") dis<-dis[dis$month=="07"|dis$month=="06",] else dis<-dis[dis$month=="01"|dis$month=="02",]
      #pull out month of disease measure with point count data
      z<-dis[as.Date(as.character(dis$month))==as.Date(paste(year(d),month(d),"01",sep="-"),format="%Y-%m-%d"),]
      if(nrow(z)==0) z<-dis[as.Date(as.character(dis$month))==as.Date(paste(year(d),month(d)+1,"01",sep="-"),format="%Y-%m-%d"),]
      dis<-z
      
      final.1[1,8:11]<-c(dis$PropCPB,dis$iCPB,as.numeric(dis$Mist),as.character(pct[1,3]))
      if(length(y[[1]])==2) final.1[1,8:10]<-0
      rm(y,x)
      #find number of birds counted
      sp<-pct[!is.na(pct$CommonNameCheck),]
      final.1[2:length(sp[,1]),1:11]<-final.1[1,1:11]
      for(m in 1:length(sp[,1])){
        final.1[m,12:15]<-c(paste(sp[m,10],sp[m,11]),as.numeric(as.character(sp[m,4])),as.numeric(as.character(sp[m,"NoIndividuals"])),as.character(sp[m,7]))
        final.1[m,16:26]<-sp[m,12:22]
        final.1[m,27:38]<-c(as.numeric(sp[m,23]),as.numeric(sp[m,26:29]),as.numeric(sp[m,31:37]))
      }
      #add 1 if no count of individuals
      final.1[is.na(final.1$NoIndividuals),"NoIndividuals"]<-1
      final.2[[k]]<-final.1
      rm(final.1)
    }
    #final.3[[j]]<-do.call(rbind.data.frame,final.2)
    #rm(final.2)
  final.3[[i]]<-do.call(rbind.data.frame,final.2)
  rm(final.2)
}
final<-do.call(rbind.data.frame,final.3)
rm(final.3)

#replace NA intensity values with 0
final[is.na(final$Intensity),"Intensity"]<-0
final[final$Intensity=="NaN","Intensity"]<-0

#save final datasheet
write.csv(final,paste0(getwd(),"/Biodiversity/Kakum_point_counts.csv"))

