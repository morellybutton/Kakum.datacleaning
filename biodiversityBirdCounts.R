#code for analyzing bird counts raw data
#library(gdata)
library(stringr)
library(gdata)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")

#names of transects to loop through
snames=c("AB","HM","KA")
#data collection name
#season=c("Wet season","Dry season")
#year="2015"

#load species reference list
ref<-read.csv(paste0(getwd(),"/Biodiversity/bird_reflist.csv"))
#remove automatically added factors
ref<-data.frame(lapply(ref, as.character), stringsAsFactors=FALSE)

#identify raw data files
fs<-list.files(path=paste0(getwd(),"/Biodiversity/"), pattern="Pointcount_rawdata_")

for(i in 1:length(fs)){
  #for(n in 1:length(snames)){
    data<-read.csv(paste0(getwd(),"/Biodiversity/",fs[i]))
    #remove factors
    data<-data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
    
    #Identify beginning of each datasheet
    index<-grep("Data Recording",as.character(data$Bird.Point.Counts))
    final.1<-list()
    #Go through each datasheet separately
    for (j in 1:length(index)){
      if(j<length(index)) sheet<-data[index[j]:(index[j+1]-1),1:7] else sheet<-data[index[j]:length(data[,1]),1:7]
      #create final datasheet list
      final<-list()
      #extract date
      pat<-"Date: *"
      ind<-grep(pat,sheet[,2])
      day<-as.Date(sub(pat," ",sheet[ind,2]),"%d/%m/%y")
      #day<-as.character(sheet[grep("Date",sheet[,1]),1])
      #extract transect
      #pat<-"Transect"
      #ind<-grep(pat,sheet[,6])
      #tr<-str_trim(as.character(sub(pat," ",sheet[ind,5])))
      #extract number
      #pat<-"Number: "
      #ind<-grep(pat,sheet[,6])
      #n<-str_trim(as.character(sub(pat," ",sheet[ind,6])))
      #if(length(n)==0) n="N/A"
      #remove "Start Time" entry
      rem<-grep("Start Time",as.character(sheet[,2]))
      if(length(rem)==0) rem=0 
      #find each measurement by time column without start time
      time<-grep("HRS",as.character(sheet[rem+1:length(sheet[,2]),2]))
      
      #if(length(time)==1) br=1 else br=length(time)-1
      for(k in 1:length(time)){
        #identify plot location
        lctn<-as.character(sheet[rem+time[k],3])
        #find time of point count
        hr<-as.character(sheet[rem+time[k],2])
        #find canopy measure
        can<-as.character(sheet[rem+time[k],5])
        #find distance measure
        dis<-as.character(sheet[rem+time[k],4])
        
        if(length(time)==k) ptct<-sheet[(rem+time[k]+1):(length(sheet[,2])),2:length(sheet[1,])] else ptct<-sheet[(rem+time[k]+1):(rem+time[k+1]-1),2:length(sheet[1,])]
        if(ptct$Bird.Point.Counts=="No Data") next
        #ptct<-sheet[(rem+time[k]+1):(rem+time[k+1]-1),2:length(sheet[1,])]
        #pull out first canopy gap measure
        #pat<-"Canopy"
        #ind<-grep(pat,sheet[,3])
        #gap<-sheet[ind,3]
        #remove empty rows
        ptct<-ptct[ptct[,3]>0,]
        if(nrow(ptct)==0) next
        #make empty data.frame
        result<-data.frame(matrix(nrow=length(ptct[,2])+1,ncol=(length(ptct[1,])+length(ref[,2:31]))))
        #put date, time and location on first row of results
        result$X1<-as.Date(result$X1,"%d/%m/%y")
        result[1,1]<-day
        result[1,2:5]<-c(hr,dis,lctn,can)
        #result[1,2:6]<-c(tr,n,sheet[time[k],1],lctn,gap)
        for (m in 1:length(ptct[,1])){
          #find species in reference list
          #extract name from pointcount
          name<-as.character(ptct[m,2])
          #break name apart into last word and descriptive word
          #name2<-str_split(name," ")
          #spp<-name2[[1]][length(name2[[1]])]
          #adj<-name2[[1]][1:(length(name2[[1]])-1)]
          #find species with same "base name"
          #ex<-grep(spp,ref[,2])
          if(name=="Unidentified") d<-t(rep(NA,29)) else 
            {extract<-grep(name,ref[,2])
            d<-ref[extract,2:31]}
          #add species details to ptct
          #ptct[m,7:(7+length(d))]
          result[m+1,2:length(result[1,])]<-cbind(ptct[m,2:ncol(ptct)],d)
        }
        final[[k]]<-result
      }
      #spit out list of pointcounts to one dataframe for combining by transect
      final.1[[j]]<-do.call(rbind.data.frame,final)
      #write datasheet to .csv
    }
    #final.2<-list()
    final.2<-do.call(rbind.data.frame,final.1)
    colnames(final.2)<-c("X1","CommonName","Distance","NoIndividuals","X5","Seen/Heard","CommonNameCheck","Family","Genus","Species","PrimaryForest","SecondaryForest","ForestEdge","Woodland/Savanna","Agriculture","Settlements","StructCanopy","StructMidStorey","StructUnderStory","StructGround","StructAllLevels","Frugivore","NectarEater","SeedEater","Omnivore","Insectivore","Order:Orthoptera","Order:Hemiptera","Order:Araneae","Order:Coleoptera","Order:Lepidoptera","Order:Diptera","Order:Isoptera","Order:Chilopoda/Diplopoda","Order:Odonata","Order:Hymenoptera")
    file.name<-gsub(" ",".",gsub(".csv","",str_split_fixed(fs[i],pattern="Pointcount_rawdata_",2)[[2]]))
    write.csv(final.2,paste0(getwd(),"/Biodiversity/Pointcount_cleaned_",file.name,".csv"))
  #}
}




