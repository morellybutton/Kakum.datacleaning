#code to calculate fruitset from raw datasheets
library(gdata)
library(lubridate)
library(stringr)
library(plyr)
library(ggplot2)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Fruitset")
trans<-c("AB","HM","KA")
year<-c("2014","2015","2016")
ns<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/plots.csv")

final<-list()
for(i in 1:length(trans)){
  final.1<-list()
  for(y in 1:length(year)){
    dataF<-read.xls(paste0(getwd(),"/FruitSet_",year[y],"_rawdata_revised.xlsx"), sheet=trans[i])
    #find the dates measured for each transect
    d<-agrep("Date: ",as.character(dataF[,5]))
    #add length of datasheet
    d[length(d)+1]<-length(dataF[,1])
    final.2<-list()
    for(j in 1:(length(d)-1)){
      dF<-dataF[d[j]:(d[j+1]),]
      dt<-as.Date(dataF[d[j],6])
      s<-grep("Plot",as.character(dF[,1]))
      #find end of datasheet
      e<-grep("Fruit",as.character(dF[,1]))
      if(length(e)==0) dataF.1<-data.frame(dF[(s+1):nrow(dF),],stringsAsFactors=FALSE) else dataF.1<-data.frame(dF[(s+1):(e-1),],stringsAsFactors=FALSE)
      #dataF.1$V15<-0
      dataF.1[,3]<-dt
      dataF.1$age<-ns[match(as.character(dataF.1[,1]),as.character(ns$name3)),"age"]
      #if(ncol(dataF.1)==17) 
      colnames(dataF.1)<-c("Plot","Tree No","Date","Cu","Cw","Cm","Pu (S)","Pu (M)","Pu (L)","Pr (S)", "Pr (M)","Pr (L)","Fb","Fo","NoSBT","NoDBT","Age") #else {
      #dataF.1[,(ncol(dataF.1)+1):17]<-NA ; colnames(dataF.1)<-c("Plot","Tree No","Date","Cu","Cw","Cm","Pu (S)","Pu (M)","Pu (L)","Pr (S)", "Pr (M)","Pr (L)","Fb","Fo","Age","NoSBT","NoDBT")}
      final.2[[j]]<-dataF.1
      rm(s,e,dataF.1,dt,dF)
    }
    final.1[[y]]<-do.call(rbind.data.frame,final.2)
  }
  #write to csv
  final<-do.call(rbind.data.frame,final.1)
  write.csv(final,paste0(getwd(),"/Fruitset_",trans[i],"_summary.csv"))
  rm(final,final.1,dt,dataF,d)
}

#calculate fruitset (number of flowers pollinated and cherelle's lost by month)
final.1<-list()
for(i in 1:length(trans)){
  f.S<-read.csv(paste0(getwd(),"/Fruitset_",trans[i],"_summary.csv"))
  f.S$Tree.No<-str_split_fixed(f.S$Tree.No," ",2)[,1]
  #identify dates of measurements
  dt<-as.Date(unique(f.S$Date))
  #keep first month and calculate from second month
  f.s<-f.S[as.Date(f.S$Date)==dt[1],]
  y1<-f.S[as.Date(f.S$Date)==dt[1],]
  f.s$chset<-0
  f.s$Cm.chng<-f.s$Cu
  f.s$Cm.lost<-f.s$Cw
  f.s$Pu.S.inc<-f.s$Pu..S.
  f.s$Pu.S.lost<-0
  f.s$Pu.M.inc<-f.s$Pu..M.
  f.s$Pu.M.lost<-0
  f.s$Pu.L.inc<-f.s$Pu..L.
  f.s$Pu.L.lost<-0
  f.s$Pr.S.inc<-f.s$Pr..S.
  f.s$Pr.S.harv<-0
  f.s$Pr.M.inc<-f.s$Pr..M.
  f.s$Pr.M.harv<-0
  f.s$Pr.L.inc<-f.s$Pr..L.
  f.s$Pr.L.harv<-0
  for(j in 2:length(dt)){
    x1<-f.S[as.Date(f.S$Date)==dt[j],]
    f.s[(nrow(f.s)+1):(nrow(f.s)+48),1:ncol(f.S)]<-x1[match(interaction(y1$Plot,y1$Tree.No),interaction(x1$Plot,x1$Tree.No)),]
    #calculate cherelle set, taking into account plot and tree number
    fl1<-cbind(as.character(interaction(f.S[as.Date(f.S$Date)==dt[j-1],"Plot"],f.S[as.Date(f.S$Date)==dt[j-1],"Tree.No"])),rowSums(cbind(f.S[as.Date(f.S$Date)==dt[j-1],"Fo"],0.95*f.S[as.Date(f.S$Date)==dt[j-1],"Fb"])))
    ch2<-cbind(as.character(interaction(f.S[as.Date(f.S$Date)==dt[j],"Plot"],f.S[as.Date(f.S$Date)==dt[j],"Tree.No"])),rowSums(cbind(f.S[as.Date(f.S$Date)==dt[j],"Cw"],f.S[as.Date(f.S$Date)==dt[j],"Cu"])))
    fl_ch<-cbind(fl1,ch2[match(ch2[,1],fl1[,1]),2])
    #if flowers zero but cherelle >0, error?
    chset<-as.numeric(fl_ch[,3])/as.numeric(fl_ch[,2])*100
    chset[fl_ch[,2]==0]<-0
    chset[fl_ch[,2]==0 & fl_ch[,3]>0]<-NA
    f.s[(nrow(f.s)-47):nrow(f.s),"chset"]<-chset[match(as.character(interaction(f.s[(nrow(f.s)-47):nrow(f.s),"Plot"],f.s[(nrow(f.s)-47):nrow(f.s),"Tree.No"])),fl_ch[,1])]
    
    rm(chset)
    #change in number of marked cherelles minus number of small unripe pods
    z1<-f.S[as.Date(f.S$Date)==dt[j-1],]
    Cm.inc<-x1$Cm-z1[match(as.character(interaction(x1$Plot,x1$Tree.No)),as.character(interaction(z1$Plot,z1$Tree.No))),"Cm"]
    Pu.S.inc<-x1$Pu..S.-z1[match(as.character(interaction(x1$Plot,x1$Tree.No)),as.character(interaction(z1$Plot,z1$Tree.No))),"Pu..S."]
    Pu.S.inc[Pu.S.inc<0]<-0
    Cm.chng<-rowSums(cbind(Pu.S.inc,Cm.inc))
    Cm.lost<-Cm.chng
    Cm.lost[Cm.lost>0]<-0
    Cm.chng[Cm.chng<0]<-0
    #add in unmarked cherelles to Cm.chng
    Cm.chng<-Cm.chng+x1$Cu
    #add in wilted cherelles to Cm.lost
    Cm.lost<-abs(Cm.lost)+x1$Cw
    
    f.s[(nrow(f.s)-47):nrow(f.s),"Cm.chng"]<-Cm.chng
    f.s[(nrow(f.s)-47):nrow(f.s),"Cm.lost"]<-abs(Cm.lost)
    #increase in number of unripe pods by size class
    #check if Pu.S.inc hasn't become Pu.M or Pr.S
    tmp<-x1$Pu..S.-z1[match(as.character(interaction(x1$Plot,x1$Tree.No)),as.character(interaction(z1$Plot,z1$Tree.No))),"Pu..S."]
    tmp1<-x1$Pu..M.-z1[match(as.character(interaction(x1$Plot,x1$Tree.No)),as.character(interaction(z1$Plot,z1$Tree.No))),"Pu..M."]
    tmp2<-x1$Pr..S.-z1[match(as.character(interaction(x1$Plot,x1$Tree.No)),as.character(interaction(z1$Plot,z1$Tree.No))),"Pr..S."]
    Pu.S.inc<-tmp+tmp1+tmp2
    Pu.S.lost<-Pu.S.inc
    Pu.S.lost[Pu.S.lost>0]<-0
    Pu.S.inc[Pu.S.inc<0]<-0
    #f.s[(nrow(f.s)-47):nrow(f.s),"Pu.S.inc"]<-x1$Pu..S.-z1[match(as.character(interaction(x1$Plot,x1$Tree.No)),as.character(interaction(z1$Plot,z1$Tree.No))),"Pu..S."]
    f.s[(nrow(f.s)-47):nrow(f.s),"Pu.S.inc"]<-Pu.S.inc
    f.s[(nrow(f.s)-47):nrow(f.s),"Pu.S.lost"]<-abs(Pu.S.lost)
    rm(Pu.S.lost,Pu.S.inc,tmp2,tmp)
    #check if Pu.M.inc hasn't become Pr.M or Pu.L.inc
    tmp<-tmp1
    tmp1<-x1$Pu..L.-z1[match(as.character(interaction(x1$Plot,x1$Tree.No)),as.character(interaction(z1$Plot,z1$Tree.No))),"Pu..L."]
    tmp2<-x1$Pr..M.-z1[match(as.character(interaction(x1$Plot,x1$Tree.No)),as.character(interaction(z1$Plot,z1$Tree.No))),"Pr..M."]
    Pu.M.inc<-tmp+tmp1+tmp2
    Pu.M.lost<-Pu.M.inc
    Pu.M.lost[Pu.M.lost>0]<-0
    Pu.M.inc[Pu.M.inc<0]<-0
    f.s[(nrow(f.s)-47):nrow(f.s),"Pu.M.inc"]<-Pu.M.inc
    f.s[(nrow(f.s)-47):nrow(f.s),"Pu.M.lost"]<-abs(Pu.M.lost)
    #f.s[(nrow(f.s)-47):nrow(f.s),"Pu.M.inc"]<-x1$Pu..M.-z1[match(as.character(interaction(x1$Plot,x1$Tree.No)),as.character(interaction(z1$Plot,z1$Tree.No))),"Pu..M."]
    rm(Pu.M.lost,Pu.M.inc,tmp2,tmp)
    #check if Pu.L.inc hasn't become Pr.L
    tmp<-tmp1
    tmp2<-x1$Pr..L.-z1[match(as.character(interaction(x1$Plot,x1$Tree.No)),as.character(interaction(z1$Plot,z1$Tree.No))),"Pr..L."]
    Pu.L.inc<-tmp+tmp2
    Pu.L.lost<-Pu.L.inc
    Pu.L.lost[Pu.L.lost>0]<-0
    Pu.L.inc[Pu.L.inc<0]<-0
    rm(tmp,tmp1,tmp2)
    #f.s[(nrow(f.s)-47):nrow(f.s),"Pu.L.inc"]<-x1$Pu..L.-z1[match(as.character(interaction(x1$Plot,x1$Tree.No)),as.character(interaction(z1$Plot,z1$Tree.No))),"Pu..L."]
    f.s[(nrow(f.s)-47):nrow(f.s),"Pu.L.inc"]<-Pu.L.inc
    f.s[(nrow(f.s)-47):nrow(f.s),"Pu.L.lost"]<-abs(Pu.L.lost)
    #increase in ripe pods by size class
    Pr.S.inc<-x1$Pr..S.-z1[match(as.character(interaction(x1$Plot,x1$Tree.No)),as.character(interaction(z1$Plot,z1$Tree.No))),"Pr..S."]
    Pr.S.harv<-Pr.S.inc
    Pr.S.harv[Pr.S.harv>0]<-0
    Pr.S.inc[Pr.S.inc<0]<-0
    f.s[(nrow(f.s)-47):nrow(f.s),"Pr.S.inc"]<-Pr.S.inc
    f.s[(nrow(f.s)-47):nrow(f.s),"Pr.S.harv"]<-abs(Pr.S.harv)
    Pr.M.inc<-x1$Pr..M.-z1[match(as.character(interaction(x1$Plot,x1$Tree.No)),as.character(interaction(z1$Plot,z1$Tree.No))),"Pr..M."]
    Pr.M.harv<-Pr.M.inc
    Pr.M.harv[Pr.M.harv>0]<-0
    Pr.M.inc[Pr.M.inc<0]<-0
    f.s[(nrow(f.s)-47):nrow(f.s),"Pr.M.inc"]<-Pr.M.inc
    f.s[(nrow(f.s)-47):nrow(f.s),"Pr.M.harv"]<-abs(Pr.M.harv)
    Pr.L.inc<-x1$Pr..L.-z1[match(as.character(interaction(x1$Plot,x1$Tree.No)),as.character(interaction(z1$Plot,z1$Tree.No))),"Pr..L."]
    Pr.L.harv<-Pr.L.inc
    Pr.L.harv[Pr.L.harv>0]<-0
    Pr.L.inc[Pr.L.inc<0]<-0
    f.s[(nrow(f.s)-47):nrow(f.s),"Pr.L.inc"]<-Pr.L.inc
    f.s[(nrow(f.s)-47):nrow(f.s),"Pr.L.harv"]<-abs(Pr.L.harv)
  
  }
  final.1[[i]]<-f.s
}
final<-do.call(rbind.data.frame,final.1)

#add month measure
final$month<-round_date(as.Date(final$Date),"month")

write.csv(final,paste0(getwd(),"/Fruitset_TTL_summary.csv"))

#calculate pollination/Cherelle set to podset to final ripened pods
tt.l<-read.csv(paste0(getwd(),"/Fruitset_TTL_summary.csv"))
#pull out plots
ps<-as.character(unique(tt.l$Plot))
final.1<-list()
for(i in 1:length(ps)){
  tt.L<-tt.l[tt.l$Plot==ps[i],]
  #pull out simple tree number
  tt.L$Tree.No<-str_split_fixed(tt.L$Tree.No," ",2)[,1]
  dts<-as.Date(unique(tt.L$Date))
  TT.L<-data.frame(cbind(tt.L[as.Date(tt.L$Date)==dts[1],3:4],tt.L[as.Date(tt.L$Date)==dts[1],17],as.character(paste0(month(round_date(dts[1],unit="month")),".",year(round_date(dts[1],unit="month"))))),stringsAsFactors = F)
  colnames(TT.L)<-c("Plot","TreeNo","Age","Date")
  TT.L$Date<-as.character(TT.L$Date)
  TT.L$Month<-round_date(as.Date(tt.L[as.Date(tt.L$Date)==dts[1],"Date"]),unit="month")
  month(TT.L$Month)<-month(TT.L$Month)-1
  TT.L[,"F.Buds"]<-tt.L[as.Date(tt.L$Date)==dts[1],15]
  TT.L[,"Flowers"]<-tt.L[as.Date(tt.L$Date)==dts[1],16]
  #TT.L[,"Flowers.tot"]<-rowSums(cbind(0.95*tt.L[as.Date(tt.L$Date)==dts[1],15],tt.L[as.Date(tt.L$Date)==dts[1],16]))
 
  for(j in 2:length(dts)){
    x1<-tt.L[as.Date(tt.L$Date)==dts[j],]
    TT.L[(nrow(TT.L)+1):(nrow(TT.L)+4),1:3]<-TT.L[1:4,1:3]
    TT.L[(nrow(TT.L)-3):nrow(TT.L),4]<-as.character(paste0(month(round_date(dts[j],unit="month")),".",year(round_date(dts[j],unit="month"))))
    TT.L[(nrow(TT.L)-3):nrow(TT.L),"Month"]<-round_date(as.Date(tt.L[as.Date(tt.L$Date)==dts[j],"Date"]),unit="month")
    month(TT.L[(nrow(TT.L)-3):nrow(TT.L),"Month"])<-month(TT.L[(nrow(TT.L)-3):nrow(TT.L),"Month"])-1
    TT.L[(nrow(TT.L)-3):nrow(TT.L),"F.Buds"]<-x1[match(TT.L[1:4,"TreeNo"],x1$Tree.No),"Fb"]
    TT.L[(nrow(TT.L)-3):nrow(TT.L),"Flowers"]<-x1[match(TT.L[1:4,"TreeNo"],x1$Tree.No),"Fo"]
    #TT.L[(nrow(TT.L)-3):nrow(TT.L),"Flowers.tot"]<-rowSums(cbind(x1[match(TT.L[1:4,"TreeNo"],x1$Tree.No),"Fo"],0.95*x1[match(TT.L[1:4,"TreeNo"],x1$Tree.No),"Fb"]))
    TT.L[(nrow(TT.L)-3):nrow(TT.L),"Chset"]<-x1[match(TT.L[1:4,"TreeNo"],x1$Tree.No),"chset"]
    #TT.L[,paste0(month(round_date(dts[j],unit="month")),".",year(round_date(dts[j],unit="month")),".Pset")]<-tt.L[as.Date(tt.L$Date)==dts[j],"pset"]
    #before summing Ch check if Cm.chng hasn't become Pu.S.inc first, already did this
    TT.L[(nrow(TT.L)-3):nrow(TT.L),"Ch.lost"]<-rowSums(cbind(x1[match(TT.L[1:4,"TreeNo"],x1$Tree.No),"Cm.lost"],x1[match(TT.L[1:4,"TreeNo"],x1$Tree.No),"Cw"]))
    TT.L[(nrow(TT.L)-3):nrow(TT.L),"SumCh"]<-rowSums(cbind(x1[match(TT.L[1:4,"TreeNo"],x1$Tree.No),"Cu"],x1[match(TT.L[1:4,"TreeNo"],x1$Tree.No),"Cm.chng"]))
    #TT.L[(nrow(TT.L)-3):nrow(TT.L),"SumPu"]<-rowSums(cbind(x1[match(TT.L[1:4,"TreeNo"],x1$Tree.No),"Pu.S.inc"],x1[match(TT.L[1:4,"TreeNo"],x1$Tree.No),"Pu.M.inc"],x1[match(TT.L[1:4,"TreeNo"],x1$Tree.No),"Pu.L.inc"]))
    TT.L[(nrow(TT.L)-3):nrow(TT.L),"SumPu"]<-rowSums(cbind(x1[match(TT.L[1:4,"TreeNo"],x1$Tree.No),"Pu.S.inc"],x1[match(TT.L[1:4,"TreeNo"],x1$Tree.No),"Pu.M.inc"],x1[match(TT.L[1:4,"TreeNo"],x1$Tree.No),"Pu.L.inc"]))
    TT.L[(nrow(TT.L)-3):nrow(TT.L),"SumPu.lost"]<-rowSums(cbind(x1[match(TT.L[1:4,"TreeNo"],x1$Tree.No),"Pu.S.lost"],x1[match(TT.L[1:4,"TreeNo"],x1$Tree.No),"Pu.M.lost"],x1[match(TT.L[1:4,"TreeNo"],x1$Tree.No),"Pu.L.lost"]))
    TT.L[(nrow(TT.L)-3):nrow(TT.L),"SumPr"]<-rowSums(cbind(x1[match(TT.L[1:4,"TreeNo"],x1$Tree.No),"Pr.S.inc"],x1[match(TT.L[1:4,"TreeNo"],x1$Tree.No),"Pr.M.inc"],x1[match(TT.L[1:4,"TreeNo"],x1$Tree.No),"Pr.L.inc"]))
    TT.L[(nrow(TT.L)-3):nrow(TT.L),"SumPr.harv"]<-rowSums(cbind(x1[match(TT.L[1:4,"TreeNo"],x1$Tree.No),"Pr.S.harv"],x1[match(TT.L[1:4,"TreeNo"],x1$Tree.No),"Pr.M.harv"],x1[match(TT.L[1:4,"TreeNo"],x1$Tree.No),"Pr.L.harv"]))
    TT.L[(nrow(TT.L)-3):nrow(TT.L),"NoSBT"]<-x1[match(TT.L[1:4,"TreeNo"],x1$Tree.No),"NoSBT"]
    TT.L[(nrow(TT.L)-3):nrow(TT.L),"NoDBT"]<-x1[match(TT.L[1:4,"TreeNo"],x1$Tree.No),"NoDBT"]
  }
  final.1[[i]]<-TT.L
}
dF<-do.call(rbind.data.frame,final.1)
dF[is.na(dF)]<-0

#add monthly biomass distance measures
tmp<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Pollination/Pollinator_2014_2015.csv")
tmp$TreeNo<-str_split_fixed(tmp$Tree," ",2)[,1]
tmp$month<-round_date(as.Date(tmp$Date,format="%d/%m/%Y"),"month")

dF$Biomass<-tmp[match(interaction(dF$Plot,dF$TreeNo,dF$Month),interaction(tmp$Plot,tmp$TreeNo,tmp$month)),"Biomass"]
dF$Banana<-tmp[match(interaction(dF$Plot,dF$TreeNo,dF$Month),interaction(tmp$Plot,tmp$TreeNo,tmp$month)),"Banana"]

write.csv(dF,paste0(getwd(),"/Fruitset_monthly_pertree.csv"))

#calculate podset
pset<-ddply(dF,.(Plot,TreeNo),transform,ChTot=cumsum(SumCh),PuTot=cumsum(SumPu),PuTot.L=cumsum(SumPu.lost),PrTot=cumsum(SumPr),PrTot.H=cumsum(SumPr.harv))

#calculate cherelle set for year 2014 (July-Sept)
#dF.1<-dF[year(dF$Month)=="2014",]
#pset.1<-ddply(dF.1,.(Plot,TreeNo),transform,ChTot=cumsum(SumCh),PuTot=cumsum(SumPu),PuTot.L=cumsum(SumPu.lost),PrTot=cumsum(SumPr),PrTot.H=cumsum(SumPr.harv))

#take monthly plot sums for flowers and mean cherelle set
final.m<-ddply(pset,.(Plot,Month),summarise,F.Buds=sum(F.Buds),Flowers=sum(Flowers),Chset=mean(Chset),Chlost=sum(Ch.lost),Chnum=sum(SumCh))
#final.m1<-ddply(pset.1,.(Plot,Month),summarise,Flowers=sum(Flowers),Chset=mean(Chset),Chnum=sum(SumCh))

#take plot max for season
#final.c<-ddply(final.m1,.(Plot),summarise,Chset=max(Chset))

#add back standing and dead banana trees
final.m$SBT<-pset[match(interaction(final.m$Plot,final.m$Month),interaction(pset$Plot,pset$Month)),"NoSBT"]
final.m$DBT<-pset[match(interaction(final.m$Plot,final.m$Month),interaction(pset$Plot,pset$Month)),"NoDBT"]

write.csv(final.m,paste0(getwd(),"/Chset_allplots_allmonths.csv"))

#final.m1$Date<-pset[match(final.m1$Date,pset$Date),"Date.1"]
ggplot(pset,aes(Month,Chset,group=factor(TreeNo)))+geom_line(aes(color=factor(TreeNo)))+facet_wrap(~Plot)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=10))
ggsave(paste0(getwd(),"/Cherelle.set.allplots.pdf"),height=8,width=11)

#take cumulative sum of each measure to calculate cherelle to unripe pod and cherelle to harvested pod
#final<-ddply(pset,.(Plot,TreeNo),summarise,ChTot=max(ChTot),PuTot=max(PuTot),PuTot.L=max(PuTot.L),PrTot=max(PrTot),PrTot.H=max(PrTot.H))
#final<-ddply(pset,.(Plot,TreeNo),summarise,Puset=max(PuTot,na.rm=T)/max(ChTot,na.rm=T),PuPr=max(PrTot,na.rm=T)/max(PuTot,na.rm=T),PuPh=max(PrTot.H,na.rm=T)/max(PuTot,na.rm=T),ChPh=max(PrTot.H,na.rm=T)/max(ChTot,na.rm=T))
#m.final<-ddply(final,.(Plot),summarise,m.Puset=mean(Puset,na.rm=T)*100,m.PuPr=mean(PuPr,na.rm=T)*100,m.PuPh=mean(PuPh,na.rm=T)*100,m.ChPh=mean(ChPh,na.rm=T)*100)

#focus on podset for year 2014/2015 (July-May)
#dF.1<-dF[year(dF$Month)=="2014"|year(dF$Month)=="2015"&month(dF$Month)<6,]
#pset.1<-ddply(dF.1,.(Plot,TreeNo),summarise,ChTot=cumsum(SumCh),PuTot=cumsum(SumPu),PuTot.L=cumsum(SumPu.lost),PrTot=cumsum(SumPr),PrTot.H=cumsum(SumPr.harv))

#final.1<-ddply(pset.1,.(Plot,TreeNo),summarise,Puset=max(PuTot,na.rm=T)/max(ChTot,na.rm=T),PuPr=max(PrTot,na.rm=T)/max(PuTot,na.rm=T),PuPh=max(PrTot.H,na.rm=T)/max(PuTot,na.rm=T),ChPh=max(PrTot.H,na.rm=T)/max(ChTot,na.rm=T))
#remove Infinities
#final.1[final.1==Inf&!is.na(final.1)]<-0
#m.final.1<-ddply(final.1,.(Plot),summarise,Puset=mean(Puset,na.rm=T),PuPr=mean(PuPr,na.rm=T),PuPh=mean(PuPh),ChPh=mean(ChPh,na.rm=T))

#write pod set and cherelle to harvested pod set
write.csv(pset,paste0(getwd(),"/Pset_allplots_alltrees.csv"))
#write.csv(m.final,paste0(getwd(),"/Pset_allplots.csv"))

#write.csv(final.1,paste0(getwd(),"/Pset_allplots_alltrees_HC1415.csv"))
#write.csv(m.final.1,paste0(getwd(),"/Pset_allplots_HC1415.csv"))

#write.csv(final,paste0(getwd(),"/Fruitset_vars.csv"))