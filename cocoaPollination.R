#Pollination analysis, drivers of pollination

library(ggplot2)
library(reshape)
library(plyr)
library(lubridate)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")

df.poll<-read.csv(paste0(getwd(),"/Pollination/Pollination_monthlyvariables.csv"))
exp.m<-read.csv(paste0(getwd(),"/Pollination/Experiment.csv"))

#create figures of pollinator abundances and diversity
#diversity by plot
ggplot(df.poll,aes(as.Date(month),Pollin.div))+geom_line()+facet_wrap(~Plot)+
  xlab("Month")+ylab("Number of Pollinator Families")+theme(
    text = element_text(size=12)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,axis.text.x=element_text(angle = 45,hjust=1)
  )
ggsave(paste0(getwd(),"/Analysis/ES/Pollination.diversity.pdf"),width=8,height=6)

#number of pollinators by family
df.poll.1<-data.frame(cbind(df.poll$Plot,df.poll$month,df.poll[,10:17]))
colnames(df.poll.1)<-c("Plot","month",colnames(df.poll[,10:17]))
df.fam<-melt(df.poll.1,id.vars=c("Plot","month"))
ggplot(df.fam,aes(as.Date(month),value,group=factor(variable)))+geom_line(aes(color=factor(variable)))+facet_wrap(~Plot)+
  xlab("Month")+ylab("Number of Pollinator Families")+theme(
    text = element_text(size=12)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,axis.text.x=element_text(angle = 45,hjust=1)
  )
ggsave(paste0(getwd(),"/Analysis/ES/Pollination.families.pdf"),width=10,height=6)

#number of pollinators by plot
ggplot(df.poll,aes(as.Date(month),Pollin.nos))+geom_line()+facet_wrap(~Plot)+
  xlab("Month")+ylab("Number of Pollinators")+theme(
    text = element_text(size=12)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,axis.text.x=element_text(angle = 45,hjust=1)
  )
ggsave(paste0(getwd(),"/Analysis/ES/Pollination.abundance.pdf"),width=8,height=6)

#add other plot measures
ns<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/plots.csv")
ns$name1<-gsub(" ","",ns$name3)

chset<-read.csv(paste0(getwd(),"/Fruitset/Chset_allplots_allmonths.csv"))
chset$treat<-exp.m[match(chset$Plot,exp.m$Plots.to.receive.treatment),"Treated.Plots"]
chset[is.na(chset$treat),"treat"]<-0
chset.1<-ddply(chset,.(Month,treat),summarise,Chset=mean(Chset,na.rm=T))
ggplot(chset.1,aes(as.Date(Month),Chset,group=factor(treat)))+geom_point(aes(color=factor(treat)))+geom_line(aes(color=factor(treat)))+xlab("Month")+
  ylab("Monthly Fruitset [%]")+theme(
    text = element_text(size=12)
    ,plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,axis.text.x=element_text(angle = 45,hjust=1)
  )
ggsave(paste0(getwd(),"/Analysis/ES/Monthly.Fruitset.pdf"))

pset.tree<-read.csv(paste0(getwd(),"/Fruitset/Pset_allplots_alltrees.csv"))

#image of monthly flower production across plots
pset.all<-ddply(pset.tree,.(Month),summarise,F.Buds=sum(F.Buds,na.rm=T),Flowers=sum(Flowers,na.rm=T),Aborted=sum(Ch.lost,na.rm=T),Cherelles=sum(SumCh,na.rm=T),Pods=sum(SumPu,na.rm=T))
pset.all.1<-melt(pset.all,id.vars="Month")
ggplot(pset.all.1[pset.all.1$variable!="Aborted",],aes(as.Date(Month),value,group=variable))+geom_point(aes(color=variable))+geom_line(aes(color=variable))+xlab("Month")+
  ylab("Monthly Totals of Buds, Flowers,\nCherelles and New Pods")+theme(
    text = element_text(size=12)
    ,plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,axis.text.x=element_text(angle = 45,hjust=1)
  )
ggsave(paste0(getwd(),"/Analysis/ES/Flower.cherelle.counts.pdf"),width=11,height=4)

#flowers vs rainfall
ppt.m<-read.csv(paste0(getwd(),"/MetData/LargeMetstation_ppt.csv"))
#ppt.m$month.1<-as.Date(paste(year(ppt.m$month),month(ppt.m$month)-1,"01",sep="-"))
#ppt.m[is.na(ppt.m$month.1),"month.1"]<-as.Date(paste(year(ppt.m[is.na(ppt.m$month.1),"month"])-1,"12","01",sep="-"))
pset.all$ppt<-ppt.m[match(as.Date(pset.all$Month),as.Date(ppt.m$month)),"Tppt"]
pset.all$year<-year(pset.all$Month)
ggplot(pset.all,aes(ppt,sqrt(Flowers),group=factor(year)))+geom_point(aes(color=factor(year)))+
  stat_smooth(method=lm,se=F,aes(color=factor(year)))+xlab("Monthly Precip [mm]")+ylab("Transformed Number of Flowers")+
  theme( text = element_text(size=12)
         ,plot.background = element_blank()
         ,panel.background = element_blank()
         ,panel.grid.major = element_blank()
         ,panel.grid.minor = element_blank()
         ,panel.border = element_blank()
         ,axis.line.x = element_line(color = 'black')
         ,axis.line.y = element_line(color = 'black')
  )
ggsave(paste0(getwd(),"/Analysis/ES/Flowers.vs.Precip.pdf"))

ggplot(pset.all,aes(ppt,sqrt(F.Buds),group=factor(year)))+geom_point(aes(color=factor(year)))+
  stat_smooth(method=lm,se=F,aes(color=factor(year)))+xlab("Monthly Precip [mm]")+ylab("Transformed Number of Flower Buds")+
  theme( text = element_text(size=12)
         ,plot.background = element_blank()
         ,panel.background = element_blank()
         ,panel.grid.major = element_blank()
         ,panel.grid.minor = element_blank()
         ,panel.border = element_blank()
         ,axis.line.x = element_line(color = 'black')
         ,axis.line.y = element_line(color = 'black')
  )
ggsave(paste0(getwd(),"/Analysis/ES/FlowerBuds.vs.Precip.pdf"))

#look at influence of temperature
m.climate<-read.csv(paste0(getwd(),"/MetData/MonthlyStress_estimates.csv"))
m.climate.1<-ddply(m.climate,.(month),summarise,meanT=mean(meanT,na.rm=T))
pset.all$meanT<-m.climate.1[match(as.Date(pset.all$Month),as.Date(m.climate.1$month)),"meanT"]

ggplot(pset.all,aes(meanT,sqrt(F.Buds),group=factor(year)))+geom_point(aes(color=factor(year)))+
  stat_smooth(method=lm,se=F,aes(color=factor(year)))+xlab("Monthly Mean Temperature [C]")+ylab("Transformed Number of Flower Buds")+
  theme( text = element_text(size=12)
         ,plot.background = element_blank()
         ,panel.background = element_blank()
         ,panel.grid.major = element_blank()
         ,panel.grid.minor = element_blank()
         ,panel.border = element_blank()
         ,axis.line.x = element_line(color = 'black')
         ,axis.line.y = element_line(color = 'black')
  )
ggsave(paste0(getwd(),"/Analysis/ES/FlowerBuds.vs.meanT.pdf"))


chset$MeanT<-m.climate[match(interaction(chset$Plot,chset$Month),interaction(m.climate$Plot,m.climate$month)),"meanT"]
chset$MaxT<-m.climate[match(interaction(chset$Plot,chset$Month),interaction(m.climate$Plot,m.climate$month)),"maxT"]
chset$year<-year(chset$Month)
chset$Plot.1<-as.character(chset$Plot)
chset[chset$treat==1,"Plot.1"]<-paste0(chset[chset$treat==1,"Plot.1"],"*")
ggplot(chset,aes(MeanT,sqrt(F.Buds),group=factor(year)))+geom_point(aes(color=factor(year)))+stat_smooth(method="lm",se=F,aes(color=factor(year)))+facet_wrap(~Plot.1)+
  xlab("Monthly Mean Temperature [C]")+ylab("Transformed Flower Buds")
ggsave(paste0(getwd(),"/Analysis/ES/FlowerBuds.vs.MeanT.pdf"),height=10,width=14)

ggplot(chset,aes(MaxT,sqrt(F.Buds),group=factor(year)))+geom_point(aes(color=factor(year)))+stat_smooth(method="lm",se=F,aes(color=factor(year)))+facet_wrap(~Plot.1)+
 xlab("Monthly Maximum Temperature [C]")+ylab("Transformed Flower Buds")
ggsave(paste0(getwd(),"/Analysis/ES/FlowerBuds.vs.MaxT.pdf"))

ggplot(chset,aes(MeanT,sqrt(Flowers),group=factor(year)))+geom_point(aes(color=factor(year)))+stat_smooth(method="lm",se=F,aes(color=factor(year)))+facet_wrap(~Plot.1)+
  xlab("Monthly Mean Temperature [C]")+ylab("Transformed Flowers")
ggsave(paste0(getwd(),"/Analysis/ES/Flowers.vs.MeanT.pdf"))

ggplot(chset,aes(MaxT,sqrt(Flowers),group=factor(year)))+geom_point(aes(color=factor(year)))+stat_smooth(method="lm",se=F,aes(color=factor(year)))+facet_wrap(~Plot.1)+
  xlab("Monthly Maximum Temperature [C]")+ylab("Transformed Flowers")
ggsave(paste0(getwd(),"/Analysis/ES/Flowers.vs.MaxT.pdf"))

chset.sbt<-ddply(chset,.(Plot,year),summarise,SBT=max(SBT,na.rm=T),DBT=max(DBT,na.rm=T),Chset=max(Chset,na.rm=T))
chset.sbt$distance<-ns[match(chset.sbt$Plot,ns$name3),"distance.1"]

df.poll$year<-year(df.poll$month)
poll.nos<-ddply(df.poll,.(Plot,year),summarise,nos=sum(Pollin.nos,na.rm=T))

chset.sbt$poll.nos<-poll.nos[match(interaction(chset.sbt$Plot,chset.sbt$year),interaction(poll.nos$Plot,poll.nos$year)),"nos"]
#Frimpong figures (podset vs distance, plantains and midges)
ggplot(chset.sbt,aes(distance,Chset,group=factor(year)))+geom_point(aes(color=factor(year)))+stat_smooth(method="lm",se=F,aes(color=factor(year)))+
  xlab("Distance from Forest [m]")+ylab("Cherelle Set [%]")+
  theme( text = element_text(size=12)
         ,plot.background = element_blank()
         ,panel.background = element_blank()
         ,panel.grid.major = element_blank()
         ,panel.grid.minor = element_blank()
         ,panel.border = element_blank()
         ,axis.line.x = element_line(color = 'black')
         ,axis.line.y = element_line(color = 'black')
         ,legend.title=element_blank()
  )
ggsave(paste0(getwd(),"/Analysis/ES/Frimpong.ChsetvsDistance.pdf"))

ggplot(chset.sbt[chset.sbt$year!=2014,],aes(SBT/0.36,Chset,group=factor(year)))+geom_point(aes(color=factor(year)))+stat_smooth(method="lm",se=F,aes(color=factor(year)))+
  xlab("Number of Plantains [ha-1]")+ylab("Cherelle Set [%]")+
  theme( text = element_text(size=12)
         ,plot.background = element_blank()
         ,panel.background = element_blank()
         ,panel.grid.major = element_blank()
         ,panel.grid.minor = element_blank()
         ,panel.border = element_blank()
         ,axis.line.x = element_line(color = 'black')
         ,axis.line.y = element_line(color = 'black')
         ,legend.title=element_blank()
  )
ggsave(paste0(getwd(),"/Analysis/ES/Frimpong.ChsetvsSBT.pdf"))

ggplot(chset.sbt[chset.sbt$year!=2016,],aes(poll.nos,Chset,group=factor(year)))+geom_point(aes(color=factor(year)))+stat_smooth(method="lm",se=F,aes(color=factor(year)))+
  xlab("Number of Pollinators")+ylab("Cherelle Set [%]")+
  theme( text = element_text(size=12)
         ,plot.background = element_blank()
         ,panel.background = element_blank()
         ,panel.grid.major = element_blank()
         ,panel.grid.minor = element_blank()
         ,panel.border = element_blank()
         ,axis.line.x = element_line(color = 'black')
         ,axis.line.y = element_line(color = 'black')
         ,legend.title=element_blank()
  )
ggsave(paste0(getwd(),"/Analysis/ES/Frimpong.ChsetvsPolls.pdf"))

ggplot(chset.sbt[chset.sbt$year!=2016,],aes(distance,poll.nos,group=factor(year)))+geom_point(aes(color=factor(year)))+stat_smooth(method="lm",se=F,aes(color=factor(year)))+
  ylab("Number of Pollinators")+xlab("Distance from Forest [m]")+
  theme( text = element_text(size=12)
         ,plot.background = element_blank()
         ,panel.background = element_blank()
         ,panel.grid.major = element_blank()
         ,panel.grid.minor = element_blank()
         ,panel.border = element_blank()
         ,axis.line.x = element_line(color = 'black')
         ,axis.line.y = element_line(color = 'black')
         ,legend.title=element_blank()
  )
ggsave(paste0(getwd(),"/Analysis/ES/Frimpong.PollsvsDist.pdf"))

ggplot(chset.sbt[chset.sbt$year!=2016&chset.sbt$year!=2014,],aes(SBT/0.36,poll.nos,group=factor(year)))+geom_point(aes(color=factor(year)))+stat_smooth(method="lm",se=F,aes(color=factor(year)))+
  ylab("Number of Pollinators")+xlab("Number of Plantains [ha-1]")+
  theme( text = element_text(size=12)
         ,plot.background = element_blank()
         ,panel.background = element_blank()
         ,panel.grid.major = element_blank()
         ,panel.grid.minor = element_blank()
         ,panel.border = element_blank()
         ,axis.line.x = element_line(color = 'black')
         ,axis.line.y = element_line(color = 'black')
         ,legend.title=element_blank()
  )
ggsave(paste0(getwd(),"/Analysis/ES/Frimpong.PollsvsSBT.pdf"))

#vs canopy gap?
chset.sbt$Gap<-ns[match(chset.sbt$Plot,ns$name3),"Gap_Jan15"]

ggplot(chset.sbt[chset.sbt$year!=2016,],aes(Gap,poll.nos,group=factor(year)))+geom_point(aes(color=factor(year)))+stat_smooth(method="lm",se=F,aes(color=factor(year)))+
  ylab("Number of Pollinators")+xlab("Canopy Gap [%]")+
  theme( text = element_text(size=12)
         ,plot.background = element_blank()
         ,panel.background = element_blank()
         ,panel.grid.major = element_blank()
         ,panel.grid.minor = element_blank()
         ,panel.border = element_blank()
         ,axis.line.x = element_line(color = 'black')
         ,axis.line.y = element_line(color = 'black')
         ,legend.title=element_blank()
  )
ggsave(paste0(getwd(),"/Analysis/ES/PollsvsCanopyGap.pdf"))

ggplot(chset.sbt,aes(Gap,Chset,group=factor(year)))+geom_point(aes(color=factor(year)))+stat_smooth(method="lm",se=F,aes(color=factor(year)))+
  ylab("Cherelle Set [%]")+xlab("Canopy Gap [%]")+
  theme( text = element_text(size=12)
         ,plot.background = element_blank()
         ,panel.background = element_blank()
         ,panel.grid.major = element_blank()
         ,panel.grid.minor = element_blank()
         ,panel.border = element_blank()
         ,axis.line.x = element_line(color = 'black')
         ,axis.line.y = element_line(color = 'black')
         ,legend.title=element_blank()
  )
ggsave(paste0(getwd(),"/Analysis/ES/ChsetvsCanopyGap.pdf"))

#take mean monthly midges per distance
df.poll$distance<-ns[match(df.poll$Plot,ns$name3),"distance"]
df<-ddply(df.poll,.(month,distance),summarise,num=mean(Pollin.nos,na.rm=T),n.se=sd(Pollin.nos,na.rm=T)/sqrt(9),abund=mean(Pollin.div),a.se=sd(Pollin.div,na.rm=T)/sqrt(9))
ggplot(df,aes(as.Date(month),poll,group=factor(distance)))+geom_point(aes(color=factor(distance)))+geom_line(aes(color=factor(distance)))
ggplot(df,aes(as.Date(month),abund,group=factor(distance)))+geom_point(aes(color=factor(distance)))+geom_line(aes(color=factor(distance)))

df<-ddply(chset,.(dist.1,Month),summarise,chset.mn=mean(Chset,na.rm=T))
ggplot(df,aes(as.Date(Month),chset.mn,group=factor(dist.1)))+geom_point(aes(color=factor(dist.1)))+geom_line(aes(color=factor(dist.1)))+
  ylab("Mean Cherelle Set [%]")+xlab("Month")+
  theme( text = element_text(size=12)
         ,plot.background = element_blank()
         ,panel.background = element_blank()
         ,panel.grid.major = element_blank()
         ,panel.grid.minor = element_blank()
         ,panel.border = element_blank()
         ,axis.line.x = element_line(color = 'black')
         ,axis.line.y = element_line(color = 'black')
         ,legend.title=element_blank()
  )
ggsave(paste0(getwd(),"/Analysis/ES/Frimpong.M.ChsetvsDistance.pdf"))
rm(chset,chset.1,chset.sbt,df,df.fam,df.poll,df.poll.1,exp.m,m.climate,ns,poll.nos,ppt.m,pset.all,pset.all.1,pset.tree)

df.poll<-read.csv(paste0(getwd(),"/Pollination/Pollination_monthlyvariables.csv"))
exp.m<-read.csv(paste0(getwd(),"/Pollination/Experiment.csv"))
d.ez<-read.csv(paste0(getwd(),"/Disease/Disease_monthlypodpests.csv"))
so.l<-read.csv(paste0(getwd(),"/Nutrients/Soils/Soil_nutrient_data.csv"))
chset<-read.csv(paste0(getwd(),"/Fruitset/Chset_allplots_allmonths.csv"))
#pset.tree<-read.csv(paste0(getwd(),"/Fruitset/Pset_allplots_alltrees.csv"))
ppt.m<-read.csv(paste0(getwd(),"/MetData/LargeMetstation_ppt.csv"))
m.climate<-read.csv(paste0(getwd(),"/MetData/MonthlyStress_estimates.csv"))
ns<-read.csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/plots.csv")
b.a<-read.csv(paste0(getwd(),"/AGB/ForestPlots/Tree_plotdata.csv"))

df.poll$treat<-exp.m[match(df.poll$Plot,exp.m$Plots.to.receive.treatment),"Treated.Plots"]
df.poll[is.na(df.poll$treat),"treat"]<-0
df.poll$soil.m<-d.ez[match(interaction(df.poll$Plot,df.poll$month),interaction(d.ez$Plot,d.ez$month)),"soil.moist"]
df.poll$PropCPB<-d.ez[match(interaction(df.poll$Plot,df.poll$month),interaction(d.ez$Plot,d.ez$month)),"PropCPB"]
df.poll$PropBP<-d.ez[match(interaction(df.poll$Plot,df.poll$month),interaction(d.ez$Plot,d.ez$month)),"PropBP"]
df.poll$iCPB<-d.ez[match(interaction(df.poll$Plot,df.poll$month),interaction(d.ez$Plot,d.ez$month)),"iCPB"]
df.poll$N.pct<-so.l[match(gsub(" ","",df.poll$Plot),so.l$Plot.Number),"N.pct"]
df.poll$C.pct<-so.l[match(gsub(" ","",df.poll$Plot),so.l$Plot.Number),"C.pct"]
df.poll$Chset<-chset[match(interaction(df.poll$Plot,df.poll$month),interaction(chset$Plot,chset$Month)),"Chset"]
df.poll$Ch.lost<-chset[match(interaction(df.poll$Plot,df.poll$month),interaction(chset$Plot,chset$Month)),"Chlost"]
df.poll$Fb<-chset[match(interaction(df.poll$Plot,df.poll$month),interaction(chset$Plot,chset$Month)),"F.Buds"]
df.poll$Fo<-chset[match(interaction(df.poll$Plot,df.poll$month),interaction(chset$Plot,chset$Month)),"Flowers"]
df.poll$SBT<-chset[match(interaction(df.poll$Plot,df.poll$month),interaction(chset$Plot,chset$Month)),"SBT"]
df.poll$ppt<-ppt.m[match(df.poll$month,ppt.m$month),"Tppt"]
df.poll$meanT<-m.climate[match(interaction(df.poll$Plot,df.poll$month),interaction(m.climate$Plot,m.climate$month)),"meanT"]
df.poll$meanRH<-m.climate[match(interaction(df.poll$Plot,df.poll$month),interaction(m.climate$Plot,m.climate$month)),"meanRH"]
df.poll$maxVPD<-m.climate[match(interaction(df.poll$Plot,df.poll$month),interaction(m.climate$Plot,m.climate$month)),"maxVPD"]
df.poll$distance<-ns[match(df.poll$Plot,ns$name3),"distance"]
df.poll$C.gap<-ns[match(df.poll$Plot,ns$name3),"Gap_Jan15"]
df.poll$C.dens<-floor(rowMeans(cbind(b.a[match(df.poll$Plot,b.a$Plot),"C.dens1"],b.a[match(df.poll$Plot,b.a$Plot),"C.dens2"])))
df.poll$S.dens<-floor(rowMeans(cbind(b.a[match(df.poll$Plot,b.a$Plot),"S.dens1"],b.a[match(df.poll$Plot,b.a$Plot),"S.dens2"])))

library(corrplot)

df.poll.15<-df.poll[year(df.poll$month)=="2015",]

#do correlation matrices to remove correlated variables
d.C<-cbind(df.poll[,4:6],df.poll[,8:ncol(df.poll)])
d.C.15<-cbind(df.poll.15[,4:6],df.poll.15[,8:ncol(df.poll)])

s<-cor(d.C,use="complete.obs")
s[is.na(s)]<-0

pdf(paste0(getwd(),"/Analysis/ES/Corrplot_monthly_pollination.pdf"))
corrplot(s, method = "circle",tl.cex = .7)
dev.off()

s.15<-cor(d.C.15,use="complete.obs")
s.15[is.na(s.15)]<-0

pdf(paste0(getwd(),"/Analysis/ES/Corrplot_monthly_pollination.2015.pdf"))
corrplot(s.15, method = "circle",tl.cex = .7)
dev.off()

rm(s,s.15)

#run glmms
library(MuMIn)
library(arm)
library(car)
library(lattice)

#check which linking function I need, see how well values stay within dashed lines
qqp(df.poll$Pollin.nos, "lnorm")

df.poll$Pollin.nos.1<-df.poll$Pollin.nos+1
df.poll$treat<-factor(df.poll$treat)
df.poll$distance<-factor(df.poll$distance)
df.poll$SBT.1<-"None"
df.poll[df.poll$SBT>0&df.poll$SBT<=11&!is.na(df.poll$SBT),"SBT.1"]<-"Low"
df.poll[df.poll$SBT>11&!is.na(df.poll$SBT),"SBT.1"]<-"High"
df.poll$SBT.1<-factor(df.poll$SBT.1)
tmp<-quantile(df.poll$C.dens)
df.poll$C.dens.1<-"Low"
df.poll[df.poll$C.dens>as.numeric(tmp[2])&df.poll$C.dens<=as.numeric(tmp[4]),"C.dens.1"]<-"Med"
df.poll[df.poll$C.dens>as.numeric(tmp[4]),"C.dens.1"]<-"High"
tmp<-quantile(df.poll$S.dens)
df.poll$S.dens.1<-"Low"
df.poll[df.poll$S.dens>as.numeric(tmp[2])&df.poll$S.dens<=as.numeric(tmp[4]),"S.dens.1"]<-"Med"
df.poll[df.poll$S.dens>as.numeric(tmp[4]),"S.dens.1"]<-"High"


#negative binomial
nbinom <- fitdistr(df.poll$Pollin.nos, "Negative Binomial")
qqp(df.poll$Pollin.nos, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])

#poisson distribution
poisson <- fitdistr(df.poll$Pollin.nos, "Poisson")
qqp(df.poll$Pollin.nos, "pois", poisson$estimate)

#gamma distribution
gamma <- fitdistr(df.poll$Pollin.nos.1, "gamma")
qqp(df.poll$Pollin.nos.1, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])

options(na.action = "na.omit")

(fm001<-glmer(Pollin.nos.1~1+(1|Plot),data=df.poll,family=Gamma))

fm01<-glmer(Pollin.nos.1~distance+C.gap+meanT+C.pct+soil.m+treat+(1|Plot),data=df.poll,family=Gamma)
summary(fm01)
fm01s<-standardize(fm01)
summary(fm01s)

fm02<-glmer(Pollin.nos.1~distance+C.gap+meanT+C.pct*soil.m+(1|Plot),data=df.poll,family=Gamma)
summary(fm02)
fm02s<-standardize(fm02)
summary(fm02s)

fm03<-glmer(Pollin.nos.1~distance+C.gap+meanT+SBT+C.pct*soil.m+(1|Plot),data=df.poll,family=Gamma)
summary(fm03)
fm03s<-standardize(fm03)
summary(fm03s)

fm04<-glmer(Pollin.nos.1~distance+C.gap+meanT+SBT+C.dens+S.dens+C.pct*soil.m+(1|Plot),data=df.poll,family=Gamma)
summary(fm04)
fm04s<-standardize(fm04)
summary(fm04s)

fm05<-glmer(Pollin.nos.1~distance+C.gap+meanT+SBT.1+C.dens+S.dens+C.pct*soil.m+(1|Plot),data=df.poll,family=Gamma)
summary(fm05)
fm05s<-standardize(fm05)
summary(fm05s)

fm06<-glmer(Pollin.nos.1~distance+C.gap+meanT+C.dens.1+S.dens+C.pct*soil.m+(1|Plot),data=df.poll,family=Gamma)
summary(fm06)
fm06s<-standardize(fm06)
summary(fm06s)

fm07<-glmer(Pollin.nos.1~distance+C.gap+meanT+S.dens.1+C.pct*soil.m+(1|Plot),data=df.poll,family=Gamma)
summary(fm07)
fm07s<-standardize(fm07)
summary(fm07s)

fm08<-glmer(Pollin.nos.1~distance+C.gap+meanT+SBT.1+C.pct*soil.m+(1|Plot),data=df.poll,family=Gamma)
summary(fm08)
fm08s<-standardize(fm08)
summary(fm08s)

fm09<-glmer(Pollin.nos.1~distance+C.gap+meanT+C.pct+soil.m+(1|Plot),data=df.poll,family=Gamma)
summary(fm09)
fm09s<-standardize(fm09)
summary(fm09s)

fm10<-glmer(Pollin.nos.1~distance+C.gap+meanT+SBT.1+S.dens+C.dens.1+C.pct*soil.m+(1|Plot),data=df.poll,family=Gamma)
summary(fm10)
fm10s<-standardize(fm10)
summary(fm10s)

#boxplot of residuals by group, to check that residuals are centered around 0, but variability changes with group
bwplot(fm10s@frame$Plot~resid(fm10s))
#compare observed responses vs within-group fitted values
## create data frame of residuals, fitted values, and variable
diagnos <- data.frame(Resid = resid(fm10s, type = "pearson"), Fitted = fitted(fm10s),Variable = df.poll[!is.na(df.poll$soil.m)&!is.na(df.poll$meanT),"Plot"])

pdf(paste0(getwd(),"/Analysis/ES/Pollin.Nos_ResidualvFittedValues_all.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos)
dev.off()

## separate by variable
pdf(paste0(getwd(),"/Analysis/ES/Pollin.Nos_ResidualvFittedValues.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted | Variable, data = diagnos)
dev.off()

#Assumption 2: Random effects are normally distributed with mean zero and covariance matrix (not depending on the group) and are independent for different groups
#test for normality of residuals
## overal QQ normal plot
qqmath(~Resid, data = diagnos, distribution = qnorm)
## separate by variable
qqmath(~Resid | Variable, data = diagnos, distribution = qnorm)
## we could add a line to indicate 'normal' with a bit more work
pdf(paste0(getwd(),"/Analysis/ES/Pollin.Nos_qqplotResiduals_byplot.pdf"),width=8,height=8)
qqmath(~Resid | Variable, data = diagnos, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

options(na.action = "na.fail")

fm10d<-dredge(fm10s)
dredg.m01<-subset(fm10d,delta<6)
#take model average... of best fit model... [fm11d]
topmodels1.avg<-model.avg(dredg.m01)
x1<-as.data.frame(summary(topmodels1.avg)$importance)
x1$Comparison<-rownames(x1)
colnames(x1)<-c("Importance","Comparison")

#create figure of coefficients with confidence intervals
tmp<-as.data.frame(t(topmodels1.avg[[2]]))
tmp$lwr<-confint(topmodels1.avg)[,1]
tmp$upr<-confint(topmodels1.avg)[,2]
tmp$Comparison <- rownames(tmp)
#add importance
tmp$Importance<-x1[match(tmp$Comparison,x1$Comparison),"Importance"]

#order by importance
tmp<-tmp[!is.na(tmp$Importance),]
#tmp<-tmp[order(tmp$Importance,decreasing=T),]
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=T),"Comparison"])

ggplot(tmp, aes(x = Comparison, y = full, ymin = lwr, ymax = upr)) + geom_errorbar() + geom_point()+geom_point(aes(x = Comparison, y = subset),size=2)+
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Influence of ES factors on Pollinator Numbers")+
  xlab("Variable [ranked by importance]")+ylab("Coefficient")

