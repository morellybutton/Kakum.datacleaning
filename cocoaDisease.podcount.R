#combine disease survey for bird analysis with monthly pod removals due to disease

library(tidyverse)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")

d.pod <- read.csv(paste0(getwd(),"/Yield/Monthly_podremovals.csv"))
to.combine <- read.csv(paste0(getwd(),"/Disease/Monthly.disease.per.tree (with unique IDs).csv"))

to.combine$Date<-as.Date(to.combine$Date,format="%d/%m/%Y")
#create unique tree ID
d.pod$treeno.1<-str_split_fixed(d.pod$treeno, " ",2)[,1]
d.pod$Plot.and.Tree.no<-paste(d.pod$plot,d.pod$treeno.1)

#sum number of pods, regardless of size and pull out kg of beans lost to each measure
d.pod2 <- d.pod %>% group_by(date,Plot.and.Tree.no) %>% mutate(no.pod.bp=sum(s.bp,m.bp,na.rm=T),no.pod.cpb=sum(s.cpb,m.cpb,l.cpb,na.rm=T),
                                                               no.pod.mam=sum(s.mam,m.mam,l.mam, na.rm=T),Date=as.Date(date,format="%Y-%m-%d")) %>% ungroup() %>%
  select(Plot.and.Tree.no,Date,no.pod.bp,bp_kg,no.pod.cpb,cpb_kg,no.pod.mam,mam_kg)


to.combine <- to.combine %>% mutate(id=1:nrow(to.combine)) 
to.combine <- left_join(to.combine,d.pod2, by=c("Plot.and.Tree.no","Date"))
to.combine<-to.combine %>% distinct(id, .keep_all = TRUE)

write.csv(to.combine,paste0(getwd(),"/Disease/Monthly.disease.per.tree (with unique IDs and lost pods).csv"))

