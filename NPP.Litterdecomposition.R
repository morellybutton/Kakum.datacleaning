### Function fine litter fall: 
# This function uses data to calculate litter decomposition from quarterly and two-weekly fine litterfall.
# Using equation from Karberg et al. 2008

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/NPP/")

library(lubridate)
#library(plyr)
library(grid)
library(gridExtra)
#library(reshape)
library(tidyverse)

plotnames = c("HM FP","KA FP","KA 100 F3","KA 100 F1","KA 500 F3","KA 1K F3","HM 5K F2","HM 500 F3","HM 100 F3","HM 500 F2")
plotsize=0.36  ### VARIABLE PLOTSIZE IS NOT YET INCLUDED: DISCUSS WITH CECILE, HOW TO INCLUDE IT...
num.lf<-9

t1<-"2014-10-01"
t2<-"2017-07-01"

#identify time periods to average over
yrs<-c("2015-10-01","2016-10-01","2017-10-01")

#calculate k-rate using equation y=exp(-kt) or k=-t/log(y)
comp<-c()
m_comp<-c()
for(i in 1:length(plotnames)){
  plotname<-plotnames[i]
  flf.q<-read.csv(paste0(getwd(),"/FineLitterFall/FLFQ_",gsub(" ","",plotname),"_pertrap_output.csv"))
  flf.w<-read.csv(paste0(getwd(),"/Litter Data/FLF_",gsub(" ","",plotname),"_pertrap_output.csv"))
  flf.w$date<-as.Date(paste(flf.w$Year,flf.w$Month,"01",sep="-"))
  #flf.w$month<-round_date(as.Date(flf.w$date),"month")
  
  #pull out only quarterly measures
  flf.q<-flf.q %>% select(dates,colnames(flf.q[,grep(".q",colnames(flf.q))]),litter_trap_num)
  flf.q$dates<-as.Date(flf.q$dates,format="%Y-%m-%d")
  #add month to quarterly measures
  flf.q$month<-round_date(flf.q$dates,unit="month")
  if(length(flf.q[is.na(flf.q$month),1])==nrow(flf.q)) flf.q$month<-round_date(flf.q$dates,unit="month")
  #pull out dates to compare with litter fall traps
  t.p<-unique(flf.q$month)
  t.p<-t.p[t.p>=t1&t.p<=t2]
  
  decomp<-list()
  for(j in 2:(length(t.p))){
    if(j>1) tr.ps<-flf.w %>% filter(date<t.p[j]&date>=t.p[j-1]) else tr.ps<-flf.w %>% filter(date<t.p[j]&date>=t1)
    
    tr.ps<-tr.ps %>% mutate_all(funs(replace(., .<0, NA))) %>% group_by(litter_trap_num,date) %>% mutate(totleafflfAs=sum(leafflfAs,leafflfcAs,na.rm = T),totleafflfAs.se=sqrt(sum(leafflfAsstd^2,leafflfcAsstd^2,na.rm = T))) %>% ungroup()
    #sum amount of litter fallen in traps during period, propagate error
    m_tr.ps<-tr.ps %>% group_by(litter_trap_num) %>% summarise(totflfAs=sum(totflfAs,na.rm = T),totleafflfAs=sum(totleafflfAs,na.rm = T),leafflfAs=sum(leafflfAs,na.rm = T),leafflfcAs=sum(leafflfcAs,na.rm = T),
                                   totflfAs.se=sqrt(sum(totflfAsstd^2,na.rm = T)),leafflfAs.se=sqrt(sum(leafflfAsstd^2,na.rm = T)),leafflfcAs.se=sqrt(sum(leafflfcAsstd^2,na.rm = T)),
                                   totleafflfAs.se=sqrt(sum(totleafflfAs.se^2,na.rm = T))) %>% ungroup()
   
    #m_tr.ps$month<- t.p[j]
    #decomp[[j]]<-m_tr.ps
    
    if(j>1) q.rtly<-flf.q %>% filter(month<=t.p[j]&month>t.p[j-1]) else q.rtly<-flf.q %>% filter(month<=t.p[j]&month>t1)
    
    #average amount of litter trap
    q.rtly<-q.rtly %>% group_by(litter_trap_num,month) %>% mutate(totleafflfAs.q=sum(leafflfAs.q,leafflfcAs.q,na.rm = T),totleafflfAs.q.se=sqrt(sum(leafflfAsstd.q^2,leafflfcAsstd.q^2,na.rm = T))) %>% 
      mutate(test=sum(seedsfAs.q,leafflfAs.q,leafflfcAs.q,fruitflfAs.q,flowerflfAs.q,branchflfAs.q)/totflfAs.q) %>% ungroup()
    #m_q.rtly<-q.rtly %>% group_by(litter_trap_num) %>% summarise(totflfAs.q=mean(totflfAs.q,na.rm = T),totleafflfAs.q=mean(totleafflfAs.q,na.rm = T),leafflfAs.q=mean(leafflfAs.q,na.rm = T),leafflfcAs.q=mean(leafflfcAs.q,na.rm = T))  %>% ungroup()
    #m_q.rtly.se<-q.rtly %>% group_by(litter_trap_num) %>% 
    #  summarise(totflfAs.q.se=sd(totflfAs.q,na.rm = T)/sqrt(n()),leafflfAs.q.se=sd(leafflfAs.q,na.rm=T)/sqrt(n()),leafflfcAs.q.se=sd(leafflfcAs.q,na.rm=T)/sqrt(n()),
    #            totleafflfAs.q.se=sd(totleafflfAs.q,na.rm=T)/sqrt(n())) %>% ungroup()                                       
                                                                 
    # m_q.rtly<-left_join(m_q.rtly,m_q.rtly.se,by="litter_trap_num")
    
    m_tr.ps<-left_join(m_tr.ps,q.rtly,by="litter_trap_num")
    
    decomp[[j]]<-m_tr.ps
    
  
  }
  d.cmp<-do.call(rbind.data.frame,decomp)
  #d.cmp2<-d.cmp %>% group_by(month) %>% select(-date1,-date2,-litter_trap_num) %>% 
  #  summarise_all(funs(mean),na.rm=T)
  
  #d.cmp.se<-d.cmp[,grep(".se",colnames(d.cmp))]
  #d.cmp<-d.cmp[,grep(".se",colnames(d.cmp),invert=T)]
  #d.cmp.se$litter_trap_num<-d.cmp$litter_trap_num
  #d.cmp.se$month<-d.cmp$month
  
  #f.nal2<-d.cmp %>%  group_by(month,litter_trap_num) %>% mutate(y_leafflfAs=leafflfAs.q/leafflfAs,y_leafflfAs.se=leafflfAs.q/leafflfAs*sqrt(sum((leafflfAs.se/leafflfAs)^2,(leafflfcAsstd.q/leafflfAs.q)^2,na.rm=T)),
  #                                             y_leafflfcAs=leafflfcAs.q/leafflfAs,y_leafflfcAs.se=leafflfcAs.q/leafflfAs*sqrt(sum((leafflfcAs.se/leafflfcAs)^2,(leafflfcAsstd.q/leafflfcAs.q)^2,na.rm=T)),
  #                                             y_totflfAs=totflfAs.q/totflfAs,y_totflfAs.se=totflfAs.q/totflfAs*sqrt(sum((totflfAs.se/totflfAs)^2,(totflfAsstd.q/totflfAs.q)^2,na.rm=T)),
  #                                             y_totleafflfAs=totleafflfAs.q/totleafflfAs,y_totleafflfAs.se=totleafflfAs.q/totleafflfAs*sqrt(sum((totleafflfAs.se/totleafflfAs)^2,(totleafflfAs.q.se/totleafflfAs.q)^2,na.rm=T))) %>% 
  #  na_if(Inf) %>% na_if(-Inf) %>% group_by(litter_trap_num) %>% summarise(y_leafflfAs=mean(y_leafflfAs,na.rm=T),y_leafflfAs.se=sqrt(mean(y_leafflfAs.se^2,na.rm=T)),
  #                                                                        y_leafflfcAs=mean(y_leafflfcAs,na.rm=T),y_leafflfcAs.se=sqrt(mean(y_leafflfcAs.se^2,na.rm=T)),
  #                                                                        y_totflfAs=mean(y_totflfAs,na.rm=T),y_totflfAs.se=sqrt(mean(y_totflfAs.se^2,na.rm=T)),
  #                                                                        y_totleafflfAs=mean(y_totleafflfAs,na.rm=T),y_totleafflfAs.se=sqrt(mean(y_totleafflfAs.se^2,na.rm=T))) %>% 
  #  ungroup()
  
  
  #flf.q<-flf.q[,grep("std.q",colnames(flf.q),invert=T)]
  #flf.q <-flf.q %>% group_by(litter_trap_num,dates) %>% mutate(totleafflfAs.q=sum(leafflfAs.q,leafflfcAs.q,na.rm = T)) %>% ungroup() 
  #flf.q<- flf.q %>% group_by(month) %>% select(-dates,-litter_trap_num) %>% 
  #  summarise_all(funs(mean,sd),na.rm=T) %>% mutate_at(vars(matches("_sd")),funs(./sqrt(11))) %>% 
  #  select(month,totflfAs.q_mean,leafflfAs.q_mean,leafflfcAs.q_mean,totleafflfAs.q_mean,totflfAs.q_sd,leafflfAs.q_sd,leafflfcAs.q_sd,totleafflfAs.q_sd) %>% 
  #  ungroup()
    
  #take average quarterly input over monitoring period
  #flf.q2<-flf.q %>% group_by(litter_trap_num) %>% select(-dates,-month) %>% 
  #  summarise_all(funs(mean,sd),na.rm=T) %>% mutate_at(vars(matches("_sd")),funs(./sqrt(11))) %>% 
  # select(litter_trap_num,totflfAs.q_mean,leafflfAs.q_mean,leafflfcAs.q_mean,totleafflfAs.q_mean,totflfAs.q_sd,leafflfAs.q_sd,leafflfcAs.q_sd,totleafflfAs.q_sd)
  
  # d.cmp<-left_join(d.cmp,flf.q,by=c("litter_trap_num","month"))
  #m_d.cmp<-d.cmp %>% group_by(month) %>% select(-litter_trap_num,-date1,-date2)%>% summarise_all(funs(mean,sd),na.rm=T) %>%  mutate_at(vars(matches("_sd")),funs(./sqrt(9))) %>% ungroup() 
  #d.cmp2<-left_join(d.cmp2,flf.q,by=c("month"))
  
  # m_d.cmp <- d.cmp %>% group_by(litter_trap_num) %>% select(-month,-dates,-date1,-date2) %>% summarise_all(funs(mean,sd)) %>% ungroup() %>% mutate_at(vars(matches("_sd")),funs(./sqrt(10)))
  # m_d.cmp.se<-d.cmp.se %>% group_by(litter_trap_num,month) %>% mutate_if(is.numeric,funs(.^2))  %>% ungroup() %>% select(-litter_trap_num) %>% 
  #  group_by(month) %>% summarise_all(mean) %>% mutate_at(vars(matches(".se")),sqrt) %>% ungroup()
 
  #m_d.cmp<-left_join(m_d.cmp ,m_d.cmp.se,by="month") 
  
  f.nal <- d.cmp %>% group_by(month) %>% select(-litter_trap_num) %>% 
      summarise(totflfAs.q=mean(totflfAs.q,na.rm = T),totleafflfAs.q=mean(totleafflfAs.q,na.rm = T),leafflfAs.q=mean(leafflfAs.q,na.rm = T),leafflfcAs.q=mean(leafflfcAs.q,na.rm = T),
                totflfAs=mean(totflfAs,na.rm = T),totleafflfAs=mean(totleafflfAs,na.rm = T),leafflfAs=mean(leafflfAs,na.rm = T),leafflfcAs=mean(leafflfcAs,na.rm = T))
  f.nal.se <- d.cmp %>% group_by(month) %>% select(-litter_trap_num) %>% 
    summarise(totflfAs.q.se=sd(totflfAs.q,na.rm = T)/sqrt(9),totleafflfAs.q.se=sd(totleafflfAs.q,na.rm = T)/sqrt(9),leafflfAs.q.se=sd(leafflfAs.q,na.rm = T)/sqrt(9),leafflfcAs.q.se=sd(leafflfcAs.q,na.rm = T)/sqrt(9),
              totflfAs.se=sd(totflfAs,na.rm = T)/sqrt(9),totleafflfAs.se=sd(totleafflfAs,na.rm = T)/sqrt(9),leafflfAs.se=sd(leafflfAs,na.rm = T)/sqrt(9),leafflfcAs.se=sd(leafflfcAs,na.rm = T)/sqrt(9))
  f.nal<-left_join(f.nal,f.nal.se,by="month")
  
   f.nal<-f.nal %>%  group_by(month) %>% mutate(k_leafflfAs=leafflfAs/leafflfAs.q,k_leafflfAs.se=leafflfAs/leafflfAs.q*sqrt(sum((leafflfAs.se/leafflfAs)^2,(leafflfAs.q.se/leafflfAs.q)^2)),
              k_leafflfcAs=leafflfcAs/leafflfAs.q,k_leafflfcAs.se=leafflfcAs/leafflfAs.q*sqrt(sum((leafflfcAs.se/leafflfcAs)^2,(leafflfcAs.q.se/leafflfcAs.q)^2)),
              k_totflfAs=totflfAs/totflfAs.q,k_totflfAs.se=totflfAs/totflfAs.q*sqrt(sum((totflfAs.se/totflfAs)^2,(totflfAs.q.se/totflfAs.q)^2)),
               k_totleafflfAs=totleafflfAs/totleafflfAs.q,k_totleafflfAs.se=totleafflfAs/totleafflfAs.q*sqrt(sum((totleafflfAs.se/totleafflfAs)^2,(totleafflfAs.q.se/totleafflfAs.q)^2))) %>% 
     na_if(Inf) %>% na_if(-Inf) %>%  mutate(r_leafflfcAs=1/k_leafflfcAs,r_leafflfcAs.se=(k_leafflfcAs.se/k_leafflfcAs)*(1/k_leafflfcAs),
                                      r_totflfAs=1/k_totflfAs,r_totflfAs.se=(k_totflfAs.se/k_totflfAs)*1/k_totflfAs,
                                      r_totleafflfAs=1/k_totleafflfAs,r_totleafflfAs.se=(k_totleafflfAs.se/k_totleafflfAs)*(1/k_totleafflfAs)) %>% 
     ungroup()
  
    #take average of litterfall and litter stock over all sub-plots
  #q_tr.ps <-  m_tr.ps %>% summarise(totflfAs=mean(totflfAs,na.rm = T),seedsfAs=mean(seedsfAs,na.rm = T),leafflfAs=mean(leafflfAs,na.rm = T),leafflfcAs=mean(leafflfcAs,na.rm = T),fruitflfAs=mean(fruitflfAs,na.rm = T),flowerflfAs=mean(flowerflfAs,na.rm = T),branchflfAs=mean(branchflfAs,na.rm = T),
  #                                   #                                                     totflfAs.se=sqrt(mean(totflfAs.se^2,na.rm=T)),seedsfAs.se=sqrt(mean(seedsfAs.se^2,na.rm=T)),leafflfAs.se=sqrt(mean(leafflfAs.se^2,na.rm=T)),leafflfcAs.se=sqrt(mean(leafflfcAs.se^2,na.rm=T)),fruitflfAs.se=sqrt(mean(fruitflfAs.se^2,na.rm=T)),
  #                                                      flowerflfAs.se=sqrt(mean(flowerflfAs.se^2,na.rm=T)),branchflfAs.se=sqrt(mean(branchflfAs.se^2,na.rm=T))) %>% mutate(totleafflfAs=sum(leafflfAs,leafflfcAs),totleafflfAs.se=sqrt(sum(leafflfAs.se^2,leafflfcAs.se^2)))
    
   
    #tr.ps.tot<-data.frame(t(colSums(tr.ps[,5:18])))
    #pull out quarterly amount of litter fall 
    #qr.tly<-flf.q[flf.q$month>=t1&flf.q$month<t2,]
  #  qr.tly<-flf.q %>% filter(month==t.p[j])
    #take average across subplots
  #  m_qr.tly<-qr.tly %>% summarise(totflfAs.q=mean(totflfAs.q,na.rm=T),seedsfAs.q=mean(seedsfAs.q,na.rm=T),
  #                                 leafflfAs.q=mean(leafflfAs.q,na.rm=T),leafflfcAs.q=mean(leafflfcAs.q,na.rm=T),
  #                                 fruitflfAs.q=mean(fruitflfAs.q,na.rm=T),flowerflfAs.q=mean(flowerflfAs.q,na.rm=T),
  #                                 branchflfAs.q=mean(branchflfAs.q,na.rm=T)) %>% mutate(totleafflfAs.q=sum(leafflfAs.q,leafflfcAs.q))
  #  m_qr.tly.se <-qr.tly %>% summarise(totflfAs.q.se=sd(totflfAs.q,na.rm=T)/sqrt(num.lf),seedsfAs.q.se=sd(seedsfAs.q)/sqrt(num.lf),
  #                                    leafflfAs.q.se=sd(leafflfAs.q,na.rm=T)/sqrt(num.lf),leafflfcAs.q.se=sd(leafflfcAs.q,na.rm=T)/sqrt(num.lf),
  #                                    fruitflfAs.q.se=sd(fruitflfAs.q,na.rm=T)/sqrt(num.lf),flowerflfAs.q.se=sd(flowerflfAs.q,na.rm=T)/sqrt(num.lf),
  #                                    branchflfAs.q.se=sd(branchflfAs.q,na.rm=T)/sqrt(num.lf)) %>% mutate(totleafflfAs.q.se=sqrt(sum(leafflfAs.q.se^2,leafflfcAs.q.se^2)))
  #  m_qr.tly<-bind_cols(m_qr.tly, m_qr.tly.se)
  #  q_tr.ps<-bind_cols(q_tr.ps,m_qr.tly)
    #qr.tly.1<-ddply(qr.tly,.(litter_trap_num), summarise, totflfAs.q=mean(totflfAs.q,na.rm=T),seedsfAs.q=mean(seedsfAs.q,na.rm=T),leafflfAs.q=mean(leafflfAs.q,na.rm=T),leafflfcAs.q=mean(leafflfcAs.q,na.rm=T),fruitflfAs.q=mean(fruitflfAs.q,na.rm=T),flowerflfAs.q=mean(flowerflfAs.q,na.rm=T),branchflfAs.q=mean(branchflfAs.q,na.rm=T))
    #qr.tly<-qr.tly[,2:15]+flf.q[flf.q$month==t.p[1],2:15]
    
    #divide by litter component per litter trap (litter trap/quarterly measures)
    #d.c.mp<-data.frame(cbind(tr.ps.tot$seedsfAs/qr.tly.1$seedsfAs.q,tr.ps.tot$leafflfAs/qr.tly.1$leafflfAs.q,tr.ps.tot$leafflfcAs/qr.tly.1$leafflfcAs.q,tr.ps.tot$fruitflfAs/qr.tly.1$fruitflfAs.q,tr.ps.tot$flowerflfAs/qr.tly.1$flowerflfAs.q,tr.ps.tot$branchflfAs/qr.tly.1$branchflfAs.q,tr.ps.tot$totflfAs/qr.tly.1$totflfAs.q))
    #calculate y (percent remaining over time elapsed, quarterly measures/summed bi-monthly measures)
  #  d.c.mp<- q_tr.ps %>% mutate(y_seedsfAs=seedsfAs.q/seedsfAs,y_seedsfAs.se=seedsfAs.q/seedsfAs*sqrt(sum((seedsfAs.se/seedsfAs)^2,(seedsfAs.q.se/seedsfAs.q)^2)),
  #                              y_leafflfAs=leafflfAs.q/leafflfAs,y_leafflfAs.se=leafflfAs.q/leafflfAs*sqrt(sum((leafflfAs.se/leafflfAs)^2,(leafflfAs.q.se/leafflfAs.q)^2)),
  #                              y_leafflfcAs=leafflfcAs.q/leafflfcAs,y_leafflfcAs.se=leafflfcAs.q/leafflfcAs*sqrt(sum((leafflfcAs.se/leafflfcAs)^2,(leafflfcAs.q.se/leafflfcAs.q)^2)),
  #                              y_fruitflfAs=fruitflfAs.q/fruitflfAs,y_fruitflfAs.se=fruitflfAs.q/fruitflfAs*sqrt(sum((fruitflfAs.se/fruitflfAs)^2,(fruitflfAs.q.se/fruitflfAs.q)^2)),
  #                              y_flowerflfAs=flowerflfAs.q/flowerflfAs,y_flowerflfAs.se=flowerflfAs.q/flowerflfAs*sqrt(sum((flowerflfAs.se/flowerflfAs)^2,(flowerflfAs.q.se/flowerflfAs.q)^2)),
  #                              y_branchflfAs=branchflfAs.q/branchflfAs,y_branchflfAs.se=branchflfAs.q/branchflfAs*sqrt(sum((branchflfAs.se/branchflfAs)^2,(branchflfAs.q.se/branchflfAs.q)^2)),
  #                              y_totflfAs=totflfAs.q/totflfAs,y_totflfAs.se=totflfAs.q/totflfAs*sqrt(sum((totflfAs.se/totflfAs)^2,(totflfAs.q.se/totflfAs.q)^2)),
  #                              y_totleafflfAs=totleafflfAs.q/totleafflfAs,y_totleafflfAs.se=totleafflfAs.q/totleafflfAs*sqrt(sum((totleafflfAs.se/totleafflfAs)^2,(totleafflfAs.q.se/totleafflfAs.q)^2))) %>%
  #    mutate(ln_seedsfAs=-log(y_seedsfAs),ln_seedsfAs.se=y_seedsfAs.se/y_seedsfAs,ln_totflfAs=-log(y_totflfAs),ln_totflfAs.se=y_totflfAs.se/y_totflfAs,ln_leafflfAs=-log(y_leafflfAs),ln_leafflfAs.se=y_leafflfAs.se/y_leafflfAs,
  #           ln_leafflfcAs=-log(y_leafflfcAs),ln_leafflfcAs.se=y_leafflfcAs.se/y_leafflfcAs,ln_fruitflfAs=-log(y_fruitflfAs),ln_fruitflfAs.se=y_fruitflfAs.se/y_fruitflfAs,ln_flowerflfAs=-log(y_flowerflfAs),ln_flowerflfAs.se=y_flowerflfAs.se/y_flowerflfAs,
  #           ln_branchflfAs=-log(y_branchflfAs),ln_branchflfAs.se=y_branchflfAs.se/y_branchflfAs,ln_totleafflfAs=-log(y_totleafflfAs),ln_totleafflfAs.se=y_totleafflfAs.se/y_totleafflfAs)
    #remove negative log values (assuming issues with data collection methodology)
  #  d.c.mp <- d.c.mp %>% mutate(ln_totflfAs.se=replace(ln_totflfAs.se,ln_totflfAs<0,NA),ln_totflfAs=replace(ln_totflfAs,ln_totflfAs<0,NA),ln_leafflfAs.se=replace(ln_leafflfAs,ln_leafflfAs<0,NA),ln_leafflfAs=replace(ln_leafflfAs,ln_leafflfAs<0,NA),
  #                              ln_leafflfcAs.se=replace(ln_leafflfcAs.se,ln_leafflfcAs<0,NA),ln_leafflfcAs=replace(ln_leafflfcAs,ln_leafflfcAs<0,NA),ln_fruitflfAs.se=replace(ln_fruitflfAs.se,ln_fruitflfAs<0,NA),ln_fruitflfAs=replace(ln_fruitflfAs,ln_fruitflfAs<0,NA),
  #                             ln_flowerflfAs.se=replace(ln_flowerflfAs.se,ln_flowerflfAs<0,NA),ln_flowerflfAs=replace(ln_flowerflfAs,ln_flowerflfAs<0,NA),ln_branchflfAs.se=replace(ln_branchflfAs,ln_branchflfAs<0,NA),ln_branchflfAs=replace(ln_branchflfAs,ln_branchflfAs<0,NA),
  #                              ln_totleafflfAs.se=replace(ln_totleafflfAs.se,ln_totleafflfAs<0,NA),ln_totleafflfAs=replace(ln_totleafflfAs,ln_totleafflfAs<0,NA))
    #replace NAs and Inf with 0s
    #d.c.mp[is.na(d.c.mp)]<-0
    #d.c.mp[!is.finite(d.c.mp)]<-0
    #myList <- setNames(lapply(vector("list", ncol(d.c.mp)), function(x) x <- 0), names(d.c.mp))
    #d.c.mp<- d.c.mp %>% replace_na(myList)
  #  }
#f.nal<-data.frame(do.call(rbind.data.frame,decomp),stringsAsFactors = F)
  #replace non-finite values
#f.nal <- do.call(data.frame, lapply(f.nal, function(x) {
#  replace(x, is.infinite(x) | is.na(x), NA)
#})

 

 #calculate k-rate using equation k=-log(y)/timediff and propagated error is sqrt((y.se/y)^2)
#  f.nal <- f.nal %>% mutate(k_totflfAs=ln_totflfAs/as.numeric(diff)*30,k_totflfAs.se=ln_totflfAs.se/as.numeric(diff)*30,
#                            k_seedsfAs=ln_seedsfAs/as.numeric(diff)*30,k_seedsfAs.se=ln_seedsfAs.se/as.numeric(diff)*30,
#                            k_leafflfAs=ln_leafflfAs/as.numeric(diff)*30,k_leafflfAs.se=ln_leafflfAs.se/as.numeric(diff)*30,
#                            k_leafflfcAs=ln_leafflfcAs/as.numeric(diff)*30,k_leafflfcAs.se=ln_leafflfcAs.se/as.numeric(diff)*30,
#                            k_fruitflfAs=ln_fruitflfAs/as.numeric(diff)*30,k_fruitflfAs.se=ln_fruitflfAs.se/as.numeric(diff)*30,
#                            k_flowerflfAs=ln_flowerflfAs/as.numeric(diff)*30,k_flowerflfAs.se=ln_flowerflfAs.se/as.numeric(diff)*30,
#                            k_branchflfAs=ln_branchflfAs/as.numeric(diff)*30,k_branchflfAs.se=ln_branchflfAs.se/as.numeric(diff)*30,
#                            k_totleafflfAs=ln_totleafflfAs/as.numeric(diff)*30,k_totleafflfAs.se=ln_totleafflfAs.se/as.numeric(diff)*30)
#  
 
  write.csv(d.cmp,paste0(getwd(),"/FineLitterFall/DCMP_",gsub(" ","",plotname),"_output_all.csv"))
  write.csv(f.nal,paste0(getwd(),"/FineLitterFall/DCMP_",gsub(" ","",plotname),"_output_avg.csv"))
  
  #take annual average and propagate errors
  k_rate<-f.nal %>% summarise(k_leafflfAs=mean(k_leafflfAs,na.rm=T),k_leafflfAs.se=sqrt(mean(k_leafflfAs.se^2,na.rm=T)),
                              k_leafflfcAs=mean(k_leafflfcAs,na.rm=T),k_leafflfcAs.se=sqrt(mean(k_leafflfcAs.se^2,na.rm=T)),
                              k_totflfAs=mean(k_totflfAs,na.rm=T),k_totflfAs.se=sqrt(mean(k_totflfAs.se^2,na.rm=T)),
                              k_totleafflfAs=mean(k_totleafflfAs,na.rm=T),k_totleafflfAs.se=sqrt(mean(k_totleafflfAs.se^2,na.rm=T)))
  k_rate$plot_name<-gsub(" ","",plotname)
  comp<-bind_rows(comp,k_rate)
}
#calculate residence times (1/k)
comp <- comp %>% mutate(r_leafflfAs=1/k_leafflfAs,r_leafflfAs.se=(k_leafflfAs.se/k_leafflfAs)*(1/k_leafflfAs),
                        r_leafflfcAs=1/k_leafflfcAs,r_leafflfcAs.se=(k_leafflfcAs.se/k_leafflfcAs)*(1/k_leafflfcAs),
                        r_totflfAs=1/k_totflfAs,r_totflfAs.se=(k_totflfAs.se/k_totflfAs)*(1/k_totflfAs),
                        r_totleafflfAs=1/k_totleafflfAs,r_totleafflfAs.se=(k_totleafflfAs.se/k_totleafflfAs)*1/k_totleafflfAs)
#calculate biomass (r*annual npp)
npp_annual<-read_csv("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/Annual.measures_all.plots_final.csv")
npp_litter<-npp_annual %>% group_by(plot_name) %>% mutate(tot.npp=sum(canopy.shade,canopy.cocoa,twigs,na.rm=T),
                                                          tot.npp.sd=sqrt(sum(canopy.shade.sd^2,canopy.cocoa.sd^2,twigs.sd^2)),
                                                          litter.npp=sum(canopy.shade,canopy.cocoa),litter.npp.sd=sqrt(sum(canopy.shade.sd^2,canopy.cocoa.sd^2))) %>% 
  select(plot_name,canopy.shade3,canopy.shade.sd3,canopy.cocoa3,canopy.cocoa.sd3,tot.npp,tot.npp.sd,litter.npp,litter.npp.sd)

comp1<-left_join(comp,npp_litter,by="plot_name")
comp1<-comp1 %>% group_by(plot_name) %>% 
  mutate(b_leafflfAs=r_leafflfAs*canopy.shade3,b_leafflfAs.se=r_leafflfAs*canopy.shade3*sqrt((canopy.shade.sd3/canopy.shade3)^2+(r_leafflfAs.se/r_leafflfAs)^2),
         b_leafflfcAs=r_leafflfcAs*canopy.cocoa3,b_leafflfcAs.se=r_leafflfcAs*canopy.cocoa3*sqrt((canopy.cocoa.sd3/canopy.cocoa3)^2+(r_leafflfcAs.se/r_leafflfcAs)^2),
         b_totleafflfAs=r_totleafflfAs*litter.npp,
         b_totleafflfAs.se=r_totleafflfAs*litter.npp*sqrt((litter.npp.sd/litter.npp)^2+(r_totleafflfAs.se/r_totleafflfAs)^2),
         b_totflfAs=r_totflfAs*tot.npp,b_totflfAs.se=r_totflfAs*tot.npp*sqrt((tot.npp.sd/tot.npp)^2+(r_totleafflfAs.se/r_totleafflfAs)^2)) %>% ungroup()

write.csv(comp1,paste0(getwd(),"/FineLitterFall/DCMP_allplots_output.csv"))
write.csv(comp1,"/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/DCMP_allplots_table.csv")


comp1<-read.csv(paste0(getwd(),"/FineLitterFall/DCMP_allplots_output.csv"))
#calculate table values
f.nal<-comp1 %>% filter(plot_name!="HMFP"&plot_name!="KAFP"&plot_name!="HM5KF2") %>% 
  summarise(k_leafflfAs=mean(k_leafflfAs,na.rm=T),k_leafflfAs.se=sqrt(mean(k_leafflfAs.se^2,na.rm=T)),
            k_leafflfcAs=mean(k_leafflfcAs,na.rm=T),k_leafflfcAs.se=sqrt(mean(k_leafflfcAs.se^2,na.rm=T)),
            k_totleafflfAs=mean(k_totleafflfAs,na.rm=T),k_totleafflfAs.se=sqrt(mean(k_totleafflfAs.se^2,na.rm=T)),
            k_totflfAs=mean(k_totflfAs,na.rm=T),k_totflfAs.se=sqrt(mean(k_totflfAs.se^2,na.rm=T)),
            r_leafflfAs=mean(r_leafflfAs,na.rm=T),r_leafflfAs.se=sqrt(mean(r_leafflfAs.se^2,na.rm=T)),
            r_leafflfcAs=mean(r_leafflfcAs,na.rm=T),r_leafflfcAs.se=sqrt(mean(r_leafflfcAs.se^2,na.rm=T)),
            r_totleafflfAs=mean(r_totleafflfAs,na.rm=T),r_totleafflfAs.se=sqrt(mean(r_totleafflfAs.se^2,na.rm=T)),
            r_totflfAs=mean(r_totflfAs,na.rm=T),r_totflfAs.se=sqrt(mean(r_totflfAs.se^2,na.rm=T)),
            b_leafflfAs=mean(b_leafflfAs,na.rm=T),b_leafflfAs.se=sqrt(mean(b_leafflfAs.se^2,na.rm=T)),
            b_leafflfcAs=mean(b_leafflfcAs,na.rm=T),b_leafflfcAs.se=sqrt(mean(b_leafflfcAs.se^2,na.rm=T)),
            b_totleafflfAs=mean(b_totleafflfAs,na.rm=T),b_totleafflfAs.se=sqrt(mean(b_totleafflfAs.se^2,na.rm=T)),
            b_totflfAs=mean(b_totflfAs,na.rm=T),b_totflfAs.se=sqrt(mean(b_totflfAs.se^2,na.rm=T)))

write.csv(f.nal,"/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/HANPP/DCMP_avgplots_table.csv")

  



###need to rewrite nutrient cycling!!!
c.nuts<-read.csv(paste0("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Nutrients/Cocoa_nutrientlittercomponents.csv"))
s.nuts<-read.csv(paste0("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Nutrients/Shadetree_nutrientlittercomponents.csv"))
b.a<-read.csv(paste0("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/Nutrients/Basalarea.perspp.prop_perplot.csv"))

#add mid date between two periods
#c.mp$mdate<-round_date(as.Date(c.mp$date1) + floor((as.Date(c.mp$date2)-as.Date(c.mp$date1))/2),"month")

tot.s<-data.frame(c.mp[c.mp$subplot=="Plotavg",])
tot.se<-data.frame(c.mp[c.mp$subplot=="Plotse",])
colnames(tot.se)<-c("X","plot","subplot",paste0(colnames(tot.se[,4:10]),".se"))
c.mp<-data.frame(cbind(tot.s,tot.se[,4:10]))
#replace 0s with NAs
c.mp[c.mp==0]<-NA
#re-order plot factors by "canopy gap"
c.mp$plot <- ordered(c.mp$plot, levels = c("HM500F3", "KA1KF3", "KA100F3","HM500F2","HM100F3","KA500F3","KA100F1","HM5KF2","KAFP","HMFP"))

tmp<-data.frame(cbind(as.character(c.mp$plot),c.mp$leafflfAs,c.mp$leafflfcAs,c.mp$totflfAs),stringsAsFactors = F)
colnames(tmp)<-c("plot","Shade Leaves","Cocoa Leaves","Total")
tmp1<-data.frame(cbind(as.character(c.mp$plot),c.mp$leafflfAs.se,c.mp$leafflfcAs.se,c.mp$totflfAs.se),stringsAsFactors = F)
colnames(tmp1)<-c("plot","Shade.Leaves.se","Cocoa.Leaves.se","Total.se")

tmp.1<-melt(tmp,id.vars="plot")
tmp1.1<-melt(tmp1,id.vars="plot")
tmp.1$value<-as.numeric(as.character(tmp.1$value))
tmp.1$se<-as.numeric(as.character(tmp1.1$value))

#re-order plot factors by "canopy gap"
tmp.1$plot <- ordered(tmp.1$plot, levels = c("HM500F3", "KA1KF3", "KA100F3","HM500F2","HM100F3","KA500F3","KA100F1","HM5KF2","KAFP","HMFP"))
#save csv
write.csv(tmp.1,paste0(getwd(),"/Total/DCMP_shade.v.cocoa.leaves.csv"))


#create comparative figure across plots
p1<-ggplot(tmp.1[tmp.1$variable=="Total",],aes(plot,value))+geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.1)+geom_point()+
  xlab("Plot")+ylab("k [yr-1]")+ggtitle("All Litter")+
  geom_hline(yintercept=1,linetype="dashed",color="grey")+ylim(0,(max(tmp.1[tmp.1$variable=="Total","value"],na.rm=T)+max(tmp.1[tmp.1$variable=="Total","se"],na.rm=T)))+theme(
    text = element_text(size=18)) +  theme(
      plot.background = element_blank()
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()
      ,panel.border = element_blank()
      ,panel.background = element_blank()
      ,axis.text.x=element_text(angle = 45,hjust=1)) + theme(axis.line = element_line(color = 'black')
  )

fmt_dcimals <- function(decimals=0){
  function(x) format(x,nsmall = decimals,scientific = FALSE)
}

p2<-ggplot(tmp.1[tmp.1$variable!="Total",],aes(plot,value,color=variable))+geom_errorbar(aes(ymin=value-se, ymax=value+se,color=variable), width=.1)+geom_point()+
  ylab("k [yr-1]")+ggtitle("Annual Decay Rate for Leaf Litter\n[Ordered by Decreasing Canopy Gap]")+
  geom_hline(yintercept=1,linetype="dashed",color="grey")+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())+scale_y_continuous(labels = fmt_dcimals(2))+theme(
    text = element_text(size=18)) +  theme(
      legend.position="top"
      ,legend.title=element_blank()
      ,plot.background = element_blank()
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()
      ,panel.border = element_blank()
      ,panel.background = element_blank()
      ,legend.key = element_blank()) + theme(axis.line = element_line(color = 'black')
      )

g1<-grid.arrange(p2, p1, nrow = 2)
ggsave(paste0(getwd(),"/FineLitterFall/ComparisonDCMP_bycanopygap.pdf"),g1,height=8,width=10)

#re-order plot factors by decreasing "cocoa density"
tmp.1$plot <- ordered(tmp.1$plot, levels = c("KA100F1","KA500F3","HM5KF2", "HM100F3","HM500F3","HM500F2","KA100F3","KA1KF3", "KAFP","HMFP"))

#create comparative figure across plots
p1<-ggplot(tmp.1[tmp.1$variable=="Total",],aes(plot,value))+geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.1)+geom_point()+
  xlab("Plot")+ylab("k [yr-1]")+ggtitle("Total Litter")+
  geom_hline(yintercept=1,linetype="dashed",color="grey")+ylim(0,(max(tmp.1[tmp.1$variable=="Total","value"],na.rm=T)+max(tmp.1[tmp.1$variable=="Total","se"],na.rm=T)))+theme(
    text = element_text(size=18)) +  theme(
      plot.background = element_blank()
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()
      ,panel.border = element_blank()
      ,panel.background = element_blank()
      ,axis.text.x=element_text(angle = 45,hjust=1)) + theme(axis.line = element_line(color = 'black')
      )

fmt_dcimals <- function(decimals=0){
  function(x) format(x,nsmall = decimals,scientific = FALSE)
}

p2<-ggplot(tmp.1[tmp.1$variable!="Total",],aes(plot,value,color=variable))+geom_errorbar(aes(ymin=value-se, ymax=value+se,color=variable), width=.1)+geom_point()+
  ylab("k [yr-1]")+ggtitle("Annual Decay Rate for Leaf Litter\n[Ordered by Decreasing Cocoa Density]")+
  geom_hline(yintercept=1,linetype="dashed",color="grey")+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())+scale_y_continuous(labels = fmt_dcimals(2))+theme(
    text = element_text(size=18)) +  theme(
      legend.position="top"
      ,legend.title=element_blank()
      ,plot.background = element_blank()
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()
      ,panel.border = element_blank()
      ,panel.background = element_blank()
      ,legend.key = element_blank()) + theme(axis.line = element_line(color = 'black')
      )

g1<-grid.arrange(p2, p1, nrow = 2)
ggsave(paste0(getwd(),"/FineLitterFall/ComparisonDCMP_bycocoadensity.pdf"),g1,height=8,width=10)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/Nutrients/NCycling_fig4.pdf",g1,height=8,width=10)



