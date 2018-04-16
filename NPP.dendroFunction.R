# Written by: C?cile Girardin September 2014 (modified by Alex Morel November 2015)

## Calculation of large tree NPP from dendrometer data

# dendrometers measured every 3 months
# assumes all trees and lianas over 10 cm measured at 1.3 meters
# generate a dbh related growth rate from dendrometers to relate to census
# generate separate demographic function to account for mortality from the census


library(gdata)
library(lubridate)
library(ggplot2)
library(scales)
library(plyr)
library(reshape)
library(tidyverse)

#run allometric equation
source("/Volumes/ELDS/ECOLIMITS/R_codes/HelperFunctions/allometricEquations.R")
#run census function
#source("/Volumes/ELDS/ECOLIMITS/R_codes/Kakum.datacleaning/NPP.censusFunction.R")

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/")
site="Kakum"

#load plotdata
plts<-read.csv(paste0(getwd(),"/",site,"/plots.csv"))
trans=c("HM","KA")
#load wood density values
#load wood density values
w.dens<-read.xls("/Volumes/ELDS/ECOLIMITS/R_codes/GlobalWoodDensityDatabase.xls",sheet="Data")
w.dens<-data_frame(family=as.character(w.dens$Family),species=as.character(w.dens$Binomial),wood_density_g_m2=as.numeric(w.dens$Wood.density..g.cm.3...oven.dry.mass.fresh.volume),Region=as.character(w.dens$Region))
w.dens$genus <- str_split_fixed(w.dens$species, " ",2)[,1]
#load cocoa height relationship
#c.ht<-read.csv(paste0(getwd(),"/",site,"/AGB/ForestPlots/cocoa_height.csv"))
#load per transect height relationships
#f.ht<-read.csv(paste0(getwd(),"/",site,"/AGB/ForestPlots/forest_heights.csv"))

#load census dates
cendates<-read.csv(paste0(getwd(),"/",site,"/AGB/ForestPlots/census.dates.csv"))

for(p in 1:length(trans)){
  #h<-read.csv(paste0(getwd(),"/",site,"/NPP/Dendrometers/",trans[p],"_treeheights.csv"))
  
  dendrometer <- data.frame(lapply(read.csv(paste0(getwd(),"/",site,"/NPP/Dendrometers/",trans[p],"_dendroAll_clean.csv")), as.character),stringsAsFactors=F)
  dendrometer<-dendrometer %>% select(-X.1,-X)
  dendrometer[,5:14]<-sapply(dendrometer[,5:14],as.numeric)
  dendrometer$wood_density_g_m2<-as.numeric(dendrometer$wood_density_g_m2)
  
  #change of tree_tag
  #dendrometer$tree_id<-paste0(gsub(" ","",dendrometer$plotname),dendrometer$tree_tag)
  
  x1<-as.character(unique(dendrometer$plotname))
  
  sf.output<-list()
  for(k in 1:length(x1)){

    # adjust options:
    plotname = x1[k]
    # trans="HM"
    plotcode<-as.character(plts[plts$name3==plotname,"PlotCode"])
    #plotsize in ha
    if(length(grep("FP",plotname))==1) plotsize<-1.0 else plotsize<-0.36
    
    splotsize<-0.04
    plotit=T
    cocoa_density<-0.34
    
    #H<-h[h$plotname==plotname,]
    #H$TagNo<-paste0(gsub(" ","",H$plotname),H$TagNo)
    
    ## get data for all trees that are in the plot selected from census & dendrometer files
    #cen1  <- subset(census, plot_code==plotname)
    #cen   <- subset(census, year==census_year) 
    #cen   <- census
    dend1 <- dendrometer[dendrometer$plotname==plotname,]
    
    dend1$cocoa<-0
    dend1[dend1$genus=="Theobroma","cocoa"]<-1
    #dend1[dend1$tree_tag==paste0(gsub(" ","",plotname),"NoTag"),"cocoa"]<-"cocoa"
    #dend1<-dend1[!is.na(dend1$cocoa),]
    # re-name year, month, day in cen
    #cen$cenyear  <- cen$year
    #cen$cenmonth <- cen$month
    #cen$cenday   <- cen$day
    
    
    # requires two .csv files: 
    if(length(grep("FP",plotname))==0) census<- data.frame(lapply(read.csv(paste0(getwd(),"/",site,"/AGB/ForestPlots/",gsub(" ","",plotname),"_LS.csv")),as.character),stringsAsFactors=F) else census<-read.csv(paste0(getwd(),"/",site,"/AGB/ForestPlots/",trans[p],"_forest.csv"))
    #colnames(census)<-c("Tree.ID","Pv..Tag.No","TagNo","T1","T2","X","Y", "Family","Species", "Tree.Notes","density","WD.Type","X.1","dbh", "DBH1", "DBH2","DBH3","DBH4","POM","F1", "F2","F3","F4", "LI","CI","height_m","F5","Census.Notes","year","month","day")         
    colnames(census)<-c("x","subplot","x.coord","y.coord","tree_tag","orig.family","orig.species","family","species","dbh1","pom","dbh2","dbh3","height","f1","f2","f3","f4","f5","f1.1","notes")
   
    #make tree_tag specific for plot
    #census$tree_tag<-paste0(gsub(" ","",plotname),census$tree_tag)
    
    cdate<-data.frame(cendates[cendates$plotname==plotname,],stringsAsFactors = F)
    cdate$date<-as.Date(cdate$date,format="%Y-%m-%d")
    for(n in 1:nrow(cdate)){
      cdate$census[n]<-paste0("census",n)
    }
    
    #put all census measures into one dbh measure
    dbh<-list()
    tmp<-census[,grep("dbh",colnames(census))]
    for(c in 1:ncol(tmp)){
      if(c==ncol(tmp)) dbh[[c]]<-cbind(as.character(census$tree_tag),as.character(census$species),tmp[,c],census$height,year(cdate[cdate$census==paste0("census",c),"date"]),month(cdate[cdate$census==paste0("census",c),"date"]),day(cdate[cdate$census==paste0("census",c),"date"]),as.character(census$f1)) else 
        dbh[[c]]<-cbind(as.character(census$tree_tag),as.character(census$species),tmp[,c],census$height,year(cdate[cdate$census==paste0("census",c),"date"]),month(cdate[cdate$census==paste0("census",c),"date"]),day(cdate[cdate$census==paste0("census",c),"date"]),as.character(census$f1.1))
    }
    census1<-do.call(rbind.data.frame,dbh)
    colnames(census1)<-c("tree_tag","species","dbh","height_m","year","month","day","life.status")
    census1<-data.frame(sapply(census1,as.character),stringsAsFactors = F)
    census1$plotname<-plotname
    census1$dbh<-as.numeric(as.character(census1$dbh))
    census1$life.status[census1$life.status!=0]<-1
    
    #add cocoa column
    census1[census1$species!="Theobroma cacao"&!is.na(census1$species),"cocoa"]<-0
    census1[census1$species=="Theobroma cacao"&!is.na(census1$species),"cocoa"]<-1
    census1[is.na(census1$species),"cocoa"]<-0
   
    #add tree_height
    census1$height_m<-as.numeric(as.character(census1$height_m))
    #census1$height_m<-H[match(census1$tree_tag,H$TagNo),"height_m"]
    
    #match wood density for species
    wds<-left_join(census %>% select(family,species),w.dens %>% filter(Region=="Africa (tropical)") %>% select(-family),by="species")
    wdens<-wds %>% group_by(family,species) %>% summarise(wood_density_g_m2=mean(wood_density_g_m2,na.rm=T)) %>% ungroup()
    #pull out genera from missing species
    wdens$genus <- str_split_fixed(wdens$species," ",2)[,1]
    wdg<-left_join(wdens %>% select(genus),w.dens %>% select(family,genus,wood_density_g_m2),by="genus")
    wdg <- wdg %>% group_by(genus) %>% summarise(gen.wood_density = mean(wood_density_g_m2,na.rm=T))
    wdens <- left_join(wdens,wdg,by="genus")
    wdens <- wdens %>% mutate(wood_density_g_m2=replace(wood_density_g_m2,is.na(wood_density_g_m2),gen.wood_density[is.na(wood_density_g_m2)]))
    #pull out missing family
    wdf <- wdens %>% filter(is.na(wood_density_g_m2))
    wdf <- left_join(wdf %>% select(family),w.dens %>% select(family,wood_density_g_m2),by="family")
    wdf <- wdf %>% group_by(family) %>% summarise(fam.wood_density = mean(wood_density_g_m2,na.rm=T)) %>% ungroup()
    wdens <- left_join(wdens,wdf,by="family")
    wdens <- wdens %>% mutate(wood_density_g_m2=replace(wood_density_g_m2,is.na(wood_density_g_m2),fam.wood_density[is.na(wood_density_g_m2)])) %>%
      select(species,wood_density_g_m2)
    
    census1<-left_join(census1,wdens,by="species")
    
    #census<-data.frame(cbind(as.numeric(census$T1),as.numeric(census$Tag),as.numeric(census$cocoa),as.numeric(census$density),as.numeric(census$DBH0),as.numeric(census$THeight),as.numeric(census$year),as.numeric(census$month),as.numeric(census$day)))
    #colnames(census)<-c("Subplot","TagNo","cocoa","density","dbh","height_m","year","month","day")
    census_year=year(cdate[cdate$census=="census1","date"])
    allometric_option=5
    height_correction_option=1
    #is DBH in mm? (if yes assign 10 to factor)
    cf=10
    #NPPacw_dendro <- function(census, dendrometer, plotname, allometric_option="Default", height_correction_option="Default", census_year) {
    census1$dbh<-as.numeric(census1$dbh)/cf
    
      # load libraries
      library(sqldf)
    
    
    ## get the data you need from the census file into the dendrometer data frame: density, height, first dbh measurement, date of first dbh measurement
    #dend <- sqldf("SELECT dend1.*, cen.density, cen.height_m, cen.dbh, cen.cenyear, cen.cenmonth, cen.cenday FROM cen JOIN dend1 ON cen.TagNo = dend1.tree_tag")
    #keep calculated dbh from dendrometer file
    Hwdens <- sqldf("SELECT census.*, wdens.WD FROM wdens JOIN census ON wdens.species = census.species")
    #Hwdens$TagNo<-paste0(gsub(" ","",Hwdens$plotname),Hwdens$TagNo)
    
    #add generated heights
    dend1$height_m<-Hwdens[match(dend1$tree_tag,Hwdens$tree_tag),"height_m"]
    dend1$WD<-Hwdens[match(dend1$tree_tag,Hwdens$tree_tag),"WD"]
    
    #dend<-sqldf("SELECT dend1.*, height_m, WD, cocoa FROM Hwdens JOIN dend1 ON Hwdens.TagNo = tree_tag")
    #write.csv(dend, file="dendtest.csv")  
    #dend1$cenyear<-census_year
    #dend1$cenmonth<-month(cdate[cdate$census=="census1","date"])
    #dend1$cenday<-day(cdate[cdate$census=="census1","date"])
    
    
      ## Allometric equation option. Set of allometric equations after Chave et al. 2005 and Chave et al. 2014 are defined in allometricEquations.R. Options defined here:
      if (allometric_option == 2 | allometric_option == "dry") {
        allometrix <- 2
        print("dry equation  is used for estimating AGB, model I.3 (see Chave et al., 2005)")
      } else if (allometric_option == 3 | allometric_option == "moist" | allometric_option == "Default" | allometric_option == 1) {
        allometrix <- 3
        print("moist equation  is used for estimating AGB, model I.6 (see Chave et al., 2005)")
      } else if (allometric_option == 4 | allometric_option == "wet") {
        allometrix <- 4
        print("wet equation  is used for estimating AGB, model I.3 (see Chave et al., 2005)")
      } else if (allometric_option == 5 | allometric_option == "Chave2014") {
        allometrix <- 5
        print("pantropical equation is used for estimating AGB, model (4) (see Chave et al., 2014)")
      } else {
        print("Please specify a valid allometric_option!")
        return()
      }
      
    
    ## Height correction options
    if (height_correction_option == 1 | height_correction_option == "Default" ) {
      predheight <- 1
      print("If you have height for more than 50 trees in your plot, estimate local diameter-height relationship. If not, choose height correction option 2.")
    } else if (height_correction_option == 2) {
      predheight <- 2
      print("height correction estimated as described by Feldpauch et al. (2012). Please check Feldpauch regional parameters in the code. Default is Brazilian shield.")
    } else {
      print("Please specify a valid height_correction_option!")
      return()
    }
    
    
      # data cleaning
      dend1$dendrometer_reading_mm[which(dend1$dendrometer_reading_mm > 1000)] <- NaN
      
      # format dates
      dend1$dbh_first_date    <- as.Date(paste(census_year, month(cdate[cdate$census=="census1","date"]), day(cdate[cdate$census=="census1","date"]), sep="."), format="%Y.%m.%d") 
      dend1$date              <- as.Date(paste(dend1$year, dend1$month, dend1$day, sep="."), format="%Y.%m.%d") 
      
      # add first dbh measurement (cm) to dendrometer measurement (cm) = thisdbh
      dend1$dendrometer_reading_mm <- as.numeric(dend1$dendrometer_reading_mm) # Ignore error message. NA introduced by coercion is ok.
      dend1$thisdbh_cm             <- dend1$dbh + (dend1$dendrometer_reading_mm/10)/pi
      # Error estimates TO DO. Error estimated as diax1er <- (diax1*pi + er)/pi in matlab code. Where diax1 <- (diameterlA[tree_ind]*pi + dendroallA[[tree_ind]]/10)/pi
      
      #create relationship between dbh and growth rate for each monitoring period
      sf.eqn<- dend1 %>% select(dbh_first_date,date,dendrometer_reading_mm,dbh,cocoa) %>% group_by(date,cocoa) %>% filter(dbh_first_date!=date) %>%
        summarise(intercept=coefficients(lm(dendrometer_reading_mm/10/pi~dbh))[1],slope=coefficients(lm(dendrometer_reading_mm/10/pi~dbh))[2],r.squared=summary(lm(dendrometer_reading_mm/10/pi~dbh))$adj.r.squared)
      
      #write.csv(sf.eqn,"/users/alex/Documents/Research/Africa/ECOLIMITS/Codes/Kakum.datacleaning/scaling.fcn.test.csv")
      #estimate increase in stem biomass using above relationship
      
    
      # estimate biomass of each tree for each new thisdbh_mm
      #loop through each tree to estimate biomass (bm) and convert to above ground carbon (agC)
    for (ii in 1:length(dend1$tree_tag)) {  
      thistree <- which(dend1$tree_tag == dend1$tree_tag[ii] & dend1$year == dend1$year[ii] & dend1$month == dend1$month[ii] & dend1$day == dend1$day[ii])     
      dbh_tree <- dend1$thisdbh_cm[thistree]
      dbh_orig <- dend1$dbh[thistree]/cf*pi
      den_tree <- dend1$WD[thistree]
      h_tree   <- dend1$height_m[thistree]
      
      # this uses allometric equations from allometricEquations.R
      
      if (allometrix == 2) {
        bm <- Chave2005_dry(diax=dbh_tree, density=den_tree, height=h_tree)
        bm1<- Chave2005_dry(diax=dbh_orig, density=den_tree, height=h_tree)
      } else if (allometrix == 3) {
        bm <- Chave2005_moist(diax=dbh_tree, density=den_tree, height=h_tree)
        bm1 <- Chave2005_moist(diax=dbh_orig, density=den_tree, height=h_tree)
      } else if (allometrix == 4) {
        bm <- Chave2005_wet(diax=dbh_tree, density=den_tree, height=h_tree)
        bm1 <- Chave2005_wet(diax=dbh_orig, density=den_tree, height=h_tree)
      } else if (allometrix == 5) {
        bm <- Chave2014(diax=dbh_tree, density=den_tree, height=h_tree)
        bm1 <- Chave2014(diax=dbh_orig, density=den_tree, height=h_tree)
      }
      
      ## TO DO ## error treatment remains to be done!
      
      # Unit conversions 
      
      dend1$agC[ii] <- (bm)*(1/(2.1097*1000)) # convert kg to Mg=1/1000=10 and convert to carbon = 47.8% (ADD REF!! Txx et al?)
      dend1$bm_kg[ii] <- (bm)
      dend1$agCdiff[ii] <-  dend1$agC[ii]-(bm1)*(1/(2.1097*1000))
    }
    
      #remove NAs from dend
      dend1<-dend1[!is.na(dend1$dendrometer_reading_mm),]
      
      # NPPacw per tree: substact bm(t) - bm(t+1) / (t)-(t+1)
      # 1. Tree above ground Carbon stock difference [Changed from dend$plot_code to dend$date]
      #I don't understand why we do this? the numbers seem to be meaningless
      #dend$agCdiff    <- ave(dend$agC, dend$plot_code, FUN = function(x) c(NA, diff(x)))
      #dend$agCdiff    <- ave(dend$agC, dend$tree_tag, FUN = function(x) c(NA, diff(x)))
    
      # 2. Date difference
    
    first_run = T
    for (ii in 1:length(dend1$tree_tag)) {  
      thistree  <- which(dend1$tree_tag == dend1$tree_tag[ii])
      #identify if dead
      if(length(thistree)<=1) next
      agC       <- dend1$agC[thistree]
      tag       <- dend1$tree_tag[thistree]
      agCdiff   <- dend1$agCdiff[thistree]
      year      <- dend1$year[thistree]
      month     <- dend1$month[thistree]
      day       <- dend1$day[thistree]
      plot_code <- dend1$subplot[thistree]
      cocoa     <- dend1$cocoa[thistree]
      datediff  <- rbind(0/0, data.frame(diff(as.matrix(dend1$date[thistree])))) #datediff <- data.frame(0/0, difftime(tail(dend$date[thistree], -1), head(dend$date[thistree], -1), units="days"))
      w         <- cbind (plotname,plot_code, tag, cocoa, year, month, day, agC, agCdiff, datediff)
        if (first_run) {
          npp_tree <- w
          first_run = F
        } else {
          npp_tree <- rbind (npp_tree, w)
        }
    }
    
    colnames(npp_tree) <- c("plotname","plot_code", "tag","cocoa", "year", "month","day", "agC", "agCdiff", "datediff")
      
      # 3. NPP: MgC per tree per day
    
      npp_tree$nppacw_tree_day  <- npp_tree$agCdiff/npp_tree$datediff
      
      # Dendrometer NPP: MgC per plot per year 
      www                        <- sqldf("SELECT plotname,plot_code,cocoa, year, month,day, AVG(nppacw_tree_day) FROM npp_tree GROUP BY plot_code,cocoa,year, month")
      colnames (www)             <- c("plotname","plot_code","cocoa", "year", "month","day","npp_avgtrees_day_dend")
      www$npp_avgtrees_day_dend  <- as.numeric(www$npp_avgtrees_day_dend)
      www$npp_avgtrees_month_dend <- www$npp_avgtrees_day_dend*29.6 
      www$npp_avgtrees_month_dend_sd <- as.numeric(www$npp_avgtrees_day_dend)/sqrt(9)*29.6 
      www$npp_avgtrees_yr_dend <- www$npp_avgtrees_month_dend*12
      www$date<-as.Date(paste(www$day,www$month,www$year,sep="-"),format="%d-%m-%Y")
    
      # scale dendrometer band data to the whole plot by applying a scaling factor 
      # scaling factor (sf) = annual NPPacw from dendrometers (~200 trees) / annual NPPacw from census (all trees)
      
      #remove trees < 10 cm dbh or 100 mm dbh
      census<-census[census$dbh>=10&!is.na(census$dbh),]
 
      # get nppacw_census value for this plot for each year
      no.yrs<-nrow(cdate)-1
      npp_cen.cocoa<-list()
      npp_cen.shade<-list()
      for(ii in 1:no.yrs){
        #add in census dates
        www[www$date>=cdate$date[ii]&www$date<cdate$date[ii+1],"census"]<-ii
        dend1[dend1$date>=cdate$date[ii]&dend1$date<cdate$date[ii+1],"census"]<-ii
        census.1<-census[census$year==year(cdate$date[ii])|census$year==year(cdate$date[ii+1]),]
        nppacw_cen<-NPPacw_census(census.1, plotname=plotname, allometric_option="Default", height_correction_option="Default", census1_year=year(cdate$date[ii]), census2_year=year(cdate$date[ii+1]))
        npp_cen.cocoa[[ii]]<-as.numeric(strsplit(nppacw_cen,"xx",2)[[1]][1])
        npp_cen.shade[[ii]]<-as.numeric(strsplit(nppacw_cen,"xx",2)[[1]][2])
      }
       
      xxx <- sqldf("SELECT plotname, cocoa, census, AVG(npp_avgtrees_yr_dend) from www GROUP BY cocoa, census")
      colnames(xxx) <- c("plot_code", "cocoa","census", "nppacw_dend")
      #remove non-census data
      xxx<-xxx[!is.na(xxx$census),]
      dend1<-dend1[!is.na(dend1$census),]
      www<-www[!is.na(www$census),]
      
      sf.coco<-list()
      sf.shade<-list()
      for(ii in 1:no.yrs){
        sf.coco[[ii]]  <- (xxx[xxx$cocoa==1&xxx$census==ii,"nppacw_dend"]*length(which(dend1$cocoa==1&dend1$census==ii))) / as.numeric(npp_cen.cocoa[ii])
        sf.shade[[ii]] <- (xxx[xxx$cocoa==0&xxx$census==ii,"nppacw_dend"]*length(which(dend1$cocoa==0&dend1$census==ii))) / as.numeric(npp_cen.shade[ii])
      }
      
      #save scaling factor of dendrometers for two censuses
      tmp<-do.call(rbind.data.frame,sf.coco)
      tmp1<-do.call(rbind.data.frame,sf.shade)
      if(nrow(tmp)!=0) tmp<-cbind(tmp,tmp1) else tmp<-cbind(NA,tmp1)
      colnames(tmp)<-c("cocoa","shade")
      tmp$census<-1:no.yrs
      tmp$plot<-plotname
      sf.output[[k]]<-tmp
      
      for(ii in 1:no.yrs){
        if(length(sf.coco)>0) www[www$cocoa==1&www$census==ii,"nppacw_month.cocoa"] <- (www[www$cocoa==1&www$census==ii,"npp_avgtrees_month_dend"]*length(which(dend1$cocoa==1&dend1$census==ii)))/as.numeric(sf.coco[ii]) else www$nppacw_month.cocoa <- 0
        www[www$cocoa==0&www$census==ii,"nppacw_month.shade"] <- (www[www$cocoa==0&www$census==ii,"npp_avgtrees_month_dend"]*length(which(dend1$cocoa==0&dend1$census==ii)))/as.numeric(sf.shade[ii])
      }
      
      www<-www[!is.na(www$npp_avgtrees_day_dend),]
      
      # test that result is correct: the annual value should be the same as the annual value obtained from NPPacw_census_function_2014
      #yy1 <- (mean (www$nppacw_month.cocoa, na.rm=T))*12
      #yy2 <- (mean (www$nppacw_month.shade, na.rm=T))*12
      
      cocotrees<-sum(www[www$year==www[1,"year"]&www$month==www[1,"month"],"cocoa"])
      shtrees<-nrow(www[www$year==www[1,"year"]&www$month==www[1,"month"],])-sum(www[www$year==www[1,"year"]&www$month==www[1,"month"],"cocoa"])
      
      monthlynppacw             <-  ddply(www,.(year,month),summarise,npp.cocoa=mean(nppacw_month.cocoa,na.rm=T),npp.cocoa.se=sd(nppacw_month.cocoa,na.rm=T)/sqrt(cocotrees) ,npp.shade=mean(nppacw_month.shade,na.rm=T),npp.shade.se=sd(nppacw_month.shade,na.rm=T)/sqrt(shtrees))
      colnames(monthlynppacw)   <- c( "year", "month", "nppacw_month.cocoa", "nppacw_month.cocoa.se","nppacw_month.shade","nppacw_month.shade.se") 
      
      #monthlynppacw             <- data.frame(cbind(www$plot_code, www$year, www$month, www$nppacw_month.cocoa, www$nppacw_month.shade))
      #colnames(monthlynppacw)   <- c("plot_code", "year", "month", "nppacw_MgC_month.cocoa", "nppacw_MgC_month.shade") 
      
      
      
      #xxx <- sqldf("SELECT plotname,cocoa,year,month,day, AVG(npp_avgplot_month_dend),STDEV(npp_avgplot_month_dend) FROM www GROUP BY plotname,cocoa,year,month")
      #colnames(xxx) <- c("plotname","cocoa","year","month","day","nppacw_month","nppacw_month_sd"	)
      #xxx$nppacw_month<-as.numeric(xxx$nppacw_month)
      #xxx[xxx$cocoa==1,"nppacw_month_se"]<-as.numeric(xxx[xxx$cocoa==1,"nppacw_month_sd"])/sqrt(sum(spc[,2]))
      #xxx[xxx$cocoa==0,"nppacw_month_se"]<-as.numeric(xxx[xxx$cocoa==0,"nppacw_month_sd"])/sqrt(sum(spca[,2]))
      #xxx$d     <- as.character(paste(xxx$month, xxx$day, xxx$year, sep="/")) 
      #xxx$date  <- as.Date(xxx$d, "%m/%d/%Y")
      #xxx<-xxx[!is.na(xxx$nppacw_month),]
      
      write.csv(monthlynppacw,paste0(getwd(),"/",site,"/NPP/Dendrometers/DENDRO_",gsub(" ","",plotname),".csv"))
      
      monthlynppacw$date<-as.Date(paste("01",monthlynppacw$month,monthlynppacw$year,sep="/"),format="%d/%m/%Y")
      
      nppacw1<-cbind(monthlynppacw[1:nrow(monthlynppacw),1:4],monthlynppacw$date)
      colnames(nppacw1)<-c("year","month","monthly.npp","monthly.npp.se","date")
      nppacw1$cocoa<-1
      nppacw2<-cbind(monthlynppacw[1:nrow(monthlynppacw),1:2],monthlynppacw[1:nrow(monthlynppacw),5:7])
      colnames(nppacw2)<-c("year","month","monthly.npp","monthly.npp.se","date")
      nppacw2$cocoa<-0
      monthlynppacw<-rbind(nppacw1,nppacw2)
      ## Plotroutine, triggered by argument 'plotit=T'
      #if (plotit==T) {
        ## ggplot2 of root npp vs time
        #pdf(paste0(getwd(),"/",site,"/NPP/Dendrometers/DENDRO_",gsub(" ","",plotname),".pdf"))
        top <- max(rowSums(cbind(monthlynppacw$monthly.npp,monthlynppacw$monthly.npp.se),na.rm=T))
        bottom<-min(monthlynppacw$monthly.npp - monthlynppacw$monthly.npp.se,na.rm=T)
        ggplot(data=monthlynppacw, aes(x=date, y=monthly.npp,group=factor(cocoa))) + 
          geom_line(aes(linetype=factor(cocoa))) + geom_ribbon(data=monthlynppacw, aes(ymin=monthly.npp-monthly.npp.se, ymax=monthly.npp+monthly.npp.se), alpha=0.2) +
          scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%Y")) +            
          scale_colour_grey() + 
          theme(text = element_text(size=17), legend.title = element_text(colour = 'black', size = 17, hjust = 3, vjust = 7, face="plain")) +
          ylim(bottom, max(top, na.rm=T)) +                          
          xlab("") + ylab(expression(paste("Dendrometer (MgC ", ha^-1, mo^-1, ")", sep=""))) +
          theme_classic(base_size = 15, base_family = "") + 
          theme(legend.position="bottom")+ 
          theme(axis.text.x= element_text(angle=45)) +
          theme(panel.border = element_rect(fill = NA, colour = "black", size = 1))+
          ggtitle(paste0("Dendrometers for ",plotname))+ scale_linetype_discrete(name="Tree type",labels=c("Canopy","Cocoa"))
        #plot1
        ggsave(paste0(getwd(),"/",site,"/NPP/Dendrometers/DENDRO_",gsub(" ","",plotname),".pdf"))
      
      #find number of stems (cocoa vs canopy) per subplot, extrapolate subplot to 1 ha and then take average of subplot quarterly measures
      #spc<-cbind(cen[cen$cocoa==1,"Subplot"],ave(cen[cen$cocoa==1,],cen[cen$cocoa==1,"Subplot"],FUN=function(x) nrow(x)))
      #spc<-unique(spc[,1:2])
      #spca<-cbind(cen[cen$cocoa==0,"Subplot"],ave(cen[cen$cocoa==0,],cen[cen$cocoa==0,"Subplot"],FUN=function(x) nrow(x)))
      #spca<-unique(spca[,1:2])
      #www$npp_avgsplot_day_dend <- sp[match(www$plot_code,sp[,1]),2]*www$npp_avgtrees_day_dend
      #www[www$cocoa==1,"npp_avgsplot_month_dend"] <- spc[match(www[www$cocoa==1,"plot_code"],spc[,1]),2]*www[www$cocoa==1,"npp_avgtrees_month_dend"]
      #www[www$cocoa==0,"npp_avgsplot_month_dend"] <- spca[match(www[www$cocoa==0,"plot_code"],spca[,1]),2]*www[www$cocoa==0,"npp_avgtrees_month_dend"]
    #www$npp_avgplot_day_dend <- ave(www$npp_avgsplot_day_dend,www$month,FUN=function(x) mean(x,na.rm=T))/plotsize
      #www$npp_avgplot_day_dend_sd <- ave(www$npp_avgsplot_day_dend,www$month,FUN=function(x) sd(x,na.rm=T))/plotsize
      #www$npp_avgplot_month_dend <- www$npp_avgsplot_month_dend/splotsize
      #www$npp_avgplot_month_dend_sd<-
      #www$npp_avgplot_yr_dend <- www$npp_avgtrees_yr_dend*nrow(cen)
      
     
     
    
    #}
  }
  sf.output1<-do.call(rbind.data.frame,sf.output)
  write.csv(sf.output1,paste0(getwd(),"/",site,"/NPP/Dendrometers/DENDRO_correctionfactors.",trans[p],".csv"))
}


