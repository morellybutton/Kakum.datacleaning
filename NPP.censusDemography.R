##############################################################################################
### NPP mortality/recruitment code 
##############################################################################################
#--- NOTES ----------------------------------------------------------------------------------#
# ! EACH PLOT REQUIRES EXTENSIVE CHECKING TO ENSURE THE CALCULATIONS ARE WORKING CORRECTLY ! #
#DO:                                                  

library(tidyverse); library(lubridate)
source("/users/alex/Documents/Research/Africa/ECOLIMITS/Codes/Kakum.datacleaning/DendroFunctions.R"); 

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")

#trans=c("HM","KA")
#load plotdata
plts<-read.csv(paste0(getwd(),"/plots.csv"))
#plts<-read.csv("/users/alex/Documents/Research/Africa/ECOLIMITS/Data/Kakum/plots.csv")
wd_lookup<-read.csv(paste0(getwd(),"/AGB/ForestPlots/WoodDensity_lookup.csv"))
t_heights <- read_csv(paste0(getwd(),"/AGB/ForestPlots/HM_treeheights.csv"))
t_heights2 <- read_csv(paste0(getwd(),"/AGB/ForestPlots/KA_treeheights.csv"))
t_heights<-bind_rows(t_heights,t_heights2)

t_heights$dbh <- t_heights$dbh*10

t_heights <- t_heights %>% mutate(height=ifelse(height_m<80, height_m, NA))
t_heights <-  t_heights %>% mutate(species=Species)
t_heights$genus <- sapply(strsplit(t_heights$species," "), "[",1)
t_heights$family <- as.character(wd_lookup[match(t_heights$Species,wd_lookup$species),"family"])
t_heights$continent <- "Africa"

zanne <- read_csv("/users/alex/Documents/Research/Africa/ECOLIMITS/Codes/Kakum.datacleaning/GlobalWoodDensityDatabase.csv")

species_wd <- find_wd(t_heights)
species_wd <- species_wd %>% distinct()
species_wd <- species_wd %>% mutate(species=paste(genus, species))
t_heights <- left_join(t_heights, species_wd %>% select(species,wd), by=c("species"))

out_put<-list()
for(p in 1:nrow(plts)){
  #dendrometer <- read_csv(paste0(getwd(),"/NPP/Dendrometers/",trans[p],"_dendroAll_clean.csv"))
  #x1<-as.character(unique(dendrometer$plotname))
  
  #for(k in 1:length(x1)){
     
    # --- settings -------------------------------------------------------------------------------
    this_plot_name = as.character(plts$name3[p])
    this_plot_code = as.character(plts$PlotCode[p]); # "KAK-26"
    transect = as.character(plts$transect[p])
    if(transect=="Aboabo") trans="AB"
    if(transect=="Homaho") trans="HM"
    if(transect=="Kwameameobang") trans="KA"
    
    #in ha
    if(length(grep("FP",this_plot_name))==0) plot_size = 0.36 else plot_size = 1
    s_plot_size = 5*0.1*0.1
    #identify if need shade tree heights calculated (1=yes,0=no)
    if(length(grep("FP",this_plot_name))==0) t_hghts=0 else t_hghts=1
    

    # LOAD DATA ---------------------------------------------------------------------------------#
    if(length(grep("FP",this_plot_name))==0) { census_all<- read_csv(paste0(getwd(),"/AGB/ForestPlots/",gsub(" ","",this_plot_name),"_LS.csv"), na=c("NA", "NaN", ""));
    census_small<- read_csv(paste0(getwd(),"/AGB/ForestPlots/",gsub(" ","",this_plot_name),"_SS.csv"), na=c("NA", "NaN", ""))
    } else { 
      census_all<-read_csv(paste0(getwd(),"/AGB/ForestPlots/",trans,"_forest.csv"), na=c("NA", "NaN", ""));
      census_small<-read_csv(paste0(getwd(),"/AGB/ForestPlots/",trans,"_forestSS.csv"), na=c("NA", "NaN", ""))
      }
   
     #census_all  <- read_csv("data/Alex_Ghana/old/KAFP_census.csv", na=c("NA", "NaN", "")) # Census_Santarem_2014_2016.csv
    names(census_all) <- tolower(names(census_all))
    names(census_all) <- gsub(pattern=" ",replacement="_",names(census_all))
    census_all <- census_all  %>% mutate(tree_tag = as.character(tag))
    
    names(census_small) <- tolower(names(census_small))
    names(census_small) <- gsub(pattern=" ",replacement="_",names(census_small))
    census_small <- census_small  %>% mutate(tree_tag = as.character(tag))
    #census dates
    census_dates<-read_csv(paste0(getwd(),"/AGB/ForestPlots/Census.dates.csv"))
    census_dates <- census_dates %>% filter(plotname==this_plot_name)
    census_dates$no<-1:nrow(census_dates)
    spec(census_all) 
    cheights<-read_csv(paste0(getwd(),"/AGB/ForestPlots/cocoaheights.csv"))
    cheights<-cheights %>% mutate(height=THeight) %>% mutate(wd=0.42)
    
    # END LOAD DATA -----------------------------------------------------------------------------#
    
    #--------------------------------------------------------------------------------------------
    # PREPROCESS CENSUS DATA --------------------------------------------------------------------
    # Steps: 
    # 1) fix col names and dates
    # 2) Get species list and wood densities from Zanne, 
    # 3) Identify problem tree_tags
    census <- census_all #%>% filter(is.na(dbh)==F)
    census <- census %>% mutate(height=ifelse(theight<80, theight, NA))
    census <- census %>% mutate(family=nfam, species=nspecies)
    census$genus <- sapply(strsplit(census$species," "), "[",1)
    census$continent <- "Africa"
    
    census_small <- census_small %>% mutate(family=nfam, species=nspecies)
    census_small$genus <- sapply(strsplit(census_small$species," "), "[",1)
    census_small$continent <- "Africa"
    
    species_wd <- find_wd(census)
    species_wd <- species_wd %>% distinct()
    species_wd <- species_wd %>% mutate(species=paste(genus, species))
    census <- left_join(census %>% select(-nfam,-nspecies,-tag), species_wd %>% select(species,wd), by=c("species"))
    census$cocoa <-0
    census <- census %>% mutate(cocoa=replace(cocoa,genus=="Theobroma",1))
    
    census<-census %>% mutate(flag1.2=flag1.1,flag1.3=flag1) %>% select(-flag1.1,-flag1)
    
    species_wd <- find_wd(census_small)
    species_wd <- species_wd %>% distinct()
    species_wd <- species_wd %>% mutate(species=paste(genus, species))
    census_small <- left_join(census_small %>% select(-nfam,-nspecies,-tag), species_wd %>% select(species,wd), by=c("species"))
    census_small$cocoa <-0
    census_small <- census_small %>% mutate(cocoa=replace(cocoa,genus=="Theobroma",1))
    
    census_small<-census_small %>% mutate(flag1.2=flag1.1,flag1.3=flag1) %>% select(-flag1.1,-flag1)
    
     
    #--- Estimate heights ----------------------------------------------------------------
    #---- For Cocoa ---------------------
    log_fit <- lm(height~log(dbh), data=cheights %>% filter(is.na(wd)==F));
    lm_fit <- lm(height~dbh, data=cheights %>% filter(is.na(wd)==F))
    #nl_fit <- nls(height~ b0*((1/wd)^b1)*dbh^b2, data=cheights,
    #start = list(b0=2.643, b1=-0.3, b2=0.5))
    #bbmle::AICctab(log_fit, lm_fit, nl_fit)
    mod_list <- list(log_fit, lm_fit)
    best_mod <- AIC(log_fit, lm_fit)[,2] %>% which.min()
    
    tmp_cocoa <- census %>% filter(cocoa==1) %>% mutate(dbh=replace(dbh,is.na(dbh),dbh2[is.na(dbh)])) %>% 
      mutate(dbh=replace(dbh,is.na(dbh),dbh3[is.na(dbh)]))
    
    census[census$cocoa==1,"height_m"] <- predict(mod_list[[best_mod]], 
                                                  newdata=data.frame(wd=tmp_cocoa$wd, dbh=tmp_cocoa$dbh))
    
    census <- census %>% mutate(height_m=replace(height_m,!is.na(height),height[!is.na(height)]))
    
    tmp_cocoa_small <- census_small %>% filter(cocoa==1) %>% mutate(dbh=replace(dbh,is.na(dbh),dbh2[is.na(dbh)])) %>% 
      mutate(dbh=replace(dbh,is.na(dbh),dbh3[is.na(dbh)]))
    
    census_small[census_small$cocoa==1,"height_m"] <-  predict(mod_list[[best_mod]], 
                                                  newdata=data.frame(wd=tmp_cocoa_small$wd, dbh=tmp_cocoa_small$dbh))
    
    
    cmod_height <- mod_list[[best_mod]]
    rm(log_fit,lm_fit,mod_list,best_mod)
    
    #---- For Shade Trees ---------------------
    #if(t_hghts==1){
      log_fit <- lm(height_m~log(dbh), data=t_heights %>% filter(is.na(wd)==F));
      lm_fit <- lm(height_m~dbh, data=t_heights %>% filter(is.na(wd)==F))
      nl_fit <- nls(height~ b0*((1/wd)^b1)*dbh^b2, data=t_heights,
                    start = list(b0=2.643, b1=-0.3, b2=0.5))
      #bbmle::AICctab(log_fit, lm_fit, nl_fit)
      mod_list <- list(log_fit, lm_fit, nl_fit)
      best_mod <- AIC(log_fit, lm_fit, nl_fit) [,2] %>% which.min()
      
      tmp_census <- census %>% filter(cocoa==0)  %>% mutate(dbh=replace(dbh,is.na(dbh),dbh2[is.na(dbh)])) %>% 
        mutate(dbh=replace(dbh,is.na(dbh),dbh3[is.na(dbh)]))
      
      census[census$cocoa==0,"height_m"] <- predict(mod_list[[best_mod]], 
                                                    newdata=data.frame(wd=tmp_census$wd, dbh=tmp_census$dbh))
      tmp_census_small <- census_small %>% filter(cocoa==0)  %>% mutate(dbh=replace(dbh,is.na(dbh),dbh2[is.na(dbh)])) %>% 
        mutate(dbh=replace(dbh,is.na(dbh),dbh3[is.na(dbh)]))
      
      census_small[census_small$cocoa==0,"height_m"] <- predict(mod_list[[best_mod]], 
                                                    newdata=data.frame(wd=tmp_census_small$wd, dbh=tmp_census_small$dbh))
      
      
      mod_height <- mod_list[[best_mod]]
     census <- census %>% mutate(height_m=replace(height_m,!is.na(height),height[!is.na(height)]))
    
    #replace estimated heights with measured heights
    #census <- census %>% mutate(height_m=replace(height_m,!is.na(theight),theight[!is.na(theight)]))
    #--- END estimate heights ------------------------------------------------------------
    
  
    #----Estimate Proportion of Annual Increment from Census------#
    cen<- census %>% select(tree_tag,family,species,genus,dbh,cocoa,height_m,wd)
    cen$flag1<-"a"
    cen$date<-census_dates$date[1]
    
    for(xts in 2:nrow(census_dates)){
      tmp<-census %>% select(tree_tag,family,species,genus,cocoa,height_m,wd,paste0("dbh",xts),paste0("flag1.",xts))
      colnames(tmp)<-c(colnames(tmp[,1:7]),"dbh","flag1")
      tmp$date<-census_dates$date[xts]
      cen<-bind_rows(cen,tmp)
    }
    
    cen$bm <- Chave2014(diax=cen$dbh/10, wd=cen$wd, height=cen$height_m)
    cen$year<-year(cen$date)
    
    cen_s<- census_small %>% select(tree_tag,family,species,genus,dbh,cocoa,height_m,wd)
    cen_s$flag1<-"a"
    cen_s$date<-census_dates$date[1]
    
    for(xts in 2:nrow(census_dates)){
      tmp<-census_small %>% select(tree_tag,family,species,genus,cocoa,height_m,wd,paste0("dbh",xts),paste0("flag1.",xts))
      colnames(tmp)<-c(colnames(tmp[,1:7]),"dbh","flag1")
      tmp$date<-census_dates$date[xts]
      cen_s<-bind_rows(cen_s,tmp)
    }
    
    cen_s$bm <- Chave2014(diax=cen_s$dbh/10, wd=cen_s$wd, height=cen_s$height_m)
    cen_s$year<-year(cen_s$date)
    
      #calculate AGB growth increment for plot, from new recruits and mortality
    out_cen <- cen %>% group_by(year,cocoa) %>% filter(flag1!=0&!is.na(flag1)&dbh>100) %>%
      summarise(total_bm=sum(bm/plot_size,na.rm=T), total_agC=sum(bm,na.rm=T)*(1/(2.1097*1000))/plot_size)
    
    out_new <- cen %>% group_by(year,cocoa) %>% filter(flag1=="an"&!is.na(flag1)&dbh>100) %>%
      summarise(bm_new=sum(bm/plot_size,na.rm=T), agC_new=sum(bm,na.rm=T)*(1/(2.1097*1000))/plot_size)
    
    out_mort <- cen %>% group_by(year,cocoa) %>% filter(flag1==0&!is.na(flag1)&dbh>100) %>%
      summarise(bm_mort=sum(bm/plot_size,na.rm=T), agC_mort=sum(bm,na.rm=T)*(1/(2.1097*1000))/plot_size)
    
    out_cen_s <- cen_s %>% group_by(year,cocoa) %>% filter(flag1!=0&!is.na(flag1)) %>%
      summarise(total_bm_s=sum(bm/plot_size,na.rm=T), total_agC_s=sum(bm,na.rm=T)*(1/(2.1097*1000))/s_plot_size)
    
    out_new_s <- cen_s %>% group_by(year,cocoa) %>% filter(flag1=="an"&!is.na(flag1)) %>%
      summarise(bm_new_s=sum(bm/plot_size,na.rm=T), agC_new_s=sum(bm,na.rm=T)*(1/(2.1097*1000))/s_plot_size)
    
    out_mort_s <- cen_s %>% group_by(year,cocoa) %>% filter(flag1==0&!is.na(flag1)) %>%
      summarise(bm_mort_s=sum(bm/plot_size,na.rm=T), agC_mort_s=sum(bm,na.rm=T)*(1/(2.1097*1000))/s_plot_size)
    
    out_cen <- left_join(out_cen,out_new, by=c("year","cocoa"))
    out_cen <- left_join(out_cen,out_mort, by=c("year","cocoa"))
    out_cen <- left_join(out_cen,out_cen_s, by=c("year","cocoa"))
    out_cen <- left_join(out_cen,out_new_s, by=c("year","cocoa"))
    out_cen <- left_join(out_cen,out_mort_s, by=c("year","cocoa"))
    
    out_cen$growth<-NA
    out_cen$growth_agC<-NA
    
    #calculate growth increment per year
    meas<-unique(cen$year)
    for(xts in 2:length(meas)){
      tmp_cen <- cen %>% filter(year==meas[xts-1]|year==meas[xts]&flag1!=0&dbh>100)
      tag.trees <- tmp_cen %>% filter(year==meas[xts]) %>% select(tree_tag) %>% unique()
      tag.trees$present<-1
      tmp_cen <- left_join(tmp_cen,tag.trees, by="tree_tag") %>% filter(present==1)
      tmp_cen_s <- cen_s %>% filter(year==meas[xts-1]|year==meas[xts]&flag1!=0)
      
      #bm1 <- tmp_cen %>% filter(year==meas[xts-1]) %>% group_by(cocoa) %>% summarise(total=sum(bm,na.rm=T)/plot_size,no.trees=length(bm))
      #bm1_s <- tmp_cen_s %>% filter(year==meas[xts-1]) %>% group_by(cocoa) %>% summarise(total=sum(bm,na.rm=T)/s_plot_size,no.trees=length(bm))
      #bm2 <- tmp_cen %>% filter(year==meas[xts]) %>% group_by(cocoa) %>% summarise(total=sum(bm,na.rm=T)/plot_size,no.trees=length(bm)) 
      #bm2_s <- tmp_cen_s %>% filter(year==meas[xts]) %>% group_by(cocoa) %>% summarise(total=sum(bm,na.rm=T)/s_plot_size,no.trees=length(bm)) 
      
      bm_1 <- tmp_cen %>% filter(year==meas[xts-1]) %>% select(tree_tag,bm,flag1,cocoa)
      bm_1_s <- tmp_cen_s %>% filter(year==meas[xts-1])  %>% select(tree_tag,bm,flag1,cocoa)
      bm_2 <- tmp_cen %>% filter(year==meas[xts]) %>% select(tree_tag,bm,flag1,cocoa)
      bm_2_s <- tmp_cen_s %>% filter(year==meas[xts]) %>% select(tree_tag,bm,flag1,cocoa)
      
      bm_l <- left_join(bm_1,bm_2,by=c("tree_tag","cocoa"))
      bm_l <- bm_l %>% mutate(tree_diff=bm.y-bm.x)
      bm_s <- left_join(bm_1_s,bm_2_s,by=c("tree_tag","cocoa"))
      bm_s <- bm_s %>% mutate(tree_diff=bm.y-bm.x)
      
      daydiff <- (tmp_cen %>% filter(year==meas[xts]) %>% select(date) %>% unique())-(tmp_cen %>% filter(year==meas[xts-1]) %>% select(date) %>% unique())
      
      #for shade trees
      shade_l <- bm_l %>% filter(cocoa==0) %>% summarise(total=sum(tree_diff,na.rm=T))
      shade_s <- bm_s %>% filter(cocoa==0) %>% summarise(total=sum(tree_diff,na.rm=T))
      
      growth <- sum(shade_l,shade_s,na.rm=T)/as.numeric(daydiff)*365
      growth_agC <- growth*(1/(2.1097*1000))
      
      cocoa_l <- bm_l %>% filter(cocoa==1) %>% summarise(total=sum(tree_diff,na.rm=T))
      cocoa_s <- bm_s %>% filter(cocoa==1) %>% summarise(total=sum(tree_diff,na.rm=T))
      growth_c <- sum(cocoa_l+cocoa_s)/as.numeric(daydiff)*365
      growth_c_agC <-growth_c*(1/(2.1097*1000))
      
      out_cen$growth[out_cen$cocoa==0&out_cen$year==meas[xts]]<-growth
      out_cen$growth[out_cen$cocoa==1&out_cen$year==meas[xts]]<-growth_c
      out_cen$growth_agC[out_cen$cocoa==0&out_cen$year==meas[xts]]<-growth_agC
      out_cen$growth_agC[out_cen$cocoa==1&out_cen$year==meas[xts]]<-growth_c_agC
      }
    #save census values
    write_csv(out_cen,path = paste0(getwd(),"/NPP/Dendrometers/census_demog_npp_",gsub(" ","",this_plot_name),".csv"))
  #------End Census Estimates-----#
  #}
    out_cen$plot<-this_plot_name
    out_put[[p]]<-out_cen
}
output<-do.call(rbind.data.frame,out_put)
write_csv(output,path = paste0(getwd(),"/NPP/Dendrometers/census_demog_npp_all.csv"))

