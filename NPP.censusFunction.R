##############################################################################################
### NPPdendrometers FOR Alex's Ghana plots. 
##############################################################################################
#--- NOTES ----------------------------------------------------------------------------------#
# ! EACH PLOT REQUIRES EXTENSIVE CHECKING TO ENSURE THE CALCULATIONS ARE WORKING CORRECTLY ! #
#DO:                                                  

library(tidyverse); library(lubridate)
source("/users/alex/Documents/Research/Africa/ECOLIMITS/Codes/Kakum.datacleaning/DendroFunctions.R"); 

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")

trans=c("HM","KA")
#load plotdata
plts<-read.csv(paste0(getwd(),"/plots.csv"))
#plts<-read.csv("/users/alex/Documents/Research/Africa/ECOLIMITS/Data/Kakum/plots.csv")
wd_lookup<-read.csv(paste0(getwd(),"/AGB/ForestPlots/WoodDensity_lookup.csv"))

for(p in 1:length(trans)){
  dendrometer <- read_csv(paste0(getwd(),"/NPP/Dendrometers/",trans[p],"_dendroAll_clean.csv"))
  x1<-as.character(unique(dendrometer$plotname))
  
  for(k in 1:length(x1)){
     
    # --- settings -------------------------------------------------------------------------------
    this_plot_name = x1[k]
    this_plot_code = as.character(plts$PlotCode[plts$name3==x1[k]]); # "KAK-26"
    
    #in ha
    if(length(grep("FP",this_plot_name))==0) plot_size = 0.36 else plot_size = 1
    s_plot_size = 5*0.1*0.1
    #identify if need shade tree heights calculated (1=yes,0=no)
    if(length(grep("FP",this_plot_name))==0) t_hghts=0 else t_hghts=1
    
    
    # LOAD DATA ---------------------------------------------------------------------------------#
    if(length(grep("FP",this_plot_name))==0) { census_all<- read_csv(paste0(getwd(),"/AGB/ForestPlots/",gsub(" ","",this_plot_name),"_LS.csv"), na=c("NA", "NaN", ""));
    census_small<- read_csv(paste0(getwd(),"/AGB/ForestPlots/",gsub(" ","",this_plot_name),"_SS.csv"), na=c("NA", "NaN", ""))
    } else { 
      census_all<-read_csv(paste0(getwd(),"/AGB/ForestPlots/",trans[p],"_forest.csv"), na=c("NA", "NaN", ""));
      census_small<-read_csv(paste0(getwd(),"/AGB/ForestPlots/",trans[p],"_forestSS.csv"), na=c("NA", "NaN", ""))}
   
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
    zanne <- read_csv("/users/alex/Documents/Research/Africa/ECOLIMITS/Codes/Kakum.datacleaning/GlobalWoodDensityDatabase.csv")
    spec(census_all) 
    cheights<-read_csv(paste0(getwd(),"/AGB/ForestPlots/cocoaheights.csv"))
    cheights<-cheights %>% mutate(height=THeight) %>% mutate(wd=0.42)
    t_heights <- read_csv(paste0(getwd(),"/AGB/ForestPlots/",trans[p],"_treeheights.csv"))
    t_heights$dbh <- t_heights$dbh*10
    
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
    
    t_heights <- t_heights %>% mutate(height=ifelse(height_m<80, height_m, NA))
    t_heights <-  t_heights %>% mutate(species=Species)
    t_heights$genus <- sapply(strsplit(t_heights$species," "), "[",1)
    t_heights$family <- as.character(wd_lookup[match(t_heights$Species,wd_lookup$species),"family"])
    t_heights$continent <- "Africa"
    
    species_wd <- find_wd(t_heights)
    species_wd <- species_wd %>% distinct()
    species_wd <- species_wd %>% mutate(species=paste(genus, species))
    t_heights <- left_join(t_heights, species_wd %>% select(species,wd), by=c("species"))
    
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
    
    
    #--- Organize the dendrometer record ---------------------------------------------------
    dat <- dendrometer %>% filter(plotname==this_plot_name)
    #dat <- dat %>% select(-X1, -X)
    dat$tree_tag <- as.character(dat$tree_tag)
    vec_tags <- dat %>% 
      group_by(tree_tag) %>% 
      summarize(u=mean(baseline_dbh_mm, na.rm=T)) %>% 
      arrange(desc(u)) %>% pull(tree_tag)
    dat <- dat %>% mutate(date=lubridate::parse_date_time(date, "ymd"))
    dat <- dat %>%  mutate(baseline_dbh = baseline_dbh_mm) %>% select(-baseline_dbh_mm)
    dat$current_dbh <- est_dbh_from_dendro(dbh = dat$baseline_dbh, 
                                           dendrometer_reading_mm = dat$dendrometer_reading_mm_cum) #!
    tmp <- census %>% select(tree_tag, species, wd)
    left_join(dat, tmp, by="tree_tag") %>% pull(wd) %>% hist #check WD distribution 
    dend <- left_join(dat, tmp, by="tree_tag")
    
    # how well do the ratios of species between dbands and census correspond?
    dend %>% pull(species.y) %>% table()
    census %>% pull(species) %>% table()
    
    ################################################################################################
    # --- begin new dband error catching
    ################################################################################################
    
    # pass1 <- dend %>% 
    #   group_by(tree_tag) %>% 
    #   arrange(date) %>% 
    #   mutate(delta1 = current_dbh-lag(current_dbh,order_by = date)) %>% 
    #   mutate(delta1_std = as.double(delta1/current_dbh)) %>%
    #   mutate(baseline_dbh = if_else(delta1_std < -0.0075, lag(current_dbh), as.double(baseline_dbh))) %>% 
    #   mutate(baseline_dbh = if_else(is.na(baseline_dbh)==T, lead(baseline_dbh), baseline_dbh)) %>% 
    #   mutate(baseline_dbh1 = cummax(baseline_dbh)) %>%
    #   mutate(current_dbh = est_dbh_from_dendro(dbh=baseline_dbh1, dendrometer_reading_mm = dendrometer_reading_mm)) 
    # 
    # pass2 <- pass1 %>% 
    #   group_by(tree_tag) %>% 
    #   arrange(date) %>% 
    #   mutate(delta1 = current_dbh-lag(current_dbh,order_by = date)) %>% 
    #   mutate(delta1_std = as.double(delta1/current_dbh)) %>%
    #   mutate(baseline_dbh = if_else(delta1_std < -0.0075, lag(current_dbh), as.double(baseline_dbh))) %>% 
    #   mutate(baseline_dbh = if_else(is.na(baseline_dbh)==T, lead(baseline_dbh), baseline_dbh)) %>% 
    #   mutate(baseline_dbh1 = cummax(baseline_dbh)) %>%
    #   mutate(current_dbh = est_dbh_from_dendro(dbh=baseline_dbh1, dendrometer_reading_mm = dendrometer_reading_mm)) 
    # 
    # dend <- pass2
    
    dend$thisdbh_cm <- dend$current_dbh/10
    dend$agC_Mg <- NA
    dend %>% names()
    if(t_hghts==1) {dend$height_pred <- predict(mod_height, 
                                                newdata=data.frame(dbh=dend$current_dbh, 
                                                                   wd=dend$wd), type="response")} else
                                                                     dend$height_pred <- census[match(dend$tree_tag,census$tree_tag),"height_m"] %>% pull(height_m)
    #rm(pass1, pass2)
    ################################################################################################
    # --- END new shit
    ################################################################################################
    
    # estimate biomass of each tree for each new thisdbh_mm
    # loop through each tree to estimate biomass (bm) and convert to above ground carbon (agC)
    dend$bm <- Chave2014(diax=dend$current_dbh/10, wd=dend$wd, height=dend$height_pred)
    
    # Unit conversions 
    dend$agC_Mg <- (dend$bm)*(1/(2.1097*1000)) # convert kg to Mg=1/1000=10 and convert to carbon = 47.8% (ADD REF!! Txx et al?)
    
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
   
     #add if has dendrometer
    dend_tags<-dend %>% select(tree_tag) %>% unique()
    dend_tags$dend<-1
    cen <- left_join(cen,dend_tags,by="tree_tag")
    cen <- cen %>% mutate(dend=replace(dend,is.na(dend),0))

    #calculate proportion of AGB represented by dendrometers
    out_cen <- cen %>% group_by(year,cocoa) %>% filter(flag1!=0&!is.na(flag1)&dbh>100) %>%
      summarise(total_bm=sum(bm/plot_size,na.rm=T), total_agC=sum(bm,na.rm=T)*(1/(2.1097*1000))/plot_size)
    
    out_den <- cen %>% group_by(year,cocoa,dend) %>% filter(flag1!=0&!is.na(flag1)&dend==1&dbh>100)%>%
      summarise(dend_bm=sum(bm/plot_size,na.rm=T), dend_agC=sum(bm,na.rm=T)*(1/(2.1097*1000))/plot_size)
     
     out_cen_s <- cen_s %>% group_by(year,cocoa) %>% filter(flag1!=0&!is.na(flag1)) %>%
      summarise(total_bm_s=sum(bm/plot_size,na.rm=T), total_agC_s=sum(bm,na.rm=T)*(1/(2.1097*1000))/s_plot_size)
     
    out_cen<-left_join(out_cen,out_den %>% select(-dend), by=c("year","cocoa"))
    out_cen<- out_cen %>% mutate(prop_dend=dend_bm/total_bm)
    out_cen <- left_join(out_cen,out_cen_s, by=c("year","cocoa"))
    
    out_cen$growth<-NA
    out_cen$growth_agC<-NA
    out_cen$prop_growth<- NA
    
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
      
      #proportion dendrometer
      bm1_dend <- tmp_cen %>% filter(year==meas[xts-1]&dend==1) %>% select(tree_tag,bm,flag1,cocoa)
      bm2_dend <- tmp_cen %>% filter(year==meas[xts]&dend==1) %>% select(tree_tag,bm,flag1,cocoa)
      
      bm_dend <- left_join(bm1_dend,bm2_dend,by=c("tree_tag","cocoa"))
      bm_dend <- bm_dend %>% mutate(tree_diff=bm.y-bm.x)
      
      prop_dend <- (sum(bm_dend %>% filter(cocoa==0) %>% pull(tree_diff),na.rm=T))/as.numeric(daydiff)*365/plot_size/growth
      prop_dend_c <- (sum(bm_dend %>% filter(cocoa==1) %>% pull(tree_diff),na.rm=T))/as.numeric(daydiff)*365/plot_size/growth_c
      out_cen$prop_growth[out_cen$cocoa==0&out_cen$year==meas[xts]]<-prop_dend
      out_cen$prop_growth[out_cen$cocoa==1&out_cen$year==meas[xts]]<-prop_dend_c
    }
    #save census values
    write_csv(out_cen,path = paste0(getwd(),"/NPP/Dendrometers/census_npp_",gsub(" ","",this_plot_name),".csv"))
  #------End Census Estimates-----#
  }
}

