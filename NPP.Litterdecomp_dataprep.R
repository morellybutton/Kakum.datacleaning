### Function fine litter fall: 
# This function uses data to prepare fine litterfall for decomposition estimates by keeping each litter trap separate
# take sum of annual litter fall and divide by average of quarterly measures.

## Read-in data:
setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/NPP/")

# load libraries
library(scales)
library(zoo)
require(ggplot2)
library(gridExtra)

# this is what we have in db:
#names(data.flf) <- c("plot_code", "year","month", "day","litterfall_trap_num", "litterfall_trap_size_m2","leaves_g_per_trap","twigs_g_per_trap","flowers_g_per_trap","fruits_g_per_trap",
# "bromeliads_g_per_trap", "epiphytes_g_per_trap","other_g_per_trap", "palm_leaves_g", "palm_flower_g", "palm_fruit_g", "quality_code", "comments")

# to test this code, you can define the following parameters
plotsize = 0.36  ### VARIABLE PLOTSIZE IS NOT YET INCLUDED: DISCUSS HOW TO INCLUDE IT.
plotnames = c("HM FP","KA FP","KA 100 F3","KA 100 F1","KA 500 F3","KA 1K F3","HM 5K F2","HM 500 F3","HM 100 F3","HM 500 F2")
num.lf<-9
year1=2014
year2=2017

for(p in 1:length(plotnames)){
  #i=i+1
  plotname<-plotnames[p]
  data.flf <- read.table(paste0(getwd(),"/Litter Data/FLF_",gsub(" ","",plotname),"_",year1,"_",year2,".csv"), sep=",", header=T)
  
  #plotname = "KA FP"
  #str(data.flf)
  
  #flf <- function(data.flf, plotname, ret="monthly.means.ts", plotit=T) {   # plotsize=1                                                                                     
    
    # define each parameter
    plotfA = data.flf$plot_code  
    yearfA = data.flf$year[which(plotname==plotfA)]
    monthfA = data.flf$month[which(plotname==plotfA)]
    pointfA = data.flf$litterfall_trap_num[which(plotname==plotfA)]
    leaffA = data.flf$leaves_g_per_trap[which(plotname==plotfA)]
    leaffcA = data.flf$crop_leaves_g_per_trap[which(plotname==plotfA)]
    branchfA = data.flf$twigs_g_per_trap[which(plotname==plotfA)]
    flowerfA = data.flf$flowers_g_per_trap[which(plotname==plotfA)]
    fruitfA = data.flf$fruits_g_per_trap[which(plotname==plotfA)]
    seedsfA <- data.flf$seeds[which(plotname==plotfA)]
    BromfA <- NA #data.flf$bromeliads_g_per_trap[which(plotname==plotfA)]
    EpiphfA <- NA #data.flf$epiphytes_g_per_trap[which(plotname==plotfA)]
    otherfA = data.flf$other_g_per_trap[which(plotname==plotfA)]
    
    ### TO DO: sanity check of the inputs.
    
    ### Calculates total litterfall (sum of branches, leaves, flowers, fruits, seeds, Broms, Epiphs, other...):
    totalfA <- NULL
    for (i in 1:length(yearfA)) {
      totalfA[i] = sum(leaffA[i], leaffcA[i], branchfA[i], flowerfA[i], fruitfA[i], seedsfA[i], BromfA[i], EpiphfA[i], otherfA[i], na.rm=T)
    }
    
    totalfA[which(totalfA>300)] <- NA   # remove outliers with totalf > 300
    totalfA[which(totalfA<0)]   <- NA   # remove implausible totallf (negative litter)
    
    # Calculate leaf area ****need density from photos, we assume average SLA = 100g/m2
    # leaflaifA = leaffA/100   # convert to area 
    fir_mon = 1
    fir_mone = 12
    fir_year = min(yearfA, na.rm=T)
    fir_yeare = max(yearfA, na.rm=T)
    
    #calculate each litter trap separately
    flf.data.monthly<-list()
    
    for(k in 1:num.lf){
      n=1
      
      # initialize variables for the loop:
      totflfAs <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
      totflfAsstd <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
      totflfAslen <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
      # leaflaifAs <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
      seedsfAs <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
      seedsfAsstd <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
      leafflfAs <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
      leafflfAsstd <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
      leafflfcAs <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
      leafflfcAsstd <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
      fruitflfAs <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
      fruitflfAsstd <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
      flowerflfAs <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
      flowerflfAsstd <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
      branchflfAs <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
      branchflfAsstd <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
      
      # TO DO: CHANGE THIS TO DATES RATHER THAN twice a month!!!!!
      # Get date of previous collection and divide by number of days. 
      # What happens with broken traps?
      # Add quality check here e.g. nothing >50
      # For the first collection interval, assume 14 days.
      
      # Conversions
      # Raw data is in g / litter trap = g / 0.25m2
      # Convert to ha: *(10000/0.25)
      # Convert to Mg: *1 g = 1.0 ?? 10-6 = 0.000001 Mg
      # Convert to C: *0.49
      
      # multiply small plot by 400 = 10,000/25*(0.5*0.5)m2 = 1600
      # convert to MgC ha month # multiply by 2 because collected twice a month (see comments below on how to change this to daily)
      
      la = (10000/0.25)*2
      
      ## calculate monthly means in each year:
      for (j in fir_year:fir_yeare) {
        m=1
        for (i in fir_mon:12) {
          ind = which(monthfA==i & yearfA==j & pointfA==k)
          
          # multiply by 2 because collected twice a month
          totflfAs[m,n] = mean(totalfA[ind],na.rm=T)*(la/(2.032*1000000)) # Mg/ha convert to carbon divide by 2
          totflfAsstd[m,n] = (sd(totalfA[ind],na.rm=T)*(la/(2.032*1000000)))/sqrt(num.lf)  # should be sqrt(length(totalfA))
          
          # Terhi suggests calculating per litter trap and getting SE per litter trap:
          # Calculate MgC/ trap/ collection interval
          # loop per litter trap per. 
          # avg1 <- monthly average per trap over several years = average ((1st collection/ collection interval) , (2st collection/ collection interval)) * number of days in that month
          # avg2 <- monthly average per plot by averaging avg1.
          # se_avg1 <- sd(avg1)/sqrt(length(avg1)).
          # this would provide a unit per day. g / m2/ day.
          
          seedsfAs[m,n] = mean(seedsfA[ind],na.rm=T)*(la/(2.032*1000000))
          seedsfAsstd[m,n] = sd(seedsfA[ind],na.rm=T)*(la/(2.032*1000000))/sqrt(num.lf) 
          
          leafflfAs[m,n] = mean(leaffA[ind],na.rm=T)*(la/(2.032*1000000))
          leafflfAsstd[m,n] = sd(leaffA[ind],na.rm=T)*(la/(2.032*1000000))/sqrt(num.lf)
          
          leafflfcAs[m,n] = mean(leaffcA[ind],na.rm=T)*(la/(2.032*1000000))
          leafflfcAsstd[m,n] = sd(leaffcA[ind],na.rm=T)*(la/(2.032*1000000))/sqrt(num.lf)
          
          fruitflfAs[m,n] = mean(fruitfA[ind],na.rm=T)*(la/(2.032*1000000))
          fruitflfAsstd[m,n] = (sd(fruitfA[ind],na.rm=T)*(la/(2.032*1000000)))/sqrt(num.lf)
          
          flowerflfAs[m,n] = mean(flowerfA[ind],na.rm=T)*(la/(2.032*1000000))
          flowerflfAsstd[m,n] = (sd(flowerfA[ind])*(la/(2.032*1000000)))/sqrt(num.lf)
          
          branchflfAs[m,n] = mean(branchfA[ind],na.rm=T)*(la/(2.032*1000000))
          branchflfAsstd[m,n] = (sd(branchfA[ind],na.rm=T)*(la/(2.032*1000000)))/sqrt(num.lf)
          
          # Add a column for reproductive material
          
          #pointfAa = pointfA[ind]      
          m=m+1;
        }
        n=n+1;
      }
      
      ## Build list with matrices that contain monthly values:
      {
        flf.data.monthly.matrix <- list(
          (totflfAs),(totflfAsstd),
          (seedsfAs),(seedsfAsstd),
          (leafflfAs),(leafflfAsstd),
          (leafflfcAs),(leafflfcAsstd),
          (fruitflfAs),(fruitflfAsstd),
          (flowerflfAs),(flowerflfAsstd),
          (branchflfAs),(branchflfAsstd))
        
        names(flf.data.monthly.matrix) <- c("totflfAs","totflfAsstd",
                                            "seedsfAs","seedsfAsstd",
                                            "leafflfAs","leafflfAsstd",
                                            "leafflfcAs","leafflfcAsstd",
                                            "fruitflfAs","fruitflfAsstd",
                                            "flowerflfAs","flowerflfAsstd",
                                            "branchflfAs","branchflfAsstd")
      }
     
      ###  Build data frame with time series structure
      {
        ##Restructure the data (according to time series structure):
        Year <- NULL
        Month <- NULL
        Day <- NULL
        
        for (i in 1:dim(totflfAs)[2]) {
          Year[((i-1)*12+1):((i-1)*12+12)] <- (rep(c(min(yearfA,na.rm=T):max(yearfA,na.rm=T))[i],12))
          Month[((i-1)*12+1):((i-1)*12+12)] <- (1:12)
          Day[((i-1)*12+1):((i-1)*12+12)] <- rep(NA,12)
        }
        
        flf.data.monthly.ts <- data.frame(Year, Month, Day,
                                          c(totflfAs),c(totflfAsstd),
                                          c(seedsfAs),c(seedsfAsstd),
                                          c(leafflfAs),c(leafflfAsstd),
                                          c(leafflfcAs),c(leafflfcAsstd),
                                          c(fruitflfAs),c(fruitflfAsstd),
                                          c(flowerflfAs),c(flowerflfAsstd),
                                          c(branchflfAs),c(branchflfAsstd))
        
        colnames(flf.data.monthly.ts) <- c("Year","Month","Day",  
                                           "totflfAs","totflfAsstd",
                                           "seedsfAs","seedsfAsstd",
                                           "leafflfAs","leafflfAsstd",
                                           "leafflfcAs","leafflfcAsstd",
                                           "fruitflfAs","fruitflfAsstd",
                                           "flowerflfAs","flowerflfAsstd",
                                           "branchflfAs","branchflfAsstd")
      }
      flf.data.monthly.ts$litter_trap_num<-k
      flf.data.monthly[[k]]<-flf.data.monthly.ts
    }
    flf.all.monthly<-do.call(rbind.data.frame,flf.data.monthly)
    write.csv(flf.all.monthly,paste0(getwd(),"/Litter Data/FLF_",gsub(" ","",plotname),"_pertrap_output.csv"))
     
    #do again for quarterly measures
    data.flf <- read.table(paste0(getwd(),"/FineLitterFall/FLFQ_",gsub(" ","",plotname),"_",year1,"_",year2,".csv"), sep=",", header=T)
    
    #plotname = "KA FP"
    #str(data.flf)
    
    #flf <- function(data.flf, plotname, ret="monthly.means.ts", plotit=T) {   # plotsize=1                                                                                     
    
    # define each parameter
    plotfA = data.flf$plot_code  
    yearfA = data.flf$year[which(plotname==plotfA)]
    monthfA = data.flf$month[which(plotname==plotfA)]
    dayfa = as.Date(paste(data.flf$day,monthfA,yearfA,sep="-"),format="%d-%m-%Y")
    pointfA = data.flf$litterfall_trap_num[which(plotname==plotfA)]
    leaffA = data.flf$leaves_g_per_trap[which(plotname==plotfA)]
    leaffcA = data.flf$crop_leaves_g_per_trap[which(plotname==plotfA)]
    branchfA = data.flf$twigs_g_per_trap[which(plotname==plotfA)]
    flowerfA = data.flf$flowers_g_per_trap[which(plotname==plotfA)]
    fruitfA = data.flf$fruits_g_per_trap[which(plotname==plotfA)]
    seedsfA <- data.flf$seeds[which(plotname==plotfA)]
    BromfA <- NA #data.flf$bromeliads_g_per_trap[which(plotname==plotfA)]
    EpiphfA <- NA #data.flf$epiphytes_g_per_trap[which(plotname==plotfA)]
    otherfA = data.flf$other_g_per_trap[which(plotname==plotfA)]
    
    ### TO DO: sanity check of the inputs.
    
    ### Calculates total litterfall (sum of branches, leaves, flowers, fruits, seeds, Broms, Epiphs, other...):
    totalfA <- NULL
    for (i in 1:length(yearfA)) {
      totalfA[i] = sum(leaffA[i], leaffcA[i], branchfA[i], flowerfA[i], fruitfA[i], seedsfA[i], BromfA[i], EpiphfA[i], otherfA[i], na.rm=T)
    }
    
    totalfA[which(totalfA>300)] <- NA   # remove outliers with totalf > 300
    totalfA[which(totalfA<0)]   <- NA   # remove implausible totallf (negative litter)
    
    # Calculate leaf area ****need density from photos, we assume average SLA = 100g/m2
    # leaflaifA = leaffA/100   # convert to area     
    
    # format dates
    flf_dates    <- unique(dayfa)
    
    #calculate each litter trap separately
    flf.data.quarterly.1<-list()
    
    for(k in 1:num.lf){
      # initialize variables for the loop:
      totflfAs <- matrix(data=NA, nrow=length(flf_dates), ncol=2,dimnames=list(c(1:length(flf_dates)),c("quarterly","monthly")))
      totflfAsstd <- matrix(data=NA, nrow=length(flf_dates), ncol=2,dimnames=list(c(1:length(flf_dates)),c("quarterly","monthly")))
      totflfAslen <- matrix(data=NA, nrow=length(flf_dates), ncol=2,dimnames=list(c(1:length(flf_dates)),c("quarterly","monthly")))
      # leaflaifAs <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
      seedsfAs <- matrix(data=NA, nrow=length(flf_dates), ncol=2,dimnames=list(c(1:length(flf_dates)),c("quarterly","monthly")))
      seedsfAsstd <- matrix(data=NA, nrow=length(flf_dates), ncol=2,dimnames=list(c(1:length(flf_dates)),c("quarterly","monthly")))
      leafflfAs <- matrix(data=NA, nrow=length(flf_dates), ncol=2,dimnames=list(c(1:length(flf_dates)),c("quarterly","monthly")))
      leafflfAsstd <- matrix(data=NA, nrow=length(flf_dates), ncol=2,dimnames=list(c(1:length(flf_dates)),c("quarterly","monthly")))
      leafflfcAs <- matrix(data=NA, nrow=length(flf_dates), ncol=2,dimnames=list(c(1:length(flf_dates)),c("quarterly","monthly")))
      leafflfcAsstd <-matrix(data=NA, nrow=length(flf_dates), ncol=2,dimnames=list(c(1:length(flf_dates)),c("quarterly","monthly")))
      fruitflfAs <- matrix(data=NA, nrow=length(flf_dates), ncol=2,dimnames=list(c(1:length(flf_dates)),c("quarterly","monthly")))
      fruitflfAsstd <-matrix(data=NA, nrow=length(flf_dates), ncol=2,dimnames=list(c(1:length(flf_dates)),c("quarterly","monthly")))
      flowerflfAs <- matrix(data=NA, nrow=length(flf_dates), ncol=2,dimnames=list(c(1:length(flf_dates)),c("quarterly","monthly")))
      flowerflfAsstd <- matrix(data=NA, nrow=length(flf_dates), ncol=2,dimnames=list(c(1:length(flf_dates)),c("quarterly","monthly")))
      branchflfAs <- matrix(data=NA, nrow=length(flf_dates), ncol=2,dimnames=list(c(1:length(flf_dates)),c("quarterly","monthly")))
      branchflfAsstd <- matrix(data=NA, nrow=length(flf_dates), ncol=2,dimnames=list(c(1:length(flf_dates)),c("quarterly","monthly")))
      
      # TO DO: CHANGE THIS TO DATES RATHER THAN twice a month!!!!!
      # Get date of previous collection and divide by number of days. 
      # What happens with broken traps?
      # Add quality check here e.g. nothing >50
      # For the first collection interval, assume 14 days.
      
      # Conversions
      # Raw data is in g / litter trap = g / 0.25m2
      # Convert to ha: *(10000/0.25)
      # Convert to Mg: *1 g = 1.0 ?? 10-6 = 0.000001 Mg
      # Convert to C: *0.49
      
      # multiply small plot by 400 = 10,000/25*(0.5*0.5)m2 = 1600
      la = (10000/0.25)
      
      
      ## calculate quarterly means in each year:
      for (i in 1:length(flf_dates)) {
        ind = which(dayfa == flf_dates[i] & pointfA==k)
        #calculate date difference
        if(i==1)  datediff <-1 else datediff  <- 29.6/as.numeric(data.frame(diff(c(flf_dates[i-1],flf_dates[i]))))
        #divide mean by days and multiply by days in month
        totflfAs[i,1] = mean(totalfA[ind],na.rm=T)*(la/(2.032*1000000)) # Mg/ha convert to carbon divide by 2
        totflfAsstd[i,1] = (sd(totalfA[ind],na.rm=T)*(la/(2.032*1000000)))/sqrt(max(data.flf$litterfall_trap_num))  # should be sqrt(length(totalfA))
        totflfAs[i,2]<-mean(totalfA[ind],na.rm=T)*(la/(2.032*1000000))*datediff
        totflfAsstd[i,2] = (sd(totalfA[ind],na.rm=T)*(la/(2.032*1000000))*datediff)/sqrt(max(data.flf$litterfall_trap_num))  # should be sqrt(length(totalfA))
        
        # Terhi suggests calculating per litter trap and getting SE per litter trap:
        # Calculate MgC/ trap/ collection interval
        # loop per litter trap per. 
        # avg1 <- monthly average per trap over several years = average ((1st collection/ collection interval) , (2st collection/ collection interval)) * number of days in that month
        # avg2 <- monthly average per plot by averaging avg1.
        # se_avg1 <- sd(avg1)/sqrt(length(avg1)).
        # this would provide a unit per day. g / m2/ day.
        
        seedsfAs[i,1] = mean(seedsfA[ind],na.rm=T)*(la/(2.032*1000000))
        seedsfAsstd[i,1] = sd(seedsfA[ind],na.rm=T)*(la/(2.032*1000000))/sqrt(max(data.flf$litterfall_trap_num)) 
        seedsfAs[i,2] = mean(seedsfA[ind],na.rm=T)*(la/(2.032*1000000))*datediff
        seedsfAsstd[i,2] = (sd(seedsfA[ind],na.rm=T)*(la/(2.032*1000000))*datediff)/sqrt(max(data.flf$litterfall_trap_num))  # should be sqrt(length(totalfA))
        
        leafflfAs[i,1] = mean(leaffA[ind],na.rm=T)*(la/(2.032*1000000))
        leafflfAsstd[i,1] = sd(leaffA[ind],na.rm=T)*(la/(2.032*1000000))/sqrt(max(data.flf$litterfall_trap_num))
        leafflfAs[i,2] = mean(leaffA[ind],na.rm=T)*(la/(2.032*1000000))*datediff
        leafflfAsstd[i,2] = sd(leaffA[ind],na.rm=T)*(la/(2.032*1000000)*datediff)/sqrt(max(data.flf$litterfall_trap_num))
        
        leafflfcAs[i,1] = mean(leaffcA[ind],na.rm=T)*(la/(2.032*1000000))
        leafflfcAsstd[i,1] = sd(leaffcA[ind],na.rm=T)*(la/(2.032*1000000))/sqrt(max(data.flf$litterfall_trap_num))
        leafflfcAs[i,2] = mean(leaffcA[ind],na.rm=T)*(la/(2.032*1000000))*datediff
        leafflfcAsstd[i,2] = sd(leaffcA[ind],na.rm=T)*(la/(2.032*1000000)*datediff)/sqrt(max(data.flf$litterfall_trap_num))
        
        fruitflfAs[i,1] = mean(fruitfA[ind],na.rm=T)*(la/(2.032*1000000))
        fruitflfAsstd[i,1] = (sd(fruitfA[ind],na.rm=T)*(la/(2.032*1000000)))/sqrt(max(data.flf$litterfall_trap_num))
        fruitflfAs[i,2] = mean(fruitfA[ind],na.rm=T)*(la/(2.032*1000000))*datediff
        fruitflfAsstd[i,2] = (sd(fruitfA[ind],na.rm=T)*(la/(2.032*1000000))*datediff)/sqrt(max(data.flf$litterfall_trap_num))
        
        flowerflfAs[i,1] = mean(flowerfA[ind],na.rm=T)*(la/(2.032*1000000))
        flowerflfAsstd[i,1] = (sd(flowerfA[ind])*(la/(2.032*1000000)))/sqrt(max(data.flf$litterfall_trap_num))
        flowerflfAs[i,2] = mean(flowerfA[ind],na.rm=T)*(la/(2.032*1000000))*datediff
        flowerflfAsstd[i,2] = (sd(flowerfA[ind])*(la/(2.032*1000000))*datediff)/sqrt(max(data.flf$litterfall_trap_num))
        
        branchflfAs[i,1] = mean(branchfA[ind],na.rm=T)*(la/(2.032*1000000))
        branchflfAsstd[i,1] = (sd(branchfA[ind],na.rm=T)*(la/(2.032*1000000)))/sqrt(max(data.flf$litterfall_trap_num))
        branchflfAs[i,2] = mean(branchfA[ind],na.rm=T)*(la/(2.032*1000000))*datediff
        branchflfAsstd[i,2] = (sd(branchfA[ind],na.rm=T)*(la/(2.032*1000000))*datediff)/sqrt(max(data.flf$litterfall_trap_num))
        
        # Add a column for reproductive material
        
        #pointfAa = pointfA[ind]      
      }
      
      totflfAs[which(totflfAs==0)] <- NaN
      
      
      ## Build data.frame with matrices that contain quarterly values:
      {
        flf.data.quarterly <- data.frame(
          (flf_dates),
          (totflfAs),(totflfAsstd),
          (seedsfAs),(seedsfAsstd),
          (leafflfAs),(leafflfAsstd),
          (leafflfcAs),(leafflfcAsstd),
          (fruitflfAs),(fruitflfAsstd),
          (flowerflfAs),(flowerflfAsstd),
          (branchflfAs),(branchflfAsstd))
        
        colnames(flf.data.quarterly) <- c("dates","totflfAs.q","totflfAs.m","totflfAsstd.q","totflfAsstd.m",
                                          "seedsfAs.q","seedsfAs.m","seedsfAsstd.q","seedsfAsstd.m",
                                          "leafflfAs.q","leafflfAs.m","leafflfAsstd.q","leafflfAsstd.m",
                                          "leafflfcAs.q","leafflfcAs.m","leafflfcAsstd.q","leafflfcAsstd.m",
                                          "fruitflfAs.q","fruitflfAs.m","fruitflfAsstd.q","fruitflfAsstd.m",
                                          "flowerflfAs.q","flowerflfAs.m","flowerflfAsstd.q","flowerflfAsstd.m",
                                          "branchflfAs.q","branchflfAs.m","branchflfAsstd.q","branchflfAsstd.m")
      }
      flf.data.quarterly$litter_trap_num<-k
      flf.data.quarterly.1[[k]]<-flf.data.quarterly
    }
      
     flf.all.quarterly<-do.call(rbind.data.frame,flf.data.quarterly.1)
     write.csv(flf.all.quarterly,paste0(getwd(),"/FineLitterFall/FLFQ_",gsub(" ","",plotname),"_pertrap_output.csv"))
  
  
  
}
  
  
# Return either monthly means (ret="monthly.means") or annual means (ret="annual.means")  
    #switch(ret,
    #monthly.means.matrix = {return(flf.data.monthly.matrix)},
    #monthly.means.ts = {return(flf.data.monthly.ts)}
    #)
    #}

