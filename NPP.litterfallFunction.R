### Function fine litter fall: 
# This function uses data to calculate NPP from fine litterfall.

## Read-in data:
setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/NPP/Litter Data")

# this is what we have in db:
#names(data.flf) <- c("plot_code", "year","month", "day","litterfall_trap_num", "litterfall_trap_size_m2","leaves_g_per_trap","twigs_g_per_trap","flowers_g_per_trap","fruits_g_per_trap",
# "bromeliads_g_per_trap", "epiphytes_g_per_trap","other_g_per_trap", "palm_leaves_g", "palm_flower_g", "palm_fruit_g", "quality_code", "comments")

# to test this code, you can define the following parameters
plotsize = 0.36  ### VARIABLE PLOTSIZE IS NOT YET INCLUDED: DISCUSS HOW TO INCLUDE IT.
plotnames = c("HM FP","KA FP","KA 100 F3","KA 100 F1","KA 500 F3","KA 1K F3","HM 5K F2","HM 500 F3","HM 100 F3","HM 500 F2")
year1=2014
year2=2017

for(i in 1:length(plotnames)){
  plotname<-plotnames[i]
  data.flf <- read.table(paste0(getwd(),"/FLF_",gsub(" ","",plotname),"_",year1,"_",year2,".csv"), sep=",", header=T)
  
  #plotname = "KA FP"
  #str(data.flf)
  
  #flf <- function(data.flf, plotname, ret="monthly.means.ts", plotit=T) {   # plotsize=1                                                                                     
  
  # load libraries
  library(scales)
  library(zoo)
  require(ggplot2)
  library(gridExtra)
  
  # define each parameter
  plotfA = data.flf$plot_code  
  yearfA = data.flf$year[which(plotname==plotfA)]
  monthfA = data.flf$month[which(plotname==plotfA)]
  #pointfA = data.flf$litterfall_trap_num[which(plotname==plotfA)]
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
      ind = which(monthfA==i & yearfA==j)
      
      # multiply by 2 because collected twice a month
      totflfAs[m,n] = mean(totalfA[ind],na.rm=T)*(la/(2.032*1000000)) # Mg/ha convert to carbon divide by 2
      totflfAsstd[m,n] = (sd(totalfA[ind],na.rm=T)*(la/(2.032*1000000)))/sqrt(25)  # should be sqrt(length(totalfA))
      
      # Terhi suggests calculating per litter trap and getting SE per litter trap:
      # Calculate MgC/ trap/ collection interval
      # loop per litter trap per. 
      # avg1 <- monthly average per trap over several years = average ((1st collection/ collection interval) , (2st collection/ collection interval)) * number of days in that month
      # avg2 <- monthly average per plot by averaging avg1.
      # se_avg1 <- sd(avg1)/sqrt(length(avg1)).
      # this would provide a unit per day. g / m2/ day.
      
      seedsfAs[m,n] = mean(seedsfA[ind],na.rm=T)*(la/(2.032*1000000))
      seedsfAsstd[m,n] = sd(seedsfA[ind],na.rm=T)*(la/(2.032*1000000))/sqrt(max(data.flf$litterfall_trap_num)) 
      
      leafflfAs[m,n] = mean(leaffA[ind],na.rm=T)*(la/(2.032*1000000))
      leafflfAsstd[m,n] = sd(leaffA[ind],na.rm=T)*(la/(2.032*1000000))/sqrt(max(data.flf$litterfall_trap_num))
      
      leafflfcAs[m,n] = mean(leaffcA[ind],na.rm=T)*(la/(2.032*1000000))
      leafflfcAsstd[m,n] = sd(leaffcA[ind],na.rm=T)*(la/(2.032*1000000))/sqrt(max(data.flf$litterfall_trap_num))
      
      fruitflfAs[m,n] = mean(fruitfA[ind],na.rm=T)*(la/(2.032*1000000))
      fruitflfAsstd[m,n] = (sd(fruitfA[ind],na.rm=T)*(la/(2.032*1000000)))/sqrt(max(data.flf$litterfall_trap_num))
      
      flowerflfAs[m,n] = mean(flowerfA[ind],na.rm=T)*(la/(2.032*1000000))
      flowerflfAsstd[m,n] = (sd(flowerfA[ind])*(la/(2.032*1000000)))/sqrt(max(data.flf$litterfall_trap_num))
      
      branchflfAs[m,n] = mean(branchfA[ind],na.rm=T)*(la/(2.032*1000000))
      branchflfAsstd[m,n] = (sd(branchfA[ind],na.rm=T)*(la/(2.032*1000000)))/sqrt(max(data.flf$litterfall_trap_num))
      
      # Add a column for reproductive material
      
      #pointfAa = pointfA[ind]      
      m=m+1;
    }
    n=n+1;
  }
  
  totflfAs[which(totflfAs==0)] <- NaN
  
  
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
  
  
  ## Plotroutine, triggered by argument 'plotit=T'
  flf.data.monthly.ts$date <- strptime(paste(as.character(flf.data.monthly.ts$Year), as.character(flf.data.monthly.ts$Month), as.character(15), sep="-"), format="%Y-%m-%d")
  flf.data.monthly.ts$yearmonth <- as.yearmon(flf.data.monthly.ts$date)
  
  # if (plotit==T) {
  quartz()
  top <- flf.data.monthly.ts$totflfAs+flf.data.monthly.ts$totflfAsstd
  flf.data.monthly.ts$date <- as.Date(flf.data.monthly.ts$date)
  plot1 <- ggplot(data=flf.data.monthly.ts, aes(x=date, y=totflfAs, na.rm=T)) +
    geom_line(linetype='solid', colour='black', size=1) +
    geom_ribbon(data=flf.data.monthly.ts, aes(ymin=totflfAs-totflfAsstd, ymax=totflfAs+totflfAsstd), alpha=0.2) +
    scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%Y")) +            
    scale_colour_grey() + 
    theme(text = element_text(size=10), legend.title = element_text(colour = 'black', size = 10, hjust = 3, vjust = 7, face="plain")) +
    ylim(0, max(top, na.rm=T)) +                          
    xlab("") + ylab(expression(paste("Total fine litterfall (MgC ", ha^-1, mo^-1, ")", sep=""))) +
    theme_classic(base_size = 10, base_family = "") + 
    theme(axis.text.x= element_text(angle=45)) +
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1))+
    ggtitle(paste0("Total Fine Litter Fall for ",plotname))
  
  #theme(legend.title=element_blank()) + theme(legend.key = element_blank())   
  
  top <- flf.data.monthly.ts$branchflfAs+flf.data.monthly.ts$branchflfAsstd
  plot2 <- ggplot(data=flf.data.monthly.ts, aes(x=date, y=branchflfAs, na.rm=T)) +
    geom_line(linetype='solid', colour='black', size=1) +
    geom_ribbon(data=flf.data.monthly.ts, aes(ymin=branchflfAs-branchflfAsstd , ymax=branchflfAs+branchflfAsstd), alpha=0.2) +
    scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%Y")) +  
    scale_colour_grey() + 
    theme(text = element_text(size=10), legend.title = element_text(colour = 'black', size = 10, hjust = 3, vjust = 7, face="plain")) +
    ylim(0, max(top, na.rm=T)) +                          
    xlab("") + ylab(expression(paste("Fine litterfall components (MgC ", ha^-1, mo^-1, ")", sep=""))) +
    theme_classic(base_size = 10, base_family = "")+ 
    theme(axis.text.x= element_text(angle=45)) + 
    theme(legend.position="left") +
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1))+
    ggtitle("Branches")
  
  top <- flf.data.monthly.ts$flowerflfAs + flf.data.monthly.ts$flowerflfAsstd
  plot3 <- ggplot(data=flf.data.monthly.ts, aes(x=date, y=flowerflfAs, na.rm=T)) +
    geom_line(linetype='solid', colour='black', size=1) +
    geom_ribbon(data=flf.data.monthly.ts, aes(ymin=flowerflfAs-flowerflfAsstd , ymax=flowerflfAs+flowerflfAsstd), alpha=0.2) +
    geom_line(data=flf.data.monthly.ts, aes(x=date, y=fruitflfAs, na.rm=T),linetype='dotted', colour='black', size=1) +
    geom_ribbon(data=flf.data.monthly.ts, aes(ymin=fruitflfAs-fruitflfAsstd, ymax=fruitflfAs+fruitflfAsstd), alpha=0.2) + 
    geom_line(data=flf.data.monthly.ts, aes(x=date, y=seedsfAs, na.rm=T),linetype='dashed', colour='black', size=1) +
    geom_ribbon(data=flf.data.monthly.ts, aes(ymin=seedsfAs-seedsfAsstd, ymax=seedsfAs+seedsfAsstd), alpha=0.2) +                 
    scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%Y")) +  
    scale_colour_grey() + 
    theme(text = element_text(size=10), legend.title = element_text(colour = 'black', size = 10, hjust = 3, vjust = 7, face="plain")) +
    ylim(0, max(top, na.rm=T)) +                          
    xlab("") + ylab(expression(paste("Fine litterfall components (MgC ", ha^-1, mo^-1, ")", sep=""))) +
    theme_classic(base_size = 10, base_family = "") + 
    theme(axis.text.x= element_text(angle=45))+
    theme(legend.position="left") +
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1)) +
    ggtitle("Flowers (solid), Fruit (dotted) and Seeds (dashed)")
  
  top <- max(flf.data.monthly.ts$leafflfAs + flf.data.monthly.ts$leafflfAsstd,flf.data.monthly.ts$leafflfcAs + flf.data.monthly.ts$leafflfcAsstd,na.rm=T)
  plot4 <- ggplot(data=flf.data.monthly.ts, aes(x=date, y=leafflfAs, na.rm=T)) +
    geom_line(linetype='solid', colour='black', size=1) +
    geom_ribbon(data=flf.data.monthly.ts, aes(ymin=leafflfAs-leafflfAsstd, ymax=leafflfAs+leafflfAsstd), alpha=0.2) +
    geom_line(data=flf.data.monthly.ts, aes(x=date, y=leafflfcAs, na.rm=T), linetype='dotted', colour='black', size=1) +
    geom_ribbon(data=flf.data.monthly.ts, aes(ymin=leafflfcAs-leafflfcAsstd, ymax=leafflfcAs+leafflfcAsstd), alpha=0.2) +
    scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%Y")) +  
    scale_colour_grey() + 
    theme(text = element_text(size=10), legend.title = element_text(colour = 'black', size = 10, hjust = 3, vjust = 7, face="plain")) +
    ylim(0, max(top, na.rm=T)) +                          
    xlab("") + ylab(expression(paste("Fine litterfall components (MgC ", ha^-1, mo^-1, ")", sep=""))) +
    theme_classic(base_size = 10, base_family = "")+ 
    theme(axis.text.x= element_text(angle=45)) + 
    theme(legend.position="left") +
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1))+
    ggtitle("Forest Leaves (solid) and Crop Leaves (dotted)")
  
  
  fig<-grid.arrange(plot1, plot2, plot3, plot4, ncol=2, nrow=2)
  ggsave(paste0(getwd(),"/FLF_",gsub(" ","",plotname),"_",year1,"_",year2,".pdf"),fig,width=11,height=8)
  write.csv(flf.data.monthly.ts,paste0(getwd(),"/FLF_",gsub(" ","",plotname),"_output.csv"))
  # }
  
  
}
  
# Return either monthly means (ret="monthly.means") or annual means (ret="annual.means")  
    #switch(ret,
    #monthly.means.matrix = {return(flf.data.monthly.matrix)},
    #monthly.means.ts = {return(flf.data.monthly.ts)}
    #)
    #}

