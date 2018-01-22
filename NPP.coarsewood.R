#if I use volume and density, this function is off by a factor of 10, no idea why

# Coarse wood NPP

# This program calculates monthly averages of coarse wood and repiration
# from this wood

# This function requires the following data:
# dataw:      data.frame that contains the following columns: see below
#             coarse litter (mass of coarse litter hanging around along the coarse woody debris transect)
# plotname:   String or number indicating which plot should be analysed
# ret:          format of return value, indicated with strings
# plotit:       logical, whether function should be plotted

library(ggplot2)

## Chris' test data:
#library(R.matlab)
#setwd("C:/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/Original-matlab-code/Ken_Iq-Tang")
setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/NPP/CWD")
#dir()
#Dataall <- readMat(con="Dataall.mat")

#setwd("C:/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/R-functions(v3)")
#setwd("D:/Dokumente/My Dropbox/Carbon_Use_Efficieny_R/R-functions(v3)")
plts<-c("HM FP","KA FP","KA 100 F3","KA 100 F1","KA 500 F3","KA 1K F3","HM 5K F2","HM 500 F3","HM 100 F3","HM 500 F2","AB 100 F1","AB 500 F2","AB 1K F2","AB 5K F2","AB FP")
plotsize=4*60
plotit=T

#names(dataw) <- c(year","month","plot","transect","diameter_class","decomposition_class","density",
                  #"dry_weight_1","width_1","width_2","width_3","width_4","length_1")

#coarsewoodNPP <- function(dataw, plotname, ret="monthly.means.ts", plotit=F) {

# Establish 4 100 meter transects, 1 meter wide
# cut all dead wood pieces >2 cm diameter, record diameter and length
# within the transect, note seperately 2-5cm, 5-10, and >10cm
# 15 catagories: 3 diameter, 5 decomposition
# See ss.4.2 (p. 63) in the RAINFOR manual

for(p in 1:length(plts)){
  plotname=plts[p]
  
  dataw = data.frame(read.csv(paste0(getwd(),"/",gsub(" ","",plotname),"_CWD.csv"))) # coarse litter (mass of coarse litter hanging around along the coarse woody debris transect)
  # this reads in the whole dataset (not just the plot we selected)
  plotw = dataw$plot
  yearw = dataw$year
  monthw = dataw$month
  transw = dataw$transect 
  decompw  = dataw$decomposition_class
  dryweight = as.numeric(dataw$dry_weight)
  densityw = as.numeric(dataw$density)
  diameter1w = colMeans(t(data.frame(dataw$width_1,dataw$width_2,dataw$width_3)),na.rm=T)
  #lenw = colMeans(t(data.frame(dataw$length_1,dataw$length_2)),na.rm=T)
  lenw = as.numeric(dataw$length_1)
  
  # initialising vectors of length zero
  diameterA <- NULL
  lengthA <- NULL
  volA <- NULL
  surface_areaA <- NULL
  dryweightA <- NULL
  #dryweightAb <- NULL
  decompA <- NULL
  monthwA <- NULL
  yearwA <- NULL
  
  # Coarse woody surface area per plot per unit ground area
  # Selecting data for the plot we need & a bit of conversions
  n=1
  for (i in 1:length(plotw)) {
      if (plotw[i]==plotname) {
          diameterA[n] = diameter1w[i]/100  # convert cm to m
          lengthA[n] = lenw[i]/100   # convert cm to m
          volA[n] = pi * (diameterA[n] / 2)^2 * lengthA[n]   # m3
          surface_areaA[n] = (2 * 3.142 * ((diameterA[n] / 2)^2)) + (2 * 3.142 * (diameterA[n] / 2) * lengthA[n])
          dryweightA[n] = dryweight[i]
          #dryweightA[n] = densityw[i]*100*100*100/1000*volA[n] #convert g/cm3 to g/m3 (100*100*100 cm3/1 m3)
          decompA[n] = decompw[i]
          monthwA[n] = monthw[i]
          yearwA[n] = yearw[i]
          n=n+1
      }
  }
  
  
  # calculate mean surface area for each decomposition catagory for each month in each year:
  fir_year = min(c(yearw),na.rm=T)
  fir_yeare = max(c(yearw),na.rm=T)
  
  ## initialize variables for the loop
  ## 1:5 are different decomposition classes!
  
  # saa is surface area
  saa1 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
  saa2 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
  saa3 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
  saa4 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
  saa5 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
  
  # va is volume
  va1 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
  va2 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
  va3 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
  va4 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
  va5 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
  
  # mass
  ma1 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
  ma2 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
  ma3 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
  ma4 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
  ma5 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
  
  ma1std <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
  ma2std <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
  ma3std <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
  ma4std <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
  ma5std <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
  
  # this loop calculates surface area, volume and branch NPP for each month in each year
  n=1
  for (j in fir_year:fir_yeare) {
      m=1
      for (i in 1:12) {
          im = which(monthwA==i & yearwA==j)
          xc = surface_areaA[im]
          x1 = decompA[im]
          xx1 = which(x1==1)
          saa1[m,n] = (sum(xc[xx1],na.rm=T))/plotsize
          xx2 = which(x1==2)
          saa2[m,n] = (sum(xc[xx2],na.rm=T))/plotsize
          xx3 = which(x1==3)
          saa3[m,n] = (sum(xc[xx3],na.rm=T))/plotsize
          xx4 = which(x1==4)
          saa4[m,n] = (sum(xc[xx4],na.rm=T))/plotsize
          xx5 = which(x1==5)
          saa5[m,n] = (sum(xc[xx5],na.rm=T))/plotsize
          
          #calculate volume for each month
          va1[m,n] = sum(volA[which(x1==1)],na.rm=T)/plotsize            #;% surface area per m2
          va2[m,n] = sum(volA[which(x1==2)],na.rm=T)/plotsize            #;% surface area per m2
          va3[m,n] = sum(volA[which(x1==3)],na.rm=T)/plotsize            #;% surface area per m2
          va4[m,n] = sum(volA[which(x1==4)],na.rm=T)/plotsize            #;% surface area per m2
          va5[m,n] = sum(volA[which(x1==5)],na.rm=T)/plotsize            #;% surface area per m2
          
          #calculate branch NPP for each month, for each decomposition class
          xac = dryweightA[im]
          xa1 = decompA[im]
          xxa1 = which(xa1==1)
          ma1[m,n] = (sum(xac[xxa1],na.rm=T))/plotsize
          ma1std[m,n] = (sd(xac[xxa1],na.rm=T))/plotsize
          xxa2 = which(xa1==2)
          ma2[m,n] = (sum(xac[xxa2],na.rm=T))/plotsize
          ma2std[m,n] = (sd(xac[xxa2],na.rm=T))/plotsize
          xxa3 = which(xa1==3)
          ma3[m,n] = (sum(xac[xxa3],na.rm=T))/plotsize
          ma3std[m,n] = (sd(xac[xxa3],na.rm=T))/plotsize
          xxa4 = which(xa1==4)
          ma4[m,n] = (sum(xac[xxa4],na.rm=T))/plotsize
          ma4std[m,n] = (sd(xac[xxa4],na.rm=T))/plotsize
          xxa5 = which(xa1==5)
          ma5[m,n] = (sum(xac[xxa5],na.rm=T))/plotsize
          ma5std[m,n] = (sd(xac[xxa5],na.rm=T))/plotsize
          m=m+1
      }
      n=n+1
  }
  
  
  ### Initializes variables 
  # NPPCWDa: NPP branch turnover
  NPPCWDa <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
  NPPCWDastd <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
  
  # sum decomposition classes, grouped over each month and year.
  for (i in 1:12) {
      for (j in 1:(fir_yeare-fir_year+1)) {
      NPPCWDa[i,j] = sum(c(ma1[i,j],ma2[i,j],ma3[i,j],ma4[i,j],ma5[i,j]),na.rm=T)/3 #%kg/m2month
      NPPCWDastd[i,j] = sum(c(ma1std[i,j],ma2std[i,j],ma3std[i,j],ma4std[i,j],ma5std[i,j]),na.rm=T)/3 #%kg/m2monthmonth ??
      }
  }
  
  # convert to Mg ha-1 month-1
  convert2 = 10000/(2.1097*1000*1000) # convert to MgC ha and 50% carbon
  NPPCWDac = NPPCWDa*convert2         # MgC ha
  NPPCWDacstd = NPPCWDastd*convert2   # MgC ha
  
  NPPCWDac[which(NPPCWDac==0)] <- NA
  NPPCWDacstd[which(NPPCWDacstd==0)] <- NA
  
  
  ## Build list with matrices that contain monthly values:
  {
    coarsewoodNPP.monthly.matrix <- list(NPPCWDac,NPPCWDacstd)
    names(coarsewoodNPP.monthly.matrix) <- c("NPPCWDac","NPPCWDacstd")
  }
  
  ###  Build data frame with time series structure
  {
    ##Restructure the data (according to time series structure):
    Year <- NULL
    Month <- NULL
    Day <- NULL
    
    for (i in 1:dim(NPPCWDac)[2]) {
      Year[((i-1)*12+1):((i-1)*12+12)] <- (rep(c(fir_year:fir_yeare)[i],12))
      Month[((i-1)*12+1):((i-1)*12+12)] <- (1:12)
      Day[((i-1)*12+1):((i-1)*12+12)] <- 1
    }
    
    coarsewoodNPP.monthly.ts <- data.frame(Year,Month,Day,
                                       c(NPPCWDac),c(NPPCWDacstd))
    
    colnames(coarsewoodNPP.monthly.ts) <- c("Year","Month","Day",  
                                        "NPPCWDac","NPPCWDacstd")
  }
  coarsewoodNPP.monthly.ts$d     <- as.character(paste(coarsewoodNPP.monthly.ts$Month,coarsewoodNPP.monthly.ts$Day, coarsewoodNPP.monthly.ts$Year, sep="/")) 
  coarsewoodNPP.monthly.ts$date  <- as.Date(coarsewoodNPP.monthly.ts$d,"%m/%d/%Y")
  
  coarsewoodNPP.monthly.ts$NPPCWDac_se <- as.numeric(coarsewoodNPP.monthly.ts$NPPCWDacstd)/sqrt(5)
  
  
  
  ## Plotroutine, triggered by argument 'plotit=T'
  if (plotit==T) {
    ## ggplot2 of root npp vs time
    #pdf(paste0(getwd(),"/CWD2_",gsub(" ","",plotname),".pdf"))
    top <- max(coarsewoodNPP.monthly.ts$NPPCWDac + coarsewoodNPP.monthly.ts$NPPCWDac_se,na.rm=T)
    plot1 <- ggplot(data=coarsewoodNPP.monthly.ts[!is.na(coarsewoodNPP.monthly.ts$NPPCWDac),], aes(x=date, y=NPPCWDac, na.rm=T)) +
      geom_line(linetype='solid', colour='black', size=1) +
      geom_ribbon(data=coarsewoodNPP.monthly.ts, aes(ymin=NPPCWDac-NPPCWDac_se, ymax=NPPCWDac+NPPCWDac_se), alpha=0.2) +
      #scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%Y")) +            
      scale_colour_grey() + 
      theme(text = element_text(size=17), legend.title = element_text(colour = 'black', size = 17, hjust = 3, vjust = 7, face="plain")) +
      ylim(0, max(top, na.rm=T)) +                          
      xlab("") + ylab(expression(paste("NPP Coarse Wood Debris (MgC ", ha^-1, mo^-1, ")", sep=""))) +
      theme_classic(base_size = 15, base_family = "") + 
      theme(legend.position="left")+ 
      theme(axis.text.x= element_text(angle=45)) +
      theme(panel.border = element_rect(fill = NA, colour = "black", size = 1))+
      ggtitle(paste0("CWD for ",plotname))
    plot1
    ggsave(paste0(getwd(),"/CWD2_",gsub(" ","",plotname),".pdf"))
    #dev.off()
    write.csv(coarsewoodNPP.monthly.ts,paste0(getwd(),"/CWD2_",gsub(" ","",plotname),"_out.csv"))
  }

}

# Return either monthly means (ret="monthly.means") or annual means (ret="annual.means")  
#switch(ret,
       #monthly.means.matrix = {return(coarsewoodNPP.monthly.matrix)},
       #monthly.means.ts = {return(coarsewoodNPP.monthly.ts)}
       #)

#}