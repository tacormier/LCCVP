library(raster)
library(zoo)
library(foreign)

#NPS station data
nps.precip <- "C:/Share/LCC-VP/ClimateStation/GRSM_climate_db_updated_2011/prcp_daily_2011_grsm.csv"
nps.tmax <- "C:/Share/LCC-VP/ClimateStation/GRSM_climate_db_updated_2011/tmax_daily_2011_grsm.csv"
nps.tmin <- "C:/Share/LCC-VP/ClimateStation/GRSM_climate_db_updated_2011/tmin_daily_2011_grsm.csv"
nps.meta <- "C:/Share/LCC-VP/ClimateStation/GRSM_climate_db_updated_2011/GRSM_Climate_stations_Fridley_wgs72_NPSonly.dbf"

#PRISM
prism.precip.dir <- "C:/Share/LCC-VP/US_PRISM/prism_800m_restricted/ppt/"
prism.tmax.dir <- "C:/Share/LCC-VP/US_PRISM/prism_800m_restricted/tmax/"
prism.tmin.dir <- "C:/Share/LCC-VP/US_PRISM/prism_800m_restricted/tmin/"

#read in prism and clip to area
ppt.list <- list.files(prism.precip.dir, pattern="*.tif", full.names=T)
ppt.list <- ppt.list[985:length(ppt.list)]
prism.ppt <- stack()


#Read in climate station data
precip <- read.csv(nps.precip)
tmax <- read.csv(nps.tmax)
tmin <- read.csv(nps.tmin)
meta <- read.dbf(nps.meta)

#Subset out just the date, ID, and nps ranger stations (gsm001 - gsm006)

sub.names <- c("ID", "date", "gsm001","gsm002","gsm003","gsm004","gsm005","gsm006")
#sub.names2 <- c("ID", "date", "precip", "tmin", "tmax")


precip <- precip[,sub.names]
tmax <- tmax[,sub.names]
tmin <- tmin[,sub.names]

#make date field a date class - instead of a factor
precip$date <- strptime(precip$date, "%m/%d/%Y")
tmax$date <- strptime(tmax$date, "%m/%d/%Y")
tmin$date <- strptime(tmin$date, "%m/%d/%Y")

#remove invalid values
precip[precip == 999.90] <- NA 
tmax[tmax == 999.90] <- NA
tmin[tmin == 999.90] <- NA

#Subset years based on metadata file - hard coded
precip.sub <- subset(precip, (date > as.POSIXlt('1976-12-31')) & (date <= as.POSIXlt('2011-12-31')))
precip.sub$monyr <- as.yearmon(paste(format(precip.sub$date, "%b"), format(precip.sub$date, "%Y")))
tmax.sub <- subset(tmax, (date > as.POSIXlt('1976-12-31')) & (date <= as.POSIXlt('2011-12-31')))
tmax.sub$monyr <- as.yearmon(paste(format(tmax.sub$date, "%b"), format(tmax.sub$date, "%Y")))
tmin.sub <- subset(tmin, (date > as.POSIXlt('1976-12-31')) & (date <= as.POSIXlt('2011-12-31')))
tmin.sub$monyr <- as.yearmon(paste(format(tmin.sub$date, "%b"), format(tmin.sub$date, "%Y")))

#Aggregate daily data to monthly data by month/year
precip.mo <- strftime(precip.sub$date, "%m")
precip.yr <- strftime(precip.sub$date, "%Y")
precip.monthly <- data.frame(precip.mo, precip.yr, precip.sub[,3:8])
precip.monthly <- aggregate(x=precip.sub[,3:8], by=list(precip.mo, precip.yr), FUN="mean", na.rm=T)
precip.monthly$date <- paste(precip.monthly$Group.1, precip.monthly$Group.2, sep="/")
precip.monthly$date <- as.yearmon(precip.monthly$date, "%m/%Y")
#remove months with fewer than 20 measurements
for (i in names(precip.sub[3:8]))
  for (k in precip.monthly$date) {
    obs <- sum(!is.na(precip.sub[[i]][precip.sub$monyr %in% k]))
    
    if (obs < 20) {
      print(paste(i, " - ", as.yearmon(k), obs))
      precip.monthly[[i]][precip.monthly$date == k] <- NA
    }#end if
  }#end loop

#aggregate tmax
tmax.mo <- strftime(tmax.sub$date, "%m")
tmax.yr <- strftime(tmax.sub$date, "%Y")
tmax.monthly <- data.frame(tmax.mo, tmax.yr, tmax.sub[,3:8])
tmax.monthly <- aggregate(x=tmax.sub[,3:8], by=list(tmax.mo, tmax.yr), FUN="mean")
tmax.monthly$date <- paste(tmax.monthly$Group.1, tmax.monthly$Group.2, sep="/")
tmax.monthly$date <- as.yearmon(tmax.monthly$date, "%m/%Y")
#remove months with fewer than 20 measurements
for (i in names(tmax.sub[3:8]))
  for (k in tmax.monthly$date) {
    obs <- sum(!is.na(tmax.sub[[i]][tmax.sub$monyr %in% k]))
    
    if (obs < 20) {
      print(paste(i, " - ", as.yearmon(k), obs))
      tmax.monthly[[i]][tmax.monthly$date == k] <- NA
    }#end if
  }#end loop

#aggregate tmin
tmin.mo <- strftime(tmin.sub$date, "%m")
tmin.yr <- strftime(tmin.sub$date, "%Y")
tmin.monthly <- data.frame(tmin.mo, tmin.yr, tmin.sub[,3:8])
tmin.monthly <- aggregate(x=tmin.sub[,3:8], by=list(tmin.mo, tmin.yr), FUN="mean")
tmin.monthly$date <- paste(tmin.monthly$Group.1, tmin.monthly$Group.2, sep="/")
tmin.monthly$date <- as.yearmon(tmin.monthly$date, "%m/%Y")
#remove months with fewer than 20 measurements
for (i in names(tmin.sub[3:8]))
  for (k in tmin.monthly$date) {
    obs <- sum(!is.na(tmin.sub[[i]][tmin.sub$monyr %in% k]))
    
    if (obs < 20) {
      print(paste(i, " - ", as.yearmon(k), obs))
      tmin.monthly[[i]][tmin.monthly$date == k] <- NA
    }#end if
  }#end loop

#Plot station data

#precip
plot(precip.monthly$date, precip.monthly$gsm006, type="l", col="blue")
#points(precip.sub$monyr, precip.sub$gsm006)

#plot other 5 stations together with similar date range - starting in 2000
max.precip <- max(precip.monthly[284:length(precip.monthly$date), 3:7], na.rm=T)
plot(precip.monthly$date[284:length(precip.monthly$date)], precip.monthly$gsm001[284:length(precip.monthly$gsm001)], type="n", ylim=c(0,max.precip))
# lines(precip.monthly$date[284:length(precip.monthly$date)], precip.monthly$gsm002[284:length(precip.monthly$gsm002)], col="red")
# lines(precip.monthly$date[284:length(precip.monthly$date)], precip.monthly$gsm003[284:length(precip.monthly$gsm002)], col="green")
# lines(precip.monthly$date[284:length(precip.monthly$date)], precip.monthly$gsm004[284:length(precip.monthly$gsm002)], col="blue")
# lines(precip.monthly$date[284:length(precip.monthly$date)], precip.monthly$gsm005[284:length(precip.monthly$gsm002)], col="darkturquiose")
lines(precip.monthly$date, precip.monthly$gsm001, col="black")
lines(precip.monthly$date, precip.monthly$gsm002, col="green")
lines(precip.monthly$date, precip.monthly$gsm003, col="orange")
lines(precip.monthly$date, precip.monthly$gsm004, col="gray")
lines(precip.monthly$date, precip.monthly$gsm005, col="darkturquoise")

#tmax
plot(tmax.monthly$date, tmax.monthly$gsm006, type="l", col="blue")
#points(tmax.sub$monyr, tmax.sub$gsm006)

#plot other 5 stations together with similar date range - starting in 1991
max.tmax <- max(tmax.monthly[169:length(tmax.monthly$date), 3:7], na.rm=T)
min.tmax <- min(tmax.monthly[169:length(tmax.monthly$date), 3:7], na.rm=T)
plot(tmax.monthly$date[169:length(tmax.monthly$date)], tmax.monthly$gsm001[169:length(tmin.monthly$gsm001)], type="n", ylim=c(min.tmax,max.tmax))
#plot(tmax.monthly$date, tmax.monthly$gsm006, type="l", col="blue")
#points(tmax.sub$monyr, tmax.sub$gsm006)
lines(tmax.monthly$date, tmax.monthly$gsm001, col="black")
lines(tmax.monthly$date, tmax.monthly$gsm002, col="green")
lines(tmax.monthly$date, tmax.monthly$gsm003, col="orange")
lines(tmax.monthly$date, tmax.monthly$gsm004, col="gray")
lines(tmax.monthly$date, tmax.monthly$gsm005, col="darkturquoise")

#tmin
plot(tmin.monthly$date, tmin.monthly$gsm006, type="l", col="blue")
#points(tmin.sub$monyr, tmin.sub$gsm006)

#plot other 5 stations together with similar date range - starting in 1991
max.tmin <- max(tmin.monthly[169:length(tmin.monthly$date), 3:7], na.rm=T)
min.tmin <- min(tmin.monthly[169:length(tmin.monthly$date), 3:7], na.rm=T)
plot(tmin.monthly$date[169:length(tmin.monthly$date)], tmin.monthly$gsm001[169:length(tmin.monthly$gsm001)], type="n", ylim=c(min.tmin,max.tmin))
#plot(tmin.monthly$date, tmin.monthly$gsm006, type="l", col="blue")
#points(tmin.sub$monyr, tmin.sub$gsm006)
lines(tmin.monthly$date, tmin.monthly$gsm001, col="black")
lines(tmin.monthly$date, tmin.monthly$gsm002, col="green")
lines(tmin.monthly$date, tmin.monthly$gsm003, col="orange")
lines(tmin.monthly$date, tmin.monthly$gsm004, col="gray")
lines(tmin.monthly$date, tmin.monthly$gsm005, col="darkturquoise")