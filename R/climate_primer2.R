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

#Output Directory
outdir <- "C:/Share/LCC-VP/ClimatePrimer/figs/"

#list prism and clip to area
ppt.list <- list.files(prism.precip.dir, pattern="*.tif", full.names=T)
ppt.list <- ppt.list[985:length(ppt.list)]
tmax.list <- list.files(prism.tmax.dir, pattern="*.tif", full.names=T)
tmax.list <- tmax.list[985:length(tmax.list)]
tmin.list <- list.files(prism.tmin.dir, pattern="*.tif", full.names=T)
tmin.list <- tmin.list[985:length(tmin.list)]


#Read in climate station data
precip <- read.csv(nps.precip)
tmax <- read.csv(nps.tmax)
tmin <- read.csv(nps.tmin)
meta <- read.dbf(nps.meta)

#Subset out just the date, ID, and nps ranger stations (gsm001 - gsm006)
#Because each table has diff # of records, we'll deal with them separately for now
#Later break down by station.

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
for (i in names(precip.sub[3:8])) {
  for (k in precip.monthly$date) {
    obs <- sum(!is.na(precip.sub[[i]][precip.sub$monyr %in% k]))
    
    if (obs < 20) {
      print(paste(i, " - ", as.yearmon(k), obs))
      precip.monthly[[i]][precip.monthly$date == k] <- NA
    }#end if
  }#end loop
}#end bigger loop
#convert station precip from in to mm
precip.monthly[,3:8] <- precip.monthly[,3:8]*2.54*10

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


#Now that tables have same num of records (bc aggregated to monthly data), 
#pull out indiv station data and put into monthly tables
mon.names <- c("date", "precip", "tmin", "tmax")
print("variables:")
for (st in names(precip.monthly[3:8])) {
  nam <- st
  print(nam)
  st.df <- as.data.frame(cbind(as.yearmon(precip.monthly$date), precip.monthly[[st]], tmin.monthly[[st]], tmax.monthly[[st]]))
  names(st.df) <- mon.names
  st.df$date <- as.yearmon(st.df$date)
  st.df$Prism_precip <- NA
  st.df$Prism_tmax <- NA
  st.df$Prism_tmin <- NA
  assign(nam, st.df)
}


#add prism columns:
#first, put station df's into a list
#st.list <- list(gsm001, gsm002, gsm003, gsm004, gsm005, gsm006)
st.list <- c("gsm001","gsm002","gsm003","gsm004","gsm005","gsm006")
#names(st.list) <- sub.names[3:8]

#coordinates in order 1-6
coords <- cbind(meta$Lon, meta$Lat)

for (n in 1:length(ppt.list)) {
  print(paste("extracting PRISM data to stations for ", gsm001$date[n], sep=""))
  ppt <- raster(ppt.list[n])
  ex.ppt <- extract(ppt, coords)

  tmax <- raster(tmax.list[n])  
  ex.tmax <- extract(tmax, coords)
  
  tmin <- raster(tmin.list[n])  
  ex.tmin <- extract(tmin, coords)
  
  #Fill in each station's extracted prism data based on date - clunky this way.
  #this could almost definitely be an lapply statement, but I was spending too 
  #much time trying to figure it out!
  pos=1
  for (df in st.list) {
    st.df <- get(df)
    st.df$Prism_precip[n] <- ex.ppt[pos]/100
    st.df$Prism_tmax[n] <- ex.tmax[pos]/100
    st.df$Prism_tmin[n] <- ex.tmin[pos]/100
    
    assign(df, st.df)
    
    #reset pos if 6 or more - only 6 stations
    if (pos < 6) {
      pos <- pos+1
    } else {
      pos <- 1
    }#end if
    }#end df for loop
  }#end img list for loop


#Plot station data vs prism modeled data
for (s in st.list) {
  #precip plots
  precip <- get(s)
  outpdf.precip <- paste(outdir,"/station_vs_Prism/", s, "_station_vs_prism_precip_bigtext.pdf", sep="")
  st.name <- meta$Station_Na[match(s, meta$StationID)]
  
  #going to skip gsm006 precip bc 0's and NA's all listed as 0 - inaccurate.
#   max <- max(gsm006$precip, gsm006$Prism_precip, na.rm=T)
#   plot(gsm006$date, gsm006$precip, type="l", lty=2, col="black", ylim=c(0, max.006))
#   lines(gsm006$date, gsm006$Prism_precip, col="red")
  #points(precip.sub$monyr, precip.sub$gsm006)
  
  #plot other 5 stations - starting in 2000
  max.precip <- max(precip[284:length(precip$date), c(2,5)], na.rm=T)
  max.precip <- max.precip + 0.05*max.precip
  pdf(file=outpdf.precip, 7, 5)
  plot(precip$date[284:length(precip$date)], precip$precip[284:length(precip$date)], main=paste("Monthly Aggregated Station vs. Prism Precipitation", "\nStation: ",st.name, sep=""), 
       xlab="Year", ylab="Precipitation (mm)", type="n", ylim=c(0,max.precip), cex=1, cex.main=1, cex.lab=1, cex.axis=1, xaxt="n")
  axis(1,at=seq(2002, 2012,by=2))
  lines(precip$date, precip$Prism_precip, col="blue", lty=2)
  lines(precip$date, precip$precip, col="black", lty=1)
  r2 <- summary(lm(precip$Prism_precip ~ precip$precip, na.action=na.exclude))$adj.r.squared
  #round off and keep trailing zeros if there are any
  r2 <- formatC(round( r2, 2 ), format='f', digits=2 )
  
  #Sort of convoluted way of putting expression text in legend - 
  #code adapted from http://lukemiller.org/index.php/2012/10/adding-p-values-and-r-squared-values-to-a-plot-using-expression/
  rp = vector('expression',1)
  rp[1] = substitute(expression(italic(R)[italic(adj)]^2 == r2), 
                     list(r2 = r2))[2]
  leg.text <- c("GRSM Station", "Prism", rp)
  lty <- c(1, 2, 0)
  col <- c("black", "blue")
  legend("topright", col=col, lty=lty, legend=leg.text, bty="n", cex=0.85)
  dev.off()
  
  #Temp plots - starting in 1991
  #tmax
  temp <- get(s)
  outpdf.temp <- paste(outdir,"/station_vs_Prism/", s, "_station_vs_prism_tmax_bigtext.pdf", sep="")
  
  max.temp <- max(temp[169:length(temp$date), c(4,6)], na.rm=T)
  max.temp <- max.temp + 0.15*max.temp
  
  min.temp <- min(temp[169:length(temp$date), c(4,6)], na.rm=T)
  min.temp <- min.temp + 0.05*min.temp
  
  #For plotting tmin and tmax on same plot
  #min.temp <- min(temp[169:length(temp$date), c(3,7)], na.rm=T)
  #min.temp <- min.temp + 0.05*min.temp
  
  pdf(file=outpdf.temp, 7, 5)
  plot(temp$date[169:length(temp$date)], temp$tmax[169:length(temp$date)], main=paste("Monthly Aggregated Station vs. Prism Tmax", "\nStation: ",st.name, sep=""), 
       xlab="Year", ylab=expression(paste("Temperature ",~degree~C)), type="n", ylim=c(min.temp,max.temp), cex=1, cex.main=1, 
       cex.lab=1, cex.axis=1, xaxt="n")
  axis(1,at=seq(1995, 2010,by=5))
  lines(temp$date, temp$Prism_tmax, col="blue", lty=2)
  lines(temp$date, temp$tmax, col="black", lty=1)
  #lines(temp$date, temp$Prism_tmin, col="red", lty=2)
  #lines(temp$date, temp$tmin, col="gray48", lty=1)
  
  #relationship bt station and modeled data
  r2 <- summary(lm(temp$Prism_tmax ~ temp$tmax, na.action=na.exclude))$adj.r.squared
  #round off and keep trailing zeros if there are any
  r2 <- formatC(round( r2, 2 ), format='f', digits=2 )
  #Sort of convoluted way of putting expression text in legend - 
  #code adapted from http://lukemiller.org/index.php/2012/10/adding-p-values-and-r-squared-values-to-a-plot-using-expression/
  rp = vector('expression',1)
  rp[1] = substitute(expression(italic(R)[italic(adj)]^2 == r2), 
                     list(r2 = r2))[2]
  leg.text <- c("GRSM Station", "Prism", rp)
  lty <- c(1, 2, 0)
  col <- c("black", "blue")
  legend("topright", col=col, lty=lty, legend=leg.text, bty="n", cex=0.85)
  dev.off()

  #tmin
  temp <- get(s)
  outpdf.temp <- paste(outdir,"/station_vs_Prism/", s, "_station_vs_prism_tmin_bigtext.pdf", sep="")
  
  max.temp <- max(temp[169:length(temp$date), c(3,7)], na.rm=T)
  max.temp <- max.temp + 0.3*max.temp
  
  min.temp <- min(temp[169:length(temp$date), c(3,7)], na.rm=T)
  min.temp <- min.temp + 0.05*min.temp
  
  #For plotting tmin and tmax on same plot
  #min.temp <- min(temp[169:length(temp$date), c(3,7)], na.rm=T)
  #min.temp <- min.temp + 0.05*min.temp
  
  pdf(file=outpdf.temp, 7, 5)
  plot(temp$date[169:length(temp$date)], temp$tmin[169:length(temp$date)], main=paste("Monthly Aggregated Station vs. Prism Tmin", "\nStation: ",st.name, sep=""), 
       xlab="Year", ylab=expression(paste("Temperature ",~degree~C)), type="n", ylim=c(min.temp,max.temp), cex=1, cex.main=1, 
       cex.lab=1, cex.axis=1, xaxt="n")
  axis(1,at=seq(1995, 2010,by=5))
  lines(temp$date, temp$Prism_tmin, col="blue", lty=2)
  lines(temp$date, temp$tmin, col="black", lty=1)
  #lines(temp$date, temp$Prism_tmin, col="red", lty=2)
  #lines(temp$date, temp$tmin, col="gray48", lty=1)
  
  #relationship bt station and modeled data
  r2 <- summary(lm(temp$Prism_tmin ~ temp$tmin, na.action=na.exclude))$adj.r.squared
  #round off and keep trailing zeros if there are any
  r2 <- formatC(round( r2, 2 ), format='f', digits=2 )
  #Sort of convoluted way of putting expression text in legend - 
  #code adapted from http://lukemiller.org/index.php/2012/10/adding-p-values-and-r-squared-values-to-a-plot-using-expression/
  rp = vector('expression',1)
  rp[1] = substitute(expression(italic(R)[italic(adj)]^2 == r2), 
                     list(r2 = r2))[2]
  leg.text <- c("GRSM Station", "Prism", rp)
  lty <- c(1, 2, 0)
  col <- c("black", "blue")
  legend("topright", col=col, lty=lty, legend=leg.text, bty="n", cex=0.85)
  dev.off()

}

#remove precip plot for gsm006 - see metadata for reason
unlink(paste(outdir, "gsm006_station_vs_prism_precip_bigtext.pdf", sep=""))


#Replot gsm006 tmax and tmin data, as it has longer record.
temp <- gsm006
#tmax
outpdf.temp <- paste(outdir, "/station_vs_Prism/", "gsm006_station_vs_prism_tmax_bigtext.pdf", sep="")

max.temp <- max(temp[, c(4,6)], na.rm=T)
max.temp <- max.temp + 0.15*max.temp

min.temp <- min(temp[, c(4,6)], na.rm=T)
min.temp <- min.temp + 0.05*min.temp

#For plotting tmin and tmax on same plot
#min.temp <- min(temp[169:length(temp$date), c(3,7)], na.rm=T)
#min.temp <- min.temp + 0.05*min.temp

pdf(file=outpdf.temp, 7, 5)
plot(temp$date, temp$tmax, main=paste("Monthly Aggregated Station vs. Prism Tmax", "\nStation: ",st.name, sep=""), 
     xlab="Year", ylab=expression(paste("Temperature ",~degree~C)), type="n", ylim=c(min.temp,max.temp), cex=1, 
     cex.main=1, cex.lab=1, cex.axis=1, xaxt="n")
axis(1,at=seq(1980, 2010,by=5))
lines(temp$date, temp$Prism_tmax, col="blue", lty=2)
lines(temp$date, temp$tmax, col="black", lty=1)
#lines(temp$date, temp$Prism_tmin, col="red", lty=2)
#lines(temp$date, temp$tmin, col="gray48", lty=1)

#relationship bt station and modeled data
r2 <- summary(lm(temp$Prism_tmax ~ temp$tmax, na.action=na.exclude))$adj.r.squared
#round off and keep trailing zeros if there are any
r2 <- formatC(round( r2, 2 ), format='f', digits=2 )

#Sort of convoluted way of putting expression text in legend - 
#code adapted from http://lukemiller.org/index.php/2012/10/adding-p-values-and-r-squared-values-to-a-plot-using-expression/
rp = vector('expression',1)
rp[1] = substitute(expression(italic(R)[italic(adj)]^2 == r2), 
                   list(r2 = r2))[2]
leg.text <- c("GRSM Station", "Prism", rp)
lty <- c(1, 2, 0)
col <- c("black", "blue")
legend("topright", col=col, lty=lty, legend=leg.text, bty="n", cex=0.85)
dev.off()

#tmin
outpdf.temp <- paste(outdir, "/station_vs_Prism/","gsm006_station_vs_prism_tmin_bigtext.pdf", sep="")

max.temp <- max(temp[, c(3,7)], na.rm=T)
max.temp <- max.temp + 0.4*max.temp

min.temp <- min(temp[, c(3,7)], na.rm=T)
min.temp <- min.temp + 0.05*min.temp

#For plotting tmax and tmin on same plot
#min.temp <- min(temp[169:length(temp$date), c(3,7)], na.rm=T)
#min.temp <- min.temp + 0.05*min.temp

pdf(file=outpdf.temp, 7, 5)
plot(temp$date, temp$tmin, main=paste("Monthly Aggregated Station vs. Prism Tmin", "\nStation: ",st.name, sep=""), 
     xlab="Year", ylab=expression(paste("Temperature ",~degree~C)), type="n", ylim=c(min.temp,max.temp), cex=1, 
     cex.main=1, cex.lab=1, cex.axis=1, xaxt="n")
axis(1,at=seq(1980, 2010,by=5))
lines(temp$date, temp$Prism_tmin, col="blue", lty=2)
lines(temp$date, temp$tmin, col="black", lty=1)
#lines(temp$date, temp$Prism_tmin, col="red", lty=2)
#lines(temp$date, temp$tmin, col="gray48", lty=1)

#relationship bt station and modeled data
r2 <- summary(lm(temp$Prism_tmin ~ temp$tmin, na.action=na.exclude))$adj.r.squared
#round off and keep trailing zeros if there are any
r2 <- formatC(round( r2, 2 ), format='f', digits=2 )

#Sort of convoluted way of putting expression text in legend - 
#code adapted from http://lukemiller.org/index.php/2012/10/adding-p-values-and-r-squared-values-to-a-plot-using-expression/
rp = vector('expression',1)
rp[1] = substitute(expression(italic(R)[italic(adj)]^2 == r2), 
                   list(r2 = r2))[2]
leg.text <- c("GRSM Station", "Prism", rp)
lty <- c(1, 2, 0)
col <- c("black", "blue")
legend("topright", col=col, lty=lty, legend=leg.text, bty="n", cex=0.85)
dev.off()
