library(raster)

deltas <- "C:/Share/LCC-VP/Parks/DEWA/climate_data/forecasts/ave_30yr_byYear/deltas/p50_rcp45/"
unadj <- "C:/Share/LCC-VP/Parks/DEWA/climate_data/forecasts/ave_30yr_byYear/p50_rcp45/"
prism <- "C:/Share/LCC-VP/Parks/DEWA/climate_data/current_PRISM/"

params <- c("ppt", "tmin", "tmax")
time <- c(1996, 2021:2086)

for (i in params) {
  in.delta <- paste(deltas, i, "/", sep="")
  in.unadj <- paste(unadj, i, "/", sep="")
  in.prism <- paste(prism, i, "/stack/", sep="")
  
  delta.list <- list.files(in.delta, pattern="*a.tif$", full.names=T)
  unadj.list <- list.files(in.unadj, pattern="*.tif$", full.names=T)
  prism.list <- list.files(in.prism, pattern="*.tif$", full.names=T)
  
  p <- brick(prism.list[1])
  d <- stack()
  u <- stack()
#   #GRRR issue with ppt grids - weren't properly adjusted to total monthly ppt
#   days <- c(31,28,31,30,31,30,31,31,30,31,30,31)

  #Take the mean of each year and stack them all together
   for (img in c(1:length(delta.list))) {
     d1 <- mean(brick(delta.list[img]))
     d <- stack(d, d1)
     u1 <- mean(brick(unadj.list[img]))
     u <- stack(u, u1)
  }#end img loop
  
  #stack all d's and u's - had issues again with extents, thus the crop.
  d.stack <- stack(mean(p), crop(d, p))
  u.stack <- stack(mean(p), crop(u, p))
  
  #prism had diff NA value, thus the mask
  d.stack <- as.data.frame(mask(d.stack, d.stack[[1]],maskvalue= -32768))
  u.stack <- as.data.frame(mask(u.stack, u.stack[[1]],maskvalue= -32768))
  
  d.mean <- apply(d.stack,MARGIN=2, FUN=mean, na.rm=T)
  plot(time, d.mean, type="l", col="blue", xlab="year", 
       ylab=paste(i, " (ppt mm*100)", sep=""), main=paste("DEWA Park \n Adjusted Values", sep=""))
  points(time,d.mean, pch=16)
  
  u.mean <- apply(u.stack,MARGIN=2, FUN=mean, na.rm=T)
  plot(time, u.mean, type="l",col="blue", xlab="year", 
       ylab=paste(i, " (mm*100)", sep=""), main=paste("GRSM Park \n Uadjusted Values", sep=""))
  points(time,u.mean, pch=16)
  
    
    
    #combine prism with deltas
    d.all <- stack(p, )
    
    #combine prism with unadj
  
}#end params loop