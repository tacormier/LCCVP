# Author: Patrick Jantz
# Date: 11/20/14
# Purpose: QAQC on adjusted 30 year averages of vpd calculated using NCO Tools on NEX

# This script extracts VPD data from a list of rasters for an area of interest 
# and graphs the resulting time series. If those check out, the script can also be use to convert
# netcdfs to tiffs while converting some values (e.g. -9999) to NA values.

# ----------------------------------------------------
# Get some functions and packages
source("C:/Share/pjantz/Scripts/LCCVP/handy_functions.R")
#or
# source("//ARCTIC/C_Drive/Share/pjantz/Scripts/LCCVP/handy_functions.R")

require(sp)
require(rgdal)
require(raster)

# Path to historical rasters
# dat.path <- "//ARCTIC/C_Drive/Share/LCC-VP/TOPS/EAST/Projections"
dat.path <- "//ARCTIC/C_Drive/Share/LCC-VP/TOPS/EAST/VPD_Test"
setwd(dat.path)

# Set AOA path
PACE.path <- "//ARCTIC/C_Drive/Share/LCC-VP/ALCC_PACE_Park_boundaries/GRSM_pace_albers.shp"
# PACE.path <- "C:/Share/LCC-VP/ALCC_PACE_Park_boundaries/SHEN_pace_albers.shp"
# PACE.path <- "C:/Share/LCC-VP/ALCC_PACE_Park_boundaries/DEWA_pace_albers.shp"

# Raster coordinate system (! check this for each case)
canomproj <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# Reproject PACE shapefile to match raster coordinate system
AOA <- readOGR(pathpop(PACE.path,1),sub("[.][^.]*$", "", basename(PACE.path), perl=TRUE))
AOA <- spTransform(AOA,canomproj)

# -------------
# List, extract, and graph values from vpd rasters
dat.vec <- list()
for (i in c("ann","jja","son","djf","mam")) {
  
  flist <- list.files(pattern=paste0("^earcp85.*",i,".*.nc"))
  #flist2 <- list.files(pattern="^prism.*ann.nc")
  #flist <- c(flist2,flist)

  # Calculate zonal means of rasters over the AOA
  dat.vec[[i]] <- unlist(lapply(flist, function(x) mean(unlist(extract(raster(x),AOA)))))
}

plot(c(1982:2040),dat.vec[["ann"]],type="l",ylim=c(400,1500),col="darkgray",lwd=3,xlab="First Year of 30yr Average",ylab="VPD (pascals)")
lines(c(1982:2040),dat.vec[["jja"]],col="red",lwd=3)
lines(c(1982:2040),dat.vec[["son"]],col="brown",lwd=3)
lines(c(1982:2040),dat.vec[["djf"]],col="blue",lwd=3)
lines(c(1982:2040),dat.vec[["mam"]],col="green",lwd=3)
legend(1982,1500,legend=c("Annual","JJA","SON","DJF","MAM"),col=c("darkgray","red","brown","blue","green"),lwd=3,cex=0.6)

# Saved manually...

# -------------
# Convert values to NA and save as TIFF

# Set working directory
dat.path <- "//ARCTIC/C_Drive/Share/LCC-VP/TOPS/EAST/Projections"
setwd(dat.path)

# Loop through seasonal and annual aggregations and make conversions
rlist <- list()
for (i in c("ann","jja","son","djf","mam")) {
  print(i)
  flist <- list.files(pattern=paste0("^earcp85.*",i,".*.nc"))
  rlist[[i]] <- lapply(flist, rasternafun, dat.path, -9999)
}








