library(raster)
source("C:/Share/LCC-VP/scripts/R/LCC-VP_functions.R")

imgdir <- "C:/Share/LCC-VP/Parks/GRSM/climate_data/PRISM_historical/adjusted_month_yearly/temp/"
outdir <- "C:/Share/LCC-VP/Parks/GRSM/climate_data/PRISM_historical/adjusted_month_yearly/"
imgext <- "tif"

#image for comparison of NA values/extent/snapping etc. 
refimg <- "C:/Share/LCC-VP/Parks/GRSM/climate_data/current_PRISM/biovars/biovars_1981-2010.tif"

vars <- c("ppt", "tmin", "tmax")

for (i in vars) {
  vardir <- paste0(imgdir, i, "/")
  varout <- paste0(outdir, i, "/")
  stackMonthToAnnual(vardir, varout, imgext, refimg)
  
}#end for
