#Stupid python reproj/clipping script, on top of having a zillion unreasonable errors,
#also output a few images with different extents. Makes zero sense, but this little 
#script can help sort out which ones need to be re-run.  Re-running them though the same script
#fixes it.  GRRR, death to arcpy.

library(raster)

dir <- "C:/Share/LCC-VP/Parks/GRSM/climate_data/forecasts/adjusted_month_yearly/p50_rcp85/tmin/"
img.list <- list.files(dir, pattern="*.tif$", full.names=T)
bioclim.outdir <- "C:/Share/LCC-VP/Parks/GRSM/climate_data/forecasts/ave_30yr_byYear/"
date <- gsub(":", "", gsub(" ", "_", date()))

log.file <- file(paste(bioclim.outdir,"calc_30yr_biovars_EXTENT_LIST_GRSM", date, "_log.txt",sep=""))
sink(log.file)
sink(log.file, type="message")
test <- brick(img.list[1])
for (i in img.list) {
  #print(i)
  b <- brick(i)
  #print(b@extent)
  if (b@extent != test@extent) {
    print (i)
    print(b@extent)
  }
}

sink()
sink(type="message")
