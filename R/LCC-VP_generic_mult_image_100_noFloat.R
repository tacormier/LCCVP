library(raster)

# # main dir with image data
img.indir <- 'C:/Share/LCC-VP/RangeWide/TWI/30m/floats/'
# 
# #output biovars directory
img.outdir <- 'C:/Share/LCC-VP/RangeWide/TWI/30m/'
# 
####################################################################################### 

imgs <- list.files(img.indir, pattern='*twi\\.tif$', full.names=T)

for (i in imgs) {
  print(paste("unfloating ", i, sep=""))
  img <- round(raster(i)*100)
  outimg <- paste(img.outdir, basename(i), sep="")
  writeRaster(img, outimg, format="GTiff", dataType="INT4S", overwrite=T)
  
}