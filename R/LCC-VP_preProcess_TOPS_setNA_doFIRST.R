#quick script to loop over a list of images and set -999 to NA
#For TOPS data, need to do this BEFORE reprojecting and clipping bc we are
#using bilinear interpolation to resample during reprojection, which then
#gives us weird values where it's -999 because factored into pixel values and shouldn't be.
library(raster)

indir <- "C:/Share/LCC-VP/TOPS/EAST/NewVPD19812010/"
outdir <- "C:/Share/LCC-VP/TOPS/EAST/NA_set/"
NAval <- -9999

x <- list.files(indir, pattern="*\\.tif$", full.names=T)
x <- x[which(!grepl("pro", x, ignore.case=TRUE))]


x#loop over and set -999 to NA
for (i in x) {
  img <- stack(i)
  img[img==NAval] <- NA
  outimg <- paste0(outdir,basename(i))
  writeRaster(img, filename=outimg, overwrite=T)
  
}