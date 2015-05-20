
# This script creates a random sample of indices corresponding to an 80/20 split
# of a data frame.
# It then uses those indices to subset a series of point files to create
# training/testing datasets.

require(raster)
require(sp)


indir <- "C:/Share/LCC-VP/RangeWide/FIA/bySpecies"
outdir <- "C:/Share/LCC-VP/RangeWide/FIA/bySpecies8020"

flist <- as.list(list.files(path=indir,pattern=".shp"))

# Read in first file and randomly sample it to get indices for subsetting
# the other files
mysample <- readOGR(indir,strsplit(flist[[1]],split="[.]")[[1]][1])
ssize <- as.integer(dim(mysample)[1]*0.8)
mysample <- mysample[sample(1:nrow(mysample), ssize,replace=FALSE),]
# Get unique identifier
uid <- mysample$PID

ssetptfun <- function(x,y=uid,z=indir,q=outdir) {
  print(x)
  indir <- z
  outdir <- q
  uid <- y
  ptfile <- readOGR(indir,strsplit(x,split="[.]")[[1]][1])
  ptfile.train <- ptfile[c(ptfile$PID %in% uid),]
  writeOGR(ptfile.train,outdir,strsplit(x,split="[.]")[[1]][1],driver="ESRI Shapefile")
  
  ptfile.test <- ptfile[c(!ptfile$PID %in% uid),]
  writeOGR(ptfile.test,outdir,paste0(strsplit(x,split="[.]")[[1]][1],"_test"),driver="ESRI Shapefile")
  
}

aa <- lapply(flist,ssetptfun)


lith <- raster("C:/Share/LCC-VP/Theobald/LandFacets/ergo5/ergo5_20140416/us_lith_90s")
