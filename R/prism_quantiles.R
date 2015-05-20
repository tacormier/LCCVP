# This script extracts spatially averaged quantiles of mean annual temperature for an area of interest
# for a period of interest

source("C:/Share/pjantz/Scripts/LCCVP/handy_functions.R")
require(sp)
require(rgdal)
require(raster)

# Path to historical PRISM
# prism.path <- "C:/Share/pjantz/grsm_hist"
# prism.path <- "C:/Share/pjantz/shen_hist"
prism.path <- "C:/Share/pjantz/dewa_hist"
setwd(prism.path)

# Output Directory
# outdir <- "C:/Share/pjantz/GRSM_Climate_Primer/"
# outdir <- "C:/Share/pjantz/SHEN_Climate_Primer/"
outdir <- "C:/Share/pjantz/DEWA_Climate_Primer/"

# Output file
# outfile <- "grsm_prism_1980_2005_quants.csv"
# outfile <- "shen_prism_1980_2005_quants.csv"
outfile <- "dewa_prism_1980_2005_quants.csv"

# Set AOA path
# PACE.path <- "C:/Share/LCC-VP/ALCC_PACE_Park_boundaries/GRSM_pace_albers.shp"
# PACE.path <- "C:/Share/LCC-VP/ALCC_PACE_Park_boundaries/SHEN_pace_albers.shp"
PACE.path <- "C:/Share/LCC-VP/ALCC_PACE_Park_boundaries/DEWA_pace_albers.shp"

# Set AOA path
# PACE.path <- "C:/Share/LCC-VP/ALCC_PACE_Park_boundaries/GRSM_pace_albers.shp"
# PACE.path <- "C:/Share/LCC-VP/ALCC_PACE_Park_boundaries/SHEN_pace_albers.shp"
PACE.path <- "C:/Share/LCC-VP/ALCC_PACE_Park_boundaries/DEWA_pace_albers.shp"

# PRISM coordinate system
canomproj <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# Reproject PACE shapefile to match climate anomaly coordinate system
AOA <- readOGR(pathpop(PACE.path,1),sub("[.][^.]*$", "", basename(PACE.path), perl=TRUE))
AOA <- spTransform(AOA,canomproj)

# PRISM calculations (1895 - 2005)


# Grab only files with a certain name length (longer names are files that are averaged across years)
inds <- lapply(list.files(pattern="^(tasmean)"),nchar)==20

# Create file list (should run from 1895 - 2005)
flist <- list.files(pattern="^(tasmean)")[inds]

# Grab only 1980 - 2005 files
flist <- flist[(length(flist)-25):length(flist)]

# Average monthly temperature to get mean annual temperature
# Brick the files and take the mean of the brick
# This creates a layer where each pixel is the mean annual temperature
# over the time period
p.ann <- unlist(lapply(flist, function(x) mean(brick(x))))

# Calculate quantiles
# This should generate a brick where each layer corresponds
# to a quantile. Default probabilities are 0 - 1 in 0.25 increments
# giving 0%, 25%, 50%, 75%, and 100% percentiles
quants <- calc(brick(p.ann), fun=quantile)

# Extract and spatially average quantiles
# Each quantile corresponds to a column
quants.mat <- extract(quants,AOA)[[1]]

# Get averages across columns
quants.savg <- apply(quants.mat,2,mean)

# Write matrix to csv
write.csv(quants.savg,file=paste(outdir,"/",outfile,sep=""))
          