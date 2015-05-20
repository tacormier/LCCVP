# Call packages
require(sp)
require(data.table)
require(rgdal)
require(raster)
require(randomForest)
require(mgcv)
source("C:/Share/pjantz/Scripts/LCCVP/handy_functions.R")
source("C:/Share/pjantz/Scripts/LCCVP/JEvans_RF_Functions.R")

# Species code
scode <- "318"
print(scode)

# Species point shapefile
# [1] "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
sfilename <- paste0("sp_",scode,"_pts")

spp <- readOGR("C:/Share/LCC-VP/RangeWide/FIA/bySpecies",layer=sfilename)
# spp <- spTransform(spp,CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))
spp <- spTransform(spp,CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
spp <- spp[order(spp@data$PID),]

# MAKE 20KM LATTICE
# 20km lattice for visualizing spatial calibration
xdim <- ceiling((extent(spp)@xmax - extent(spp)@xmin)/20000)
ydim <- ceiling((extent(spp)@ymax - extent(spp)@ymin)/20000)

grd <- GridTopology(cellcentre.offset=c(extent(spp)@xmin,extent(spp)@ymin), cellsize=c(20000,20000), cells.dim=c(xdim,ydim))
SpP_grd <- as.SpatialPolygons.GridTopology(grd)
trdata <- data.frame(ID=c(1:dim(coordinates(SpP_grd))[1]),
                     row.names=sapply(slot(SpP_grd, "polygons"), function(i) slot(i, "ID")))
SpPDF <- SpatialPolygonsDataFrame(SpP_grd, trdata)
proj4string(SpPDF) <- CRS(projection(spp))
writeOGR(SpPDF,"C:/Share/LCC-VP/RangeWide/FIA",layer="lattice20k",driver="ESRI Shapefile")

# READ 20KM LATTICE
lat20k <- readOGR("C:/Share/LCC-VP/RangeWide/FIA",layer="lattice20k")

# REMOVE DUPLICATE POINTS OR DISTANT POINTS
# Get indices of any points farther than 200km from any other point
sppsset <- spp[spp@data$PRES==1,]
sdists <- spDists(sppsset,longlat=T)
diag(sdists) <- NA
sdists <- apply(sdists,1,min,na.rm=T)
if (length(which(sdists > 200000))) {
  # Get indices of isolated points
  del.dist <- which(spp@data$PID==sppsset[which(sdists > 200000),"PID"]$PID)
  print(length(del.dist))
} else {
  del.dist <- c() # Make an empty vector if there are no isolated points
}

# Read in predictors
pframe <- fread("C:/Share/LCC-VP/Rangewide/fia_35_predictors.csv")
pframe[,V1:=NULL]
pframe <- pframe[order(PID),]

# Find cells with multiple plots and delete plots that are coded as zero if there
# is a plot in the same cell with a presence
# Use gdd as the grid template raster
gdd <- raster("C:/Share/LCC-VP/RangeWide/Climate/gdd_PRISM_30yr_norm_x100.tif")
dups <- extract(gdd,spp,cellnumbers=T)

# Winnowing function to get rid of conflicting zero plots
winnow <- function(x){
  inds <- which(dups[,'cells']==x)
  if (diff(range(spp@data[inds,'PRES'])) > 0) {
    z <- inds[which(spp@data[inds,'PRES']==0)]
    return(z)
  }
}

del.dups <- sapply(dups[,'cells'][duplicated(dups[,'cells'])], winnow)
del.dups <- unlist(del.dups[sapply(del.dups,function(x) !is.null(x))])

# Add the distance based indices to the deletion set
del.dups <- c(del.dups,del.dist)

# Subset points
spp <- spp[-c(del.dups),]

# SPATIAL JOIN
# Get polygon IDs that overlap with points and attribute IDs to point file
spp <- IntersectPtWithPoly(spp, lat20k)

# Find number of points in each polygon
aa <- tapply(spp$ID, spp$ID, length)
# Get rid of those with less than 5 observations
aa <- aa[aa>=5]
spp2 <- spp[spp$ID %in% names(aa),]

# Extract suitability values to points
sppgrid <- raster("C:/Share/LCC-VP/RangeWide/FIA/RF_spp_out_new/s318_rcp45_pa_best_temp_19812010.tif")
spp2 <- spTransform(spp2, CRS(projection(sppgrid)))
vals <- extract(sppgrid,spp2)

# Calculate fraction of points in each cell as present
prop.obs <- tapply(spp2$PRES,spp2$ID,sum)/tapply(spp2$PRES,spp2$ID,length)
# Calculate suitability at points
prop.pred <- tapply(vals,spp2$ID,mean)

# Make spatial calibration info into a dataframe
sp.cal <- data.frame(ID=names(prop.obs),obs=prop.obs,pred=prop.pred)

# Make columns in lattice to hold spatial calibration data
lat20k$obs <- NA
lat20k$pred <- NA

# Calculate columns
lat20k$obs[lat20k$ID %in% sp.cal$ID] <- sp.cal$obs
lat20k$pred[lat20k$ID %in% sp.cal$ID] <- sp.cal$pred

# Write to file
writeOGR(lat20k,"C:/Share/LCC-VP/RangeWide/FIA",layer="lattice20k_318",driver="ESRI Shapefile")






