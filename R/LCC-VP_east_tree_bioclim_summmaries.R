# This script does something with bioclimatic variables but I'm not sure what
# THere are useful snippets, however, for future reference

# Check bioclimatic variables
for (i in list(12,97,110,111,121,123,126,129,131,132,261,315,316,317,318,319,332,371,403,407,408,409,531,541,602,611,621,746,762,802,824,832,833,835,837,951,971,972)) {
  
  # Load packages
  require(sp)
  require(raster)
  require(randomForest)
  require(mgcv)
  require(rgdal)
  require(PresenceAbsence)
  
  # Command line arguments
  rcp <- "rcp85" # RCP
  vset <- "best"
  scode <- i # Species code
  year <- c(1,2,3) # Should be baseline climate (1981-2010), future centered on 2055 and 2080
  AOA <- "DEWA"
  
  # BIOCLIM MAPPING
  # Map biclimate variables for a future time period
  
  # Load model objects
  # Best variables
  modfile <- paste0("C:/Share/LCC-VP/Rangewide/RFoutput/s",scode,"_pa_mod",vset,"temp_rfobject.RData")
  load(modfile)
  
  # Load extent object
  # variable name == latlonminext
  load("C:/Share/LCC-VP/Rangewide/latlonminextent_object.RData")
  
  # BEST VARIABLES MODEL
  # Get variables used in model
  vlist <- rownames(mod.best.temp$importance)
  
  # Predictor grids
  pgrids <- list()
  
  # bioclimatic variables
  ingrids <- paste0("C:/Share/LCC-VP/RangeWide/Climate/",c("ea30_nexdcp_es_rcp85_bioclim_2030_delta.nc",
                                               "ea30_nexdcp_es_rcp85_bioclim_2050_delta.nc",
                                               "ea30_nexdcp_es_rcp85_bioclim_2080_delta.nc"))
  
  # Load Extent
  AOA <- readOGR("C:/Share/LCC-VP/ALCC_PACE_Park_boundaries",paste0(AOA,"_pace_albers"))
  
  # Reproject AOA
  # Coordinate system
  canomproj <- CRS(projection(raster(ingrids[[1]])))

  # Reproject PACE shapefile to match raster coordinate system
  AOA <- spTransform(AOA,canomproj)
  
  for (i in c(1:3)) {
    inbio <- crop(ingrids[i],extent(AOA))
    
    if (length(vlist[grep("bio",vlist)]) > 0) {
      if (length(vlist[grep("bio",vlist)]) > 1) {
        # brick 19 bioclimate variables
        bclim <- brick(inbio)
        # drop ones not used in the model
        inds <- !c(1:19) %in% as.numeric(unlist(lapply(strsplit(vlist[grep("bio",vlist)],"bio"), function(x) x[2])))
        bclim <- dropLayer(bclim,c(1:19)[inds])
        bclim <- bclim
        names(bclim) <- paste0("bio",c(1:19)[!inds])
      } else {
        inds <- c(1:19) %in% as.numeric(unlist(lapply(strsplit(vlist[grep("bio",vlist)],"bio"), function(x) x[2])))
        bclim <- raster(inbio,band=c(1:19)[inds])
        bclim <- bclim
        names(bclim) <- paste0("bio",c(1:19)[inds])
      }
 
      pgrids <- c(pgrids,bclim)
      
    }
  }
}


# Grab bioclim vars for current period, crop to the eastern U.S. and then brick
aa <- load("C:/Share/LCC-VP/RangeWide/latlonminextent_object.RData")
# glist <- list()
for (i in 1:19) {
  inbclim <- paste0("C:/Share/LCC-VP/RangeWide/PRISM_30yr_norm_x100_bio",i,"_pro.tif")
  inbclim <- raster(inbclim)
  inbclim <- crop(inbclim,get(aa))
  inbclim <- inbclim/100
  glist <- c(glist,inbclim)
}

gbrick <- brick(glist)
#writeRaster(gbrick,"C:/Share/LCC-VP/RangeWide/PRISM_30yr_norm_bioclim.nc",driver="CDF")

# MAT, prism 800m 30 year normals
prismbio1 <- raster(paste0("C:/Share/LCC-VP/RangeWide/PRISM_30yr_norm_x100_bio",1,"_pro.tif"))
prismbio1 <- crop(prismbio1,get(aa))
prismbio1 <- prismbio1/100
r <- calc(prismbio1, fun=function(x){x / 100})

# MAT 30yr avg. delta rcp85 centered on 2080
bclim2080 <- raster("C:/Share/LCC-VP/RangeWide/Climate/ea30_nexdcp_es_rcp85_bioclim_2080_delta.nc")

# MAT 30yr avg. no delta rcp85 centered on 2080
bclim2080unadj <- rotate(raster("C:/Share/LCC-VP/RangeWide/Climate/rcp85_30yravg.nc"))
extent(bclim2080unadj) <- extent(bclim2080)

# Crop 2080 layers
bclim2080 <- crop(bclim2080,get(aa))
bclim2080unadj <- crop(bclim2080unadj,get(aa))

# Subtract PRISM from each 2080 layer
bclimdiff <- bclim2080-prismbio1
bclimdiffunadj <- bclim2080unadj-prismbio1

par(mfrow=c(1,2))
par(oma=c(0,0,0,0))
par(mar=c(2,0,2,2))
# Plot difference
plot(bclimdiff,useRaster=F)
plot(bclimdiffunadj,useRaster=F)


# Bio5, prism 800m 30 year normals
prismbio5 <- raster(paste0("C:/Share/LCC-VP/RangeWide/PRISM_30yr_norm_x100_bio",5,"_pro.tif"))
prismbio5 <- crop(prismbio5,get(aa))
prismbio5 <- prismbio5/100

# MAT 30yr avg. delta rcp85 centered on 2080
bclim5.2080 <- stack("C:/Share/LCC-VP/RangeWide/Climate/ea30_nexdcp_es_rcp85_bioclim_2080_delta.nc")
bclim5.2080 <- bclim5.2080[[5]]

# MAT 30yr avg. no delta rcp85 centered on 2080
bclim5.2080unadj <- stack("C:/Share/LCC-VP/RangeWide/Climate/rcp85_30yravg.nc")
bclim5.2080unadj <- bclim5.2080unadj[[5]]

extent(bclim5.2080unadj) <- extent(bclim5.2080)

# Crop 2080 layers
bclim5.2080 <- crop(bclim5.2080,get(aa))
bclim5.2080unadj <- crop(bclim5.2080unadj,get(aa))

# Subtract PRISM from each 2080 layer
bclim5.diff <- bclim5.2080-prismbio5
bclim5.diffunadj <- bclim5.2080unadj-prismbio5

par(mfrow=c(1,2))
par(oma=c(0,0,0,0))
par(mar=c(2,0,2,2))
# Plot difference
plot(bclim5.diff,useRaster=F)
plot(bclim5.diffunadj,useRaster=F)


# BIO12, prism 800m 30 year normals
prismbio12 <- raster(paste0("C:/Share/LCC-VP/RangeWide/PRISM_30yr_norm_x100_bio",12,"_pro.tif"))
prismbio12 <- crop(prismbio12,get(aa))
prismbio12 <- prismbio12/100 #mm/year

# MAT 30yr avg. delta rcp85 centered on 2080
bclim12.2080 <- stack("C:/Share/LCC-VP/RangeWide/Climate/ea30_nexdcp_es_rcp85_bioclim_2080_delta.nc")
bclim12.2080 <- bclim12.2080[[12]] #mm/year

# MAT 30yr avg. no delta rcp85 centered on 2080
bclim12.2080unadj <- stack("C:/Share/LCC-VP/RangeWide/Climate/rcp85_30yravg.nc")
bclim12.2080unadj <- bclim12.2080unadj[[12]]

extent(bclim12.2080unadj) <- extent(bclim12.2080)

# Crop 2080 layers
bclim12.2080 <- crop(bclim12.2080,get(aa))
bclim12.2080unadj <- crop(bclim12.2080unadj,get(aa))

# Subtract PRISM from each 2080 layer
bclim12.diff <- bclim12.2080-prismbio12
bclim12.diffunadj <- bclim12.2080unadj-prismbio12

par(mfrow=c(1,2))
par(oma=c(0,0,0,0))
par(mar=c(2,0,2,2))
# Plot difference
plot(bclim12.diff,useRaster=F)
plot(bclim12.diffunadj,useRaster=F)
