
# This appears to be some sort of manipulation of NPS veg data

# Get reference projection
refproj <- projection(raster("C:/Share/LCC-VP/RangeWide/RFoutput/binary_rasters/s261_GRSM_PACE_rcp85_pa_best_temp_eth.tif",
                             band=1))

# Read in park boundary
AOA <- readOGR("C:/Share/LCC-VP/ALCC_PACE_Park_boundaries","GRSM_boundary_albers")
AOA <- spTransform(AOA,CRS(refproj))

# NPS FIELD DATA
# Get species data from NPS/USGS inventory
parkspp <- read.csv("C:/Share/LCC-VP/Parks/GRSM/NPS_data/field_data/sp_cov.csv")

# Plot data
parkplots <- read.csv("C:/Share/LCC-VP/Parks/GRSM/NPS_data/field_data/plots.csv")
# Get only plots with coordinates
parkplots <- parkplots[complete.cases(parkplots$Field_X),]


# Declare coordinate system
EPSG <- make_EPSG()
cosys <- CRS(subset(EPSG, code==26717)$prj4)

# Make spatial points data frame
coos <- cbind(parkplots$Field_X,parkplots$Field_Y)
colnames(coos) <- c("coords.x1","coords.x2")
coordinates(parkplots) <- coos
projection(parkplots) <- cosys

# Loop through species
snames <- c('Tsuga canadensis (L.) Carr.')
scd <- c(261)

for (i in c(1:length(snames))) {
  # Grab plots with the target species
  preslocs <- parkspp[which(parkspp$Field_Name == snames[i]),]
  # Get rid of non-unique plot observations (only want one entry per plot)
  preslocs <- preslocs[!duplicated(preslocs$Plot_Event),]
  # Add a field for presence/absences of target species
  preslocs$PRES <- 1
    
  # Link up species with plots
  vpts <- merge(parkplots[,c("Plot_Event","Elevation")],preslocs,by="Plot_Event",sort=F,all.x=T)
  vpts <- spTransform(vpts,CRS(refproj))
  
  # Get only points located in the Park
  vpts <- vpts[complete.cases(over(vpts,AOA)[,2]),]

  # Change PRES NA values to zero
  vpts$PRES[is.na(vpts$PRES)] <- 0
  
  # Get raster for comparison
  rast <- raster(paste0("C:/Share/LCC-VP/RangeWide/RFoutput/binary_rasters/s",scd[i],"_GRSM_PACE_rcp85_pa_best_temp_eth.tif"),
                 band=1)
  
  # Extract raster to points
  nps.comp <- extract(rast,vpts,cellnumbers=T,df=T)
  
  names(nps.comp) <- c("ID","CELLNO","MODPRES")
  
  nps.comp$OBS <- vpts$PRES
 
  #adf <- data.frame(ID=pts.prob$ID,OBS=pts$PRES,PROB=pts.prob$PROB)
  # Get rid of cells containing multiple points and NAs
  pts.prob.sub <- pts.prob[!pts.prob$ID %in% pts.prob$ID[duplicated(pts.prob$CELLNO)],]
  pts.prob.sub <- pts.prob.sub[complete.cases(pts.prob.sub),]
  
  
}

