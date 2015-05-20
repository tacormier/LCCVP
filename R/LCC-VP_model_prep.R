library(raster)
library(maptools)
library(rasterVis)

#source parameter file for input and output directories
source("C:/Share/LCC-VP/scripts/R/LCC-VP_parameters.R")


# redirect output to file (for selected variables) - FIX: sinking to log file
#is not appending - just overwriting. Not sure why.
#For now, decided to just write unique log file each time this script is run. 
#Be careful not to let logs pile up. 
date.now <- gsub(":", "", gsub(" ", "_", date()))
log.file <- file(paste(path,"logs/model_prep_", paste(spp, collapse="-"), "_", date.now, "_log.txt",sep=""))
sink(log.file)
sink(log.file, type="message")
print("*********************************************************************")
print("******** LCC-VP_call_model_prep_python.R Log ********")
print("*********************************************************************")

print(date.now)
print(park)
print(paste("biovars image = ", biovars, sep=""))
print(paste("unique PRISM grid = ", zonesPRISM, sep=""))
print(paste("fine scale veg classification shapefile/fc = ", veg, sep=""))
print(paste("resolution of rasterized veg classification = ", res, sep=""))
print(paste("Name of field in veg class that holds class info = ", classFld, sep=""))
print("Models will be generated for the following classes: ")
print(classes)
print("")
print(paste("path to analysis directory (for outputs) is ", path, sep=""))
print(paste("list of spp directory names: ", sep=""))
print(spp)
print("")
print(paste("Will a map be produced? ", map, sep=""))
print(paste("model now or just prep layers? ", model, sep=""))
print(paste("output directory = ", subDir, sep=""))
print(paste("List of bands in biovars image is here: ", biovars.names, sep=""))
print(paste("Forecast now?  ", forecast, sep=""))
print(paste("Directory containing forecasted predictors", forecast.dir, sep=""))

#Create spp directories if they don't already exist
for (i in 1:length(spp)) {
  #if the spp directory doesn't exist, create it. Can ignore warnings if it DOES exist.
  analysis.dir <- (paste(path, spp[i], sep=""))
  dir.create(analysis.dir, showWarnings=FALSE)
  
  #also create input and output directories
  dir.create(paste(analysis.dir, "inputs", subDir, sep="/"), showWarnings=FALSE)
  dir.create(paste(analysis.dir, "outputs", subDir, sep="/"), showWarnings=FALSE)
}


#Calculate res factor for % cover calcs. Based on 800 m (or 640,000 m2) PRISM pixel.
res.factor <- as.character(640000/as.numeric(res)^2 / 100) 

#open veg polys
veg.poly <- readShapePoly(veg)

#open ref layer and resample to specified resolution
ref <- raster(biovars)
#set all values to 1, just for ease of computing
ref <- setValues(ref, 1)
#resample ref to resolution specified by "res" variable - must be a factor of 
#resolution of biovars/ref because fact arg must be an integer
fact <- res(ref)[1]/as.numeric(res)
ref.res <- disaggregate(ref, fact=fact)

j=1
#loop over classes and prep layers
for (j in c(1:length(classes))) {
  
  #create spp subdir if it doesn't exist
  geopath <- paste0(path, spp[j], "/inputs/", subDir, "/")
  dir.create(geopath, showWarnings=FALSE)
  
  #Select class to model and output to new shapefile
  veg.poly$classcode[veg.poly[[classFld]] == classes[j]] <- 1
  #veg.poly$classcode[veg.poly[[classFld]] == 0] <- 0
  veg.poly$classcode[is.na(veg.poly[[classFld]])] <- 0
  
  #rasterize veg polys
  veg.ras <- rasterize(veg.poly, ref.res, field="classcode")

  
  
  
  
  
  
  
  
  
  
}#end classes loop





