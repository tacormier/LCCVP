#source parameter file for input and output directories
source("C:/Share/LCC-VP/scripts/R/LCC-VP_parameters.R")


# redirect output to file (for selected variables) - FIX: sinking to log file
#is not appending - just overwriting. Not sure why.
#For now, decided to just write unique log file each time this script is run. 
#Be careful not to let logs pile up. 
date <- gsub(":", "", gsub(" ", "_", date()))
log.file <- file(paste(path,"logs/model_prep_", paste(spp, collapse="-"), "_", date, "_log.txt",sep=""))
sink(log.file)
sink(log.file, type="message")
print("*********************************************************************")
print("******** LCC-VP_call_model_prep_python.R Log ********")
print("*********************************************************************")

print(date)
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
print(paste("park-level predictors = ", parkPreds, sep=""))
print(paste("model extent (park or range) = ", mod.extent, sep=""))
print(paste("big data? ", bigdata.samp, sep=""))
print(paste("For big data, using ", bigdata.samp.size, " samples.", sep=""))
print(paste("output directory = ", subDir.mod, sep=""))
print(paste("List of bands in biovars image is here: ", biovars.names, sep=""))
print(paste("Forecast now?  ", forecast, sep=""))
print(paste("Directory containing forecasted predictors:", forecast.dir, sep=""))

#Create spp directories if they don't already exist
for (i in 1:length(spp)) {
  #if the spp directory doesn't exist, create it. Can ignore warnings if it DOES exist.
  analysis.dir <- (paste(path, spp[i], sep=""))
  dir.create(analysis.dir, showWarnings=FALSE)
  
  #also create input and output directories
  dir.create(paste(analysis.dir, "inputs", subDir.mod, sep="/"), showWarnings=FALSE)
  dir.create(paste(analysis.dir, "outputs", subDir.mod, sep="/"), showWarnings=FALSE)
}

#Write out list of classes to be modeled to a file.  This txt file will be read into python.
classes.txt <- paste(path, park, "_classes.csv", sep="")
write.table(classes, file=classes.txt, row.names=F, col.names=F, sep=",", quote=F)

#write out list of spp sub-dirs to a file.  This txt file will be read into python
spp.txt <- paste(path, park, "_sppDirs.csv", sep="")
write.table(spp, file=spp.txt, row.names=F, col.names=F, sep=",",quote=F )

#Calculate res factor for % cover calcs. Based on 800 m (or 640,000 m2) PRISM pixel.
res.factor <- as.character(640000/as.numeric(res)^2 / 100) 

x <- system(paste("c:/Python27/python C:/Share/LCC-VP/scripts/python/LCC-VP_model_prep.py", 
                  park, biovars, zonesPRISM, veg, res, classFld, classes.txt, path, spp.txt, map, 
                  res.factor,model,subDir.mod,biovars.names, forecast,forecast.dir,bigdata.samp,bigdata.samp.size, parkPreds,modExtent, sep=" "))

if (x != 0) stop("python script was not called successfully.")

sink()
sink(type="message")
