#source parameter file for input and output directories
source("C:/Share/LCC-VP/scripts/R/LCC-VP_parameters.R")
#Reads in variables: park, clim.indir, clim.outdir, clim.clip

# redirect output to file (for selected variables) - FIX: sinking to log file
#is not appending - just overwriting. Not sure why.
#For now, decided to just write unique log file each time this script is run. 
#Be careful not to let logs pile up. 
date <- gsub(":", "", gsub(" ", "_", date()))
log.file <- file(paste(outStackDir,"clip_stack_", date, "_log.txt",sep=""))
sink(log.file)
sink(log.file, type="message")
print("*********************************************************************")
print("******** LCC-VP_LCC-VP_clip_stack_predictors_python.R Log ********")
print("*********************************************************************")

print(date)
print(park)
print(paste("var_dir = ", var_dir, sep=""))
print(paste("clip_ref = ", clip_ref, sep=""))
print(paste("outStackDir = ", outStackDir, sep=""))

#List of txt files in var_dir
vars <- list.files(var_dir, pattern="*.txt", full.names=T)

#Loop over txt files and run LCC-VP_clip_stack_predictors.py

for (i in c(1:length(vars))) {
  layers <- vars[i]
  outStack <- paste(outStackDir,strsplit(basename(layers), ".txt")[1], ".tif", sep="")
  
  #System call to python script that does the work!
  x <- system(paste("c:/Python27/ArcGISx6410.2/python C:/Share/LCC-VP/scripts/python/LCC-VP_clip_stack_predictors.py", 
                    park, layers, clip_ref, outStack, sep=" "))
  
  if (x != 0) stop("python script was not called successfully.")
  
}#end for

#Restore message to console
sink()
sink(type="message")