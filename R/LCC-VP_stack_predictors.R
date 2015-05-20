library(raster)

#source parameter file for input and output directories
source("C:/Share/LCC-VP/scripts/R/LCC-VP_parameters.R")
#Reads in variables: park, clim.indir, clim.outdir, clim.clip

# redirect output to file (for selected variables) - FIX: sinking to log file
#is not appending - just overwriting. Not sure why.
#For now, decided to just write unique log file each time this script is run. 
#Be careful not to let logs pile up. 
date <- gsub(":", "", gsub(" ", "_", date()))
log.file <- file(paste(logdir,"clip_stack_", date, "_log.txt",sep=""))
sink(log.file)
sink(log.file, type="message")
print("*********************************************************************")
print("******** LCC-VP_LCC-VP_clip_stack_predictors_python.R Log ********")
print("*********************************************************************")

print(date)
print(park)
print(paste("var_dir = ", var_dir, sep=""))
print(paste("clip_ref = ", clip_ref, sep=""))
print(paste("logdir = ", logdir, sep=""))

#List of txt files in var_dir
vars <- list.files(var_dir, pattern="*.csv", full.names=T)

#read in ref layer
ref <- stack(clip_ref)


for (i in c(1:length(vars))) {
  layers <- vars[i]
  l <- read.csv(vars[i], header = F, as.is = T)
  names(l) <- c("imgs", "outdir")
  outStackDir <- l$outdir[1]
  outstack <- paste(outStackDir,strsplit(basename(layers), ".csv")[1], ".tif", sep="")
  
  #for now, assume that the layers have been projected and clipped to the ref already - just need to stack them.
  #can add some logic to this later to deal with multiple scenarios.
  st <- stack(l$imgs)
  
  #doing this step because in blue ridge example, projections are the same, but the proj text is slightly different, resulting in a
  #stupid error if I try to do anything with these layers. This is by no means a good idea to do in general without knowing if 
  #they are actually different or not.
  projection(st) <- projection(ref)
  
  #also, round off the decimal to avoid floating point errors. 
  st <- round(st)
  
  #write out stacked file
  writeRaster(st, filename = outstack, overwrite=T)
  
  
#   
#   for (ras in l) {
#     rasvar <- stack(ras)
#     #doing this step because in blue ridge example, projections are the same, but the proj text is slightly different, resulting in a
#     #stupid error. This is by no means a good idea to do in general without knowing if they are actually different or not.
#     projection(rasvar) <- projection(ref)
#     
#     #make sure it is masked by the ref layer
#     cr <- crop(rasvar, ref)
#     ma <- mask(cr, ref)
#     
#   }

  
  
}#end for

#Restore message to console
sink()
sink(type="message")