#Tina Cormier

#load libraries
library(raster)

#source parameter file
source("C:/Share/LCC-VP/scripts/R/LCC-VP_parameters.R")

#############################  SET VARIABLES HERE TO RUN MANUALLY #########################
#
# # main dir with 30-yr mean forecast data
# delta.indir <- 'C:/Share/LCC-VP/Parks/GRSM/climate_data/forecasts/ave_30yr/'
# 
# #output directory - typically looking for directory like:
#"C:/Share/LCC-VP/PACE/GRSM/climate_data/forecasts/adjusted_month_yearly/", and it has
# scenario directories underneath it, which each have ppt, tmin, tmax: /ea_rcp26/ppt
# delta.outdir <- 'C:/Share/LCC-VP/PACE/GRSM/PRISM/forecasts/adjusted_month_yearly/'
#
# #Specify the reference image  for this scenario - do I need this? based on dir structure, can code
# to figure out where ref image should be?
# delta.ref <- 
#
# #Specify the observed image - i.e., PRISM image
# delta.obs <- 
# 
# #use median
# med <- "p50"
# 
# #use rcp8.5
# rcp <- "85"
# 
############################# FUNCTIONS #################################
# computeDelta function calculates delta images and writes them out to a file.
# Requires an input image name (image you want to adjust), output image name, reference image name (i.e. modeled 1981-2010 image), and 
# the observed image name (i.e. PRISM observed 1981-2010 image). Requires Raster package. 
computeDelta <- function(img, out.img, delta.ref, delta.obs) {
  #first use the obs img to crop the in.img and ref.img - need same extent. They should already have the same NA areas, as they were
  #created from the same boundaries - some of them have two extra columns of nodata for some unknown reason.
  inras <- brick(img)
  ref <- brick(delta.ref)
  obs <- brick(delta.obs)
  
  #For some images, pixel values of -32768 were not set as nodata, so seen as valid data.
  obs2=obs
  obs2[obs==-32768] <- NA
  
  #This is a one-time "thing" to do for hx prism vs. observed...their projections are the same but not recorded exactly the same so it produces errors...
  #just set one to the other (comment this out later, bc could cause huge errors otherwise!!):
  projection(inras) <- projection(obs2)
  
  inras.crop <- crop(inras, obs2)
  inras.mask <- mask(inras.crop, obs2)
  ref.crop <- crop(ref, obs2)
  
  delta <- (inras.crop - ref.crop) + obs
  writeRaster(delta, file=out.img, format="GTiff",dataType="INT4S", overwrite=T)
}#end computeDelta function

#write looping logic to loop over all "combos" - take from other scripts.
#second loop to iterate over params - "ppt, tmin, tmax"

# redirect output to file (for selected variables) - FIX: sinking to log file
#is not appending - just overwriting. Not sure why.
date.now <- gsub(":", "", gsub(" ", "_", date()))
log.file <- file(paste(delta.outdir,"compute_climate_deltas_", date.now, "_log.txt",sep=""), open="wt")
sink(log.file)
sink(log.file, type="message")
print("*********************************************************************")
print("************** LCC-VP_compute_climate_deltas.R Log *************")
print("*********************************************************************")

print(date.now)
print(paste("delta.indir = ", delta.indir, sep=""))
print(paste("delta.outdir = ", delta.outdir, sep=""))
print(paste("delta.comboYN = ", delta.comboYN, sep=""))
#print(paste("delta.allcombo = ", delta.allcombo, sep=""))
print(paste("delta.params = ", delta.params, sep=""))
#print(paste("delta.ref = ", delta.ref, sep=""))
print(paste("delta.obsDir = ", delta.obsDir, sep=""))

#If all combos should be calculated, loop over serially
#would be nice to do on the cluster or in parallel, but I just want to get it
#running for now rather than dealing with moving data all around.
if (delta.comboYN == "yes") {
  for (combo in delta.allcombo) {
    print(combo)
    
    #overwrite med and rcp variables that were passed with paramters script
    #to reflect combo
    med <- unlist(strsplit(combo, "_"))[1]
    rcp <- unlist(strsplit(combo, "rcp"))[2]
    
    #This is a break from how I was doing it with other scripts, but submit each param and file separately, in case
    #we want to have more control over which subdirs we are analyzing - esp if we need to redo just a subset.
    for (dp in delta.params) {
      #set new indir/outdir based on input dir and med and rcp values
      delta.indir2 <- paste(delta.indir, med, "_rcp", rcp, "/", dp, "/", sep="")
      delta.outdir2 <- paste(delta.outdir, med, "_rcp", rcp, "/",dp, "/", sep="")
      
      #in imgs
      in.imgs <- list.files(delta.indir2, pattern="*.tif$", full.names=T)
      #ref img - currently, delta.ref is hardcoded - script expects it to be here based on our file structure!
      delta.ref <- paste(delta.indir, "deltas/ref/", med, "_rcp", rcp, "/", dp, "/", sep="")
      #should only be one file in this directory
      delta.ref <- list.files(delta.ref, pattern="*.tif$", full.names=T)
      #observed image - this is hardcoded - script expects this image to be here based on our current file structure!
      delta.obs <- list.files(paste(delta.obsDir, dp, sep=""), pattern="*.tif$", full.names=T)
      
      if (length(delta.ref) != 1 | length(delta.obs) != 1) {
        stop("there should only be one reference image and one observed image. Your lists have - delta.ref: ", length(delta.ref), ", delta.obs: ", length(delta.obs))
      } else {
          for (img in in.imgs) {
            print(paste("computing delta image for ", img, sep=""))
            print(paste("reference image is: ", delta.ref))
            print(paste("observed image is: ", delta.obs))
            out.img <- paste(delta.outdir2, unlist(strsplit(basename(img), "\\."))[1], "_delta.tif", sep="")
            
            #run computeDeltas function
            deltas <- computeDelta(img, out.img, delta.ref, delta.obs)
          }#end img loop
      }#end delta.obs if
    }#end dp for loop   
  }#end combo for loop
  
 
  #if comboYN is no, still looking at forecasts, but just the single one identified in parameters file.
  } else if (delta.comboYN == "no") {
      #This is a break from how I was doing it with other scripts, but submit each param and file separately, in case
      #we want to have more control over which subdirs we are analyzing - esp if we need to redo just a subset.
      for (dp in delta.params) {
        #set new indir/outdir based on input dir and med and rcp values
        delta.indir2 <- paste(delta.indir, delta.med, "_rcp", delta.rcp, "/", dp, "/", sep="")
        delta.outdir2 <- paste(delta.outdir, delta.med, "_rcp", delta.rcp, "/",dp, "/", sep="")
        
        #in imgs
        in.imgs <- list.files(delta.indir2, pattern="*.tif$", full.names=T)
        #ref img - currently, delta.ref is hardcoded - script expects it to be here based on our file structure!
        delta.ref <- paste(delta.indir, "deltas/ref/", med, "_rcp", rcp, "/", dp, "/", sep="")
        #should only be one file in this directory
        delta.ref <- list.files(delta.ref, pattern="*.tif$", full.names=T)
        #observed image - this is hardcoded - script expects this image to be here based on our current file structure!
        delta.obs <- list.files(paste(delta.obsDir, dp, sep=""), pattern="*.tif$", full.names=T)
        
        if (length(delta.ref) != 1 | length(delta.obs) != 1) {
          stop("there should only be one reference image and one observed image. Your lists have - delta.ref: ", length(delta.ref), ", delta.obs: ", length(delta.obs))
        } else {
            for (img in in.imgs) {
              print(paste("computing delta image for ", img, sep=""))
              print(paste("reference image is: ", delta.ref))
              print(paste("observed image is: ", delta.obs))
              out.img <- paste(delta.outdir2, unlist(strsplit(basename(img), "\\."))[1], "_delta.tif", sep="")
              
              #run computeDeltas function
              deltas <- computeDelta(img, out.img, delta.ref, delta.obs)
            }#end img loop
        }#end delta.obs if
      }#end dp for loop
      
  } else if (delta.comboYN=="prism"){
      for (dp in delta.params) {
        #set new indir/outdir based on input dir and med and rcp values
        delta.indir2 <- paste(delta.indir, dp, "/", sep="")
        delta.outdir2 <- paste(delta.outdir,dp, "/", sep="")
        
        #in imgs
        in.imgs <- list.files(delta.indir2, pattern="*.tif$", full.names=T)
        #ref img - currently, delta.ref is hardcoded - script expects it to be here based on our file structure!
        delta.ref <- paste(delta.indir, "deltas/ref/", dp, "/", sep="")
        #should only be one file in this directory
        delta.ref <- list.files(delta.ref, pattern="*.tif$", full.names=T)
        #observed image - this is hardcoded - script expects this image to be here based on our current file structure!
        delta.obs <- list.files(paste(delta.obsDir, dp, sep=""), pattern="*.tif$", full.names=T)
        
        if (length(delta.ref) != 1 | length(delta.obs) != 1) {
          stop("there should only be one reference image and one observed image. Your lists have - delta.ref: ", length(delta.ref), ", delta.obs: ", length(delta.obs))
        } else {
          for (img in in.imgs) {
            print(paste("computing delta image for ", img, sep=""))
            print(paste("reference image is: ", delta.ref))
            print(paste("observed image is: ", delta.obs))
            out.img <- paste(delta.outdir2, unlist(strsplit(basename(img), "\\."))[1], "_delta.tif", sep="")
            
            #run computeDeltas function
            deltas <- computeDelta(img, out.img, delta.ref, delta.obs)
          }#end img loop
        }#end delta.obs if
      }#end dp for loop
  
  } else {
      print("ComboYN must be 'yes' or 'no' for this script. Check your parameters.R file")
  }#end delta.comboYN else



#Restore message to console
sink()
sink(type="message")

