library(raster)
library(maptools)

#source parameter file for input and output directories
source("C:/Share/LCC-VP/scripts/R/LCC-VP_parameters.R")

#################### Hardcoded for testing ###############################
# park <- "DEWA"
# 
# #Enter EITHER the parent directory that contains the above-mentioned subdirectories OR path/filename 
# #of text file containing list of files.
# clim.in <- "C:/Share/LCC-VP/US_PRISM/prism_800m_restricted/"
# 
# #crs of in images = sometimes we need to define the projection first before reprojecting. Use WKT.
# crs.orig <- "+proj=longlat +ellps=WGS72 +towgs84=0,0,4.5,0,0,0.5540000000000001,0.2263 +no_defs"
# 
# #list variables (by way of their subdir name) that you want to reproj/clip.
# params <- c("ppt", "tmin", "tmax")
# 
# #outdir - this is the parent directory and should contain either the ppt, tmin, and tmax subdirs
# #or the different combo directories (i.e. ea_rcp26, p50_rcp85, etc), which subsequently
# #contain ppt, tmin, tmax, biovars, GDD.
# clim.outdir <- "C:/Share/LCC-VP/Parks/DEWA/climate_data/PRISM_historical/adjusted_month_yearly/temp/"
# 
# #Do your images need their proj defined (hard-coded in py script to WGS84) and reprojected to albers nad83 before clipping?
# #accepted responses: "both" "clip_only"
# clim.reproj <- "both"
# 
# #Clip file - must be a raster that has the projection, extent, and snapping grid you desire.
# #will be used to mask in-imgs.
# clim.clip <- "C:/Share/LCC-VP/Parks/DEWA/climate_data/current_PRISM/biovars/biovars_1981-2010.tif"
# 
# #For forecast data, do you want to do a number of possible combinations? (note all lowercase - avoiding 
# #regular expressions for now) "yes", "no", or "na"("na" if PRISM or other single-level directory data rather forecast data; "list" if
# #entering text file list of inputs.)
# clip.comboYN <- "na"
# clip.allcombo <- c('p25_rcp45','p25_rcp85','p50_rcp45','p50_rcp85','p75_rcp45','p75_rcp85')
# #clip.allcombo <- c("ea_rcp26", "p25_rcp26", "p50_rcp26", "p75_rcp26", "ea_rcp45", "p25_rcp45", 'p50_rcp45', 'p75_rcp45',
# #'ea_rcp60', 'p25_rcp60', 'p50_rcp60', 'p75_rcp60', 'ea_rcp85', 'p25_rcp85', "p50_rcp85", 'p75_rcp85')

###################################################

#Function to reproject and mask images
makeItRain <- function(inImg, crs.orig, outImg, clim.reproj, clim.clipImg) {
  
  #first define original projection - not always necessary, but can't hurt (some PRISM datasets don't
  #have their projection info defined, and we need to do this in order to REproject).
  projection(inImg) <- crs.orig
  
  if (clim.reproj == "both") {
    #align with extent and reproject to crs of clim.clip  
    reproj <- projectRaster(from=inImg, to=clim.clipImg, method = "bilinear")
    inImg <- reproj
    #inImg <- round((reproj * c(100,100,1,100,100,100,100,100,100,100,100,100,100,100,1,100,100,100,100)),2)
  } #end clim.reproj if
  
  #clip then mask by clim.clipImg
  clip <- crop(inImg, clim.clipImg)
  mask <- mask(clip, clim.clipImg, filename=outimg, overwrite=T)
  
}#end makeItRain

# redirect output to file (for selected variables) - FIX: sinking to log file
#is not appending - just overwriting. Not sure why.
#For now, decided to just write unique log file each time this script is run. 
#Be careful not to let logs pile up. 
d <- gsub(":", "", gsub(" ", "_", date()))
log.file <- file(paste(clim.outdir,"clim_reproj_clip_", d, "_log.txt",sep=""))
sink(log.file)
sink(log.file, type="message")
print("*********************************************************************")
print("******** LCC-VP_call_batch_reproj_clip_climdata.R Log ********")
print("*********************************************************************")

print(d)
print(park)
print(paste("clim.in = ", clim.in, sep=""))
print(paste("params = ", paste(params,collapse=", "), sep=""))
print(paste("clim.outdir = ", clim.outdir, sep=""))
print(paste("clim.reproj = ", clim.reproj, sep=""))
print(paste("clim.clip = ", clim.clip, sep=""))
print(paste("clip.comboYN = ", clip.comboYN, sep=""))
print(paste("clip.allcombo = ", clip.allcombo, sep=""))


#If all combos should be calculated, loop over serially
#would be nice to do on the cluster or in parallel, but I just want to get it
#running for now rather than dealing with moving data all around.

#HAVEN'T TESTED THIS SINCE MOVING OVER TO R. 
if (clip.comboYN == "yes") {
  for (combo in clip.allcombo) {
    print(combo)
    
    #overwrite med and rcp variables that were passed with paramters script
    #to reflect combo
    clip.med <- unlist(strsplit(combo, "_"))[1]
    clip.rcp <- unlist(strsplit(combo, "rcp"))[2]
    
    #loop over params as well
    for (clim in params) {
      print(clim)
      #set new indir/outdir based on input dir and med and rcp values
      clim.indir2 <- paste(clim.in, clip.med, "_rcp", clip.rcp, "/", clim, "/", sep="")
      clim.outdir2 <- paste(clim.outdir, clip.med, "_rcp", clip.rcp, "/", clim, "/", sep="") 
      
      #list files
      files <- list.files(clim.indir2, pattern="*.tif$", full.names=T)
      clim.clipImg <- stack(clim.clip)
      
      for (img in files) {
        inImg <- brick(img)
        outimg <- paste0(clim.outdir2, unlist(strsplit(basename(img), "\\."))[1], "_albers_clip.tif")
        
        makeItRain(inImg, crs.orig, outImg, clim.reproj, clim.clipImg)
                
      }#end img loop
    }#end clim loop    
  }#end combo loop
  
  #Finally, if running on PRISM or one quantile/rcp scenario
} else if (clip.comboYN == "na" || clip.comboYN == "no") {
  #loop over params to submit current indir/outdir
  for (clim in params) {
    print(clim)
    
    #set new indir/outdir
    clim.indir2 <- paste(clim.in, clim, "/", sep="")
    clim.outdir2 <- paste(clim.outdir, clim, "/", sep="") 
    
    #list files
    files <- list.files(clim.indir2, pattern="*.tif$", full.names=T)
    clim.clipImg <- raster(clim.clip)
    
    for (img in files) {
      inImg <- stack(img)
      outimg <- paste0(clim.outdir2, unlist(strsplit(basename(img), "\\."))[1], "_albers_clip.tif")
      
      #make it rain! :)
      makeItRain(inImg, crs.orig, outImg, clim.reproj, clim.clipImg)
      
    }
    
    
  }#end clim loop
  
} else {
  stop(paste("clip.comboYN must be 'yes', 'no', 'na', or 'list'. If 'list', dl must be 'l'. Otherwise, dl must be 'd.' ",
             "You entered: clip.comboYN = ", clip.comboYN, " and dl = ", dl, sep=""))
}#end if 



#Restore message to console
sink()
sink(type="message")

