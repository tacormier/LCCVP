#Create bioclimatic variables from 800m monthly forecast data.
#Tina Cormier

#load libraries
library(raster)
library(dismo)


#source parameter file for input and output directories
source("C:/Share/LCC-VP/scripts/R/LCC-VP_parameters.R")
#Reads in variables: park, bioclim.indir, bioclim.outdir, med, and rcp

#############################  SET VARIABLES HERE TO RUN MANUALLY #########################
# #
# # main dir with yearly forecast data
# bioclim.indir <- 'C:/Share/LCC-VP/PACE/GRSM/climate_data/forecasts/adjusted_month_yearly/'
#
#Where to output 30-yr average rasters
#bioclim.outdir <- 'C:/Share/LCC-VP/PACE/GRSM/climate_data/forecasts/ave_30yr_byYear/biovars/'
# 
# #use median
# med <- "p50"
# 
# #use rcp8.5
# rcp <- "85"
############################# FUNCTIONS #################################
#requires raster package
calcBiovars <- function(ppt, tmin, tmax, out.file) {  
  #calc biovars
  #delta-adjusted 30-yr means for ppt, tmin, tmax.
  ppt.img <- brick(ppt)
  tmin.img <- brick(tmin)
  tmax.img <- brick(tmax)
  
  #Round biovars to whole numbers and mult by 100 - avoid issues with floats but still maintain some precision.
  #inputs are already multiplied by 100, so output is x100.
  #bio.vars <- round((biovars(ppt.img, tmin.img, tmax.img)*100))
  #if inputs are already multiplied by 100
  bio.vars <- round((biovars(ppt.img, tmin.img, tmax.img)))
  writeRaster(bio.vars, file=out.file, format="GTiff", dataType="INT4S", overwrite=T)
}#end function

# redirect output to file (for selected variables) - FIX: sinking to log file
#is not appending - just overwriting. Not sure why.
#For now, decided to just write unique log file each time this script is run. 
#Be careful not to let logs pile up. 
date.now <- gsub(":", "", gsub(" ", "_", date()))
log.file <- file(paste(bioclim.outdir,"calc_biovars_", date.now, "_log.txt",sep=""))
sink(log.file)
sink(log.file, type="message")
print("*********************************************************************")
print("************** LCC-VP_calc_biovars.R Log *************")
print("*********************************************************************")

print(date.now)
print(paste("bioclim.indir = ", bioclim.indir, sep=""))
print(paste("bioclim.outdir = ", bioclim.outdir, sep=""))

#If all combos should be calculated, loop over serially
#would be nice to do on the cluster or in parallel, but I just want to get it
#running for now rather than dealing with moving data all around.
if (comboYN == "yes") {
  for (combo in allcombo) {
    print(combo)
    
    #overwrite med and rcp variables that were passed with paramters script
    #to reflect combo
    med <- unlist(strsplit(combo, "_"))[1]
    rcp <- unlist(strsplit(combo, "rcp"))[2]
    
    #set new indir/outdir based on input dir and med and rcp values
    bioclim.indir2 <- paste(bioclim.indir, med, "_rcp", rcp, "/", sep="")
    bioclim.outdir2 <- paste(bioclim.outdir, med, "_rcp", rcp, "/", sep="") 
    
    # list images
    ppt.dir <- paste(bioclim.indir2, "ppt/", sep="")
    #So this nested grep line is a little complicated, but it helps avoid listing
    #.tif.xml files that we don't want to operate on...just tif files!
    #ppt.list <- grep(".tif$", grep(paste(med, "_pr_rcp", rcp, sep=""), list.files(ppt.dir, full.names=T), value=T), value=T)
    ppt.list <- list.files(ppt.dir, pattern="*.tif$", full.names=T)
    
    tmin.dir <- paste(bioclim.indir2, "tmin/", sep="")
    tmin.list <- list.files(tmin.dir, pattern="*.tif$", full.names=T)
    #tmin.list <- grep(".tif$", grep(paste(med, "_tasmin_rcp", rcp, sep=""), list.files(tmin.dir, full.names=T), value=T), value=T)
    
    tmax.dir <- paste(bioclim.indir2, "tmax/", sep="")
    tmax.list <- list.files(tmax.dir, pattern="*.tif$", full.names=T)
    #tmax.list <- grep(".tif$", grep(paste(med, "_tasmax_rcp", rcp, sep=""), list.files(tmax.dir, full.names=T), value=T), value=T)
    
    for (i in c(1:length(ppt.list))) {
      ppt <- ppt.list[i]
      tmin <- tmin.list[i]
      tmax <- tmax.list[i]
      
      out.file <- paste(bioclim.outdir2, "biovars/", sub("pr_", "", unlist(strsplit(basename(ppt.list[i]), "\\."))[1]), "_biovars.tif", sep="")
      print(paste("calculating biovar file ", out.file, sep=""))
      print("Inputs: ")
      print(ppt)
      print(tmin)
      print(tmax)
      
      biovars <- calcBiovars(ppt, tmin, tmax, out.file)
    }#end file for
  }#end combo for
  
  #if comboYN is no, still looking at forecasts, but just the single one identified in parameters file in bioclim.indir
} else if (comboYN == "no") {
    #set new indir/outdir based on input dir and med and rcp values
    #bioclim.indir2 <- paste(bioclim.indir, med, "_rcp", rcp, "/", sep="")
    #bioclim.outdir2 <- paste(bioclim.outdir, med, "_rcp", rcp, "/", sep="") 
    
    # list images
    ppt.dir <- paste(bioclim.indir, "ppt/", sep="")
    #So this nested grep line is a little complicated, but it helps avoid listing
    #.tif.xml files that we don't want to operate on...just tif files!
    #ppt.list <- grep(".tif$", grep(paste(med, "_pr_rcp", rcp, sep=""), list.files(ppt.dir, full.names=T), value=T), value=T)
    ppt.list <- list.files(ppt.dir, pattern="*.tif$", full.names=T)
    
    tmin.dir <- paste(bioclim.indir, "tmin/", sep="")
    tmin.list <- list.files(tmin.dir, pattern="*.tif$", full.names=T)
    #tmin.list <- grep(".tif$", grep(paste(med, "_tasmin_rcp", rcp, sep=""), list.files(tmin.dir, full.names=T), value=T), value=T)
    
    tmax.dir <- paste(bioclim.indir, "tmax/", sep="")
    tmax.list <- list.files(tmax.dir, pattern="*.tif$", full.names=T)
    #tmax.list <- grep(".tif$", grep(paste(med, "_tasmax_rcp", rcp, sep=""), list.files(tmax.dir, full.names=T), value=T), value=T)
    
    for (i in c(1:length(ppt.list))) {
      ppt <- ppt.list[i]
      tmin <- tmin.list[i]
      tmax <- tmax.list[i]
      
      out.file <- paste(bioclim.outdir, "biovars/", sub("pr_", "", unlist(strsplit(basename(ppt.list[i]), "\\."))[1]), "_biovars.tif", sep="")
      biovars <- calcBiovars(ppt, tmin, tmax, out.file)
    }#end file for
    
} else {
  print("ComboYN must be 'yes' or 'no' for this script. Check your parameters.R file")
}#end if
#Restore message to console
sink()
sink(type="message")

#detach("package:dismo")
#detach("package:raster")