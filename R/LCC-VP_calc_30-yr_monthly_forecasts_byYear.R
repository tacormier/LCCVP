#Create 30-year average stacks from 800m monthly forecast data.
#assumes that yearly forecasted data has already been processed.
#Tina Cormier

#load libraries
library(raster)
library(stringr)
#library(dismo)

#source parameter file for input and output directories
source("C:/Share/LCC-VP/scripts/R/LCC-VP_parameters.R")
source("C:/Share/LCC-VP/scripts/R/LCC-VP_functions.R")

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
#############################  #############################  #############################  

# redirect output to file (for selected variables) - FIX: sinking to log file
#is not appending - just overwriting. Not sure why.
#For now, decided to just write unique log file each time this script is run. 
#Be careful not to let logs pile up. 
date.now <- gsub(":", "", gsub(" ", "_", date()))
log.file <- file(paste(bioclim.outdir,"calc_30yr_monthly_imgs_", date.now, "_log.txt",sep=""))
sink(log.file)
sink(log.file, type="message")
print("*********************************************************************")
print("************** LCC-VP_calc_30-yr_monthly_forecasts_byYear.R Log *************")
print("*********************************************************************")

print(date.now)
print(paste("bioclim.indir = ", bioclim.indir, sep=""))
print(paste("bioclim.outdir = ", bioclim.outdir, sep=""))

#If all combos should be calculated, loop over serially
#would be nice to do on the cluster or in parallel, but I just want to get it
#running for now rather than dealing with moving data all around.

#bioclim.params == "" if/else statements have not been tested.
if (comboYN == "yes") {
  for (combo in allcombo) {
    print(combo)
    
    #overwrite med and rcp variables that were passed with paramters script
    #to reflect combo
    med <- unlist(strsplit(combo, "_"))[1]
    rcp <- unlist(strsplit(combo, "rcp"))[2]
    
    if (bioclim.params != "") {
      #loop over climate vars
      for (param in bioclim.params){
        #set new indir and outdir based on input dir and med and rcp values
        bioclim.indir2 <- paste(bioclim.indir, med, "_rcp", rcp, "/", param, "/", sep="")
        bioclim.outdir2 <- paste(bioclim.outdir, med, "_rcp", rcp, "/", param, "/", sep="")
        
        #Get appropriate image list
        img.list <- list.files(bioclim.indir2, pattern="*.tif$", full.names=T)
        
        calc30yr(img.list, bioclim.outdir2)
      }#end param loop 
      
    }else if (bioclim.params == ""){
      #list files
      #set new indir/outdir based on input dir and med and rcp values
      bioclim.indir2 <- paste(bioclim.indir, med, "_rcp", rcp, "/", sep="")
      bioclim.outdir2 <- paste(bioclim.outdir, med, "_rcp", rcp, "/", param, sep="")
      
      #Get appropriate image list
      img.list <- list.files(bioclim.indir2, pattern="*.tif$", full.names=T)
      
      #calc 30-yr avg
      calc30yr(img.list, bioclim.outdir2)
    }# end params == "" if/else
  }#end combo loop
  
  #if comboYN is no, still looking at forecasts, but just the single one identified in parameters file.
} else if (comboYN == "no") {
    
    if (bioclim.params != "") {
      #loop over climate vars
      for (param in bioclim.params){
        #set new indir and outdir based on input dir and med and rcp values
        bioclim.indir2 <- paste(bioclim.indir, param, "/", sep="")
        bioclim.outdir2 <- paste(bioclim.outdir,param, "/", sep="")
        
        #Get appropriate image list
        img.list <- list.files(bioclim.indir2, pattern="*.tif$", full.names=T)
        
        calc30yr(img.list, bioclim.outdir2)
      }#end param loop 
      
    }else if (bioclim.params == ""){
      #Get appropriate image list
      img.list <- list.files(bioclim.indir, pattern="*.tif$", full.names=T)
      
      calc30yr(img.list, bioclim.outdir)
      
    }# end params == "" if/else
  
  #If comboYN is NA, then we are working with PRISM data and don't need to pass med and rcp variables  
} else {
  print("ComboYN must be 'yes' or 'no' for this script. Check your parameters.R file")
}#end if
#Restore message to console
sink()
sink(type="message")

#detach("package:dismo")
#detach("package:raster")