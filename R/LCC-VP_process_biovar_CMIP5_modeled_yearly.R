#Create 12-month stacks and Calculate bioclimatic variables 
#from 800m monthly historical modeled data. 
#script does not use rcp variable 

#Tina Cormier

#load libraries
library(raster)
library(dismo)

#source parameter file
source("C:/Share/LCC-VP/scripts/R/LCC-VP_parameters.R")
#Reads in variables: park, bioclim.indir, bioclim.outdir, med

#############################  SET VARIABLES HERE TO RUN MANUALLY #########################
#
# # main dir with forecast data
# bioclim.indir <- 'LCC-VP/PACE/DEWA/climate_data/forecasts/raw_yearly/p50_historical/'
# 
# #output directory - typically looking for directory like:
#"C:/Share/LCC-VP/PACE/DEWA/climate_data/forecasts/adjusted_month_yearly/p50_historical/", and it has
# scenario directories underneath it, which each have ppt, tmin, tmax: /ea_rcp26/ppt
# bioclim.outdir <- 'C:/Share/LCC-VP/PACE/DEWA/climate_data/forecasts/adjusted_month_yearly/p50_historical/'
# 
# #use median
# med <- "p50"
# 
# #use rcp8.5
# rcp <- NA
# 
#
############################# FUNCTIONS #################################
# For precip data adjustment
# precip data are in mm/day - need to mult each ppt ras by #days in the month.
# mult factors
adj_ppt <- function(image, ppt.outdir) {
  days <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  ppt.out <- paste(ppt.outdir, unlist(strsplit(basename(image), "\\."))[1], "_adj.tif$", sep="")
  img <- brick(image)
  
  #Convert to mm*100 like PRISM - to avoid arithmetic errors in doing operations on floats. 
  #So, round off additional decimals. 
  img <- round(img*100)
  dataType(img) <- "INT4S"
  
  print(paste("adjusting precip values for ", basename(image), sep=""))
  
  #Convert to mm*100 to avoid floating point errors. Round off any additional decimals.
  #Make adjustment to change precip/day to precip/month.
  adjust <- round(img*days)
  dataType(adjust) <- "INT4S"
  
  # test if days were applied correctly (i.e. correct monthly value assigned to each 
  # band/month should equal value from days:
  if (identical(as.vector(cellStats(adjust, min) / cellStats(img, min)), days)) {
    writeRaster(img, file=ppt.out, format="GTiff",dataType="INT4S", overwrite=T)
  } else {
    stop(print(paste("problem adjusting, ", image, " to monthly values*100. . .investigate"), sep=""))
  } #endif    
}#end adj_ppt function

#put whole script into a function to aid in error message control and logfile
#documentation.
biovars_yrly <- function(bioclim.indir, bioclim.outdir, med) {
  
  #Get appropriate image list
  ppt.dir <- paste(bioclim.indir, "ppt/", sep="")
  pattern <- paste("*.tif$", sep="")
  img.list <- list.files(ppt.dir, pattern=pattern, full.names=T)
  #ppt.outdir <-  sub("raw_yearly/", "adjusted_month_yearly/ppt/", bioclim.indir)
  ppt.outdir <- paste(bioclim.outdir, "ppt/", sep="")
  #image <- 'C:/Share/LCC-VP/PACE/GRSM/climate_data/forecasts/raw_yearly/ppt/ea_pr_rcp26_2006_out.tif'
  
  #Make sure yearly ppt forecasts have been calculated
  if (length(img.list) == 0)
    stop(paste("no images in ", ppt.dir, sep=""))
  
  #make monthly adjustment to each ppt image
  #test.list <- img.list[1:3]
  ppt_adj <- sapply(img.list, adj_ppt, ppt.outdir)
  
  #get lists of ppt, tmin, tmax
  adjppt.dir <- ppt.outdir
  ppt.pattern <- "*adj.tif$"
  ppt.list <- list.files(adjppt.dir, pattern=ppt.pattern, full.names=T)
  
  tmin.dir <- paste(bioclim.indir, "tmin/", sep="")
  tmin.pattern <- "*.tif$"
  tmin.list <- list.files(tmin.dir, pattern=tmin.pattern, full.names=T)
  
  tmax.dir <- paste(bioclim.indir, "tmax/", sep="")
  tmax.pattern <- "*.tif$"
  tmax.list <- list.files(tmax.dir, pattern=tmax.pattern, full.names=T)
  
  #calc biovars for forecasts
  for (j in c(1:(length(tmax.list)))) {
    bio.name <- unlist(strsplit(basename(tmax.list[j]), "_"))
    bio.name <- paste(bio.name[1], "_", bio.name[3], "_", bio.name[4], "_biovars.tif", sep="")
    print(paste("calculating biovars and writing ", bio.name, sep=""))
    
    #B/c of ppt adjustment, ppt should already be rounded to nearest integer to avoid floating point errors.
    ppt <- brick(ppt.list[j])
    
    #mult tmin and tmax by 100 to match units in PRISM (degrees C *100)..and therefore the RF model. 
    #ALSO, round to nearest integer to avoid floating point errors.
    #write out unit-adjusted tmin
    tmin <- round(brick(tmin.list[j])*100)
    
    adjtmin.dir <- paste(bioclim.outdir, "tmin/", sep="")
    tmin.name <- unlist(strsplit(basename(tmin.list[j]), "_"))
    tmin.name <- paste(adjtmin.dir, tmin.name[1], "_", tmin.name[2], "_", tmin.name[3], "_", tmin.name[4], "_adj.tif", sep="")
    writeRaster(tmin, tmin.name, format="GTiff", dataType="INT4S", overwrite=T)
    
    tmax <- round(brick(tmax.list[j])*100)
    #write out unit-adjusted tmax
    tmax <- round(brick(tmax.list[j])*100)
    
    adjtmax.dir <- paste(bioclim.outdir, "tmax/", sep="")
    tmax.name <- unlist(strsplit(basename(tmax.list[j]), "_"))
    tmax.name <- paste(adjtmax.dir, tmax.name[1], "_", tmax.name[2], "_", tmax.name[3], "_", tmax.name[4], "_adj.tif", sep="")
    writeRaster(tmax, tmax.name, format="GTiff", dataType="INT4S", overwrite=T)
    
    
    biovar.out <- paste(bioclim.outdir, "biovars/", bio.name, sep="")
    #Round biovars to whole numbers - avoid issues with floats.
    bio.vars <- round(biovars(ppt,tmin,tmax))
    writeRaster(bio.vars,biovar.out, format="GTiff", dataType="INT4S", overwrite=T)
  } #end j loop
  
}#end function

# redirect output to file (for selected variables) - FIX: sinking to log file
#is not appending - just overwriting. Not sure why.
date <- gsub(":", "", gsub(" ", "_", date()))
log.file <- file(paste(bioclim.outdir,"process_biovars_CMIP5_modeled_", date, "_log.txt",sep=""), open="wt")
sink(log.file)
sink(log.file, type="message")
print("*********************************************************************")
print("************** LCC-VP_process_biovar_CMIP5_modeled_yearly.R Log *************")
print("*********************************************************************")

print(date)
print(paste("bioclim.indir = ", bioclim.indir, sep=""))
print(paste("bioclim.outdir = ", bioclim.outdir, sep=""))

#This script assume rcp="hx"
biovars_yr <- biovars_yrly(bioclim.indir, bioclim.outdir, med)
    
  
#Restore message to console
sink()
sink(type="message")

detach("package:dismo")
detach("package:raster")

