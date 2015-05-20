#Create 12-month stacks and Calculate bioclimatic variables from 800m PRISM monthly normals.
#Tina Cormier

#load libraries
library(raster)
#library(rgdal)
library(dismo)

#source parameter file for input and output directories
source("C:/Share/LCC-VP/scripts/R/LCC-VP_parameters.R")
#Reads in variables: park, bioclim.indir, bioclim.outdir

#set up logfile
# redirect output to file (for selected variables)
date <- gsub(":", "", gsub(" ", "_", date()))
log.file <- file(paste(bioclim.outdir,"calc_prism_biovars_", date, "_log.txt",sep=""))
sink(log.file)
sink(log.file, type="message")
print("*********************************************************************")
print("************** LCC-VP_calc_PRISM_biovars.R Log *************")
print("*********************************************************************")

print(date)
print(paste("bioclim.indir = ", bioclim.indir, sep=""))
print(paste("bioclim.outdir = ", bioclim.outdir, sep=""))

#names of subdirectories within bioclim.indir
clim.vars <- c("ppt", "tmax", "tmin")


#Assumption - all files in directory will store data in separate bands
#representing monghtly data.

#Assumption - only one time step in directory (i.e. 1981 - 2010)
#loop over files in bioclim.indir and stack.
for (i in clim.vars) {
  print(i)
  #get list of rasters
  dir <- paste(bioclim.indir, i, "/", sep="")
  rasters <- list.files(dir, pattern=".tif$", full.names=T)
  #stack
  stack <- stack(rasters)
  #Assign stack to a unique variable to use later!
  assign(paste(i, "_stk", sep=""), stack) 
} #end loop

#Calc biovars - Round biovars to whole numbers - avoid issues with floats.
bio.vars <- round(biovars(ppt_stk, tmin_stk, tmax_stk))
#Write to new raster (might be able to skip this and keep in memory for modeling piece...would
#add on to this script).
outras <- paste(bioclim.outdir, "biovars_1981-2010.tif", sep="")
writeRaster(bio.vars,outras,format="GTiff", dataType="INT4S")

#Restore output to console
sink()
sink(type="message")