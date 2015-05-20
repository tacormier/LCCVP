#Create 30-year average stacks from 800m monthly forecast data and Calculate bioclimatic variables.
#assumes that yearly forecasted data has already been processed.
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
#bioclim.outdir <- 'C:/Share/LCC-VP/PACE/GRSM/climate_data/forecasts/ave_30yr/biovars/'
# 
# #use median
# med <- "p50"
# 
# #use rcp8.5
# rcp <- "85"
############################# FUNCTIONS #################################
# calculate mean monthly values for 30 year blocks 
#CHANGE TO ONE FUNCTION AND HAVE AN ARGUMENT FOR MONTH #....THEN LOOP
#THROUGH A LIST OF MONTHS?
jan <- function(x) {round(mean(stack(x[c(seq(1,360,12))])))}
feb <- function(x) {round(mean(stack(x[c(seq(2,360,12))])))}
mar <- function(x) {round(mean(stack(x[c(seq(3,360,12))])))}
apr <- function(x) {round(mean(stack(x[c(seq(4,360,12))])))}
may <- function(x) {round(mean(stack(x[c(seq(5,360,12))])))}
jun <- function(x) {round(mean(stack(x[c(seq(6,360,12))])))}
jul <- function(x) {round(mean(stack(x[c(seq(7,360,12))])))}
aug <- function(x) {round(mean(stack(x[c(seq(8,360,12))])))}
sep <- function(x) {round(mean(stack(x[c(seq(9,360,12))])))}
oct <- function(x) {round(mean(stack(x[c(seq(10,360,12))])))}
nov <- function(x) {round(mean(stack(x[c(seq(11,360,12))])))}
dec <- function(x) {round(mean(stack(x[c(seq(12,360,12))])))}

biovar30yr <- function(bioclim.indir, bioclim.outdir, med, rcp) {
  
  # determine 30-year groupings
  grps <- list(1:30, 31:60, 61:90)
  bands <- list(1:360, 361:720, 721:1080)
  
  # list yearly images
  ppt.dir <- paste(bioclim.indir, "ppt/", sep="")
  #So this nested grep line is a little complicated, but it helps avoid listing
  #.tif.xml files that we don't want to operate on...just tif files!
  ppt.list <- grep(".tif$", grep(paste(med, "_pr_rcp", rcp, sep=""), list.files(ppt.dir, full.names=T), value=T), value=T)
  
  tmin.dir <- paste(bioclim.indir, "tmin/", sep="")
  tmin.list <- grep(".tif$", grep(paste(med, "_tasmin_rcp", rcp, sep=""), list.files(tmin.dir, full.names=T), value=T), value=T)
  
  tmax.dir <- paste(bioclim.indir, "tmax/", sep="")
  tmax.list <- grep(".tif$", grep(paste(med, "_tasmax_rcp", rcp, sep=""), list.files(tmax.dir, full.names=T), value=T), value=T)
  
  #Make sure yearly biovar forecasts have been calculated
  if (length(ppt.list) == 0 || length(tmin.list) == 0 || length(tmax.list) == 0)
    stop(paste("Must compute yearly forecasts before calculating 30-year averages. 
             Please check that annual biovar forecasts have been computed."))
  
  # Create stacks containing all years of data.
  print(paste("opening 95-yr stacks for ppt, tmin, and tmax = 1140 bands"))
  ppt.stk <- unstack(stack(ppt.list))
  tmin.stk <- unstack(stack(tmin.list))
  tmax.stk <- unstack(stack(tmax.list))
  
  #Loop through to name output 30-yr images
  for (i in c(1:length(grps))) {
    #ppt image lists and output name
    ppt.imgs <- ppt.list[grps[[i]]]
    mid <- as.numeric(unlist(strsplit(basename(ppt.imgs[1]), "_"))[4]) + 15
    span <- paste(unlist(strsplit(basename(ppt.imgs[1]), "_"))[4],"-", unlist(strsplit(basename(ppt.imgs[length((grps[[i]]))]), "_"))[4], sep="") 
    out.ppt <- paste(sub("biovars", "ppt", bioclim.outdir), med, "_pr_rcp", rcp, "_",mid, "_yearRange_", span, ".tif", sep="")
    
    #tmin image lists and output name
    tmin.imgs <- tmin.list[grps[[i]]]
    out.tmin <- paste(sub("biovars", "tmin", bioclim.outdir), med, "_tasmin_rcp", rcp, "_",mid, "_yearRange_", span, ".tif", sep="")
    
    #tmax image lists and output name
    tmax.imgs <- tmax.list[grps[[i]]]
    out.tmax <- paste(sub("biovars", "tmax", bioclim.outdir), med, "_tasmax_rcp", rcp, "_",mid, "_yearRange_", span, ".tif", sep="")
    
    #biovar name
    out.biovars <- paste(bioclim.outdir, med, "_biovars_rcp", rcp, "_",mid, "_yearRange_", span, ".tif", sep="")
    
    #stack 30 year ppt block - bc of repetition, these can prob be put into a function.
    #quick bc it's a list :)
    print(paste("calculating 30-year averages for ", basename(out.ppt), sep=""))
    b <- ppt.stk[c(as.vector(bands[[i]]))]
    p.30 <- brick(jan(b), feb(b), mar(b), apr(b), may(b), jun(b), jul(b), aug(b), sep(b), oct(b), nov(b), dec(b)) 
    writeRaster(p.30, file=out.ppt, format="GTiff", dataType="INT4S", overwrite=T)
    
    #stack 30 year tmin block
    #quick bc it's a list :)
    print(paste("calculating 30-year averages for ", basename(out.tmin), sep=""))
    b <- tmin.stk[c(as.vector(bands[[i]]))]
    tmin.30 <- brick(jan(b), feb(b), mar(b), apr(b), may(b), jun(b), jul(b), aug(b), sep(b), oct(b), nov(b), dec(b)) 
    writeRaster(tmin.30, file=out.tmin, format="GTiff", dataType="INT4S", overwrite=T)
    
    #stack 30 year tmax block
    #quick bc it's a list :)
    print(paste("calculating 30-year averages for ", basename(out.tmax), sep=""))
    b <- tmax.stk[c(as.vector(bands[[i]]))]
    tmax.30 <- brick(jan(b), feb(b), mar(b), apr(b), may(b), jun(b), jul(b), aug(b), sep(b), oct(b), nov(b), dec(b)) 
    writeRaster(tmax.30, file=out.tmax, format="GTiff", dataType="INT4S", overwrite=T)
    
    #calc biovars
    print(paste("calculating 30-year averages for ", basename(out.biovars), sep=""))
    #Round biovars to whole numbers - avoid issues with floats.
    bio.vars <- round(biovars(p.30, tmin.30, tmax.30))
    writeRaster(bio.vars, file=out.biovars, format="GTiff", dataType="INT4S", overwrite=T)
    
  }#end loop.
}#end function

# redirect output to file (for selected variables) - FIX: sinking to log file
#is not appending - just overwriting. Not sure why.
#For now, decided to just write unique log file each time this script is run. 
#Be careful not to let logs pile up. 
date <- gsub(":", "", gsub(" ", "_", date()))
log.file <- file(paste(bioclim.outdir,"calc_30yr_biovars_", date, "_log.txt",sep=""))
sink(log.file)
sink(log.file, type="message")
print("*********************************************************************")
print("************** LCC-VP_calc_30-year_avg_forecasted_biovars.R Log *************")
print("*********************************************************************")

print(date)
print(paste("bioclim.indir = ", bioclim.indir, sep=""))
print(paste("bioclim.outdir = ", bioclim.outdir, sep=""))

biovars30 <- biovar30yr(bioclim.indir, bioclim.outdir, med, rcp)

#Restore message to console
sink()
sink(type="message")

#detach("package:dismo")
#detach("package:raster")