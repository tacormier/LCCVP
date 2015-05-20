#Calculate Annual Growing Degree Days above 5 C (GDD5) from
#from 800m PRISM monthly forecast data. Assumes input data are multiplied by 100 (as the PRISM data are).
#Equation from Sork et al. (2010)

#Tina Cormier

#load libraries
library(raster)

#source parameter file
source("C:/Share/LCC-VP/scripts/R/LCC-VP_parameters.R")

# #############################  SET VARIABLES HERE TO RUN MANUALLY #########################
# 
# # main dir with adjusted yearly forecast data
# gdd.indir <- 'C:/Share/LCC-VP/PACE/GRSM/climate_data/forecasts/adjusted_month_yearly/'
# 
# #output GDD directory
# gdd.outdir <- 'C:/Share/LCC-VP/PACE/GRSM/climate_data/forecasts/adjusted_month_yearly/GDD/'
# 
# #use median
# gdd.med <- "p50"
# 
# #use rcp8.5
# gdd.rcp <- "85"

#######
#Little hard-coded hack for current PRISM data - can delete when finished
# gdd.indir <- "C:/Share/LCC-VP/Parks/DEWA/climate_data/current_PRISM/"
# gdd.outdir <- "C:/Share/LCC-VP/Parks/DEWA/climate_data/current_PRISM/GDD/"
# 
# tmin.dir <- paste(gdd.indir, "tmin/stack/", sep="")
# tmin.list <- grep(".tif$", list.files(tmin.dir, full.names=T), value=T)
# 
# tmax.dir <- paste(gdd.indir, "tmax/stack/", sep="")
# tmax.list <- grep(".tif$", list.files(tmax.dir, full.names=T), value=T)

###########################################################################################
#GDD5 function takes a tmin and tmax 12-band stack and an output directory.
GDD5 <- function(in.tmin, in.tmax, gdd.outdir) {
  print(in.tmin)
  print(in.tmax)
  
  #Number of days in each month
  days <- c(31,28,31,30,31,30,31,31,30,31,30,31)
    
  #Read in raster stacks
  tmin <- stack(in.tmin)
  tmax <- stack(in.tmax)
  
  #output annual GDD raster
  file.parts <- unlist(strsplit(basename(in.tmin), "_"))
  #gdd.out <- paste(gdd.outdir, "gdd_", paste(file.parts[-c(2,7)], collapse="_"), ".tif", sep="")
  gdd.out <- paste(gdd.outdir, "gdd_", sub(pattern = "pr_", "", paste(file.parts, collapse="_")), sep="")
  gdd.out <- sub(pattern = "tasmin_", "", gdd.out)
  gdd.out <- sub(pattern = "tasmax_", "", gdd.out)
  
  
  #create raster of all 0's to start - will add monthly GDD to it!
  gdd <- raster(in.tmin)
  gdd <- setValues(gdd, 0)
  
  #make raster of days where the values for each band are the days in that month
  #Then fill in adjusted days based on if they are less than 5C
  print(paste("calculating annual GDD*100 for ",  file.parts[4], sep=""))
  days.ras <- tmin
  days.ras <- setValues(days.ras, 0)
  
  for (i in c(1:length(days))) {
    #print(i)
    days.ras[[i]] <- setValues(days.ras[[i]], days[i], layer=i)
    #When Tmin-i < 5_C < Tmax-i, we adjust the number of days in the month using D* = D(Tmax - 5)/(Tmax - Tmin).
    days.ras[[i]][tmin[[i]] < 500 & tmax[[i]] > 500] <- days.ras[[i]][tmin[[i]] < 500 & tmax[[i]] > 500]*(tmax[[i]][tmin[[i]] < 500 & tmax[[i]] > 500]-500)/(tmax[[i]][tmin[[i]] < 500 & tmax[[i]] > 500]-tmin[[i]][tmin[[i]] < 500 & tmax[[i]] > 500])
    
    #now create monthy GDD based on days.ras[[i]] and add to annual gdd
    #we are looping through each month, doing the calc, then setting anything <0 to 0
    gdd.mon <- (((tmin[[i]] + tmax[[i]])/2) - 500)*days.ras[[i]]
    gdd.mon[gdd.mon <0] <- 0
    gdd <- gdd + gdd.mon
        
  } #end creation of days.ras and GDD
  
  #Now round to whole number to avoid float errors in later calcs
  gdd <- round(gdd)
  
  #This is the original calculation, which did not set negative monthly values to 0.
  #gdd.orig <- round(sum((((tmin + tmax)/2) - 500)*days.ras))


  
  # At the end, sum up all 12 months.
  writeRaster(gdd, file=gdd.out, format="GTiff",dataType="INT4S", overwrite=T)
}

# redirect output to file (for selected variables) - FIX: sinking to log file
#is not appending - just overwriting. Not sure why.
date <- gsub(":", "", gsub(" ", "_", date()))
log.file <- file(paste(gdd.outdir,"calc_annual_GDD_", date, "_log.txt",sep=""), open="wt")
sink(log.file)
sink(log.file, type="message")
print("*********************************************************************")
print("************************** LCC-VP_GDD.R Log *************************")
print("*********************************************************************")

print(date)
print(paste("gdd.indir = ", gdd.indir, sep=""))
print(paste("gdd.outdir = ", gdd.outdir, sep=""))
print(paste("gdd.med = ", gdd.med, sep=""))
print(paste("gdd.rcp = ", gdd.rcp, sep=""))


#If all combos should be calculated, loop over serially
#would be nice to do on the cluster or in parallel, but I just want to get it
#running for now rather than dealing with moving data all around.
if (gdd.comboYN == "yes") {
  for (combo in gdd.allcombo) {
    print(combo)
    
    #overwrite med and rcp variables that were passed with paramters script
    #to reflect combo
    gdd.med <- unlist(strsplit(combo, "_"))[1]
    gdd.rcp <- unlist(strsplit(combo, "rcp"))[2]
    
    #set new outdir based on input dir and med and rcp values
    gdd.outdir2 <- paste(gdd.outdir, gdd.med, "_rcp", gdd.rcp, "/GDD/", sep="")
    
    #So this nested grep line is a little complicated, but it helps avoid listing
    #.tif.xml files that we don't want to operate on...just tif files!
    tmin.dir <- paste(gdd.indir, gdd.med, "_rcp", gdd.rcp,"/tmin/", sep="")
    #tmin.list <- grep(".tif$", grep(paste(gdd.med, "_tasmin_rcp", gdd.rcp, sep=""), list.files(tmin.dir, full.names=T), value=T), value=T)
    tmin.list <- list.files(tmin.dir, pattern="*.tif$", full.names=T)
    
    tmax.dir <- paste(gdd.indir,gdd.med, "_rcp", gdd.rcp,"/tmax/", sep="")
    #tmax.list <- grep(".tif$", grep(paste(gdd.med, "_tasmax_rcp", gdd.rcp, sep=""), list.files(tmax.dir, full.names=T), value=T), value=T)
    tmax.list <- list.files(tmax.dir, pattern="*.tif$", full.names=T)
    
    
    for (i in c(1:length(tmin.list))) {
      ann.gdd <- GDD5(tmin.list[i], tmax.list[i], gdd.outdir2)
    }#end for
  }#end for
  
  #if comboYN is no, still looking at forecasts, but just the single one identified in parameters file.
} else if (gdd.comboYN == "no") {
    #set outdir based on med and rcp values
    #set new outdir based on input dir and med and rcp values
    gdd.outdir2 <- paste(gdd.outdir, gdd.med, "_rcp", gdd.rcp, "/GDD/", sep="")
    
    #So this nested grep line is a little complicated, but it helps avoid listing
    #.tif.xml files that we don't want to operate on...just tif files!
    tmin.dir <- paste(gdd.indir, gdd.med, "_rcp", gdd.rcp,"/tmin/", sep="")
    #tmin.list <- grep(".tif$", grep(paste(gdd.med, "_tasmin_rcp", gdd.rcp, sep=""), list.files(tmin.dir, full.names=T), value=T), value=T)
    tmin.list <- list.files(tmin.dir, pattern="*.tif$", full.names=T)
    
    tmax.dir <- paste(gdd.indir,gdd.med, "_rcp", gdd.rcp,"/tmax/", sep="")
    #tmax.list <- grep(".tif$", grep(paste(gdd.med, "_tasmax_rcp", gdd.rcp, sep=""), list.files(tmax.dir, full.names=T), value=T), value=T)
    tmax.list <- list.files(tmax.dir, pattern="*.tif$", full.names=T)
    
    for (i in c(1:length(tmin.list))) {
      ann.gdd <- GDD5(tmin.list[i], tmax.list[i], gdd.outdir2)
    }#end for

  
  #If comboYN is NA, then we are working with PRISM data and don't need to pass med and rcp variables
} else if (gdd.comboYN == "na") {
    gdd.outdir2 <- gdd.outdir
    
    #So this nested grep line is a little complicated, but it helps avoid listing
    #.tif.xml files that we don't want to operate on...just tif files!
    tmin.dir <- paste(gdd.indir,"tmin/", sep="")
    #tmin.list <- grep(".tif$", list.files(tmin.dir, full.names=T), value=T)
    tmin.list <- list.files(tmin.dir, pattern="*.tif$", full.names=T)
    
    tmax.dir <- paste(gdd.indir,"tmax/", sep="")
    #tmax.list <- grep(".tif$", list.files(tmax.dir, full.names=T), value=T)
    tmax.list <- list.files(tmax.dir, pattern="*.tif$", full.names=T)
    
    for (i in c(1:length(tmin.list))) {
      ann.gdd <- GDD5(tmin.list[i], tmax.list[i], gdd.outdir2)
    }#end for
}#end if




#Restore message to console
sink()
sink(type="message")

