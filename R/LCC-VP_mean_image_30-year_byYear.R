#Create 30-year average images from a list of yearly images.
#This script works with single band rasters only. 
#Window increments by one year.

#Tina Cormier

#load libraries
library(raster)
library(dismo)

#source parameter file for input and output directories
source("C:/Share/LCC-VP/scripts/R/LCC-VP_parameters.R")

############################  SET VARIABLES HERE TO RUN MANUALLY #########################
# #
# # main dir with yearly forecast data
# img30.indir <- 'C:/Share/LCC-VP/PACE/GRSM/climate_data/forecasts/adjusted_month_yearly/GDD/'
# 
# # Where to output 30-yr average rasters
# img30.outdir <- 'C:/Share/LCC-VP/PACE/GRSM/climate_data/forecasts/ave_30yr_byYear/GDD/'
# 
# #start year
# s.year <- 2006
# 
# #end year
# e.year <- 2100
############################# FUNCTIONS #################################

#Calculate 30-year mean images from single band yearly images.
img_30yr <- function(img30.indir, img30.outdir, start.year, end.year) {
  
  # list yearly images
  #So this nested grep line is a little complicated, but it helps avoid listing
  #.tif.xml files that we don't want to operate on...just tif files!
  img30.list <- grep(".tif$", list.files(img30.indir, full.names=T), value=T)
  
  
  #Make sure yearly biovar forecasts have been calculated
  if (length(img30.list) == 0)
    stop(paste("Must compute yearly forecasts before calculating 30-year averages. 
             Please check that annual biovar forecasts have been computed."))
  
  # Create stacks containing all years of data.
  print(paste("opening yearly images and stacking"))
  img30.stk <- unstack(stack(img30.list))
  
  # determine 30-year groupings
  grps <- list()
   
  #iterators
  j <- 30
  
  for (m in c(1:(length(img30.list)-29))) {
    grps[[m]] <- c(m:j)
    
    #advance iterators
    j <- j+1
  }
  
  #Loop through to name output 30-yr images
  for (i in c(1:length(grps))) {
    #ppt image lists and output name
    img30.imgs <- img30.list[grps[[i]]]
    #years
    years <- c(s.year:e.year)
    mid <- ceiling(median(years[grps[[i]]]))
    span <- paste(years[grps[[i]]][[1]], "-", years[grps[[i]]][[length(years[grps[[i]]])]], sep="")
    
    #outfile name
    out.img30 <- paste(img30.outdir, gsub(as.character(years[i]), "", unlist(strsplit(basename(img30.imgs[1]), "\\."))[1]), mid, "_yearRange_", span, ".tif", sep="")
    
    print(paste("calculating 30-year averages for ", basename(out.img30), sep=""))
    #stack 30 year block - and take the mean.
    img30.mean <- round(mean(stack(img30.stk[grps[[i]]])))
    writeRaster(img30.mean, file=out.img30, format="GTiff", dataType="INT4S", overwrite=T)
    
  }#end loop.
}#end function

# redirect output to file (for selected variables) - FIX: sinking to log file
#is not appending - just overwriting. Not sure why.
#For now, decided to just write unique log file each time this script is run. 
#Be careful not to let logs pile up. 
date <- gsub(":", "", gsub(" ", "_", date()))
log.file <- file(paste(img30.outdir,"calc_30yr_mean_image_", date, "_log.txt",sep=""))
sink(log.file)
sink(log.file, type="message")
print("*********************************************************************")
print("************** LCC-VP_mean_image_30-year_byYear.R Log ***************")
print("*********************************************************************")

print(date)
print(paste("img30.indir = ", img30.indir, sep=""))
print(paste("img30.outdir = ", img30.outdir, sep=""))
print(paste("start year = ", s.year, sep=""))
print(paste("end year = ", e.year, sep=""))

#If all combos should be calculated, loop over serially
#would be nice to do on the cluster or in parallel, but I just want to get it
#running for now rather than dealing with moving data all around.
if (img30.comboYN == "yes") {
  for (combo in img30.allcombo) {
    print(combo)
    
    #overwrite med and rcp variables that were passed with paramters script
    #to reflect combo
    med <- unlist(strsplit(combo, "_"))[1]
    rcp <- unlist(strsplit(combo, "rcp"))[2]
    
    #set new indir/outdir based on input dir and med and rcp values
    img30.indir2 <- paste(img30.indir, med, "_rcp", rcp, sep="")
    img30.outdir2 <- paste(img30.outdir, med, "_rcp", rcp, sep="")  

    mean_30yr <- img_30yr(img30.indir2, img30.outdir2, s.year, e.year)
    
  }#endfor
} else {
    mean_30yr <- img_30yr(img30.indir, img30.outdir, s.year, e.year)
}#end if 

#Restore message to console
sink()
sink(type="message")

#detach("package:dismo")
#detach("package:raster")