#Adjust forecasted PPT from mm/day to mm/month and *100 to match PRISM. 
#Adjust forecasted tmin and tmax to deg C*100 to match PRISM.

#Tina Cormier

#load libraries
library(raster)

#source parameter file
source("C:/Share/LCC-VP/scripts/R/LCC-VP_parameters.R")
#Reads in variables: park, bioclim.indir, bioclim.outdir, med, and rcp

#############################  SET VARIABLES HERE TO RUN MANUALLY #########################
#
# # main dir with forecast data
# bioclim.indir <- 'C:/Share/LCC-VP/PACE/GRSM/PRISM/forecasts/adjusted_month_yearly/'
# 
# #output directory - typically looking for directory like:
#"C:/Share/LCC-VP/PACE/GRSM/climate_data/forecasts/adjusted_month_yearly/", and it has
# scenario directories underneath it, which each have ppt, tmin, tmax: /ea_rcp26/ppt
# bioclim.outdir <- 'C:/Share/LCC-VP/PACE/GRSM/PRISM/forecasts/adjusted_month_yearly/'
# 
# #use median
# med <- "p50"
# 
# #use rcp8.5
# rcp <- "85"
# 
#
############################# FUNCTIONS #################################
# For precip data adjustment
# precip data are in mm/day - need to mult each ppt ras by #days in the month.
# mult factors
adj_ppt <- function(image, out.file) {
  print(paste("adjusting precip values for ", basename(image), sep=""))
  days <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  
  #make adjustment - mult image * # of days in month for each band. Mult that by 100 to match Prism (mm*100). 
  #Round to avoid float errors in
  #subsequent math operations.
  img.adj <- round((brick(image) * days)*100)
  
  #set output datatype, though this argument is somehow usually ignored??
  dataType(img.adj) <- "INT4S"

  #write out new, adjusted raster
  writeRaster(img.adj, file=out.file, format="GTiff",dataType="INT4S", overwrite=T)

}#end adj_ppt function


#Adjust tmin and tmax to DegC*100 to match PRISM
adj_tminmax <- function(image, out.file) {
  print(paste("adjusting temperature values to deg C*100 for ", basename(image), sep=""))
  img <- round(brick(image)*100)
  writeRaster(img, out.file, format="GTiff", dataType="INT4S", overwrite=T)
}#end adj_tminmax function


# redirect output to file (for selected variables) - FIX: sinking to log file
#is not appending - just overwriting. Not sure why.
date <- gsub(":", "", gsub(" ", "_", date()))
log.file <- file(paste(bioclim.outdir,"LCC-VP_adjust_clim_forecasts_toPRISM_yearly_", date, "_log.txt",sep=""), open="wt")
sink(log.file)
sink(log.file, type="message")
print("*********************************************************************")
print("************** LCC-VP_adjust_clim_forecasts_toPRISM_yearly.R Log *************")
print("*********************************************************************")

print(date)
print(paste("bioclim.indir = ", bioclim.indir, sep=""))
print(paste("bioclim.outdir = ", bioclim.outdir, sep=""))
print(paste("bioclim.params = ", bioclim.params, sep=""))

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
    
    #loop over climate vars
    for (param in bioclim.params){
      #set new indir and outdir based on input dir and med and rcp values
      bioclim.indir2 <- paste(bioclim.indir, med, "_rcp", rcp, "/", param, "/", sep="")
      bioclim.outdir2 <- paste(bioclim.outdir, med, "_rcp", rcp, "/", param, "/", sep="")
      
      #Get appropriate image list
      img.list <- list.files(bioclim.indir2, pattern="*.tif$", full.names=T)
      
      if (param == "ppt") {
        #adjust from mm/day to mm/month and then *100 to get mm*100/month
        for (img.ppt in img.list){
          out.img <- paste(bioclim.outdir2, unlist(strsplit(basename(img.ppt), "\\."))[1], "_adj.tif", sep="")
          adj_ppt(img.ppt, out.img)
        }#end img.ppt loop
        
      } else if (param == "tmin" || param == "tmax") {
        #adjust by *100 to get degC*100
        for (img.tm in img.list) {
          out.img <- paste(bioclim.outdir2, unlist(strsplit(basename(img.tm), "\\."))[1], "_adj.tif", sep="")
                    
          #run adj_tminmax function
          adj_tminmax(img.tm, out.img) 
        }#end img.tm loop
        
      } else {
          stop(paste("params variable may include 'ppt,' 'tmin,' and 'tmax' - you set params to: ", paste(params, collapse=" ", sep=" "), sep=""))
      }# end params if
    }# end params for
  }#end combo loop

  #if comboYN is no, still looking at forecasts, but just the single one identified in parameters file bioclim.indir and bioclim.outdir.
} else if (comboYN == "no") {
  #loop over climate vars
  for (param in bioclim.params){
    #set new indir and outdir based on input dir and med and rcp values
    bioclim.indir2 <- paste(bioclim.indir, param, "/", sep="")
    bioclim.outdir2 <- paste(bioclim.outdir, param, "/", sep="")
    
    #Get appropriate image list
    img.list <- list.files(bioclim.indir2, pattern="*.tif$", full.names=T)
    
    if (param == "ppt") {
      #adjust from mm/day to mm/month and then *100 to get mm*100/month
      for (img.ppt in img.list){
        out.img <- paste(bioclim.outdir2, unlist(strsplit(basename(img.ppt), "\\."))[1], "_adj.tif", sep="")
        adj_ppt(img.ppt, out.img)
      }#end img.ppt loop
      
      #adjust by *100 to get degC*100
    } else if (param == "tmin" || param == "tmax") {
      for (img.tm in img.list) {
        out.img <- paste(bioclim.outdir2, unlist(strsplit(basename(img.tm), "\\."))[1], "_adj.tif", sep="")
        
        #run adj_tminmax function
        adj_tminmax(img.tm, out.img) 
      }#end img.tm loop
      
    } else {
      stop(paste("params variable may include 'ppt,' 'tmin,' and 'tmax' - you set params to: ", paste(params, collapse=" ", sep=" "), sep=""))
    }# end params if
  }# end params for
  
  
} else {
  print("ComboYN must be 'yes' or 'no' for this script. Check your parameters.R file")
}#end if



#Restore message to console
sink()
sink(type="message")


#detach("package:raster")


