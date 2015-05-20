#Create 12-month stacks and Calculate bioclimatic variables 
#from 800m monthly forecast data. 

#Tina Cormier

#load libraries
library(raster)
library(dismo)

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

#put whole script into a function to aid in error message control and logfile
#documentation.
adj_tminmax <- function(image, out.file) {
  


    
    #mult tmin and tmax by 100 to match units in PRISM (degrees C *100)..and therefore the RF model. 
    #ALSO, round to nearest integer to avoid floating point errors.
    #write out unit-adjusted tmin
    tmin <- round(brick(tmin.list[j])*100)
    
    adjtmin.dir <- paste(bioclim.outdir, "tmin/", sep="")
    tmin.name <- unlist(strsplit(basename(tmin.list[j]), "_"))
    tmin.name <- paste(adjtmin.dir, tmin.name[1], "_", tmin.name[2], "_", tmin.name[3], "_", tmin.name[4], "_adj.tif", sep="")
    writeRaster(tmin, tmin.name, format="GTiff", dataType="INT4S", overwrite=T)
    
    #write out unit-adjusted tmax
    tmax <- round(brick(tmax.list[j])*100)
    
    adjtmax.dir <- paste(bioclim.outdir, "tmax/", sep="")
    tmax.name <- unlist(strsplit(basename(tmax.list[j]), "_"))
    tmax.name <- paste(adjtmax.dir, tmax.name[1], "_", tmax.name[2], "_", tmax.name[3], "_", tmax.name[4], "_adj.tif", sep="")
    writeRaster(tmax, tmax.name, format="GTiff", dataType="INT4S", overwrite=T)
    
    
#     biovar.out <- paste(bioclim.outdir, "biovars/", bio.name, sep="")
#     #Round biovars to whole numbers - avoid issues with floats.
#     bio.vars <- round(biovars(ppt,tmin,tmax))
#     writeRaster(bio.vars,biovar.out, format="GTiff", dataType="INT4S", overwrite=T)
  } #end j loop
  
}#end function

# redirect output to file (for selected variables) - FIX: sinking to log file
#is not appending - just overwriting. Not sure why.
date <- gsub(":", "", gsub(" ", "_", date()))
log.file <- file(paste(bioclim.outdir,"calc_yearly_forecasted_biovars_", date, "_log.txt",sep=""), open="wt")
sink(log.file)
sink(log.file, type="message")
print("*********************************************************************")
print("************** LCC-VP_calc_yearly_forecasted_biovars.R Log *************")
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
        for (img.ppt in img.list){
          #out.img <- paste(bioclim.outdir2, unlist(strsplit(basename(img), "\\."))[1], "_adj.tif", sep="")
          #just changed outname for now for the sake of fixing ppt images. Put back later (2/26/14)
          out.img <- paste(bioclim.outdir2, unlist(strsplit(basename(img.ppt), "\\."))[1], ".tif", sep="")
          ppt.out <- adj_ppt(img.ppt, out.img)
          
          
        }#end img loop
      } else if (param == "tmin" || param == "tmax") {
          for (img.tm in img.list)
            out.img <- paste(bioclim.outdir2, unlist(strsplit(basename(img.tm), "\\."))[1], "_adj.tif", sep="")
            #just changed outname for now for the sake of fixing ppt images. Put back later (2/26/14)
            #out.img <- paste(bioclim.outdir2, unlist(strsplit(basename(img), "\\."))[1], ".tif", sep="")
          
            #run adj_tminmax function
            biovars_yr <- biovars_yrly(bioclim.indir, bioclim.outdir2, med, rcp)
      } else {
          stop(paste("params variable may include 'ppt,' 'tmin,' and 'tmax' - you set params to: ", paste(params, collapse=" ", sep=" "), sep=""))
      }# end params if
  }# end params for

  #if (p == "ppt"){
    
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
    ppt.pattern <- paste(med, "_pr_rcp", rcp, "_[0-9][0-9][0-9][0-9]_out_adj.tif$", sep="")
    ppt.list <- list.files(adjppt.dir, pattern=ppt.pattern, full.names=T)
    
    tmin.dir <- paste(bioclim.indir, "tmin/", sep="")
    tmin.pattern <- paste(med, "_tasmin_rcp", rcp,"_[0-9][0-9][0-9][0-9]_out.tif$", sep="")
    tmin.list <- list.files(tmin.dir, pattern=tmin.pattern, full.names=T)
    
    tmax.dir <- paste(bioclim.indir, "tmax/", sep="")
    tmax.pattern <- paste(med, "_tasmax_rcp", rcp,"_[0-9][0-9][0-9][0-9]_out.tif$", sep="")
    tmax.list <- list.files(tmax.dir, pattern=tmax.pattern, full.names=T)
    
  #calc biovars for forecasts
  for (j in c(1:(length(tmax.list)))) {
  
  
}#end params loop
  

  
  #if comboYN is no, still looking at forecasts, but just the single one identified in parameters file.
} else if (comboYN == "no") {
    #set outdir based on med and rcp values
    bioclim.outdir <- paste(bioclim.outdir, med, "_rcp", rcp, "/", sep="")
    #run biovars_yrly function
    biovars_yr <- biovars_yrly(bioclim.indir, bioclim.outdir, med, rcp)
    
  #If comboYN is NA, then we are working with PRISM data and don't need to pass med and rcp variables  
} else {
    print("ComboYN must be 'yes' or 'no' for this script. Check your parameters.R file")
}#end if
    

#Restore message to console
sink()
sink(type="message")

detach("package:dismo")
detach("package:raster")


