##########################################################################################
# Date: November, 2012
# Authors: Tina Cormier 
# Purpose: Habitat suitability mapping using random forest
#
##########################################################################################

#Load libraries
require(maptools)
require(randomForest)
require(raster)
require(foreign)
require(rgdal)
require("biomod2")
#
#############################  SET VARIABLES HERE TO RUN MANUALLY #########################
#
# #
# Response Image
# res.img <- 'C:/Share/LCC-VP/parks/DEWA/analysis/EasternHemlock_NHF/inputs/COVER_EasternHemlock_NHF_inPRISM_800m.tif'
# 
# Variable Stack
# v.stk <- 'C:/Share/LCC-VP/parks/DEWA/analysis/EasternHemlock_NHF/inputs/biovars_1981-2010_masked.tif'
#
# Text file containing variable names.  Names should be listed
# in text file - each name on a separate line and in quotes.  They should 
# be in the same order as they appear in v.stk and be the same length 
# (# bands = # lines in text file).
# Example file: C:/Share/LCC-VP/Parks/misc/DEWA_var_list.txt
# var.list <- "C:/Share/LCC-VP/Parks/DEWA/analysis/common_inputs/DEWA_var_list.txt"
# 
# Do you want to predict an output map layer? Answer Y or N.
# map <- "Y"
# 
# Path/directory where output data will be stored.
# outDir <- 'C:/Share/LCC-VP/parks/DEWA/analysis/EasternHemlock_NHF/outputs/'
# 
# #model name (usually spp name)
# mod.name <- "EasternHemlock_NHF"
#
#NOT YET IMPLEMENTED
# Do you want to predict on forecasted data? Answer Y or N.
#
# If you do want to forecast, enter the path that contains forecasts.
#forecast.dir <- 

################## UNCOMMENT THIS TO CALL FROM PYTHON OR OTHER SCRIPT#####################
#
#Get the parameter file from the supplied argument
paramfile <- commandArgs(trailingOnly=TRUE)
#for testing:
#paramfile <- "C:/Share/LCC-VP/Parks/DEWA/analysis/EasternHemlock_NHF/inputs/EasternHemlock_NHF_R_params.txt"

# This script can be called by either LCC-VP_model_prep.py or run on it's own (would source R param file).
# If script is called by python, param file will be with it (and length of paramfile will will be > 0). 
# If calling from R, length of paramfile variable will be 0, so source R parameters file. 
if (length(paramfile) == 0) {
  #source R parameter file (LCC-VP_parameters.R)
  source("C:/Share/LCC-VP/scripts/R/LCC-VP_parameters.R")
} else {
    source(paramfile)
  }#end if.


################# PRINT VARIABLES ####################
#
print(res.img)
print(v.stk)
print(map)
print(outDir)
print(mod.name)
print(var.list)
#
################# VARIABLE NAMES #######################

# Text file containing variable names.  Names should be listed
# in text file - each name on a separate line and in quotes.  They should 
# be in the same order as they appear in v.stk and be the same length 
# (# bands = # lines in text file).
var.names <- read.table(var.list, sep="\n")[,1]

# b1 <- "Annual Mean Temperature"
# b2 <- "Mean Diurnal Range"
# b3 <- "Isothermality"
# b4 <- "Temperature Seasonality"
# b5 <- "Max Temperature of Warmest Month"
# b6 <- "Min Temperature of Coldest Month"
# b7 <- "Temperature Annual Range"
# b8 <- "Mean Temperature of Wettest Quarter"
# b9 <- "Mean Temperature of Driest Quarter"
# b10 <- "Mean Temperature of Warmest Quarter"
# b11 <- "Mean Temperature of Coldest Quarter"
# b12 <- "Annual Precipitation"
# b13 <- "Precipitation of Wettest Month"
# b14 <- "Precipitation of Driest Month"
# b15 <- "Precipitation Seasonality"
# b16 <- "Precipitation of Wettest Quarter"
# b17 <- "Precipitation of Driest Quarter"
# b18 <- "Precipitation of Warmest Quarter"
# b19 <- "Precipitation of Coldest Quarter"

# Check that number of bands in v.stk = # of lines in text file
# If yes, run script. If not, error.
b <- GDALinfo(v.stk)[3]
num.names <- length(var.names)

if (b == num.names) {
  
  #####################################################
  
  # Naming the output images by manipulating res.img
  outImage <- paste(sub("inPRISM_800m.tif", "", sub("COVER_", "", sub("inputs", "outputs", res.img))),"current_suitability_rf.tif", sep="")
  diffImage <- paste(sub("inPRISM_800m.tif", "", sub("COVER_", "", sub("inputs", "outputs", res.img))),"modeled_suitability_minus_ref_rf.tif", sep="")
  
  # Load the response raster. 
  print(paste("loading response raster for ", mod.name, sep=""))
  r.img <- getValues(raster(res.img))
  r.out <- r.img
  r.diff <- r.img
  
  # Load predictor stack
  print(paste("loading predictor stack for ", mod.name, sep=""))
  pred.stk <- as.data.frame(getValues(brick(v.stk)))
  names(pred.stk) <- var.names
  #manual check to make sure NAs match.
  r.na <- is.na(r.img)
  pred.na <- is.na(pred.stk[,1])
  
  #Only run the rest of the code IF response image and predictor stack have identical
  #dimensions and positions of NA values.
  if (identical(r.na, pred.na)) {
    
    #If they match, remove NAs
    r.img <- na.omit(r.img)
    pred.stk <- na.omit(pred.stk)
    
    #format data to be used with biomod2
    
    
    # Run Random Forest classification algorithm
    print(paste("computing random forest model. . ."))
    randfor <- randomForest(pred.stk, r.img)
    
    # View classification results.
    randfor
    
    # View variable importance plot.
    #varImpPlot(randfor)
    
    #save rf model for later :)
    rf.mod <- paste(outDir,"rf_model_", mod.name, ".RData", sep="")
    save(randfor, file= rf.mod)
    
    ############### Model output summary files ############################
    # Plotting parameters
    str <- 1
    dpi <- 400
    
    # Variable Importance Plot.
    pdf(file=paste(outDir,"rf_varImp_", mod.name, ".pdf", sep=""),family="Times")
    varImpPlot(randfor, pch=16, col="blue",main=paste("Variable Importance - ", mod.name, sep=' '))
    dev.off()
    
    # redirect output to file (for selected variables)
    sink(file=paste(outDir,"rf_model_results_", mod.name, ".txt",sep=""))
    print(paste("***********************************************************************", sep=""))
    print(paste("************** randomForest model results - ", mod.name," *************", sep=""))
    print(paste("***********************************************************************", sep=""))
    print(randfor)
    #turn off output to file
    sink()
    
    ################### Habitat Suitability Map Generation ######################
    
    #Do you want to predict an output image (Y or N)? Set "map" variable at top of script. 
    if (map=="Y" | map == "y") {
      # Prediction
      # Detach the raster package because we want to use the randomForest version of 
      # predict, instead of the "raster" version.
      detach("package:raster")
      pred <- predict(randfor, pred.stk)
      r.out[is.na(r.out) == F] <- pred
      
      diff <- pred - r.img
      r.diff[is.na(r.diff) == F] <- diff
      
      # Write the output raster maps.
      # Reload the dismo and raster package.
      require(raster)
      print("Writing output raster")
      
      # Create the output suitability map.
      img.out <- raster(res.img)
      img.out <- setValues(img.out, round(r.out, digits=0))
      writeRaster(img.out, filename=outImage, format="GTiff", dataType="INT1U", overwrite=T)
      
      # Create diff raster.
      img.diff <- raster(res.img)
      img.diff <- setValues(img.diff, round(r.diff, digits=0))
      writeRaster(img.diff, filename=diffImage, format="GTiff", dataType="INT1S", overwrite=T)
    } #End if
    
  } else {
    print("ERROR: response image and predictor stack do not exactly match in either dimensions or positions of NA values")
  }
} else {
  print("ERROR, number of bands in variable stack does not equal number of names in variable list.")
}# end if 

 
