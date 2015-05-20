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
require(RColorBrewer)
#require(biomod2)
#
#############################  SET VARIABLES HERE TO RUN MANUALLY #########################
#
#
# #Response Image - reference image
# res.img <- 'C:/Share/LCC-VP/Parks/GRSM/analysis/spruce_fir/inputs/clim_srad/COVER_Spruce_fir_inPRISM_800m.tif'
# 
# #Variable Stack
# v.stk <- 'C:/Share/LCC-VP/Parks/GRSM/analysis/spruce_fir/inputs/clim_srad/biovars_1981-2010_srad_masked.tif'
# 
# #Text file containing variable names.  Names should be listed
# #in text file - each name on a separate line and in quotes.  They should 
# #be in the same order as they appear in v.stk and be the same length 
# #(# bands = # lines in text file).
# #Example file: C:/Share/LCC-VP/Parks/misc/DEWA_var_names.txt
# var.names <- "C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/GRSM_var_names_clim_srad.txt"
# 
# #Do you want to predict an output map layer? Answer Y or N.
# map <- "Y"
# 
# #Path/directory where output data will be stored.
# outDir <- 'C:/Share/LCC-VP/parks/GRSM/analysis/Spruce_fir/outputs/clim_srad/'
# 
# #model name (usually ES name)
# mod.name <- "Spruce_fir"
# 
# #Class name (as listed in attribute table of original classification) - NOT NEEDED?
# class.name <- "Cove Forest"
# 
# # Cull Variables for Multicolinearity? (Y/N)
# multicol <- "Y"
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

##########################################################################################
#Source the LCC-VP functions file
source("C:/Share/LCC-VP/scripts/R/LCC-VP_functions.R")

#set up logfile
# redirect output to file (for selected variables)
date <- gsub(":", "", gsub(" ", "_", date()))
log.file <- file(paste(outDir,mod.name,"_sdm_RF_", date, "_log.txt",sep=""))
sink(log.file)
sink(log.file, type="message")
print("*********************************************************************")
print("************** LCC-VP_sdm_RF.R Log *************")
print("*********************************************************************")

print(date)
print(paste("Training/response image = ", res.img, sep=""))
print(paste("Variable stack = ", v.stk, sep=""))
print(paste("list of variable names is in this txt file ", var.names, sep=""))
print(paste("Do you want a map = ", map, sep=""))
print(paste("Output directory = ", outDir, sep=""))
print(paste("Ecological System modeled = ", mod.name, sep=""))

#
################# VARIABLE NAMES #######################

# Text file containing variable names.  Names should be listed
# in text file - each name on a separate line and in quotes.  They should 
# be in the same order as they appear in v.stk and be the same length 
# (# bands = # lines in text file).
var.names <- read.table(var.names, sep="\n")[,1]

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
  
  # Load the response raster. 
  print(paste("loading response raster for ", mod.name, sep=""))
  r.img <- getValues(raster(res.img))
  #r.out <- r.img
  #r.diff <- r.img
  
  # Load predictor stack
  print(paste("loading predictor stack for ", mod.name, sep=""))
  pred.stk <- as.data.frame(getValues(brick(v.stk)))
  names(pred.stk) <- var.names
  
  #Use suitability image as a mask for NAs - has exact extent of valid data we want.
  #Also make sure suitability and preds have same length before applying mask.
  if (length(r.img) == dim(pred.stk)[1]) {
    #Set pred.stk to NA where r.img is NA
    pred.stk[is.na(r.img),] <- NA
    
    # Now make reverse NA mask with pred.stk and use it to mask r.img - ensures no NAs in model data, so avoids errors
    # that would be due to dimensions and positions of NAs.
    #Build composite vector of NAs from every band in pred.stk
    pred.mask <- as.vector(1:length(r.img))
    for (i in 1:ncol(pred.stk)) {
      pred.mask[is.na(pred.stk[,i])] <- NA
     } #end for
    
    #Now mask r.img with pred.mask
    r.img[is.na(pred.mask)] <- NA
    
    pred.na <- is.na(pred.mask)
    r.na <- is.na(r.img)  
  
    #Only run the rest of the code IF response image and predictor stack have identical
    #dimensions and positions of NA values.
    if (identical(r.na, pred.na)) {
      
      #If they match, remove NAs - pred.stk here is the kitchen sink group of predictors
      r.img <- na.omit(r.img)
      pred.stk <- na.omit(pred.stk)
      
      # For naming - descriptive of model type - kitchen sink, multicollinear removed, var selection etc.
      mod.type <- "ks"
      
      # a couple of lines if we decide having all model outputs in same directory to be too cumbersome.
      # just uncomment and replace outDir in rf.mod command to ks.dir
      #ks.dir <- paste(outDir, mod.type, sep="")
      #Create ks.dir - ignore if already exists.
      #dir.create(ks.dir, showWarnings=FALSE)
      
      # Run Random Forest classification algorithm for kitchen sink model 
      # writes results to outDir and returns model object
      randfor <- rf.mod(pred.stk, r.img, outDir, mod.name, mod.type)
      

      ################### Habitat Suitability Map Generation - Kitchen Sink ######################
      
      #Do you want to predict an output image (Y or N)? Set "map" variable at top of script. 
      if (map=="Y" | map == "y") {
        # Prediction
        pred <- pred.map(randfor, pred.stk, pred.mask, res.img, outDir, mod.name, mod.type)

      } #End if - kitchen sink map
      
      
      
      ################### CULL VARIABLES BASED ON MULTICOLLINEARITY ##############################
      if (multicol =="Y" | multicol == "y") {
        
        # For naming - descriptive of model type - kitchen sink, multicollinear removed, var selection etc.
        mod.type <- "multiCo"
        
        # Run Random Forest classification algorithm for kitchen sink model 
        # writes results to outDir and returns model object
        pred.mc <- multiCo(pred.stk, 0.05)
        randfor.mc <- rf.mod(pred.mc[[1]], r.img, outDir, mod.name, mod.type)
        
        #removed variables
        pred.rem <- pred.mc[[2]]
        
        # Graphics - correlation coefficients - we need to figure out exactly how to interpret QR Decomp from 
        # multicolinear function - later.
        preds.corr <- cor(pred.stk)
        
        pdf(file=paste(outDir,mod.name, "_corr_plot_", mod.type, ".pdf", sep=""), width=8, height=8)
        corr.graph <- circle.corr(preds.corr, pred.rem, bg="gainsboro")
        dev.off()
        
 
        ################ Habitat Suitability Map Generation - Culled for Multicollinearity #######
        if (map=="Y" | map == "y") {
          # Prediction
          pred <- pred.map(randfor.mc, pred.stk.mc, pred.mask, res.img, outDir, mod.name, mod.type)
          
        } #End if - multico map
      } #end if - multicolinear pred
      
      
      
    } else {
      print("ERROR: response image and predictor stack do not exactly match in positions of NA values")
    }
  } else {
    print("ERROR: Response image and predictor stack do not exactly match in dimensions")
  }
} else {
    print("ERROR, number of bands in variable stack does not equal number of names in variable list.")
}# end if 

#Detach packages
detach("package:maptools")
detach("package:randomForest")
detach("package:raster")
detach("package:foreign")
detach("package:rgdal")

 
