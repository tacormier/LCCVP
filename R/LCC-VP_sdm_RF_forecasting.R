##########################################################################################
# Date: November, 2012
# Authors: Tina Cormier 
# Purpose: Habitat suitability projections using random forest
#
#
# Load libraries
require(raster)
require(randomForest)

#
#############################  SET VARIABLES HERE TO RUN MANUALLY ########################
# 
# #Enter /path to dir containing of RF models (saved in previous step from LCC-VP_sdm_RF.R)
# rfdir <- "C:/Share/LCC-VP/Parks/GRSM/analysis/Cove_Forest_all/outputs/allVars_BiovarsGddVpdSoilsTWIsrad_20141209/"
# 
# #Original suitability image used as reference (not modeled).
# suit.img <- "C:/Share/LCC-VP/parks/GRSM/analysis/Cove_Forest_all/inputs/allVars_BiovarsGddVpdSoilsTWIsrad_20141209/COVER_Cove_Forest_all_inPRISM_800m_masked.tif"
# 
# #Directory containing forecasted predictors. Stacks must contain same bands in the same
# #order as biovars or v.stk.
# f.dir <- "C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/allVars_BiovarsGddVpdSoilsTWIsrad_20141222/forecast_imgs/ea_rcp85/"
# 
# #Model output directory - where output forecast maps will be stored.
# m.dir <- "C:/Share/LCC-VP/Parks/GRSM/analysis/Cove_Forest_all/outputs/allVars_BiovarsGddVpdSoilsTWIsrad_20141209/forecasts/ea_rcp85/"
# 
# # Text file containing variable names.  Names should be listed
# # in text file - each name on a separate line and in quotes.  They should 
# # be in the same order as they appear in predictor stacks and in original modeling stack 
# #and be the same length (# bands = # lines in text file).
# # Example file: C:/Share/LCC-VP/Parks/misc/DEWA_var_names.txt
# pred.names <- "C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/GRSM_var_names_allVars_20141009_BiovarsGddVpdSoilsTwiSrad.txt"
# 
# #original model name - would be same as mod.name in previous step. Name to use in forecasting.
# f.mod.name <- "Cove_Forest_all"

################## UNCOMMENT THIS TO CALL FROM PYTHON OR OTHER SCRIPT#####################
#
#Get the parameter file from the supplied argument
paramsForecast <- commandArgs(trailingOnly=TRUE)
#for testing:
#paramfile <- "C:/Share/LCC-VP/Parks/DEWA/analysis/EasternHemlock_NHF/inputs/EasternHemlock_NHF_R_params.txt"

# This script can be called by either LCC-VP_model_prep.py or run on it's own (would source R param file).
# If script is called by python, param file will be with it (and length of paramfile will will be > 0). 
# If calling from R, length of paramsForecast variable will be 0, so source R parameters file. 
if (length(paramsForecast) == 0) {
  #source R parameter file (LCC-VP_parameters.R)
  source("C:/Share/LCC-VP/scripts/R/LCC-VP_parameters.R")
} else {
  source(paramsForecast)
}#end if.

##########################################################################################
#set up logfile
# redirect output to file (for selected variables)
date <- gsub(":", "", gsub(" ", "_", date()))
log.file <- file(paste(m.dir,"sdm_RF_forecasting", date, "_log.txt",sep=""))
sink(log.file)
sink(log.file, type="message")
print("*********************************************************************")
print("************** LCC-VP_sdm_RF_forecasting.R Log *************")
print("*********************************************************************")

print(date)
print(paste("rf model dir = ", rfdir, sep=""))
print(paste("reference suitability image = ", suit.img, sep=""))
print(paste("directory containing forecasted predictor stacks = ", f.dir, sep=""))
print(paste("forecasted predictions will write to ", m.dir, sep=""))
print(paste("names of predictor layers found in = ", pred.names, sep=""))
print(paste("Ecological System modeled = ", f.mod.name, sep=""))


################# BIOVAR NAMES #######################
# Text file containing names of bands in predictor stack.  Names should be listed
# in text file - each name on a separate line and in quotes.  They should 
# be in the same order as they appear in v.stk and be the same length 
# (# bands = # lines in text file).
pred.names <- read.table(pred.names, sep="\n")[,1]


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

#####################################################
#list of forecast files
forecasts <- list.files(f.dir, pattern="*.tif$", full.names=T)

#Read in original suitability image used as reference data for modeling.
suitability.img <- getValues(raster(suit.img))

#create some directories if they don't exist
dir.create(paste0(m.dir), showWarnings=FALSE)

#f=forecasts[1]
#Loop over forecast data and predict map for each one:
for (f in forecasts) {
  b <- GDALinfo(f)[3]
  num.names <- length(pred.names)
  
  if (b == num.names) {
    es <- f.mod.name
    #assumes directory name is combo of rcp and med
    rcpmed <- unlist(strsplit(f, "/"))
    rcpmed <- rcpmed[length(rcpmed)-1]
    # FIX - this only works if the year is at the end of the file name, just before
    # the file extension
    yr.pos <- regexpr("[0-9]{4}\\.", basename(f))
    yr <- sub(pattern="\\.",replacement = "",regmatches(basename(f),yr.pos))
    
    print(paste("predicting habitat suitability for ", yr, sep=""))
    pred.stk <- as.data.frame(getValues(brick(f)))
    names(pred.stk) <- pred.names
    
    #THIS IS ONLY A HACK TO DEAL WITH OUR FIRST ROUND OF HIERARCHICAL MODELING,
    #DURING WHICH I USED BIOVARS THAT WERE NOT *100. FIX AND REMOVE THESE LINES. 
    #Divide every column by 100 except columns 3 and 15
#     pred.stk <- pred.stk/100
#     pred.stk[,3] <- pred.stk[,3]*100
#     pred.stk[,15] <- pred.stk[,15]*100
 
    #Set some variables of same dims as original suitability image used as reference data for modeling.
    s.img <- p.out <- d.out <- suitability.img
    
    #Use suitability image as a mask for NAs - has exact extent of valid data we want.
    #Also make sure suitability and preds have same length before applying mask.
    if (length(s.img) == dim(pred.stk)[1]) {
      
      #Set pred.stk to NA where s.img is NA
      pred.stk[is.na(s.img),] <- NA
      
      #Now make reverse NA mask with pred.stk and use it to mask s.img - ensures no NAs in model data, so avoids errors
      #due to dimensions and positions of NAs.
      #Build composite vector of NAs from every band in pred.stk
      pred.mask <- as.vector(1:length(s.img))
      for (i in 1:ncol(pred.stk)) {
        pred.mask[is.na(pred.stk[,i])] <- NA
      } #end for
      
      #Now mask s.img with pred.mask
      s.img[is.na(pred.mask)] <- NA
      
      pred.na <- is.na(pred.mask)
      s.na <- is.na(s.img)

      
      #Only run the rest of the code IF suitability image and predictor stack have identical
      #dimensions and positions of NA values.
      if (identical(pred.na, s.na)) {
        
        #set up p.out and d.out
        #set output and diff images - only need one layer - just to write output map.
        #Use pred.mask bc used it as a mask above.
        p.out[is.na(pred.mask)] <- NA
        #d.out[is.na(pred.mask)] <- NA
        
        pred.stk <- na.omit(pred.stk)
        s.img <- na.omit(s.img)
    
        # loop over rf models and predict each one for each year
        rf.mods <- list.files(rfdir,pattern="*.RData", full.names=T)
        for (rf in rf.mods) {
          randfor <- get(load(rf))
          #assumes model variation is the last part of the rdata file name.
          mod.type <- unlist(strsplit(rf, "_"))
          mod.type <- sub(pattern = "\\.RData", replacement = "", mod.type[length(mod.type)])
          
          #create subdirs for each model
          subd <- paste0(m.dir, mod.type,"/")
          dir.create(subd, showWarnings=FALSE)
          
          # want to use rf predict function, not raster's (addressed this by loading packages
          # in a different order).
          #detach("package:raster")
          #Don't need to specify columns from pred.stk, but I want to just be explicit so I know what's going on.
          pred <- predict(randfor, pred.stk[,names(randfor$forest$ncat)])
          #require(raster)
         
          #Insert predictions into full raster - i.e. put NA's back in so we
          #can write out the image.
          #First, resent p.out to 999 where it is not NA - this will help with QA/QC - 
          #if any 999 in final product = problem.
          p.out[!is.na(p.out)] <- 999
          p.out[!is.na(p.out)] <- pred
          #summary(pred.stk)
          
          #diff image
          #diff <- pred - s.img
          #d.out[is.na(d.out) == F] <- diff
          
          # Create the output suitability map.
          img.out <- raster(suit.img)
          img.out <- setValues(img.out, round(p.out, digits=0))
          
          #edited by TC for hierarchical testing
          outImage <- paste0(subd, sub("rf_model_", "", basename(sub("\\.RData", "", rf))), "_pred_", rcpmed, "_",yr, ".tif")
                            
          
          writeRaster(img.out, filename=outImage, format="GTiff", dataType="INT1U", overwrite=T)
        }# end model loop
        
#         # Create diff raster between future forecast and present suitability. 
#         # High values are over-predicted and vice versa.
#         img.diff <- raster(suit.img)
#         img.diff <- setValues(img.diff, round(d.out, digits=0))
#         #diffImage <- paste(m.dir, es, "_", p, "_", rcp, "_", yr, "_minus_current_projection_rf.tif", sep="")
#         diffImage <- paste(m.dir, es, "_", yr, "_minus_current_projection_rf.tif", sep="")
#         writeRaster(img.diff, filename=diffImage, format="GTiff", dataType="INT1S", overwrite=T)
      } else {
          print("ERROR: response image and predictor stack do not exactly match in either dimensions or positions of NA values")
      } #end if   
    } else {
        print(paste("dimensions for suitability layer and predictor stack are not the same - cannot predict.", sep=""))
      } #end if - dimensions and NA position
  } else {
      print("ERROR, number of bands in variable stack does not equal number of names in variable list.")
  } #end bands if
} #end loop


#Restore output to console
sink()
sink(type="message")

#Detach packages
# detach("package:randomForest")
# detach("package:raster")


