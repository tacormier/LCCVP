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
require(caret)
#require(biomod2)
#
# #############################  SET VARIABLES HERE TO RUN MANUALLY #########################
#
# #
# #Response Image - reference image
# res.img <- 'C:/Share/LCC-VP/Parks/GRSM/analysis/spruce_fir/inputs/clim_srad/COVER_Spruce_fir_inPRISM_800m.tif'
res.img <- 'C:/Share/LCC-VP/EcoRegions/Bailey/analysis/Cove_Forest_GAP_GRSM/inputs/clim_only_BiovarsGddVpd/COVER_Cove_Forest_GAP_GRSM_inPRISM_800m_masked.tif'

# #Variable Stack
# v.stk <- 'C:/Share/LCC-VP/Parks/GRSM/analysis/spruce_fir/inputs/clim_srad/biovars_1981-2010_srad_masked.tif'
v.stk <- 'C:/Share/LCC-VP/EcoRegions/Bailey/analysis/Cove_Forest_GAP_GRSM/inputs/clim_only_BiovarsGddVpd/H1_BlueRidge_climOnly_masked.tif'
#
# #Text file containing variable names.  Names should be listed
# #in text file - each name on a separate line and in quotes.  They should 
# #be in the same order as they appear in v.stk and be the same length 
# #(# bands = # lines in text file).
# #Example file: C:/Share/LCC-VP/Parks/misc/DEWA_var_names.txt
# var.names <- "C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/GRSM_var_names_clim_srad.txt"
var.names <- "C:/Share/LCC-VP/EcoRegions/Bailey/analysis/common_inputs/current_var_lists/H1_BlueRidge_climOnly/H1_climOnly_var_names.txt"
# 
# #Do you want to predict an output map layer? Answer Y or N.
 map <- "Y"
# 
# #Path/directory where output data will be stored.
# outDir <- 'C:/Share/LCC-VP/parks/GRSM/analysis/Spruce_fir/outputs/clim_srad/'
outDir <- "C:/Share/LCC-VP/EcoRegions/Bailey/analysis/Cove_Forest_GAP_GRSM/outputs/clim_only_BiovarsGddVpd/"
# 
# #model name (usually spp name)
# mod.name <- "Spruce_fir"
mod.name <- "Cove_Forest_all"
#
# #Class name (as listed in attribute table of original classification)
# class.name <- "Spruce-fir forest"
class.name <- "Cove Forest"
#
# # Cull Variables for Multicolinearity? (Y/N)
# # multicol <- "Y"
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
log.file <- file(paste(outDir,"sdm_RF_", date, "_log.txt",sep=""))
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
  
  #Use suitability image as a mask for NAs - has exact extent of valid data we want.
  #Also make sure suitability and preds have same length before applying mask.
  if (length(r.img) == dim(pred.stk)[1]) {
    #Set pred.stk to NA where r.img is NA
    pred.stk[is.na(r.img),] <- NA
    #Now make reverse NA mask with pred.stk and use it to mask r.img - ensures no NAs in model data, so avoids errors
    #due to dimensions and positions of NAs.
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
      
      #SOME sampling for large datasets that exceed memory 
      #Stratified random sample
      
      # Function from http://adammcelhinney.com/2012/04/10/r-function-for-stratified-sampling/ modified by
      # Tina Cormier to preserve original order of the data
      
      #First add some columns to r.img to ID the groups as well as the original order of the data
      ord <- c(1:length(r.img))
      r.img.strat <- as.data.frame(cbind(r.img, ord))
      # Tried intitially to create 10 groups (plus 0 with its own group), 
      # but won't have enough data in the upper groups to sample:
      #  1     2     3     4     5     6     7     8     9    10 
      #34733 19095  5240  1893  1531  1344  1055   528   198    68 
      r.img.strat$class[r.img.strat$r.img == 0] <- 0
      r.img.strat$class[r.img.strat$r.img > 0 & r.img.strat$r.img <= 20] <- 1
      r.img.strat$class[r.img.strat$r.img > 20 & r.img.strat$r.img <= 40] <- 2
      r.img.strat$class[r.img.strat$r.img > 40 & r.img.strat$r.img <= 60] <- 3
      r.img.strat$class[r.img.strat$r.img > 60 & r.img.strat$r.img <= 80] <- 4
      r.img.strat$class[r.img.strat$r.img > 80 & r.img.strat$r.img <= 100] <- 5

      # Stratified random sample of the response data using function from rs functions code.
      # Couldn't just use "sample" function from base pkg bc it doesn't reach 200 samples 
      # in every class.
      r.samp <- stratified(r.img.strat, group="class", size=200)
      
        
#       stratifiedSampling<-function(df,id, size) {
#         #df is the data to sample from
#         #id is the column to use for the groups to sample
#         #size is the count you want to sample from each group
#         
#         # Order the data based on the groups
#         df<-df[order(df[,id],decreasing = FALSE),]
#         
#         # Get unique groups
#         groups<-unique(df[,id])
#         group.counts<-c(0,table(df[,id]))
#         #group.counts<-table(df[,id])
#         
#         rows<-mat.or.vec(nr=size, nc=length(groups))
#         
#         # Generate Matrix of Sample Rows for Each Group
#         for (i in 1:(length(group.counts)-1)) {
#           start.row<-sum(group.counts[i])+1
#           samp<-sample(group.counts[i+1]-1,size,replace=FALSE)
# 
#           rows[,i]<-start.row+samp
#         }
# 
#         sample.rows<-as.vector(rows)
#         #df[sample.rows,]
#         return(sample.rows)
#       }# end sampling function
#       
#       samp <- stratifiedSampling(r.img.strat, "class", 200)
#       
# 
#       #old, but keep for now in case I need to come back to this...
#       stratSamp <- function(img, size) {
#         
#         strat.samp <- createDataPartition(y=img, p=0.25, groups=50, list=F)
#         
#         
#         
#         
#         
#         
#         #First create a stratification layer by reclassifying img
#         reclass.rules <- c(0,0,0, 0,20,1, 20,40,2, 40,60,3, 60,80,4, 80,100,5)
#         rclmat <- matrix(reclass.rules, ncol=3, byrow=TRUE)
#         strat <- reclassify(img, rclmat)
#         s.samp <- sampleStratified(strat, exp=1000, size = 3)
#         #Get sampled values from img
#         
#         
# #         strat <- as.data.frame(img)
# #         #strat[strat == 0] <- 0
# #         strat[strat > 0 & strat < 20 ] <- 1
# #         strat[strat >= 20 & strat < 40] <- 2
# #         strat[strat >= 40 & strat < 60] <- 3
# #         strat[strat >= 60 & strat < 80] <- 4
# #         strat[strat >= 80 & strat < 100] <- 5
#         
#       }#end stratSamp function
#       
#       #SOME sampling for large datasets that exceed memory 
#       #first, figure out ratio of "present" to "absent" - our cutoff is 20% cover
#       pres.length <- length(r.img[r.img >= 20])
#       abs.length <- length(r.img[r.img < 20])
#       tot <- pres.length + abs.length
#       
#       pres.percent <- pres.length/tot*100
#       abs.percent <- abs.length/tot*100
#       
#       #find positions of random sample of r.img 
#       idxsample=function(m,n){ 
#         pres.index <- c(1:length(m[m >=20]))
#         abs.index <- c(1:length(m[m < 20]))
#         index <- c(pres.index, abs.index)
#         pos=sample(index,n,replace=F)#Random sample of matrix indexes 
#         return(pos) 
#       } #end function
#       
#       pos.random <- idxsample(r.img, 30000)
#       
#       #need to write in a test here to see if the ratio of pres to abs in the sample
#       #is similar to the ratio of pres/abs in the whole dataset.
#       
      #also, would probably be helpful to write in a test to check the range of the predictors
      #in the sample vs the whole image.
      #test <- (r.img[pos.random])

          
      #to check manually - for 100k for cove forest, looks good!
      #summary(pred.stk)
      #summary(pred.stk[pos.random,])
      
      
      
      # Run Random Forest classification algorithm for kitchen sink model
      print(paste("computing random forest model. . ."))
      randfor <- randomForest(pred.stk[pos.random,], r.img[pos.random])
      #randfor <- randomForest(pred.stk, r.img)
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
      varImpPlot(randfor, pch=16, col="blue",main=paste("Variable Importance - ", class.name, sep=' '))
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
        r.out[is.na(pred.mask) == F] <- pred
        
        diff <- pred - r.img
        r.diff[is.na(pred.mask) == F] <- diff
        
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

 
