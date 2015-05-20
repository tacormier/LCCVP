##########################################################################################
# Date: November, 2012
# Authors: Tina Cormier 
# Purpose: Habitat suitability mapping using random forest
#
#
#############################  SET VARIABLES HERE TO RUN MANUALLY #########################
# #
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
# #model name (usually spp name)
# mod.name <- "Spruce_fir"
#
# #Class name (as listed in attribute table of original classification)
# class.name <- "Spruce-fir forest"
#
# #Big data y or n - used to decide sampling or to use all pixels
# bigdata <- "n"
#
# #If sampling, how many samples per strata do you want?
# bigdata.size <- 1000
#
# Is this a rangewide model(r) or a park model (p)? 
# mod.extent <- "r"
#
# #analysis subdir - will be used in file naming to help distinguish model runs.
# subDir <- "allVars_BiovarsGddVpdSoilsTWIsrad_20141030"
#
# #park-extent predictors - for calculating park-level results
# parkPreds2 = "C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/biovarsGddSoilsTWIsrad_20150217/BlueRidge_predVars_stacking_20150217_albers_clip.tif"
#
################## UNCOMMENT THIS TO CALL FROM PYTHON OR OTHER SCRIPT#####################
#
# Set number of trees here - hardcoded
ntree <- 5000
#
#Get the parameter file from the supplied argument
paramfile <- commandArgs(trailingOnly=TRUE)
# OR
#source parameter file for input and output directories
#source("C:/Share/LCC-VP/scripts/R/LCC-VP_parameters.R")

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
print(paste("park-level predictors = ", parkPreds2, sep=""))

##########################################################################################

#Load libraries
require(maptools)
require(raster)
require(randomForest)
#require(caret)
#require(mgcv)
#require(foreign)
require(RColorBrewer)
require(rgdal)
#require(ggplot2)
#require(gridExtra)
require(rfUtilities)
require(foreach)
require(doParallel)
#require(biomod2)
#
################# VARIABLE NAMES #######################

# Text file containing variable names.  Names should be listed
# in text file - each name on a separate line and in quotes.  They should 
# be in the same order as they appear in v.stk and be the same length 
# (# bands = # lines in text file).
var.names <- read.table(var.names, sep="\n")[,1]
#hard coded based on 34 variables as of November 21, 2014
# short.names <- c("meanAnnTemp","MeanDiRng", "Isotherm", "TempSnlty", "MaxTWarmMo", "MinTColdMo", "TAnnRng", "MeanTWetQtr", "MeanTDryQtr", 
#                  "MeanTWarmQtr", "MeanTColdQtr", "AnnPr", "PrWetMo", "PrDryMo", "PrSnlty", "PrWetQtr", "PrDryQtr", "PrWarmQtr", "PrColdQtr", 
#                  "gdd", "vpd_mam", "vpd_jja", "vpd_son", "vpd_djf", "vpd_ann", "bdens", "clay", "ph", "sand", "silt", "awc", "bdepth","TWI", 
#                  "srad")

# Check that number of bands in v.stk = # of lines in text file
# If yes, run script. If not, error.
b <- GDALinfo(v.stk)[3]
num.names <- length(var.names)

if (b == num.names) {
  
  #####################################################
  
  # Load the response raster. 
  print(paste("loading response raster for ", mod.name, sep=""))
  r.img <- getValues(raster(res.img))
  # Before "getting values," let's extract the xy coords from each cell to include in the modeling in an attempt
  # to include a simple representation of spatial structure.
  #r.img <- raster(res.img)
  #r.coords <- xyFromCell(r.img, cell=c(1:ncell(r.img)))
  #r.img <- getValues(r.img)
  
  #set up output images before removing NAs
  r.out <- r.img
  r.diff <- r.img
  
  # Load predictor stack
  print(paste("loading predictor stack for ", mod.name, sep=""))
  pred.stk <- as.data.frame(getValues(brick(v.stk)))
  names(pred.stk) <- var.names
  
  # Load park-extent preds and clip/mask r.img to get park-level response
  if (mod.extent == "r") {
    parkPreds <- brick(parkPreds2)
    names(parkPreds) <- var.names
    parkResp <- mask(crop(raster(res.img), parkPreds), parkPreds[[1]])
    
    parkPreds <- na.omit(as.data.frame(getValues(parkPreds)))
    parkResp <- na.omit(getValues(parkResp))
  }

  
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
    
    #Now mask r.img with pred.mask - also r.coords
    r.img[is.na(pred.mask)] <- NA
    
    pred.na <- is.na(pred.mask)
    r.na <- is.na(r.img)
    #r.coords.na <- is.na(r.coords)[,1]
  
  
    #Only run the rest of the code IF response image and predictor stack have identical
    #dimensions and positions of NA values.
    if (identical(r.na, pred.na)) {
      
      #If they match, remove NAs - pred.stk here is the kitchen sink group of predictors
      r.img <- na.omit(r.img)
      pred.stk <- na.omit(pred.stk)
      
      # Keep full versions of both r.img and pred.stk to use for predicting the map after - 
      # Makes no difference if there is no sampling. But in the case of sampling, want to predict the
      # whole area, not just the samples. Easier to do this than re-write the randomForest code, as the
      # sampling piece was added later.
      r.img.orig <- r.img
      pred.stk.orig <- pred.stk
      
      # SOME sampling for large datasets that exceed memory 
      # Stratified random sampling:      
      # Function from http://adammcelhinney.com/2012/04/10/r-function-for-stratified-sampling/ modified by
      # Tina Cormier to preserve original order of the data
      if (bigdata == "y") {
        print(paste0("Big data - need a stratified sample! Now taking ", bigdata.size, " samples per class."))
        # Figure out proper random sample size
        # Since there are two tails of the normal distribution, the 95% confidence level would imply the 97.5th percentile of the normal distribution at the upper tail.
        # Degree of confidence - want to be 95% sure sample mean is within a threshold of population mean
        zstar <- qnorm(.975) 
        # Standard deviation of entire range
        sigma <- sd(r.img.orig) 
        # allowable error - how close do we want the sample to be to the population mean? Within 1/2 percent suitability.
        E <- 0.1 
        #calc # of samples
        n <- round(zstar^2 * sigma^2/ E^2, digits=0)
        
        samp <- sample(c(1:length(r.img.orig)), n)
        r.img <- r.img.orig[samp]
        pred.stk <- pred.stk.orig[samp,]
        
        # During the testing phase of this method, I checked to see if the sample mean was in fact within 1/2 percent
        # of the rangewide mean and it was. Similar standard deviations, too. 
        
        
        
############### Below is what I tried for stratified sampling ###############        
#         #First add some columns to r.img to ID the groups as well as the original order of the data
#         ord <- c(1:length(r.img))
#         r.img.strat <- as.data.frame(cbind(r.img, ord))
        # Tried intitially to create 10 groups (plus 0 with its own group (this example is for cove forests)), 
        # but won't have enough data in the upper groups to sample:
        #  1     2     3     4     5     6     7     8     9    10 
        #34733 19095  5240  1893  1531  1344  1055   528   198    68 
        
#         #spruce fir - so need to group classes...and doing so equally doesn't work - hope this is ok!:
# #             0     1     2     3     4     5     6     7     8     9    10 
# #         69381   616   153   112    74    47    45    21    17     7     2 
#           r.img.strat$class[r.img.strat$r.img == 0] <- 0
#           r.img.strat$class[r.img.strat$r.img > 0 & r.img.strat$r.img <= 10] <- 1
#           r.img.strat$class[r.img.strat$r.img > 10 & r.img.strat$r.img <= 20] <- 2
#           r.img.strat$class[r.img.strat$r.img > 20 & r.img.strat$r.img <= 30] <- 3
#           r.img.strat$class[r.img.strat$r.img > 30 & r.img.strat$r.img <= 50] <- 4
#           r.img.strat$class[r.img.strat$r.img > 50] <- 5
# 
# 
#         # What I did for Cove Forests:
#           r.img.strat$class[r.img.strat$r.img == 0] <- 0
#           r.img.strat$class[r.img.strat$r.img > 0 & r.img.strat$r.img <= 10] <- 1
#           r.img.strat$class[r.img.strat$r.img > 10 & r.img.strat$r.img <= 20] <- 2
#           r.img.strat$class[r.img.strat$r.img > 20 & r.img.strat$r.img <= 30] <- 3
#           r.img.strat$class[r.img.strat$r.img > 30 & r.img.strat$r.img <= 40] <- 4
#           r.img.strat$class[r.img.strat$r.img > 40 & r.img.strat$r.img <= 50] <- 5
#           r.img.strat$class[r.img.strat$r.img > 50 & r.img.strat$r.img <= 60] <- 6
#           r.img.strat$class[r.img.strat$r.img > 60 & r.img.strat$r.img <= 70] <- 7
#           r.img.strat$class[r.img.strat$r.img > 70 & r.img.strat$r.img <= 80] <- 8
#           r.img.strat$class[r.img.strat$r.img > 80 & r.img.strat$r.img <= 90] <- 9
#           r.img.strat$class[r.img.strat$r.img > 90 & r.img.strat$r.img <= 100] <- 10
#     
        # Stratified random sample of the response data using function from rs functions code.
        # Couldn't just use "sample" function from base pkg bc it doesn't reach 200 samples 
        # in every class.
#         r.samp <- stratified(r.img.strat, group="class", size=as.numeric(bigdata.size))
#         
#         r.img <- r.samp$r.img
#         pred.stk <- pred.stk[r.samp$ord,]
      } #end bigdata if



      
  ###################### Spearman Correlations for culling variables ###############################

      
      # Now generate different variable lists based on diff variable selection processes.
      # First, VPD Model: Pick the variable from among VPD variables that has the strongest association with the response. 
      # Delete any temperature or other vpd variables that have a >0.7 spearman correlation coefficient with the selected 
      # vpd variable. Pick the precipitation variable that has the strongest association with the response. Delete other 
      # correlated precipitation variables. Include all soils and elevation derivatives (topographic wetness index and 
      # relative radiation).
      #
      # Then a similar temp model: b) Pick the variable from among the temperature variables that has the strongest association 
      # with the response. Delete any temperature or vpd variables that have a >0.7 spearman correlation coefficient with the selected 
      # temperature variable. Pick the precipitation variable that has the strongest association with the response. Delete other 
      # correlated (>0.7) precipitation variables. Include all soils and elevation derivatives (topographic wetness index and 
      # relative radiation).
      #
      # Spearman corr matrix - this section is for use if we are included VPD. See following section if we don't use it.
#       print("removing correlated variables")
#       alldata <- cbind(r.img, pred.stk)
#       #names(alldata) <- c("cover", short.names)
#       names(alldata)[1] <- "cover"
#       s.cor <- cor(alldata, method="spearman")
#       
#       # Use first row of the correlation matrix (contains all relationships between response and preds.) 
#       # Remember the 1st column is the response variable so needs to be accounted for when querying correlation values.
#       # Best temperature variables
#       best.t.var <- names(which.max(abs(s.cor[1,c(2:12,21)])))
#       # Best precipitation variable
#       best.p.var <- names(which.max(abs(s.cor[1,c(13:20)])))
#       # Best vpd variable
#       best.v.var <- names(which.max(abs(s.cor[1,c(22:26)])))
#       
#       # Test correlation between best temperature variable and other temperature variables
#       # Keep those that are uncorrelated at 0.7 level
#       t.row <- s.cor[rownames(s.cor) %in% best.t.var, c(2:12,21,22:26)]
#       t.keep <- names(t.row)[t.row < 0.7 & t.row > -0.7]
#       # Test correlation between best ppt variable and other ppt variables
#       p.row <- s.cor[rownames(s.cor) %in% best.p.var,c(13:20)]
#       p.keep <- names(p.row)[p.row < 0.7 & p.row > -0.7]
#       # Test correlation between best vpd variable and other vpd variables
#       v.row <- s.cor[rownames(s.cor) %in% best.v.var,c(2:12,21,22:26)]
#       v.keep <- names(v.row)[v.row < 0.7 & v.row > -0.7]
#       # Always keep these
#       all.keep <- row.names(s.cor[27:35,])
#       
#       # List final sets
#       t.set <- c(best.t.var, t.keep, p.keep, all.keep)
#       vpd.set <- c(best.v.var,v.keep,p.keep,all.keep)
#       
#   # create predictor stacks from selected non-correlated variables 
#       t.preds <- pred.stk[,names(pred.stk) %in% t.set]
#       v.preds <- pred.stk[,names(pred.stk) %in% vpd.set]

###############
      # If we aren't using VPD - comment out above section. 
      print("removing correlated variables")
      alldata <- cbind(r.img, pred.stk)
      #names(alldata) <- c("cover", short.names)
      names(alldata)[1] <- "cover"
      s.cor <- cor(alldata, method="spearman")
      
      # Use first row of the correlation matrix (contains all relationships between response and preds.) 
      # Remember the 1st column is the response variable so needs to be accounted for when querying correlation values.
      # Best temperature variables
      best.t.var <- names(which.max(abs(s.cor[1,c(2:12,21)])))
      # Best precipitation variable
      best.p.var <- names(which.max(abs(s.cor[1,c(13:20)])))
      
      # Test correlation between best temperature variable and other temperature variables
      # Keep those that are uncorrelated at 0.7 level
      t.row <- s.cor[rownames(s.cor) %in% best.t.var, c(2:12,21)]
      t.keep <- names(t.row)[t.row < 0.7 & t.row > -0.7]
      # Test correlation between best ppt variable and other ppt variables
      p.row <- s.cor[rownames(s.cor) %in% best.p.var,c(13:20)]
      p.keep <- names(p.row)[p.row < 0.7 & p.row > -0.7]
      # Always keep these
      all.keep <- row.names(s.cor[22:30,])
      
      # List final sets
      t.set <- c(best.t.var, t.keep, p.keep, all.keep)
      
      # create predictor stacks from selected non-correlated variables 
      t.preds <- pred.stk[,names(pred.stk) %in% t.set]
     
        
  ##################### Using GAMs to further cull variables that don't add to the model ################################
  
#       #-------------
#       # GAM fitting, univariate
#       # Used to identify variables with minimal relationships with response variable
#       gamlist <- list()
#       gamplots <- list()
#       for (i in c(2:ncol(alldata))) {
#         print(paste(i,names(alldata)[i]))
#         vname <- names(alldata)[i]
#         gamlist[[i-1]] <- gam(alldata[,"cover"]~s(alldata[,vname],bs="cr"),family=gaussian,method="REML", link="identity")
#         #gamplots[[i-1]] <- 
#       }
#       names(gamlist) <- names(alldata[2:ncol(alldata)])
#       #some plotting for fun
#       #par(mfrow=c(round(sqrt(length(gamlist))), round(sqrt(length(gamlist))+1)))
#       par(mfrow=c(6,6))
#       for (p in c(1:length(gamlist))) {
#         plot.gam(gamlist[[p]], ylab="% Cover", xlab=names(gamlist[p]), scheme=1, rug=FALSE)
#       }
#       
#       
#   ##
#   
#       gam.cors <- unlist(lapply(gamlist, function(x) summary(x)$dev.expl*100))
#       names(alldata[2:length(alldata)])[which(gam.cors < 5)]
#       names(alldata[2:length(alldata)])[which(gam.cors >= 5)]
#       
#       # Subset and rerun gam function
#       alldata.pres <- alldata[alldata$cover > 0,]
#       alldata.abs <- alldata[alldata$cover==0,]
#       
#       gam.cors.ss <- list()
#       for (j in c(1:10)) {
#         rvec <- sample(c(1:dim(alldata.abs)[1]),size=2*dim(alldata.pres[1]))
#         alldata.ss <- rbind(alldata.pres,alldata.abs[rvec,])
#         gamlist <- list()
#         for (i in seq_along(names(alldata.ss)[2:length(alldata.ss)])) {
#           print(paste(i,names(alldata.ss)[2:length(alldata.ss)][i]))
#           vname <- names(alldata.ss)[2:length(alldata.ss)][i]
#           gamlist[[i]] <- gam(alldata.ss[,"cover"]~s(alldata.ss[,vname],bs="cr"),family="gaussian",method="REML", link="identity")
#         }
#         gam.cors.ss[[j]] <- unlist(lapply(gamlist, function(x) summary(x)$dev.expl*100))  
#       }
#       
#       # Get average values of correlations across 10 runs and remove those that
#       # average < 5%
#       gam.aves <- apply(do.call("rbind",gam.cors.ss),2,FUN="mean")
#       alldata.col.ss <- alldata[,!names(alldata) %in% names(alldata.ss)[2:length(alldata.ss)][which(gam.aves<5)]]
#       
          
  #################### Variable selection using rfUtilities by Jeffrey Evans #################################
# After culling some variables that exhibit multicollinearity and whose relationships are not likely to change
# under future scenarios, we can remove any remaining variables that do not improve the models. Using MIR as
# the type of scaling for importance values (Murphy et al. 2010)
        #ntree <- 5000
        #set up parallel cluster (doParallel package)
        #detectCores()
        cl <- makeCluster(20)
        registerDoParallel(cl)
        #getDoParWorkers()


# STOPPED HERE 1/9/2015 - BECAUSE OF SAMPLING, NO LONGER NEED THIS IF/ELSE BECAUSE WE CAN CALC THE MODELSEL VARS WITHOUT RUNNING
# OUT OF MEMORY. Also, should make all of this parallel - would increase efficiency. ALSO, change in map prediction r.img and pred.stk
# to r.img.orig and pred.stk.orig :)

          print("removing unimportant variables using the Model Improvement Ratio (MIR)")
          #Can use this parallel code once I figure out how to combine results (the .combine=combine only works for RF)
#           t.varsel03 <- foreach(ntree=rep(ntree/20, 20), .combine=combine .packages='rfUtilities') %dopar%
#               rf.modelSel(t.preds, r.img, imp.scale="mir", final=TRUE, plot.imp=TRUE, parsimony=0.03, ntree=ntree)
#           v.varsel03 <- foreach(ntree=rep(ntree/20, 20), .combine=combine, .packages='rfUtilities') %dopar%
#             rf.modelSel(v.preds, r.img, imp.scale="mir", final=TRUE, plot.imp=TRUE, parsimony=0.03, ntree=ntree)
          t.varsel03 <- rf.modelSel(t.preds, r.img, imp.scale="mir", final=TRUE, plot.imp=TRUE, parsimony=0.03, ntree=ntree)
          #v.varsel03 <- rf.modelSel(v.preds, r.img, imp.scale="mir", final=TRUE, plot.imp=TRUE, parsimony=0.03, ntree=ntree)

          
          print("computing t.randfor")
          set.seed(55)
          # different call for rangewide vs park model - has to do with extent of results calcs - want park only 
          # no matter what.
          if (mod.extent == "r") {
            t.randfor <- foreach(ntree=rep(ntree/20, 20), .combine=combine, .packages='randomForest') %dopar%
              randomForest(pred.stk[,t.varsel03$SELVARS], r.img, ntree=ntree, importance=T, keep.forest = T, xtest=parkPreds[,t.varsel03$SELVARS], ytest=parkResp)
            
            t.r2.park <- round(1 - sum((parkResp - t.randfor$test$predicted)^2)/sum((parkResp - mean(parkResp))^2), digits=2)
            t.rmse.park <- sqrt(sum((parkResp-t.randfor$test$predicted)^2)/length(parkResp))
            
            rngPred <- predict(t.randfor, pred.stk.orig)
            t.r2.rng <- round(1 - sum((r.img.orig - rngPred)^2)/sum((r.img.orig - mean(r.img.orig))^2), digits=2)
            t.rmse.rng <- sqrt(sum((r.img.orig-rngPred)^2)/length(r.img.orig))
                        
            t.nvar <- length(t.varsel03$SELVARS)
          
          } else if (mod.extent == "p") {
              t.randfor <- foreach(ntree=rep(ntree/20, 20), .combine=combine, .packages='randomForest') %dopar%
                randomForest(pred.stk[,t.varsel03$SELVARS], r.img, ntree=ntree, importance=T, keep.forest = T, xtest=pred.stk[,t.varsel03$SELVARS], ytest=r.img)
              t.r2 <- round(1 - sum((r.img-t.randfor$predicted)^2)/sum((r.img-mean(r.img))^2), digits=2)
              t.rmse <- round(sqrt(sum((r.img-t.randfor$predicted)^2)/length(r.img)), digits=2)
              t.nvar <- length(t.varsel03$SELVARS)
              
          } else {
              print(paste("mod.extent must be 'p' or 'r'. You entered ", mod.extent, sep=0))
            } #end mod.extent if

      
#          print("computing v.randfor")
#           v.randfor <- foreach(ntree=rep(ntree/20, 20), .combine=combine, .packages='randomForest') %dopar%
#               randomForest(pred.stk[,v.varsel03$SELVARS], r.img, ntree=ntree, importance=T, keep.forest = T)
#           v.r2 <- 1 - sum((r.img-v.randfor$predicted)^2)/sum((r.img-mean(r.img))^2)
#           v.rmse <- sqrt(sum((r.img-v.randfor$predicted)^2)/length(r.img))
#           v.nvar <- length(v.varsel03$SELVARS)
#   
          #save the models
          t.rf.mod <- paste(outDir,"rf_model_", mod.name, "_TempModel.RData", sep="")
          save(t.randfor, file= t.rf.mod)
  
#           v.rf.mod <- paste(outDir,"rf_model_", mod.name, "_VPDModel.RData", sep="")
#           save(v.randfor, file= v.rf.mod)

  #####################################################
      # Run Random Forest classification algorithm for kitchen sink model
      print(paste("computing kitchen sink random forest model. . ."))
      # different call for rangewide vs park model - has to do with extent of results calcs - want park only 
      # no matter what.
      if (mod.extent == "r") {
        randfor <- foreach(ntree=rep(ntree/20, 20), .combine=combine, .packages='randomForest') %dopar%
          randomForest(pred.stk, r.img, keep.forest = T, importance=T, ntree=ntree, 
                       xtest=parkPreds, ytest=parkResp)
        
        #when running randomForest in parallel, need to calc r2 and rmse
        ks.r2.park <- round(1 - sum((parkResp-randfor$test$predicted)^2)/sum((parkResp-mean(parkResp))^2), digits=2)      
        ks.rmse.park <- sqrt(sum((parkResp-randfor$test$predicted)^2)/length(parkResp))
        
        ks.rngPred <- predict(randfor, pred.stk.orig)
        ks.r2.rng <- round(1 - sum((r.img.orig-ks.rngPred)^2)/sum((r.img.orig-mean(r.img.orig))^2), digits=2)      
        ks.rmse.rng <- sqrt(sum((r.img.orig-ks.rngPred)^2)/length(r.img.orig))
        
        ks.nvar <- length(names(pred.stk))      
        
      } else if (mod.extent == "p") {
          randfor <- foreach(ntree=rep(ntree/20, 20), .combine=combine, .packages='randomForest') %dopar%
            randomForest(pred.stk, r.img, keep.forest = T, importance=T, ntree=ntree)
            
          #when running randomForest in parallel, need to calc r2 and rmse
          ks.r2 <- round(1 - sum((r.img-randfor$predicted)^2)/sum((r.img-mean(r.img))^2), digits=2)
          ks.rmse <- round(sqrt(sum((r.img-randfor$predicted)^2)/length(r.img)),digits=2)
          ks.nvar <- length(names(pred.stk))      
      } else {
          print(paste("mod.extent must be 'p' or 'r'. You entered ", mod.extent, sep=0))
        } #end mod.extent if

      #shut down parallel cluster
      stopCluster(cl)
      
      # View classification results.
      #randfor
      
      # View variable importance plot.
      #varImpPlot(randfor)
      
      #save rf model for later :)
      rf.mod <- paste(outDir,"rf_model_", mod.name, "_KSModel.RData", sep="")
      save(randfor, file= rf.mod)
  
      #now try some model selection
      #rf.mir <- rf.modelSel(pred.stk, r.img, imp.scale="mir",final=TRUE)
      #rf.se <- rf.modelSel(pred.stk, r.img, imp.scale="se",final=TRUE)
  #####################################################
      # Compute R2 and RMSE in the park only - this is important for rangewide models - we want to know
      # how well we are doing in the park, not the whole range.
      

          
      ############### Model output summary files ############################
      # Plotting parameters
      str <- 1
      dpi <- 400
      
      # Variable Importance Plots. 
      
      pdf(file=paste(outDir,"rf_results_",subDir,"_", mod.name, ".pdf", sep=""),family="Times", width=10, height=10)
      par(mfrow=c(2,2))
      varImpPlot(randfor, pch=16, col="blue",type=1,main=paste("Variable Importance \nKitchen Sink Model \n", class.name, sep=' '))
      # The "sprintf" portion of these expressions is to assure that trailing zeroes are printed on the graph.
      
      if (mod.extent == "r") {
        leg.txt <- c(as.expression(bquote("rangewide r"^"2" == ~.(sprintf("%.2f", ks.r2.rng)))),
                     as.expression(bquote("rangewide rmse" == .(sprintf("%.2f", ks.rmse.rng)))),
                     as.expression(bquote("park r"^"2" == ~.(sprintf("%.2f", ks.r2.park)))),
                     as.expression(bquote("park rmse" == .(sprintf("%.2f", ks.rmse.park)))),
                     as.expression(bquote(nvar == .(ks.nvar))))
        
      } else if (mod.extent == "p") {
          leg.txt <- c(as.expression(bquote(r^2 == ~.(sprintf("%.2f", ks.r2)))), 
                    as.expression(bquote(rmse == .(sprintf("%.2f", ks.rmse)))),
                    as.expression(bquote(nvar == .(ks.nvar))))
        }

      legend("bottomright", legend=leg.txt, bty='n', cex=0.75)

      varImpPlot(t.randfor, pch=16, col="blue",type=1,main=paste("Variable Importance \nTemperature Model \n", class.name, sep=' '))
      if (mod.extent == "r") {
        leg.txt2 <- c(as.expression(bquote("rangewide r"^"2" == ~.(sprintf("%.2f", t.r2.rng)))),
                     as.expression(bquote("rangewide rmse" == .(sprintf("%.2f",t.rmse.rng)))),
                     as.expression(bquote("park r"^"2" == ~.(sprintf("%.2f", t.r2.park)))),
                     as.expression(bquote("park rmse" == .(sprintf("%.2f", t.rmse.park)))),
                     as.expression(bquote(nvar == .(t.nvar))))
        
      } else if (mod.extent == "p") {
        leg.txt2 <- c(as.expression(bquote(r^2 == ~.(sprintf("%.2f", t.r2)))), 
                     as.expression(bquote(rmse == .(sprintf("%.2f", t.rmse)))),
                     as.expression(bquote(nvar == .(t.nvar))))
      }
      legend("bottomright", legend=leg.txt2, bty='n', cex=0.75)

#       varImpPlot(v.randfor, pch=16, col="blue",type=1,main=paste("Variable Importance \nVPD Model \n", class.name, sep=' '))
#       leg.txt <- c(as.expression(bquote(r^2 == ~.(sprintf("%.2f",round(v.r2, digits=2))))), 
#                    as.expression(bquote(rmse == .(sprintf("%.2f",round(v.rmse, digits=2))))),
#                    as.expression(bquote(nvar == .(v.nvar))))
#       legend("bottomright", legend=leg.txt, bty='n', cex=0.85)
      #dev.off()
      
      ######### ******* Move Variable Importance stuff into this new mod loop someday!!

      #Partial correlation plot for top 5 most important predictors:
      # Partial dependence is the dependence of the probability of presence on one predictor
      # variable after averaging out the effects of the other predictor variables in the model (Hastie 2001).
      par(mfrow=c(2,2))
      
      #list of models for which to create partial dependency plots (hard coded)
#       mod.list <- c("randfor", "t.randfor", "v.randfor")
      mod.list <- c("randfor", "t.randfor")
      #sort variables by importance and select the first 5:
      #impt <- names(sort(randfor$importance[,1], dec=T)[1:5])
      for (mod in mod.list) {
        print(paste0("Generating partial dependency plots for ", mod))
        mod.obj <- get(mod)
        impt.vals <- sort(importance(mod.obj, type=1)[,1], dec=T)[1:5]
        impt.names <- names(impt.vals)
        
        
        prednames <- names(pred.stk)
        pp.imp <-list()
        pp.preds <- list()
        
        #### Loop through variables and calculate partial dependency for each
        for (var in (impt.names)){
          #if(!is.numeric(pred.stk[, j])){ next }
          #if(length(unique(pred.stk[, j])) <= 10){ next }
          
          pp <- eval(bquote(partialPlot(mod.obj, pred.stk, .(var), plot=F)))
          
          pp.imp[[length(pp.imp) + 1]] <- pp
          names(pp.imp)[length(pp.imp)]<- var
          }#end var pd loop
        
        # Plots each individual frame of the partial dependency plots
          cols <- brewer.pal(length(pp.imp), "Set1")
          ylim1 <- sapply(pp.imp, function(d){ return(c(min(d$y), max(d$y)))})
          ylim <- c(min(ylim1[1,]-1.5), max(ylim1[2,]))
          
          #par(mar=c(10.1, 4.1, 4.1, 2), xpd=TRUE)
          par(mar=c(5.1, 6.1, 4.1, 2.1))
        
          #set up plotting area
          if (mod == "randfor") {
              mod.text <- "Kitchen Sink Model"
          } else if (mod == "t.randfor") {
              mod.text <- "Temperature Model"
          } else if (mod == "v.randfor") {
              mod.text <- "VPD Model"
          } else {
              warning("Model types are currently hard coded and must be 'Kitchen Sink,' 'Temperature', or 'VPD.' Please check your model type.")
          }
          plot(c(0, 1), ylim, type="n", xlab="Percentile of Predictor Data",ylab=paste0(class.name, "\nPercent Cover"),  
               cex.axis=1, cex.lab=1, main=paste0("Partial Dependency\n", mod.text))
          
          #Plot lines for top 5 variables
          for (k in 1:length(pp.imp)){
            Fn <- ecdf(pp.imp[[k]]$x)
            lines(Fn(pp.imp[[k]]$x), pp.imp[[k]]$y, col=cols[k])
          }
          
          #ypos <- ylim[1] - (ylim[2]-ylim[1])*0.3
          #ypos <- ylim[1] - ylim[1]*.5 
          legend("bottom", names(pp.imp), col=cols,ncol=3, lty=1, cex=0.85, bty="n", xjust=0.5, yjust=1)
        } #end mod for loop
        dev.off()
      
             
#       # redirect output to file (for selected variables)
#       sink(file=paste(outDir,"rf_model_results_", mod.name, ".txt",sep=""))
#       print(paste("***********************************************************************", sep=""))
#       print(paste("************** randomForest model results - ", mod.name," *************", sep=""))
#       print(paste("***********************************************************************", sep=""))
#       print(randfor)
#       #turn off output to file
#       sink()
#       
      ################### Habitat Suitability Map Generation ######################
      
      #Do you want to predict an output image (Y or N)? Set "map" variable at top of script. 
      if (map=="Y" | map == "y") {
        # Prediction for each of the models
        for (mod.pred in mod.list) {
          print(paste0("predicting ", mod.pred, " for the training time period."))
          mod.obj2 <- get(mod.pred)
          
          # Naming the output images by manipulating res.img
          outImage <- paste(sub("inPRISM_800m_masked.tif", "", sub("COVER_", "", sub("inputs", "outputs", res.img))),"current_suitability_", mod.pred, ".tif", sep="")
          diffImage <- paste(sub("inPRISM_800m_masked.tif", "", sub("COVER_", "", sub("inputs", "outputs", res.img))),"modeled_suitability_minus_ref_", mod.pred, ".tif", sep="")
        
          # Detach the raster package because we want to use the randomForest version of 
          # predict, instead of the "raster" version.
          #detach("package:raster")
          pred <- predict(mod.obj2, pred.stk.orig)
          r.out[is.na(pred.mask) == F] <- pred
          
          diff <- pred - r.img.orig
          r.diff[is.na(pred.mask) == F] <- diff
                    
          # Write the output raster maps.
          # Reload the dismo and raster package.
          #require(raster)
          print("Writing output raster")
          
          # Create the output suitability map.
          img.out <- raster(res.img)
          img.out <- setValues(img.out, round(r.out, digits=0))
          writeRaster(img.out, filename=outImage, format="GTiff", dataType="INT1U", overwrite=T)
          
          # Create diff raster.
          img.diff <- raster(res.img)
          img.diff <- setValues(img.diff, round(r.diff, digits=0))
          writeRaster(img.diff, filename=diffImage, format="GTiff", dataType="INT1S", overwrite=T)
        }#End mod.list for loop
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
print("done")
sink()
sink(type="message")

#Detach packages
# detach("package:maptools")
# detach("package:randomForest")
# detach("package:raster")
# detach("package:foreign")
# detach("package:rgdal")

 
