##########################################################################################
# Date: November, 2012
# Authors: Tina Cormier 
# Purpose: Habitat suitability mapping using random forest
#
##########################################################################################

#Load libraries
require(maptools)
require(randomForest)
require(mgcv)
require(raster)
require(foreign)
require(RColorBrewer)
require(rgdal)
require(ggplot2)
require(gridExtra)
require(rfUtilities)
#require(biomod2)
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
# #analysis subdir - will be used in file naming to help distinguish model runs.
# subDir <- "allVars_BiovarsGddVpdSoilsTWIsrad_20141030"
#
# # Cull Variables for Multicolinearity? (Y/N)
# multicol <- "Y"
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

#
################# VARIABLE NAMES #######################

# Text file containing variable names.  Names should be listed
# in text file - each name on a separate line and in quotes.  They should 
# be in the same order as they appear in v.stk and be the same length 
# (# bands = # lines in text file).
var.names <- read.table(var.names, sep="\n")[,1]
#hard coded based on 34 variables as of November 21, 2014
short.names <- c("bio1","bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", 
                 "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19", "gdd", 
                 "vpd_mam", "vpd_jja", "vpd_son", "vpd_djf", "vpd_ann", "bdens", "clay", "ph", "sand",
                 "silt", "awc", "bdepth","TWI", "srad")

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
  
  #add on xy data
  #pred.stk <- cbind(pred.stk, r.coords)
  
  
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
      
  ###################### Testing Jeffrey Evans' new rfUtilities package ###############################
      #multicolinearity testing
#       names(pred.stk) <- short.names
#       mc <- MultiColinear(pred.stk, p=0.05)
#       #remove multicolinear preds
#       keepvars <- names(pred.stk) %in% mc
#       pred.nomc <- pred.stk[,-which(names(pred.stk)==mc)]
#       pred.nomc <- pred.stk[,keepvars==F]
#       
#       #PCA biplot of variables
#       pca.test <- prcomp(pred.stk[,1:ncol(pred.stk)], scale=TRUE)
#       biplot(pca.test, arrow.len=0.1, xlabs=rep(".", length(pca.test$x[,1])))
#       
#       #rf with no collinear variables
#       rf.nomc <- randomForest(pred.nomc, r.img, keep.forest = T, importance=T)
#       rf.nomc
#       
#       
#       ###
#       # Partial plots with probability distribution - keep our own function bc this only plots one
#       # variable at a time and doesn't support regression trees.
#       #probPartialPlot(rf.nomc, pred.nomc, names(pred.nomc[1]))
#       
#       ###
#       # Model Improvement Ratio
#       rf.nomc.mir <- rf.modelSel(pred.nomc, r.img, imp.scale="mir",final=TRUE)
#       rf.nomc.se <- rf.modelSel(pred.nomc, r.img, imp.scale="se",final=TRUE)
      
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
      # Spearman corr matrix
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
      # Best vpd variable
      best.v.var <- names(which.max(abs(s.cor[1,c(22:26)])))
      
      # Test correlation between best temperature variable and other temperature variables
      # Keep those that are uncorrelated at 0.7 level
      t.row <- s.cor[rownames(s.cor) %in% best.t.var, c(2:12,21,22:26)]
      t.keep <- names(t.row)[t.row < 0.7 & t.row > -0.7]
      # Test correlation between best ppt variable and other ppt variables
      p.row <- s.cor[rownames(s.cor) %in% best.p.var,c(13:20)]
      p.keep <- names(p.row)[p.row < 0.7 & p.row > -0.7]
      # Test correlation between best vpd variable and other vpd variables
      v.row <- s.cor[rownames(s.cor) %in% best.v.var,c(2:12,21,22:26)]
      v.keep <- names(v.row)[v.row < 0.7 & v.row > -0.7]
      # Always keep these
      all.keep <- row.names(s.cor[27:35,])
      
      # List final sets
      t.set <- c(best.t.var, t.keep, p.keep, all.keep)
      vpd.set <- c(best.v.var,v.keep,p.keep,all.keep)
      
  # create predictor stacks from selected non-correlated variables 
      t.preds <- pred.stk[,names(pred.stk) %in% t.set]
      v.preds <- pred.stk[,names(pred.stk) %in% vpd.set]
  
  
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

        
        t.varsel03 <- rf.modelSel(t.preds, r.img, imp.scale="mir", final=TRUE, plot.imp=TRUE, parsimony=0.03, ntree=ntree)
        v.varsel03 <- rf.modelSel(v.preds, r.img, imp.scale="mir", final=TRUE, plot.imp=TRUE, parsimony=0.03, ntree=ntree)

        t.randfor <- randomForest(pred.stk[,t.varsel03$SELVARS], r.img, ntree=ntree, importance=T, keep.forest = T)
        v.randfor <- randomForest(pred.stk[,v.varsel03$SELVARS], r.img, ntree=ntree, importance=T, keep.forest = T)

        #save the models
        t.rf.mod <- paste(outDir,"rf_model_", mod.name, "_TempModel.RData", sep="")
        save(t.randfor, file= t.rf.mod)

        v.rf.mod <- paste(outDir,"rf_model_", mod.name, "_VPDModel.RData", sep="")
        save(v.randfor, file= v.rf.mod)

        #varImpPlot(t.randfor, type=1)
        #varImpPlot(v.randfor, type=1)
        
        



  #####################################################
      # Run Random Forest classification algorithm for kitchen sink model
      print(paste("computing random forest model. . ."))
      randfor <- randomForest(pred.stk, r.img, keep.forest = T, importance=T, ntree=ntree)
      
      # View classification results.
      randfor
      
      # View variable importance plot.
      #varImpPlot(randfor)
      
      #save rf model for later :)
      rf.mod <- paste(outDir,"rf_model_", mod.name, "_KSModel.RData", sep="")
      save(randfor, file= rf.mod)
  
      #now try some model selection
      #rf.mir <- rf.modelSel(pred.stk, r.img, imp.scale="mir",final=TRUE)
      #rf.se <- rf.modelSel(pred.stk, r.img, imp.scale="se",final=TRUE)
          
      ############### Model output summary files ############################
      # Plotting parameters
      str <- 1
      dpi <- 400
      
      # Variable Importance Plots. 
      
      pdf(file=paste(outDir,"rf_results_",subDir,"_", mod.name, ".pdf", sep=""),family="Times", width=10, height=10)
      par(mfrow=c(2,2))
      varImpPlot(randfor, pch=16, col="blue",type=1,main=paste("Variable Importance \nKitchen Sink Model \n", class.name, sep=' '))
      leg.txt <- c(as.expression(bquote(r^2 == ~.(round(randfor$rsq[length(randfor$rsq)], digits=2)))), 
                as.expression(bquote(mse == .(round(randfor$mse[length(randfor$mse)], digits=2)))))
      legend("bottomright", legend=leg.txt, bty='n', cex=0.85)

      varImpPlot(t.randfor, pch=16, col="blue",type=1,main=paste("Variable Importance \nTemperature Model \n", class.name, sep=' '))
      leg.txt <- c(as.expression(bquote(r^2 == ~.(round(t.randfor$rsq[length(t.randfor$rsq)], digits=2)))), 
                   as.expression(bquote(mse == .(round(t.randfor$mse[length(t.randfor$mse)], digits=2)))))
      legend("bottomright", legend=leg.txt, bty='n', cex=0.85)

      varImpPlot(v.randfor, pch=16, col="blue",type=1,main=paste("Variable Importance \nVPD Model \n", class.name, sep=' '))
      leg.txt <- c(as.expression(bquote(r^2 == ~.(round(v.randfor$rsq[length(v.randfor$rsq)], digits=2)))), 
                   as.expression(bquote(mse == .(round(v.randfor$mse[length(v.randfor$mse)], digits=2)))))
      legend("bottomright", legend=leg.txt, bty='n', cex=0.85)
      #dev.off()
      
      #########################################
      # ******* Move Variable Importance stuff into this new mod loop someday!!

      #Partial correlation plot for top 5 most important predictors:
      par(mfrow=c(2,2))
      
      #list of models for which to create partial dependency plots (hard coded)
      mod.list <- c("randfor", "t.randfor", "v.randfor")
      #sort variables by importance and select the first 5:
      #impt <- names(sort(randfor$importance[,1], dec=T)[1:5])
      for (mod in mod.list) {
        mod.obj <- get(mod)
        impt.vals <- sort(importance(randfor, type=1)[,1], dec=T)[1:5]
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
          legend("bottom", names(pp.imp), col=cols, lty=1, cex=0.85, bty="n", xjust=0.5, yjust=1, ncol=2)
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
          mod.obj2 <- get(mod.pred)
          
          # Naming the output images by manipulating res.img
          outImage <- paste(sub("inPRISM_800m_masked.tif", "", sub("COVER_", "", sub("inputs", "outputs", res.img))),"current_suitability_", mod.pred, ".tif", sep="")
          diffImage <- paste(sub("inPRISM_800m_masked.tif", "", sub("COVER_", "", sub("inputs", "outputs", res.img))),"modeled_suitability_minus_ref_", mod.pred, ".tif", sep="")
        
          # Detach the raster package because we want to use the randomForest version of 
          # predict, instead of the "raster" version.
          detach("package:raster")
          pred <- predict(mod.obj2, pred.stk)
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

sink()
sink(type="message")

#Detach packages
# detach("package:maptools")
# detach("package:randomForest")
# detach("package:raster")
# detach("package:foreign")
# detach("package:rgdal")

 
