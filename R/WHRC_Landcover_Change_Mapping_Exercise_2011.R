##########################################################################################
# Date: September, 2011
# Authors: Woods Hole Research Center Staff 
# Purpose: Landcover Change Mapping Exercise
#
# 
# Details:
#
#   This script reads graound reference data from a DBF or CSV file with the 
#   fields Xlocation, Ylocation, F/NF ground reference label (as integers). 
#
#   The script spatially joins the reference data locations with objects contained in an 
#   segmented raster file.  A random forest classification is then performed. 
#   The final output is a landcover change map and model reports.
#
##########################################################################################

#Load libraries
require(maptools)
require(randomForest)
require(raster)
require(rgdal)
require(foreign)
require(time)
source("C:\\Data\\forest_cover_change\\scripts\\kw.R")
#
#############################  SET VARIABLES HERE  #######################################
#
#
# Segmentation attributes as CSV or DBF (from segmentation software). Note that the first column must contain segment numbers.
segTbl <- 'C:\\Data\\forest_cover_change\\Brazil\\data\\multitemp_stack_2007-2010_segments_45_95.dbf'

# Name and location of the output segment raster TIF from eCognition. 
segImage <- 'C:\\Data\\forest_cover_change\\Brazil\\data\\multitemp_stack_2007-2010mergedLabel_45_95'

# Segment raster nodata value.
nd <- 0

# Name and location of the DBF or CSV file containing the Xlocation, Ylocation, and reference data.
pointData <- 'C:\\Data\\forest_cover_change\\Brazil\\data\\brazil_reference_points.dbf'

# Enter EITHER the name (case sensitive and in quotes) or the column number of the column containing x coordinates.
x <- 'x'

# Enter EITHER the name (case sensitive and in quotes) or the column number of the column containing y coordinates.
y <- 'y'

# Enter EITHER the name (case sensitive and in quotes) or the column number of the column containing response values.
response <- 'class'

#Do you want to predict an output map layer? Answer Y or N.
map <- "Y"

# Path/directory where output data will be stored.
outDir <- 'C:\\Data\\forest_cover_change\\Brazil\\output\\'

########################## FUNCTIONS ##################################################

# Function to uniformly format the column variables in pointData based on variable type (either name or number).
format.col <- function(v) if (is.numeric(v)) {
  v <- names(pts[v])
  } else if (is.character(v)) {
      v <- v
  }
############################################################################

# Naming the output imageoutImage <- unlist(strsplit(segImage, "\\\\"))outImage <- paste(outDir, unlist(strsplit(outImage[length(outImage)], "\\."))[1], "_classified.tif", sep="")

#*********************************************************************************************
# Read the pointData file 
# NOTE: Make sure that projection for raster(s) and point data are the same before continuing.
#*********************************************************************************************

#determine file type of point data.
ptFileType <- unlist(strsplit(pointData, "\\."))[2]

if (ptFileType=="dbf") {
  pts <- read.dbf(pointData)

  xName <- format.col(x)
  x <- pts[,xName]
  # Read in and format y coord.
  yName <- format.col(y)
  y <- pts[,yName]
  xy <- cbind(x,y)
  # Read in and format response variable.  
  responseName <- format.col(response)
  response <- pts[,responseName]
  response <- as.factor(response)
  
  } else if (ptFileType=="csv" | ptFileType=="CSV") {
      pointTable <- read.table(pointData, header=FALSE, sep=",")
      pts <- pointTable

      # Read in and format x coord.
      xName <- format.col(x)
      x <- pts[,xName]
      # Read in and format y coord.
      yName <- format.col(y)
      y <- pts[,yName]
      xy <- cbind(x,y)
      # Read in and format response variable.  
      responseName <- format.col(response)
      response <- pts[,responseName]
      response <- as.factor(response)
      }
      
# Load the output segment raster. 
segImg <- raster(segImage)
      
# Extract the segment ID number associated with each reference point from the segment raster.
print("Getting the segment numbers for each plot")
trainvals <- cbind(response, extract(segImg, xy)) 

# Remove NA values and zero values from the extraction.
trainvals <- as.data.frame(na.omit(trainvals))
trainvals <- subset(trainvals, trainvals$V2 > 0)
names(trainvals) <- c("response","segid")

# Read in segment attribute data (csv or dbf). 
segFileType <- unlist(strsplit(segTbl, "\\."))[2]
if (segFileType=="dbf") {
  segAtr <- read.dbf(segTbl)
} else if (segFileType=="csv" | segFileType=="CSV") {
	segAtr <- read.csv(segTbl, header=TRUE)
	}

#Join reference data with the segment attribute data to form training data set.
#Again, remember that the first column of the segTbl must contain segment IDs. 
train <- segAtr[match(trainvals$segid, segAtr[,1]),]
#make sure there are no NA's - that would mean that not all of the
#training segment IDs matched up with the segment raster = PROBLEM.
#summary(train)

# Run Random Forest classification algorithm
Time1 <- paste(getTime()[1], getTime()[2], getTime()[3], sep=":")
print(paste("Starting to calculate random forest object at ", Time1, sep=""))
randfor <- randomForest(as.factor(response) ~. , data=train[,-1])

# View classification results.
randfor

# View variable importance plot.
varImpPlot(randfor)

# If interested in all information output from the Random Forest model:
#str(randfor)

# Type the command: randfor$pred to display the predictions for each of the reference points. 


############### Model output summary files ############################

# Confusion matrix
randfor.bs <- table(response, randfor$pred) # Bootstrapped result

# Kappa stats
randfor.kw <- kw(randfor.bs)

################### USER CAN CHANGE FILE NAMES HERE ######################
# Plotting parameters
str <- 1
dpi <- 400

# Variable Importance Plot.
pdf(file=paste(outDir,"landcover_change_VarImp.pdf", sep=""),family="Times")
varImpPlot(randfor, cex=1.2, pch=16, col="blue",main=paste("Variable Importance - All Variables Included", sep=' '))
dev.off()

# redirect output to file (for selected variables)
sink(file=paste(outDir,"landcover_change_rf_results.txt",sep=""))
print("*********************************************************************")
print("**************randomForest Bootstrapped Confusion Matrix*************")
print("*********************************************************************")
print(randfor.bs)
summary.kw(randfor.kw)
randfor$err.rate[100]*100
#turn off output to file
sink()

################### Landcover Change Map Generation ######################

#Do you want to predict an output image (Y or N)? Set "map" variable at top of script. 
if (map=="Y" | map == "y") {
    # Prediction
    # Detach the raster package because we want to use the "stats" version of predict, instead of the "raster" version.
    detach("package:raster")
    pred <- predict(randfor, segAtr)
    pred.out <- data.frame(segAtr[,1], pred)

    # Write the output raster map.
    # Reload the raster package.
    require(raster)
    bs <- blockSize(segImg)
    print("Writing output raster")

    # Create the output raster and begin writing to it.
    img.out <- raster(segImg)
    #NAvalue(img.out) <- 0
    img.out <- writeStart(img.out, outImage, format='GTiff', overwrite=TRUE)
    
    # Loop over blocks of the output raster and write the new classified value.
    # This looping method will allow for the input of larger rasters without memory problems.
    for (i in 1:bs$n) {
        Time <- paste(getTime()[1], getTime()[2], getTime()[3], sep=":")
        print(paste("Writing prediction for block ", as.character(i), " of ", length(bs$row), " at ", Time, sep=""))
        # require(raster)
	    img <- getValues(segImg, row=bs$row[i], nrows=bs$nrows[i])
	    # Set the no data value to NA so it doesn't get converted to a predicted value
	    is.na(img) <- img == nd
	    # Convert the segment ID to the predicted (numeric) class so that a nodata value can be set.
        img.match <- as.numeric(pred.out$pred[match(img, pred.out[,1])])
        # Set the no data value to the default value for the output image
        img.match[is.na(img.match) == TRUE] <- NAvalue(img.out)
        writeValues(img.out, img.match, bs$row[i])
        }
      # End Loop.

    # Finish saving and close the connection to the image.
    img.out <- writeStop(img.out)
    endTime <- paste(getTime()[1], getTime()[2], getTime()[3], sep=":")
    print(paste("Finished writing ", outImage, " at ", endTime, sep=""))
    }
    
 
