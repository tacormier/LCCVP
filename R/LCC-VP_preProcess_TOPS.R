# PURPOSE: This script does some pre-processing to TOPS ET, soilw, vpd, and swe layers.
# Layers expected are indivual bands representing each month of a year (i.e. 195001 is 
# January 1950, 196108 is August 1961). For now, all individual layers for each type of data
# are expected in the same directory, but preprocessed files will be saved to new dirs 
# representing each data type.
#
# DATE: 9/11/2014
#
# AUTHOR: Tina Cormier
###########################################################################################
library(raster)

#source parameter file - this script is not incorporated into the param file yet
# - source("C:/Share/LCC-VP/scripts/R/LCC-VP_parameters.R")
#source functions
source("C:/Share/LCC-VP/scripts/R/LCC-VP_functions.R")

##########################  SET VARIABLES HERE TO RUN MANUALLY ############################
#set indir
tops.indir <- "C:/Share/LCC-VP/EcoRegions/Bailey/climate_data/current/"

# which variables are we processing? (what you put here should correspond to exactly how
# the names are listed in the file names and the names of their subdirs). 
#tops.vars <- c("et","soilw","swe","vpd")
tops.vars <- c("vpd")

#set outdir - should be one level above individual type directories (et, soilw, vpd, and swe)
tops.outdir <- "C:/Share/LCC-VP/Parks/GRSM/TOPS/"

#ref image for QA/QC of snapping/extent etc - usually the current biovars image
tops.ref <- "C:/Share/LCC-VP/Parks/GRSM/climate_data/current_PRISM/biovars/biovars_1981-2010.tif"

###########################################################################################
# Start by creating annual, 12-mo stacks - for now just for vpd. 
whole.list <- list.files(tops.indir, "*\\.tif", full.names=T) 

#outdir for these temp 12-mo stacks - this will vary by dataset, but for now, semi-hardcoded
out.temp <- paste0(tops.indir, "annual/")
stackMonthToAnnual(tops.indir, out.temp, "tif", tops.ref)

#stackMonthToAnnual <- function(imgdir, outdir, imgext, refimg) {


# Generate file lists for each variable
for (var in tops.vars) {
  pattern <- sprintf("*_%s_*", var)
  
  var.list <- grep(pattern=pattern, whole.list, value=T)
  varname <- paste0(var, ".files")
  assign(varname, var.list)
}#end tops.vars loop

# Now there SHOULD be a file list for each var listed in tops.vars,
# and we can do stuff!



if (exists("et.files")) {
  for (et.f in et.files) {
    #first stack into 12-month stack
    et <- 
    
    
  }#end et.files loop
  
}#end et.files if
