#General Parameters - may be used by several scripts
#Just used for naming
#
#NOTE: ALWAYS use trailing slashes for directory names.
#
#Enter abbreviation of park or pace:
park <- "GRSM"

###################################################

#STEP - use R to reproject and clip climate data to park boundary - 

#The following variables Work with the following scripts:
# LCC-VP_batch_reproj_clip_climdata.R

#Do you want the script to work on a directory (d) or on a list of files (l)?
dl <- "d"

#Enter EITHER the parent directory that contains the above-mentioned subdirectories OR path/filename 
#of csv file containing list of files in column 1 and the output directory for each input in column 2. 
#No header row.
#clim.in <- "C:/Share/LCC-VP/Parks/GRSM/misc/misc_lists/GRSM_clipTWI.csv"
clim.in <- "c:/Share/LCC-VP/EcoRegions/Bailey/analysis/common_inputs/biovarsGddSoilsTWIsrad_20150217/"

#crs of in images = sometimes we need to define the projection first before reprojecting. Use WKT.
#crs.orig <- "+proj=longlat +ellps=WGS72 +towgs84=0,0,4.5,0,0,0.5540000000000001,0.2263 +no_defs"
#crs.orig <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
crs.orig <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#Enter proj4 projection info for images you want to clip - sometimes the projection is undefined, so enter it here.
#proj.def <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#proj.def <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj.def <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#proj.def <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

#list variables (by way of their subdir name) that you want to reproj/clip. If dl="l" (list), this doesn't
#matter. If there are no params to loop over, write: params <- ""
#params <- c("ppt", "tmin", "tmax")
#params = c("GDD")
params <- ""

#Enter spatial reference text (desired spatial ref - represented as arcpy spatial ref)
#sr <- "Albers_Conical_Equal_Area"

#outdir - this is the parent directory and should contain either the ppt, tmin, and tmax subdirs
#or the different combo directories (i.e. ea_rcp26, p50_rcp85, etc), which subsequently
#contain ppt, tmin, tmax, biovars, GDD.
#if dl is a list, this will be ignored, as it will be stored in text file called by clim.in
clim.outdir <- "C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/biovarsGddSoilsTWIsrad_20150217/"

#Do your images need their proj defined (hard-coded in py script to WGS84) and reprojected to albers nad83 before clipping?
#accepted responses: "both" "clip_only"
clim.reproj <- "clip_only"

#Clip file - must be a raster or shapefile that has the projection you desire.
#clim.clip <- "C:/Share/LCC-VP/Parks/DEWA/climate_data/current_PRISM/biovars/biovars_1981-2010.tif"
#clim.clip <- "C:/Share/LCC-VP/Parks/GRSM/climate_data/current_PRISM/biovars/biovars_1981-2010.tif"

#set snap raster - Need ref for everything to align and clip to.
clim.clip <- "C:/Share/LCC-VP/Parks/GRSM/climate_data/current_PRISM/biovars/biovars_1981-2010.tif"

#fail file - will write list of variables to fail file to do over. This variable
#can be removed once I figure out why the python script crashes.
#clim.fail <- "C:/Share/LCC-VP/PACE/DEWA/climate_data/current_PRISM/clip_fail.txt"

#For forecast data, do you want to do a number of possible combinations? (note all lowercase - avoiding 
#regular expressions for now) "yes", "no", "na", or "list" ("na" if PRISM or other single-level directory data rather forecast data; "list" if
#entering text file list of inputs.) 
#NOTE, this should be listed as "list" if you are entering a text file list of inputs.
clip.comboYN <- "na"
clip.allcombo <- c("ea_rcp45", "ea_rcp85")
#clip.allcombo <- c('p25_rcp45','p25_rcp85','p50_rcp45','p50_rcp85','p75_rcp45','p75_rcp85')
#clip.allcombo <- c("ea_rcp26", "p25_rcp26", "p50_rcp26", "p75_rcp26", "ea_rcp45", "p25_rcp45", 'p50_rcp45', 'p75_rcp45',
#'ea_rcp60', 'p25_rcp60', 'p50_rcp60', 'p75_rcp60', 'ea_rcp85', 'p25_rcp85', "p50_rcp85", 'p75_rcp85')


###################################################

#STEP: Calculate bioclimatic variables from precip, tmin, tmax

#The following variables work with the following scripts:
#LCC-VP_calc_biovars.R - calcs bioclimatic vars from ppt, tmin, tmax
#LCC-VP_adjust_clim_forecasts_toPRISM_units_yearly.R - does ppt adjustments and *100 to match PRISM for ppt, tmin, tmax
#LCC-VP_calc_30-yr_monthly_forecasts_byYear.R - just generates 30yr average monthly images - from 12-band annual images. 
#LCC-VP_process_biovar_CMIP5_modeled_yearly.R - possibly obsolete now - can use LCC-VP_adjust_clim_forecasts_toPRISM_units_yearly.R
#LCC-VP_Hx_BackCalcs.R - hacked together script with some hardcoded stuff in it too (noted in comments). 

#Are you calculating biovars for PRISM or forecast data? Note that for forecast data,
#yearly forecasts must be done before 30-yr averages.
#enter either: "prism", "f.yearly", or "f.30yr"
#clim.type <- "prism"

#inputs for PRISM AND forecast data
#main directory containing ppt, tmin, and tmax sub-directories - keep trailing slash

#For yearly forecast data, bioclim.indir is the directory containing the raw yearly data - 
#process_biovar_forecasts_yearly.R makes adjustment for number of days in month. In this case,
#bioclim.outdir should be ".../adjusted_month_yearly/" and the script will figure out to put
#your output files in the correct sub folders by looking at med and rcp and then whichever
#variable (e.g., ppt, tmin, tmax. )

#For 30-yr average forecasts, bioclim.indir is the directory containing the adjusted yearly
#ppt, tmin, tmax subdirectories - OR, the directory just above that one, if you are calculating multiple
#quantile/RCP combos.
bioclim.indir <- "C:/Share/LCC-VP/Parks/DEWA/climate_data/forecasts/ave_30yr_byYear/deltas/"
# #output directory - typically looking for directory like:
#"C:/Share/LCC-VP/PACE/GRSM/climate_data/forecasts/adjusted_month_yearly/", and it has
# scenario directories underneath it, which each have ppt, tmin, tmax: e.g.,  /ea_rcp26/ppt
bioclim.outdir <- "C:/Share/LCC-VP/Parks/DEWA/climate_data/forecasts/ave_30yr_byYear/deltas/"

#params - which climate variables do you want to process? (ppt, tmin, tmax). Does not apply for 
#LCC-VP_calc_biovars.R.  If there are no params to loop over (e.g., "tmin","tmax","ppt"), 
#write: bioclim.params <- ""
bioclim.params <- c("ppt", "tmin", "tmax")
#bioclim.params <- ""

#For forecast data, do you want to do all possible combinations? (note all lowercase - avoiding 
#regular expressions for now) "yes" or "no" (if PRISM or Hx data with no combos or single scenario forecast data)
#Need to fix this a little for when we add more quantiles for hx stuff.  For now, just enter "no" for hx images.
comboYN <- "yes"
allcombo <- c('ea_rcp45','ea_rcp85')
#allcombo <- c('p50_rcp45','p50_rcp85','p25_rcp45', 'p25_rcp85','p75_rcp45','p75_rcp85')

#Otherwise, specify scenario here for forecast data.
#For forecast data:
#select median value: ea, p25, p50, or p75.  Enter NA for historical CMIP5 modeled 
#grids (no rcp). 
# med <- "p50"
# #select rcp value: 26, 45, 60, or 85
# rcp <- 85

###################################################
#STEP: Calculate delta images based on modeled vs. observed images.
#The following variables work with the subsequent scripts
#LCC-VP_compute_climate_deltas.R

#For forecasts, delta.indir is the directory containing the adjusted yearly
#ppt, tmin, tmax subdirectories - OR, the directory just above that one, if you are calculating multiple
#quantile/RCP combos.
delta.indir <- "C:/Share/LCC-VP/Parks/GRSM/climate_data/forecasts/ave_30yr_byYear/ea_cmip5_hx/"

#output directory - typically looking for directory like:
#""C:/Share/LCC-VP/Parks/GRSM/climate_data/forecasts/ave_30yr_byYear/deltas/"", and it has
# scenario directories underneath it, which each have ppt, tmin, tmax: e.g.,  /ea_rcp26/ppt
delta.outdir <- "C:/Share/LCC-VP/Parks/GRSM/climate_data/forecasts/ave_30yr_byYear/deltas/"

#list variables (by way of their subdir name) for which you want to calc deltas
#delta.params <- c("ppt","tmin", "tmax")
delta.params <- c("ann","djf","jja","mam","son")
#Observed image directory (i.e. PRISM). Contains subdirs ppt, tmin, tmax 
#(e.g. "C:/Share/LCC-VP/Parks/GRSM/climate_data/current_PRISM/")
delta.obsDir <- "C:/Share/LCC-VP/Parks/GRSM/climate_data/current_PRISM/"

#For forecast data, do you want to do all possible combinations? (note all lowercase - avoiding 
#regular expressions for now) "yes", "no", or "prism" (for historical prism calcs)
delta.comboYN <- "no"
delta.allcombo <- c('ea_rcp45','ea_rcp85')
#allcombo <- c('p25_rcp45', 'p25_rcp85','p50_rcp45','p50_rcp85','p75_rcp45','p75_rcp85')

#Otherwise, specify scenario here for forecast data.
#For forecast data:
#select median value: ea, p25, p50, or p75.  Enter NA for historical CMIP5 modeled 
#grids (no rcp). 
delta.med <- "p50"
#select rcp value: 26, 45, 60, or 85
delta.rcp <- 45

###################################################

#STEP: Calculate annual Growing Degree Days*100 (GDD)
#Calculate Annual Growing Degree Days above 5 C (GDD5) from
#from 800m PRISM monthly forecast data. 
#Equation from Sork et al. (2010)

#The following variables work with the following script(s):
#LCC-VP_GDD.R

# main dir with adjusted yearly forecast data - should be main directory containing
# sub directories of climate scenarios, which each contain ppt, tmin, tmax, biovars, GDD.
# For PRISM, should be directory containing ppt, tmin, tmax, biovars, GDD directories
gdd.indir <- 'C:/Share/LCC-VP/Parks/UPDE/climate_data/forecasts/ave_30yr_byYear/deltas/'

# output GDD directory
# For Forecasts using multiple scenarios, should be same as gdd.indir and script
# will figure out where to put it.  For only a single scenario, can either be the same
# as gdd.indir or the exact GDD directory. For PRISM, put desired output directory.
gdd.outdir <- 'C:/Share/LCC-VP/Parks/UPDE/climate_data/forecasts/ave_30yr_byYear/deltas/'

#For forecast data, do you want to do all possible combinations? (note all lowercase - avoiding 
#regular expressions for now) "yes", "no", or "na" (for PRISM data or other data where rcps and quantiles don't apply)
gdd.comboYN <- "yes"
gdd.allcombo <- c('ea_rcp45','ea_rcp85')

#OR, use median
gdd.med <- "p50"

#use rcp8.5
gdd.rcp <- "85"

###################################################

#STEP: Create 30-year average images from a list of yearly images.
#This script works with single band rasters only. 
#Window increments by one year.

#Depending on the datasets, #LCC-VP_mean_image_30-year_byYear.R may
#have to be edited in order to get the right list of images based on image names.

#The following variables work with the following script(s):
#LCC-VP_mean_image_30-year_byYear.R


# main dir with yearly forecast data
#If looking at forecast data, indir should be main dir for the adjusted-month yearly data.
#ex: C:/Share/LCC-VP/PACE/GRSM/climate_data/forecasts/adjusted_month_yearly/
img30.indir <- 'C:/Share/LCC-VP/CMIP5/Historical/p50/tmax/'

# Where to output 30-yr average rasters
#If looking at forecast data, outdir should be main dir for the 30-year average data.
#ex: C:/Share/LCC-VP/PACE/GRSM/climate_data/forecasts/ave_30yr_byYear/
img30.outdir <- 'C:/Share/LCC-VP/CMIP5/Historical/adjustment_tifs_for_ref/'

#start year
s.year <- 1951

#end year
e.year <- 1980

#For forecast data, do you want to do all possible combinations? (note all lowercase - avoiding 
#regular expressions for now) "yes", "no", or "na" ("na" if PRISM data and not forecast data)
img30.comboYN <- "no"
#img30.allcombo <- c("ea_rcp26", "p25_rcp26", "p50_rcp26", "p75_rcp26", "ea_rcp45", "p25_rcp45", 'p50_rcp45', 'p75_rcp45', 
#                  'ea_rcp60', 'p25_rcp60', 'p50_rcp60', 'p75_rcp60', 'ea_rcp85', 'p25_rcp85', "p50_rcp85", 'p75_rcp85')
img30.allcombo <- c("ppt", "tmin", "tmax")

###################################################

#STEP: Take a list of rasters, stack them, and clip to either a raster or vector mask.

#The following variables work with the following scripts:
#LCC-VP_stack_predictors.R

#OUTDATED:
#LCC-VP_clip_stack_predictors_python.R, which calls
#LCC-VP_clip_stack_predictors.py

#Name of text file containing list layers to be stacked/clipped.

#Layers should be listed with no quotes. NOTE: Can create var lists with LCC-VP_make_variable_forecast_txtFiles.R
#example: C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/GRSM_var_files_example.txt
#layers <- "C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/GRSM_soils_files.txt"

#Name of directory containing csv files of variable file locations. 
#There should be no other csv files in this directory, as the script will
#loop over them.
#CSV file should contain list of files in column 1
#Having a second column is useful so you can use same structure as text files used in LCC-VP_batch_reproj_clip_climdata.R.
#For this script, the output directory will be the same for every image (as there is only one image being output).
#No header row.
var_dir <- "C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/biovarsGddSoilsTWIsrad_20150217/forecast_lists/ea_rcp45/"

#Clip layer - raster
clip_ref <- "C:/Share/LCC-VP/Parks/GRSM/climate_data/current_PRISM/biovars/biovars_1981-2010.tif"
#clip_ref <- "C:/Share/LCC-VP/EcoRegions/Bailey/climate_data/current/biovars//PRISM_30yr_normal_800mM2_stack_x100_biovars_albers_clip.tif"

#Output directory name - don't need this currently bc outdir is part of the csv input file:
#outStackDir <- "C:/Share/LCC-VP/EcoRegions/Bailey/analysis/Cove_Forest_GAP-GRSM/inputs/clim_only_BiovarsGddVpd/"

#Logdir - for storing log files
logdir <- "C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/logs/"

#

###################################################
#STEP: Use existing raster extent to generate a new raster 
#in which each cell has a unique value. 

#The following variables work with the following scripts:
#LCC-VP_generate_unique_values_ras.R

#For this project, we usually use the current PRISM data as existingRas

#Enter path/name of existing ras with which to create new, unique values.
#Can be a single layer or a stack - script only needs/reads first layer.
existingRas <- "C:/Share/LCC-VP/EcoRegions/Bailey/analysis/Cove_Forest_GAP-GRSM/inputs/clim_only-biovars-gdd/H1_BlueRidge_climOnly.tif"
uni.outdir <- "C:/Share/LCC-VP/EcoRegions/Bailey/analysis/common_inputs/"

###################################################
#STEP: prep layers for modeling

#The following variables work with the following scripts:
#LCC-VP_call_model_prep_python.R - which calls LCC-VP_model_prep.py

#TIP: If you are going to be running this over and over for different parks, it's a good idea
#to save these variables in a text file (or somewhere), so you can come back to it later 
#if necessary.  Also helps avoid looking up the same paths over and over again.

#input stack of biologically relevant predictor variables - 
biovars = "C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/biovarsGddSoilsTWIsrad_20150217/BlueRidge_predVars_stacking_20150217_albers_clip.tif"
#biovars = "c:/Share/LCC-VP/EcoRegions/Bailey/analysis/common_inputs/biovarsGddSoilsTWIsrad_20150217//BlueRidge_predVars_stacking_20150217.tif"

#Unique grid - usually based on PRISM
zonesPRISM = "C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/biovars_1981-2010_unique.tif"
#zonesPRISM =  "C:/Share/LCC-VP/EcoRegions/Bailey/analysis/common_inputs/H1_BlueRidge_climOnly_unique.tif"

#fine scale veg classification (shapefile or fc)
veg = "C:/Share/LCC-VP/Parks/GRSM/veg/GRSM_veg_polys_albers.shp"
#veg = "C:/Share/LCC-VP/Parks/GRSM/analysis/Cove_Forest_GAP/veg/blueridge_gap_cove_forest.shp"

#Desired resolution (in meters) of rasterized veg map (10 for parks, trying 20 for range?)
#Must be a factor of resolution of biovars 
res = "10"

#Name of field in veg that contains class information - case sensitive
classFld = "lumped"
#classFld = "Class"

#Classes interseted in mapping from classFld - must be exactly as they appear in the
#attribute table.
#classes = c("Cove Forest", "Spruce-fir forest")
classes = "Yellow Pine Forest"

#Path to park analysis folder
path = "C:/Share/LCC-VP/Parks/GRSM/analysis/"
#path = "C:/Share/LCC-VP/EcoRegions/Bailey/analysis/"

#List of names of spp directories under path - these should be listed in the same order as in "classes" above.
#If these directories don't exist, they will be created.
#spp = c("Cove_Forest_all", "spruce_fir")
spp = "Yellow_Pine"

#Do you want to run the model now, or just prep the layers? (enter "now" or "prep_only")
model = "now"

# Is this a rangewide model(r) or a park model (p)? 
modExtent <- "p"

#BIG data (y/n)? Do we need to sample or can we run a model with all pixels?
bigdata.samp <- "n"

# Big data sampling size per class. If not sampling, can just leave any number here. THIS IS OUTDATED, but I haven't removed it from
# all connected scripts yet.
bigdata.samp.size <- 50

#Map - do you want to predict and output a map layer?  Answer Y or N (caps).
map = "Y"

#park-extent predictors - for calculating park-level results. only needed for rangewide models.
parkPreds = "C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/biovarsGddSoilsTWIsrad_20150217/BlueRidge_predVars_stacking_20150217_albers_clip.tif"

#Enter the name of subdirectory in which to store model outputs. For example,
#If you run GRSM Spruce-fir multiple times with different input variables, you may want 
#to create subdirectories in the output folders, e.g. in C:\Share\LCC-VP\Parks\GRSM\analysis\spruce_fir\outputs\
#you may want to add a subdir called "clim_only"  Put the name of that subdir below.
#If the directory does not exist, R wrapper script will create it.
#NOTE: script will also create same dir in inputs.
subDir.mod <- "biovarsGddSoilsTWIsrad_20150310_parkModel"

# Text file containing variable name for each band in biovars.  Names should be listed
# in text file - each name on a separate line and in quotes.  They should 
# be in the same order as they appear in biovars and be the same length 
# (# bands = # lines in text file).
# Same as var.names, below.
# Example file: C:/Share/LCC-VP/Parks/misc/DEWA_var_names.txt
biovars.names <- "C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/GRSM_var_names_Vars_20150217_BiovarsGddSoilsTwiSrad_SHORT.txt"

#Do you want to run any forecasts now? (enter "now" or "prep_only")
# FIX - the forecasting script is currently only set up to run models with images that have the same
# dimensions as the suitability image created from the python model prep script. For hierarchical models
# where the forecasts will be at a different scale (park only), this doesn't work. Need to run the forecasting 
# part separately for now and use a park-level image as the suitability image - just using it for dimensions.
# Can fix this later - easy fix, just pressed for time right now.
forecast <- "now"

#Enter the name of the directory containing forecasted predictors (right now, the scripts are not set up
# to handle multiple ea/rcp combos. Can update later).
forecast.dir <- "C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/biovarsGddSoilsTWIsrad_20150217/forecast_imgs/ea_rcp85/"



###################################################
#STEP: SDM - predict current % cover

#The following variables work with the following scripts:
#LCC-VP_sdm_RF.R
# 

# Response Image - training/"truth" image. Usually the 800m %cover image.
res.img <- "C:/Share/LCC-VP/Parks/GRSM/analysis/spruce_fir_GAP_GRSM/inputs/biovarsGddSoilsTWIsrad_20150217_rngwideModel/COVER_spruce_fir_GAP_GRSM_inPRISM_800m_masked.tif"
# 
# Predictor variable Stack
v.stk <- "C:/Share/LCC-VP/Parks/GRSM/analysis/spruce_fir_GAP_GRSM/inputs/biovarsGddSoilsTWIsrad_20150217_rngwideModel/BlueRidge_predVars_stacking_20150217_masked.tif"
#
# Text file containing variable name for each band in v.stk.  Names should be listed
# in text file - each name on a separate line and in quotes.  They should 
# be in the same order as they appear in v.stk and be the same length 
# (# bands = # lines in text file).
# Example file: C:/Share/LCC-VP/Parks/misc/DEWA_var_names.txt
var.names <- "C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/GRSM_var_names_Vars_20150217_BiovarsGddSoilsTwiSrad_SHORT.txt"

#park-extent predictors - for calculating park-level results - don't need this if it's a park-level model.
parkPreds2 = "C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/biovarsGddSoilsTWIsrad_20150217/BlueRidge_predVars_stacking_20150217_albers_clip.tif"

#BIG data (y/n)? Do we need to sample or can we run a model with all pixels?
bigdata <- "y"

# Big data sampling size per class. If not sampling, can just leave any number here. THIS IS OUTDATED, but I haven't removed it from
# all connected scripts yet. 
bigdata.size <- 50

# Is this a rangewide model(r) or a park model (p)? 
mod.extent <- "r"

# Do you want to predict an output map layer? Answer Y or N.
map <- "Y"
# 
# Path/directory where output data will be stored.
outDir <- "C:/Share/LCC-VP/Parks/GRSM/analysis/spruce_fir_GAP_GRSM/outputs/biovarsGddSoilsTWIsrad_20150217_rngwideModel/"

# Analysis subdir - will be used in file naming to help distinguish model runs.
subDir <- "biovarsGddSoilsTWIsrad_20150217_rngwideModel"
# #model name (name of ecological system subdirectory will work here). i.e. "EasternHemlock_NHF"
mod.name <- "spruce_fir_GAP_GRSM"
#
#Class name (as listed in attribute table of original classification)
class.name <- "Spruce-fir forest"

###################################################
#STEP: SDM Forecasting - predict future scenarios

#The following variables work with the following scripts:
#LCC-VP_sdm_RF_forecasting.R

#Enter /path to dir containing of RF models (saved in previous step from LCC-VP_sdm_RF.R)
rfdir <- "C:/Share/LCC-VP/Parks/GRSM/analysis/spruce_fir_GAP_GRSM/outputs/biovarsGddSoilsTWIsrad_20150217_rngwideModel/"

#Original suitability image used as reference for dimensions (not modeled). At this point, used only for matching dimensions, so
# the image listed here should be of the area you are predicting.
suit.img <- "C:/Share/LCC-VP/parks/GRSM/analysis/spruce_fir/inputs/biovarsGddSoilsTWIsrad_20150304_parkModel/COVER_spruce_fir_inPRISM_800m_masked.tif"

#Directory containing forecasted predictors. Stacks must contain same bands in the same
#order as biovars or v.stk.
f.dir <- "C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/biovarsGddSoilsTWIsrad_20150217/forecast_imgs/ea_rcp85/"

#Model output directory - where output forecast maps will be stored.
m.dir <- "C:/Share/LCC-VP/Parks/GRSM/analysis/spruce_fir_GAP_GRSM/outputs/biovarsGddSoilsTWIsrad_20150217_rngwideModel/forecasts/ea_rcp85/"

# Text file containing variable names.  Names should be listed
# in text file - each name on a separate line and in quotes.  They should 
# be the same names in the same order as they appear in predictor stacks for the model 
# and be the same length (# bands = # lines in text file).
# Example file: C:/Share/LCC-VP/Parks/misc/DEWA_var_names.txt
pred.names <- "C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/GRSM_var_names_Vars_20150217_BiovarsGddSoilsTwiSrad_SHORT.txt"

#original model name - would be same as mod.name in previous step. Name to use in forecasting.
f.mod.name <- "spruce_fir_GAP_GRSM"

