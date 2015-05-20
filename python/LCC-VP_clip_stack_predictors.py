# Import system modules
import arcpy,os, sys
from arcpy import env
from arcpy.sa import *

arcpy.env.overwriteOutput = True

######################## VARIABLES - to run manually for a single text file ###############################

##stack of current bioclimatic variables
##biovars_current = "C:/Share/LCC-VP/Parks/GRSM/climate_data/current_PRISM/biovars/biovars_1981-2010.tif"

##List of layers to stack/clip- text file
##The first layer listed in this file will be the reference file for projection
##and snapping preferences.
#layers = "C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/GRSM_var_files_ALLvars.txt"

##directory containing projected/forecasted bioclimatic variable stacks
##biovars_forecasts = "C:/Share/LCC-VP/Parks/GRSM/climate_data/forecasts/ave_30yr/biovars/"

##List of projected/forecasted ancillary layers -text file.
##These layers must represent the same variables in the SAME order as aLayers_current.
##aLayers_forecasts = "C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/GRSM_var_list_current.txt"

##clip layer
#clip_ref = "C:/Share/LCC-VP/ALCC_PACE_Park_boundaries/GRSM_boundary_buff800m_albers.shp"

##output file root name - a descriptive name that describes this model run (e.g. "biovars_gdd_soils_twi_srad_")
##Do not include file extension - year and file extension will be appended.

##output stack name:
#outStack = "C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/allVars/current_1981-2010_biovars_gdd_soils_twi_srad.tif"

#########################################################################################

park = sys.argv[1]
layers = sys.argv[2]
clip_ref = sys.argv[3]
outStack = sys.argv[4]

#Read in current list of ancillary layers:
cal_file = open(layers, "rb")
#read lines into a list and use python generator to strip off eol characters
cal = [this.rstrip("\r\n") for this in cal_file.readlines()]
cal_file.close()

#get spatial ref of first image in list.
sr_ref = arcpy.Describe(cal[0]).spatialReference.name

##set up proj = good variable
#goodProj = []
##Loop over aLayers_current and make sure they have the same spatial ref. If not, error.
#try:
    #for alayer in cal:
        #print alayer
        #sr_al = arcpy.Describe(alayer).spatialReference.name
        #if sr_ref != sr_al:
            #goodProj.append("bad")
            #print "Error: projections do not match between biovars_current and " + alayer + " " + sr_ref + " VS " + sr_al
#except Exception, projcheck:
    #print projcheck

#If all checked out in projection checking, then proceed to clipping and stacking. Assumed that biovars_
try:
    #if all projections match - yay! Keep going!
    #if len(goodProj) == 0:
    env.snapRaster = env.cellSize = cal[0]
    env.extent = clip_ref
    #env.mask only works with spatial analyst tools, which composite bands is not.
    #env.Mask = clip_ref
    arcpy.CompositeBands_management(cal, outStack)

    #now clip by polygon
    arcpy.CheckOutExtension("Spatial")
    outExtractByMask = ExtractByMask(outStack, clip_ref)
    outExtractByMask.save(outStack)

except Exception, compBands:
    print compBands


