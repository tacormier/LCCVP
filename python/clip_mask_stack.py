# Import system modules
import arcpy, glob, os, sys
from arcpy import env
from arcpy.sa import *
arcpy.env.overwriteOutput = True

arcpy.CheckOutExtension("Spatial")

biovars = "C:/Share/LCC-VP/Parks/GRSM/climate_data/forecasts/ave_30yr/biovars/p50_biovars_rcp85_2081_yearRange_2066-2095_albers_clip.tif"
file2 = "C:/Share/LCC-VP/Parks/GRSM/radiation/srad800.tif"

outstack = "C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/p50_biovars_rcp85_2081_yearRange_2066-2095_albers_clip_srad.tif"

env.snapRaster = env.extent = env.cellSize = env.mask = biovars
#outExtractMask = ExtractByMask(file2, biovars)

mystring = (biovars, file2)
joinstring = ';'.join(mystring)

arcpy.CompositeBands_management([biovars, file2], outstack)