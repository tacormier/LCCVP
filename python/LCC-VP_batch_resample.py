#This script takes an indir full of rasters and resamples them to a given resolution
#This script does not use parameters.R - it is an independent utility.

# Import system modules
import arcpy, glob, os, shutil
from arcpy import env
from arcpy.sa import *

arcpy.env.overwriteOutput = True

####################### VARIABLES - to run manually ###################################

#set input directory
indir = "C:/Share/LCC-VP/RangeWide/TWI/30m/"

#output cell size
cs = "40"

#Reference layer for snapping
snapRas = "C:/Share/LCC-VP/Parks/GRSM/climate_data/current_PRISM/biovars/biovars_1981-2010.tif"

#output directory
outdir = "C:/Share/LCC-VP/RangeWide/TWI/40m/"
########################################################################################
env.workspace = indir

#list rasters in indir (tifs)
rasList = glob.glob(indir + '*alb_ecog_twi.tif')

#settings:
env.snapRaster = snapRas
#env.cellSize = cs

#Loop over list and resample:
for img in rasList:
    print img
    try:
        outRas = outdir+os.path.basename(img).split(".")[0] + "_40m.tif"
        arcpy.Resample_management(img, outRas, cs, "BILINEAR")
        shutil.move(outRas, outdir+outRas)
    except:
        print "Resample failed for " + img
        print arcpy.GetMessages()


