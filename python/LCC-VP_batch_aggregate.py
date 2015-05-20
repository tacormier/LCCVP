#This script takes an indir full of rasters and aggregates them to a
#larger cell size based on the aggregation factor.
#This script does not use parameters.R - it is an independent utility.

# Import system modules
import arcpy, glob, os, shutil
from arcpy import env
from arcpy.sa import *

arcpy.env.overwriteOutput = True

####################### VARIABLES - to run manually ###################################

#set input directory
indir = "C:/Share/LCC-VP/RangeWide/TWI/40m/"

#aggregation factor
af = "20"

#Reference layer for snapping
snapRas = "C:/Share/LCC-VP/Parks/GRSM/climate_data/current_PRISM/biovars/biovars_1981-2010.tif"

#output directory
outdir = "C:/Share/LCC-VP/RangeWide/TWI/new800m/"
########################################################################################
env.workspace = indir

#list rasters in indir (tifs)
rasList = glob.glob(indir + '*alb_ecog_twi_40m.tif')

#settings:
env.snapRaster = snapRas
#env.cellSize = cs

#Loop over list and resample:
for img in rasList:
    print img
    try:
        outRas = outdir+os.path.basename(img).split(".")[0].replace("40m","800m.tif")
        dat = Aggregate(img, 20, "MEAN", "EXPAND", "DATA")
        dat.save(outRas)
        del dat
    except:
        print "Aggregate failed for " + img
        print arcpy.GetMessages()


