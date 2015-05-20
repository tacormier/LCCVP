# Import system modules
import arcpy,os, sys
from arcpy import env
from arcpy.sa import *

arcpy.env.overwriteOutput = True
env.workspace = "C:/Share/LCC-VP/RangeWide/TWI/30m/"

imgList = arcpy.ListRasters("*", "TIF")
outdir = "C:/Share/LCC-VP/RangeWide/TWI/90m/"

arcpy.CheckOutExtension("Spatial")

for i in imgList:
    outimg = outdir + i.split(".")[0] + "_90m.tif"
    print("aggregating " + outimg)
    outAggreg = Aggregate(i, 3, "MEAN")
    outAggreg.save(outimg)
