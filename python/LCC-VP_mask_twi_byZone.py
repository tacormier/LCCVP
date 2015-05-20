# Import system modules
import arcpy, glob, os
from arcpy import env
from arcpy.sa import *

arcpy.env.overwriteOutput = True

####################### VARIABLES - to run manually ###################################
twiDir = "C:/Share/LCC-VP/RangeWide/TWI/new800m/"
mappingzones = "C:/Share/LCC-VP/boundaries_other/Mapping_zones.shp"
field = "new_zone_i"
outdir = "C:/Share/LCC-VP/RangeWide/TWI/new800m/"
workingdir = "C:/Share/LCC-VP/RangeWide/TWI/"

#######################################################################################

twiList = glob.glob(twiDir + "*.tif")

zoneList = []
for i in twiList:
    zoneList.append(os.path.basename(i).split("_")[0].split("Z")[1])

n=0
for zone in zoneList:
    #Select Zone
    outSel = workingdir + "/MZ_" + zone + ".shp"
    where = '"' + field + '\" =' + "\'" + zone +"'"
    arcpy.Select_analysis(mappingzones, outSel, where)

    #Buffer by 800m
    buffOut = outSel.split(".")[0] + "_buff800m.shp"
    arcpy.Buffer_analysis(outSel, buffOut, "800 Meters")

    #mask by buffered zone
    env.snapRaster = "C:/Share/LCC-VP/RangeWide/TWI/new800m/MZ26_ned_alb_ecog_twi_800m.tif"

    arcpy.CheckOutExtension("Spatial")
    outMasked = outdir + os.path.basename(twiList[n]).split(".")[0] + "_clipped.tif"
    outExtractByMask = ExtractByMask(twiList[n], buffOut)
    outExtractByMask.save(outMasked)

    n = n+1

