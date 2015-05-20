#This script will calculate a topographic wetness index for a list of dems.
#This script does NOT use the parameters.R file - it is stand-alone.

import arcpy, glob, os, sys
from arcpy import env
from arcpy.sa import *

#variables
#demdir = "C:/Share/LCC-VP/RangeWide/ned/"
demdir = "C:/Share/tcormier/hummingbirds/ned/10m/regional_usgs_gridfloat/UTM/"
#outdir = "C:/Share/LCC-VP/RangeWide/TWI/"
outdir = "C:/Share/tcormier/hummingbirds/TWI/regional/"
cellsize = 10

# arcpy.env.snapRaster = "C:/Share/LCC-VP/RangeWide/ned/MZ31_ned_alb_ecog.img"

env.cellSize = cellsize
arcpy.env.overwriteOutput = True

img_list = glob.glob(demdir + '*.img')
arcpy.CheckOutExtension("Spatial")
for i in img_list:
    try:
        #outname
        #outfile = outdir + os.path.basename(i).replace(".tif", "_twi.tif")
        outfile = outdir + os.path.basename(i).replace(".img", "_twi.tif")
        #This code does not write out intermediate steps.
        #Code found at http://forums.arcgis.com/threads/32896
        print "calculating flow direction for " + i
        fd = FlowDirection(i)
        print "calculating flow accumulation for " + i
        sca = FlowAccumulation(fd)
        slope = ( Slope(i) * 1.570796 ) / 90
        tan_slp = Con( slope > 0, Tan(slope), 0.001 )
        sca_scaled = ( sca + 1 ) * cellsize
        cti = Ln( sca_scaled / tan_slp )
        cti.save(outfile)
    except Exception, geoproc:
        print "geprocessing failed. . ."
        print geoproc