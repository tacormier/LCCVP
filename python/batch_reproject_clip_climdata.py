# Import system modules
import arcpy, glob, os, sys, time, argparse
from arcpy import env
from arcpy.sa import *

arcpy.env.overwriteOutput = True

#Deal with passed arguments
p = argparse.ArgumentParser(prog="LCC-VP_batch_reproject_clip_climdata.py", description="reproject image stacks and clip to desired boundary.")
p.add_argument("-p", dest="park", required=True, help="PACE or Park name")
p.add_argument("-i", dest="clim_in", required=True, help="indir or infile")
g = p.add_mutually_exclusive_group(required=True)
g.add_argument("-d", dest="fdir", required=False, action="store_true", help="flag if you are inputting parent directory that contains quantile/rcp info, which contains the subdirectories ppt, tmin, tmax")
g.add_argument("-f", dest="img", required=False, action="store_true", help="flag if you are inputting individual file to reproject and clip")
p.add_argument("-o", dest="clim_outdir", required=True, help="output directory")
p.add_argument("-cf", dest="clim_clip", required=True, help="raster or shapefile to which to clip")
p.add_argument("-s", dest="clim_snap", required=True, help="set snap raster file")
p.add_argument("-e", dest="clim_fail", required=True, help="text file where failed images are recorded")
p.add_argument("-r", dest="clim_reproj", required=True, help="do you want imgs' projection defined and reproj? Or just clipped? 'both' or 'clip_only")

args = p.parse_args()

park = args.park
clim_in = args.clim_in
clim_outdir = args.clim_outdir
clim_reproj = args.clim_reproj
clim_clip = args.clim_clip
clim_snap = args.clim_snap
clim_fail = args.clim_fail




#previous way of dealing with passed args
#park = sys.argv[1]
#clim_in = sys.argv[2]
#clim_outdir = sys.argv[3]
#clim_clip = sys.argv[4]
#clim_snap = sys.argv[5]
#clim_fail = sys.argv[6]
#clim_sr = sys.argv[5]

print
print "park = " + park
print "args.fdir = " + str(args.fdir)
print "args.img = " + str(args.img)
print "clim_in = " + clim_in
print "clim_outdir = " + clim_outdir
print "clim_reproj = " + clim_reproj
print "clim_clip = " + clim_clip
print "clim_snap = " + clim_snap
print "clim_fail = " + clim_fail
print

#sys.exit()

def makeitrain(park, clim_infile, clim_outdir, clim_reproj, clim_clip, clim_snap, clim_fail):
    try:
        clim_indir = os.path.dirname(clim_infile) + "/"
        outras1 = clim_outdir + os.path.basename(clim_infile).split(".")[0] + "_proj.tif"
        #first define projection because files do not come with .prj file
        if clim_reproj == "both":
            print "reprojecting " + str(clim_infile) + " to Albers"

            #TC commented out
            #sr = arcpy.SpatialReference("WGS 1984")
            #sr = arcpy.SpatialReference("GCS WGS 1972")
            #sr = arcpy.SpatialReference("WGS 1983")

            #TC commented out
            #arcpy.DefineProjection_management(clim_infile, sr)
            #for testing/debugging
            #time.sleep(3)

            #Now project into same proj as clim_clip.
            #These env settings don't work if clipfile is NOT a raster.
            env.snapRaster = clim_snap
            #env.extent = clipfile
            #TC commented out
            #arcpy.ProjectRaster_management(clim_infile,outras1, clim_clip, "BILINEAR", "800", "NAD_1983_To_WGS_1984_5", "#", "#")
            #arcpy.ProjectRaster_management(clim_infile,outras1, clim_clip, "BILINEAR", "800", "WGS_1972_To_WGS_1984_1 + WGS_1984_(ITRF00)_To_NAD_1983", "#", "#")
            #arcpy.ProjectRaster_management(clim_infile,outras1, clim_clip, "BILINEAR", "800")

            #for testing/debugging
            #time.sleep(5)

            #And clip to study boundaries
            print "Clipping " + str(clim_infile)
            outfile = clim_outdir + os.path.basename(clim_infile).split(".")[0] + "_albers_clip.tif"
            #outfile = clim_outdir + "p50_" + os.path.basename(clim_infile).split(".")[0].split("_")[1] + "_historical_" + os.path.basename(clim_infile).split("_")[3] + "_albers_clip.tif"
            outExtractByMask = ExtractByMask(outras1, clim_clip)
            outExtractByMask.save(outfile)


            #housekeeping - don't need country-wide projected files...just keep clipped files.
            delfiles = glob.glob(clim_outdir + "\\*proj*")
            for j in delfiles:
                os.remove(j)

        elif clim_reproj == "clip_only":
            sr = arcpy.SpatialReference("WGS 1983")
            arcpy.DefineProjection_management(clim_infile, sr)

            #And clip to study boundaries
            print "Clipping " + str(clim_infile)
            outfile = clim_outdir + os.path.basename(clim_infile).split(".")[0] + "_clip.tif"
            #outfile = clim_outdir + os.path.basename(clim_infile).split(".")[0] + "_albers_clip.tif"
            #outfile = clim_outdir + "p50_" + os.path.basename(clim_infile).split(".")[0].split("_")[1] + "_historical_" + os.path.basename(clim_infile).split("_")[3] + "_albers_clip.tif"
            #TC commented out bc using a shapefile to mask
            #env.snapRaster = env.extent = clim_snap
            outExtractByMask = ExtractByMask(clim_infile, clim_clip)
            outExtractByMask.save(outfile)
        else: print "ERROR: clim_clip must be either 'both' or 'clip_only.' You entered: " + clip_reproj

    except Exception, cmsg:
        print clim_infile + " FAILED. . . "
        print cmsg
        f.write(clim_indir + clim_infile + "\n")
        f.flush()


#Set climate parameters
#params = ["biovars", "GDD", "ppt","tmin", "tmax"]
#params = ["tmin", "tmax"]

#open connection to fail file
f = open(clim_fail, "a")

arcpy.CheckOutExtension("Spatial")

#STILL WORKING ON SETTING THIS UP!
if args.fdir:
    #for clim in params:
    #set workspace
    env.workspace = clim_outdir
    print env.workspace
    #env.workspace = "C:\\Share\\LCC-VP\\PACES\\GRSM\\PRISM\\forecasts\\biovars\\ave_30yr\\"

    files = glob.glob(os.path.dirname(clim_in)+"/*.tif")

    for clim_infile in files:
        makeitrain(park, clim_infile, clim_outdir, clim_reproj, clim_clip, clim_snap, clim_fail)
    print clim_in + " done!"

elif args.img:
    clim_infile = clim_in
    env.workspace = os.path.dirname(clim_in)
    makeitrain(park, clim_infile, clim_outdir, clim_reproj, clim_clip, clim_snap, clim_fail)

#check spatial analyst extension back in
arcpy.CheckInExtension("Spatial")

#close fail file
f.close()