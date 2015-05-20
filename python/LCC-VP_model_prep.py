# Import system modules
import arcpy, glob, os, sys, shutil, subprocess, time
from arcpy import env
from arcpy.sa import *

arcpy.env.overwriteOutput = True

##### VARIABLES - to run manually #####
##Biovars image
#biovars = "C:/Share/LCC-VP/Parks/DEWA/climate_data/current_PRISM/biovars/biovars_1981-2010.tif"

##Unique PRISM grid
#zonesPRISM = "C:/Share/LCC-VP/Parks/DEWA/analysis/common_inputs/biovars_1981-2010_unique.tif"

##park veg classification (shapefile or fc)
#veg = "C:/Share/LCC-VP/Parks/DEWA/NPS_data/veg_map_etc/dewaveg/DEWA_Vegetation_2006_public_albers.shp"

##park image for dimensions
#dimImg <- "C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/allVars_BiovarsGddVpdSoilsTWIsrad_20141229/GRSM_AllVars_stacking_20141201_clip.tif"

##Desired resolution in meters of rasterized veg map.
#res = "10"

##Name of class field (either code or character).
#classFld = "LOCAL_NAME"

##Classes interested in mapping - must be exactly as they appear in the attribute table
#classes = ["Spruce-fir forest", "Hemlock forest (typic type)", "Spruce-hemlock forest", "Hemlock forest (white pine type)"]

##Path to park analysis folder
#path = "C:/Share/LCC-VP/GRSM/analysis/"

##List of spp directories
#spp = ["spruce_fir", "hemlock_typic_type", "spruce_hemlock", "hemlock_white_pine_type"]

##Map - do you want to predict and output map layer. Answer Y or N
#map = "Y"

#Enter the name of subdirectory in which to store model outputs. For example,
#If you run GRSM Spruce-fir multiple times with different input variables, you may want
#to create subdirectories in the output folders, e.g. in C:\Share\LCC-VP\Parks\GRSM\analysis\spruce_fir\outputs\
#you may want to add a subdir called "clim_only"  Put the name of that subdir below.
#If the directory does not exist, R wrapper script will create it.
#NOTE: script will also create same dir in inputs.
#subDir <- "clim_only"

################### VARIABLES READ IN FROM R WRAPPER ####################################

park = sys.argv[1]
biovars = sys.argv[2]
zonesPRISM = sys.argv[3]
veg = sys.argv[4]
res = sys.argv[5]
classFld = sys.argv[6]
classes_file = sys.argv[7]
path = sys.argv[8]
spp_file = sys.argv[9]
map = sys.argv[10]
resFactor = sys.argv[11]
model = sys.argv[12]
subDir = sys.argv[13]
biovars_names = sys.argv[14]
forecast = sys.argv[15]
forecast_dir = sys.argv[16]
bigdata = sys.argv[17]
bigdata_size = sys.argv[18]
parkPreds = sys.argv[19]
modExtent = sys.argv[20]

#########################################################################################

i=0
#Process each veg class
#open text file of classes (cf for class file)
cf = open(classes_file, "rb")
#read lines into a list and use python generator to strip off eol characters
classes = [this.rstrip("\r\n") for this in cf.readlines()]
classes = [this.replace("\"", "") for this in classes]
cf.close()

#Open spp subdir list (sf for spp file)
sf = open(spp_file, "rb")
#read lines into a list and use python generator to strip off eol characters
spp = [this.rstrip("\r\n") for this in sf.readlines()]
sf.close()

cl = classes[0]
for cl in classes:
    try:
        arcpy.os.outputCoordinateSystem = biovars
        #output directory for geoprocessing
        geopath = path + spp[i] + "/inputs/" + subDir + "/"
        if not os.path.exists(geopath):
            os.makedirs(geopath)

        #TC Commented out this whole section of adding 1's and 0's to shapefile and
        #merging back together. This is already done for cove forests! Uncomment when needed!

        #select class and export to new shapefile
        print "writing new spp shapefile for " + cl
        outSel = geopath + spp[i] + ".shp"
        where = '"' + classFld + '\" = ' + "\'" + cl + "'"
        arcpy.Select_analysis(veg, outSel, where)

        #Add field of 1's
        arcpy.AddField_management(outSel, "code", "SHORT")
        arcpy.CalculateField_management(outSel, "code", "1", "PYTHON")

        #Erase spp shapefile from orig veg polygons
        print "executing Erase on veg polygons with " + cl
        outErase = geopath + "veg_erase_" + spp[i] + ".shp"
        arcpy.Erase_analysis(veg, outSel, outErase)

        #Add field of 0's
        arcpy.AddField_management(outErase, "code", "SHORT")
        arcpy.CalculateField_management(outErase, "code", "0", "PYTHON")

        #Merge back together (rather than altering original veg polys and potentially
        #making an error on the orginal copy.
        print "merging veg polygons and " + cl + " into newly coded shapefile"
        #TC commented this out 2/20 -
        outMerge = geopath + "allVeg_merge_" + spp[i] + ".shp"
        #outMerge = veg
        arcpy.Merge_management([outSel, outErase], outMerge)

        #Rasterize merged file - be sure to set env variables.
        #TC changed "code" to "GRIDCODE" in line 121 for shapefile that was already ready to rasterize!
        #Also added line 119 to set input shapefile to outMerge and pick up at this step.
        #outMerge = veg
        print "rasterizing new polygons to " + str(res) + "m"
        env.snapRaster = env.extent = biovars
        outRas = geopath + spp[i] + "_" + str(res) + "m.tif"
        arcpy.PolygonToRaster_conversion(outMerge, "code", outRas, "MAXIMUM_COMBINED_AREA", "", res)

        #Zonal stats with unique prism ras as zones.
        print "calculating percent cover of " + cl + " for every PRISM pixel"
        arcpy.CheckOutExtension("Spatial")
        env.mask = biovars
        env.cellSize = outRas
        outSum = geopath + "SUM_" + spp[i] + "_inPRISM.tif"
        #first calc RAT for zone ras if it doesn't exist
        if not os.path.exists(zonesPRISM + ".vat.dbf"):
            arcpy.BuildRasterAttributeTable_management(zonesPRISM)
        outZonal = ZonalStatistics(zonesPRISM, "Value", outRas, "SUM")
        outZonal.save(outSum)

        #Normalization 0-100% cover.
        arcpy.CheckOutExtension("3D")
        outCover = geopath + "COVER_" + spp[i] + "_inPRISM.tif"
        #Cover = RoundUp(Raster(outSum) / float(resFactor))
        #Cover.save(outCover)
        #apparently SA is broken on arctic, so let's try the divide command in 3D analyst :)
        arcpy.Divide_3d(outSum, float(resFactor),outCover)

        #Resample outCover to 800m.
        outResamp = geopath + "COVER_" + spp[i] + "_inPRISM_800m.tif"
        env.cellSize = biovars
        #Chose nearest neighbor bc these should directly overlay with PRISM and the cells shouldn't actually change values.
        arcpy.Resample_management(outCover, outResamp, "800", "NEAREST")

        #Mask the biovar stack with  newly created COVER image bc veg data might not reach edges of orig
        #PRISM stack. Need response and predictor rasters to have the same dimensions and NAs.
        print "masking " + spp[i] + " cover raster to ensure that response and predictor rasters have same dimensions"
        env.snapRaster = env.extent = env.cellSize = biovars
        outMasked = geopath + os.path.basename(biovars.split(".")[0]) + "_masked.tif"
        outExtractMask = ExtractByMask(biovars, outResamp)
        outExtractMask.save(outMasked)

        #Mask the response image (COVER) by newly masked biovars bc veg data might not reach edges of orig
        #PRISM stack. Need response and predictor rasters to have the same dimensions and NAs.
        print "masking " + spp[i] + " cover raster to ensure that response and predictor rasters have same dimensions"
        env.snapRaster = env.extent = env.cellSize = biovars
        outMasked2 = geopath + os.path.basename(outResamp.split(".")[0]) + "_masked.tif"
        outExtractMask2 = ExtractByMask(outResamp, outMasked)
        outExtractMask2.save(outMasked2)


    except Exception, geoproc:
        print "geprocessing failed. . ."
        print geoproc


    #housekeeping - move intermediate files to diff directory.
    #THIS SECTION FAILS BC PYTHON IS HOLDING CONNECTIONS TO SOME OF THE FILES - HOW TO FIX? FIGURE IT OUT.
    #fileList = [outSel, outErase, outMerge, outRas, outSum, outCover]
    #interDir = geopath + "intermediate/"

    ##If the file already exists in the in the intermediate directory, overwrite it.
    #for file in fileList:
        #dstFile = interDir + os.path.basename(file)
        #dstFiles = glob.glob(dstFile.split(".")[0] + ".*")
        #mvFiles = glob.glob(file.split(".")[0] + ".*")
        #if os.path.exists(dstFile):
            #for rmFile in dstFiles:
                #try:
                    #os.remove(rmFile)
                #except Exception, cmsg:
                    #print "Delete files FAILED. . . "
                    #print cmsg
                    #pass
        #for mvFile in mvFiles:
            #try:
                #shutil.move(mvFile, interDir)
            #except Exception, cmsg:
                #print "Move files FAILED. . . "
                #print cmsg
                #pass

    ##also remove biovars, as it's saved in the park/PRISM dir anyway - no need to have mult copies
    #os.remove(biovars)
    ##mv unique PRISM to intermediate dir
    #zonesDst = interDir + os.path.basename(zonesPRISM)
    #if os.path.exists(zonesDst):
        #os.remove(zonesDst)
    #shutil.move(zonesPRISM, interDir)

    #Run model now?
    if model == "now":
        #Call R script to perform modeling.
        #Script requires you to pass several variables: outMasked2, biovars, #bands in biovars, map output (Y or N), outDir
        b = arcpy.GetRasterProperties_management(biovars, "BANDCOUNT")
        #map = "Y"
        outDir = path + spp[i] + "/outputs/" + subDir + "/"
        if not os.path.exists(outDir):
            os.makedirs(outDir)

        #write parameter file for R
        line1 = "res.img <- \"" + outMasked2 + "\""
        line2 = "v.stk <- \"" + outMasked + "\""
        line3 = "var.names <- \"" + biovars_names + "\""
        line4 = "map <- \"" + map + "\""
        line5 = "outDir <- \"" + outDir + "\""
        line6 = "mod.name <- \"" + spp[i] + "\""
        line7 = "class.name <- \"" + cl + "\""
        line8 = "bigdata <- \"" + bigdata + "\""
        line9 = "bigdata.size <- \"" + bigdata_size + "\""
        line10 = "subDir <- \"" + subDir + "\""
        line11 = "parkPreds2 <- \"" + parkPreds + "\""
        line12 = "mod.extent <- \"" + modExtent + "\""

        lineList = [line1,line2,line3,line4,line5,line6,line7,line8,line9,line10,line11,line12]

        paramsFile = geopath + spp[i] + "_R_params.txt"
        params = open(paramsFile, "w")
        for line in lineList:
            params.write(line + '\n')
        params.close()

        # Stuff to pass to R command
        output_name = spp[i]
        script_filename = r'C:\Share\LCC-VP\scripts\R\LCC-VP_sdm_RF.R'
        #need to write in some logic about big data (i.e. Hierarchical model level 1) sampling and level 2).
        #if bigdata == "n":
            #script_filename = r'C:\Share\LCC-VP\scripts\R\LCC-VP_sdm_RF.R'
        #elif bidata == "y":
            #script_filename = r'C:\Share\LCC-VP\scripts\R\LCC-VP_sdm_RF_BigData_sampling.R'
        #else:
            #print"bigdata variable must be y or no. You entered " + bigdata

        param_filename = paramsFile
        result_filename = outDir + spp[i] + "_R_out.txt"

        #Run and print this line to the console, then copy and paste it into windows command line to test manually.
        cmd = r"C:\\Program Files\\R\\R-3.1.2\\bin\\R.exe --vanilla --args " + param_filename + " < " + script_filename + " > "+ result_filename
        #cmd = "C:\\Program Files\\R\\R-3.0.2\\bin\\R.exe --vanilla --args " + paramsFile + " < " + script_filename + " > "+ result_filename
        #by adding the communicate line, we are forcing the script to wait until process is finished before moving on.
        process = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        #proc = subprocess.Popen(cmd).wait
        procout = process.communicate()

        #So that all files needed by forecasting exist on the file system before forecast tries to call them.
        time.sleep(15)

        print "rf model for " + spp[i] + " finished or broke - check it out!"

    else:
        print "Model prep finished. User opted not to run models at this time."

    if forecast == "now":
        #Call R script to perform forecasting.
        #Script requires you to pass several variables: rf, suit.img, f.dir, m.dir, pred.names, f.mod.name
        # the ea_rcp85 is hardcoded here for the moment. RUSH.
        m_dir = outDir + "forecasts/ea_rcp85/"
        if not os.path.exists(m_dir):
            os.makedirs(m_dir)

        #write parameter file for R
        line8 = "rfdir <- \"" + outDir + "\""
        line9 = "suit.img <- \"" + outMasked2 + "\""
        line10 = "f.dir <- \"" + forecast_dir + "\""
        line11 = "m.dir <- \"" + m_dir + "\""
        line12 = "pred.names <- \"" + biovars_names + "\""
        line13 = "f.mod.name <- \"" + spp[i] + "\""
        #line14 = "f.class.name <- \"" + cl + "\""

        lineList2 = [line8,line9,line10,line11,line12,line13]
        paramsForecast = outDir + spp[i] + "_R_ForecastParams.txt"
        params2 = open(paramsForecast, "w")

        for line in lineList2:
            params2.write(line + '\n')
        params2.close()

        #Stuff to pass to R
        script_filename2 = r'C:\Share\LCC-VP\scripts\R\LCC-VP_sdm_RF_forecasting.R'
        result_filename2 = outDir + spp[i] + "_R_Forecast_out.txt"

        #Compose R command
        cmd2 = "C:\\Program Files\\R\\R-3.1.2\\bin\\R.exe --vanilla --args " + paramsForecast + " < " + script_filename2 + " > "+ result_filename2
        process = subprocess.Popen(cmd2, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        procout = process.communicate()

        print "rf forecast for " + spp[i] + " finished or broke - check it out!"

    else:
        print "Model prep finished. User opted not to forecast at this time."
    i+=1
