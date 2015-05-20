#will create quantile/rcp combination directories, each containing ppt, tmin, tmax, and then move files from raw folder to
#the appropriate new dir
import os
import glob
import shutil as sh

indir="C:/Share/LCC-VP/PACE/SHEN/climate_data/forecasts/raw_yearly/"
clims=["ppt", "tmax", "tmin"]
newdirs=["ea_rcp26", "p25_rcp26", "p50_rcp26", "p75_rcp26", "ea_rcp45", "p25_rcp45", "p50_rcp45", "p75_rcp45", "ea_rcp60", "p25_rcp60", "p50_rcp60", "p75_rcp60", "ea_rcp85", "p25_rcp85", "p50_rcp85", "p75_rcp85"]

#make newdirs
#os.chdir(indir)

for i in newdirs:
    #make new dirs
    pptdir = indir+i+"/"+clims[0]
    tmaxdir = indir+i+"/"+clims[1]
    tmindir = indir+i+"/"+clims[2]
    os.makedirs(pptdir)
    os.makedirs(tmaxdir)
    os.makedirs(tmindir)

    #move files from main directory into their respective subdirs
    for clim in clims:
        q, rcp = i.split("_")

        if clim == "ppt":
            print("moving ppt images for " + i)
            files=glob.glob(indir+"/"+clim +"/"+q+"_pr_"+rcp+"*.tif")
            for f in files:
                sh.move(f,pptdir)
        elif clim == "tmax":
            print("moving tmax images for " + i)
            files=glob.glob(indir+"/"+clim +"/"+q+"_tasmax_"+rcp+"*.tif")
            for f in files:
                sh.move(f,tmaxdir)
        elif clim == "tmin":
            print("moving tmin images for " + i)
            files=glob.glob(indir+"/"+clim +"/"+q+"_tasmin_"+rcp+"*.tif")
            for f in files:
                sh.move(f,tmindir)


