#Quick script to create 1981-2010 mean based on modeled climate data. Very hardcoded as hopefully 1-time run.

#Tina Cormier

#load libraries
library(raster)
library(stringr)
library(maptools)

#PACE/Park
p <- "UPDE"

#quantiles
#quan <- c("p25", "p50", "p75")
#quan <- c("p50")
quan <- "ea_"

#indir <- paste("C:/Share/LCC-VP/Parks/", p, "/climate_data/forecasts/adjusted_month_yearly/", sep="")
indir <- paste("C:/Share/LCC-VP/Parks/", p, "/climate_data/forecasts/adjusted_month_yearly/", sep="")

boundary <- paste("c:/Share/LCC-VP/ALCC_PACE_Park_boundaries/", p, "_boundary_buff800m_albers.shp", sep="")

dirs <- list.dirs(indir, full.names=T, recursive=F)
#just leave outdir at "deltas/ref/" - rest will be figured out below.
#outdir <- paste("C:/Share/LCC-VP/Parks/", p, "/climate_data/forecasts/ave_30yr_byYear/deltas/ref/", sep="")
outdir <- paste("C:/Share/LCC-VP/Parks/", p, "/climate_data/forecasts/ave_30yr_byYear/deltas/ref/", sep="")

#subdirs that contain rasters
subdirs <- c("ppt", "tmin", "tmax")

#for PRISM historical only~
hx.dir <- indir

#This loop will get a list of directories for each quantile.
 for (q in quan) {
  print(paste("working on quantile: ", q, sep=""))
  pos.match <- grep(pattern=q, dirs)
  q.dirs <- dirs[pos.match]
  
  #is there a "historical" directory? If not, don't continue! Need Hx stuff to do adjustment.
  hx <- grep("historical|hx", q.dirs)
  
  if (hx == 1) {
    #will assign a new variable (i.e. p50_dirs) to the list of directories for that quantile.
    #q.dirlist <- paste(q, "_dirs", sep="")
    #assign(q.dirlist,q.dirs)
    
    #dir where we'll get 1981-2005
    hx.dir <- q.dirs[grep("historical|hx", q.dirs)]
    #dir(s) where we'll get 2006-2010
    fore.dir <- q.dirs[-grep("historical|hx", q.dirs)]
    
    #Loop over fore.dir
    for (fore in fore.dir){
      
      #loop over subdirs
      for (sub in subdirs) {
        #get list of 30yrs of images
        hx.subname <- paste(hx.dir, "/", sub, sep="")
        hx.sub <- list.files(hx.subname, pattern="*.tif$", full.names=T)
        fore.subname <- paste(fore,"/", sub, sep="")
        fore.sub <- list.files(fore.subname, pattern="*.tif$", full.names=T)
        
        #find positions of 1981 and 2005 images
        hx.sub81 <- grep(pattern="*historical_1981_*", hx.sub)
        hx.sub05 <- grep(pattern="*historical_2005_*", hx.sub)
        hx.files <- hx.sub[hx.sub81:hx.sub05]
        
        #find positions of 2006 and 2010 images
        fore.sub06 <- grep(pattern="*_2006_*", fore.sub)
        fore.sub10 <- grep(pattern="*_2010_*", fore.sub)
        fore.files <- fore.sub[fore.sub06:fore.sub10]
        
        #hx.sub81 <- grep(pattern="*_1981_", hx.sub)
        #hx.sub10 <- grep(pattern="*_2010_", hx.sub)
        #hx.files <- hx.sub[hx.sub81:hx.sub10]
        
        #get rcp from forecast files - this will help later when saving - make sure it's to the right dir.
        #rcp <- unlist(strsplit(basename(fore.files[1]), "_"))[3]
        #print(paste(rcp, sep=""))
        
        #combine into one list
        imgs.30 <- c(hx.files, fore.files)
        #imgs.30 <- hx.files
        #print(imgs.30)
        
        #make sure list is 30 images long - otherwise, ERROR and go to next part of loop.
        #print(paste(fore, "/", sub, sep=""))
        if (length(imgs.30) != 30) {
          stop("ERROR: There are not 30 images in your combined list to average.")
        } else {
          
          
          #There has to be an easier, more elegant way to do this using lists and apply statement with overlay, but I can't figure out
          #the proper indexing to just shove all the 12-band rasters into a list and take 30-yr avg. Also, it's friday and I'm in a rush!
          yrs <- paste("a", c(1:30), sep="")
          print(boundary)
          b <- readShapePoly(boundary)
          
          #args <- vector()                  
          
          #USE RASTER'S OVERLAY - Still need to figure out how to handle all of these stacks efficiently.
          #print(paste("calculating mean reference image for ", p, ": ", sub, " ", rcp, sep=""))
          for (i in c(1:length(imgs.30))) {
            #cropping makes sure the extents are equal.
            #br <- crop(brick(imgs.30[i]))
            #assign(yrs[i], crop(brick(imgs.30[i]),b))
            assign(yrs[i], brick(imgs.30[i]))
            #statement to piece together overlay command - cannot get this to work - GRRRRR! Overlay won't work with text or lists.
            #args[i] <- paste("get(yrs[", i, "])", sep="")          
          } #imgs.30
          
          #Print this to the console so you can copy it into the overlay function. Lame, but it works. I did check this manually for select cells,
          #and it's doing what it's supposed to do.
          #print(paste(yrs, collapse=",", sep=""), quote=F)
          mean.30 <- round(overlay(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30, 
                                   fun=mean, na.rm=T))
          
          #for naming
          if (sub == "ppt") {
            type <- "pr"
          }else if (sub == "tmin") {
            type <- "tmin"
          }else if (sub == "tmax") {
            type="tmax"
          }#end if
          
          #outname <- paste(q, type, rcp, "1981-2010_deltaRef_albers_clip.tif", sep="_")
          outname <- paste(type,"_PRISM_historical_1981-2010_deltaRef_albers_clip.tif", sep="")
          scenario <- unlist(strsplit(fore,"/"))[length(unlist(strsplit(fore,"/")))]
          outras <- paste(outdir, scenario, "/", sub, "/",outname, sep="")
          print(paste("writing", outras))
          writeRaster(mean.30, file=outras,format="GTiff",dataType="INT4S", overwrite=T)
        }#end imgs.30 if else  
      }#end subdir loop
   }#end fore.dir loop
  }#end if
}#end quantile loop to find directories
