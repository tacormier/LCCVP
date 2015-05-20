library(raster)
library(maptools)
library(rgdal)

#source("C:/Share/LCC-VP/scripts/R/PRISM_hx_analysis.R")
#source parameter file
source("C:/Share/LCC-VP/scripts/R/LCC-VP_parameters.R")

#############################  SET VARIABLES HERE TO RUN MANUALLY #########################
# # #
# # # which park or pace?
# park <- "GRSM"
# #
# #Do you want the script to work on a directory (d) or on a list of files (l)?
# dl <- "l"
#
#Enter EITHER the parent directory that contains the subdirectories listed in params OR path/filename 
#of text file containing list of files in column 1 and the output directory for each input in column 2. 
#No header row.
# clim.in <- "C:/Share/LCC-VP/US_PRISM/prism_800m_restricted/"
# 
# #Enter proj4 projection info for images you want to clip - sometimes the projection is undefined, so enter it here.
# proj.def <- "+proj=longlat +ellps=WGS72 +towgs84=0,0,4.5,0,0,0.5540000000000001,0.2263 +no_defs"
# 
# #outdir - this is the parent directory and should contain either the ppt, tmin, and tmax subdirs
# #or the different combo directories (i.e. ea_rcp26, p50_rcp85, etc), which subsequently
# #contain ppt, tmin, tmax, biovars, GDD.
# clim.outdir <- "C:/Share/LCC-VP/Parks/GRSM/climate_data/PRISM_historical/adjusted_month_yearly/temp/"
# 
# #Clip file - must be a raster or shapefile that has the projection you desire.
# #clim.clip <- "C:/Share/LCC-VP/ALCC_PACE_Park_boundaries/SHEN_pace_albers_buff800m.tif"
# 
# #set snap raster - need ref for everything to align. Also used to mask/clip.
# clim.snap <- "C:/Share/LCC-VP/Parks/GRSM/climate_data/current_PRISM/biovars/biovars_1981-2010.tif"
# 
# #For forecast data, do you want to do a number of possible combinations? (note all lowercase - avoiding 
# #regular expressions for now) "yes", "no", or "na" ("na" if PRISM data and not forecast data)
# clip.comboYN <- "na"
# clip.allcombo <- c("ea_rcp26", "p25_rcp26", "p50_rcp26", "p75_rcp26", "ea_rcp45", "p25_rcp45", 'p50_rcp45', 'p75_rcp45', 
#                    'ea_rcp60', 'p25_rcp60', 'p50_rcp60', 'p75_rcp60', 'ea_rcp85', 'p25_rcp85', "p50_rcp85", 'p75_rcp85')
# 
# #set climate parameters to clip (e.g., ppt, tmin, tmax, GDD, biovars)
# params <- c("tmin", "tmax")

#################################################################################
clipProj <- function(img, outimg,proj.def,clim.snapras) {
  
  #set projection of img to proj.def, just in case it's not defined.
  projection(img) <- proj.def
  
  #if x coordinates are lat-long range 0-360 instead of -180 to 180, fix!
  #get xmax from extent object
  xmax <- bbox(extent(img))[1,2]
  if (grepl("proj\\=longlat", projection(img)) & xmax > 180){
    img <- shift(img, -360)
  }
  
  #reproject to Albers using the park-level biovars as a reference (for projection and snapping)
  #first get projection of clim.snapras
  newproj <- projection(clim.snapras)
  #newproj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
  
  #now project and clip to clim.snapras
  #Ok, sometimes you can't go straight from Proj to newproj - no valid transformation from wgs72 - nad83 for example, 
  #so need to do it in 2 steps.First to wgs84, then to nad83
  #inter.proj <- projectRaster(img, "+proj=longlat +datum=WGS84 +no_defs", method='bilinear')
  
  #if-else logic deals with what to do if img is already in same projection as clim.snapras - just clips.
  if (proj.def != newproj) {
    r.proj <- round(projectRaster(img, clim.snapras, method='bilinear'))
    #r.proj <- projectRaster(img, clim.snapras, method='bilinear')
    
    #now mask by the snapras
    r.mask <- mask(r.proj, mask = clim.snapras)
    
    #check to make sure that NA values in masked file are in same locations as the ones in the snapras
    #This will help to avoid and alert us of missing or wrong data.
    m <- as.data.frame(r.mask)
    s <- as.data.frame(clim.snapras)
    
    m.na <- as.vector(is.na(m))
    s.na <- as.vector(is.na(s))
  
    writeRaster(r.mask, outimg, overwrite=T)
    #in case we later want to do more with this in the script than just write it out...
    return(r.mask)
  
  } else {
      #first crop img by clim.snapras to make sure they have the same extent
      cr <- crop(img, clim.snapras)
      #then mask to make sure NAs are the same:
      r.mask <- mask(cr, mask=clim.snapras)
      
      #change outname to remove "_albers_"
      oldname <- outimg
      outimg <- sub(pattern = "_albers", replacement = "",oldname)
      
      writeRaster(r.mask, outimg, overwrite=T)
      #in case we later want to do more with this in the script than just write it out...
      return(r.mask)
      
    
  }#end if-else
    
 

  
  
}#end function



#################################################################################

#Read in snap raster - only want to do this once, so outside of loop.
clim.snapras <- raster(clim.snap)

#go through all climate scenarios?
if (clip.comboYN == "yes") {
  for (combo in allcombo) {
    print(combo)
    
    #overwrite med and rcp variables that were passed with paramters scriptpor
    #to reflect combo
    clip.med <- unlist(strsplit(combo, "_"))[1]
    clip.rcp <- unlist(strsplit(combo, "rcp"))[2]
    
    if (params != "") {
      #loop over climate vars
      for (clim in params) {
        print(clim)
        #set new indir/outdir based on input dir and med and rcp values
        clim.indir2 <- paste(clim.in, clip.med, "_rcp", clip.rcp, "/", clim, "/", sep="")
        clim.outdir2 <- paste(clim.outdir, clip.med, "_rcp", clip.rcp, "/", clim, "/", sep="") 
        
        #list files
        files <- list.files(clim.indir2, pattern="*.tif$", full.names=T)
      
        #project and clip
        for (inImg in files) {
          img <- brick(inImg)
          outimg <- paste0(clim.outdir2, unlist(strsplit(basename(inImg), "\\."))[1], "_albers_clip.tif")
          
          #clip/mask
          img.clipped <- clipProj(img,outimg,proj.def,clim.snapras)
              
        }#end inImg loop
      }#end clim for loop
      
    #***This scenario has not been tested yet!***
    }else if (params == ""){
      #list files
      #set new indir/outdir based on input dir and med and rcp values
      clim.indir2 <- paste(clim.in, clip.med, "_rcp", clip.rcp, "/", sep="")
      clim.outdir2 <- paste(clim.outdir, clip.med, "_rcp", clip.rcp, "/", sep="") 
      
      files <- list.files(clim.indir2, pattern="*.tif$", full.names=T)
      
      #project and clip
      for (inImg in files) {
        img <- brick(inImg)
        outimg <- paste0(clim.outdir2, unlist(strsplit(basename(inImg), "\\."))[1], "_albers_clip.tif")
        
        #clip/mask
        img.clipped <- clipProj(img,outimg,proj.def,clim.snapras)
        
      }#end inImg loop
    }#end params !="" if
  }#end combo for loop
  
  #If running on PRISM or one quantile/rcp scenario
} else if (clip.comboYN == "na" || clip.comboYN == "no") {
    #loop over params (if there are any) to submit current indir/outdir
    if (params != "") {
      for (clim in params) {
        print(clim)
        
        #set new indir/outdir
        clim.indir2 <- paste(clim.in, clim, "/", sep="")
        clim.outdir2 <- paste(clim.outdir, clim, "/", sep="") 
        
        #list files
        files <- list.files(clim.indir2, pattern="*.tif$", full.names=T)
              
        for (inImg in files) {
          img <- brick(inImg)
          outimg <- paste0(clim.outdir2, unlist(strsplit(basename(inImg), "\\."))[1], "_albers_clip.tif")
          
          #clip/mask
          img.clipped <- clipProj(img,outimg,proj.def,clim.snapras)
        }# end inImg loop
      }#end params loop
     } else if (params == "") {
         #list files
         files <- list.files(clim.in, pattern="*.tif$", full.names=T)
         
         for (inImg in files) {
           img <- brick(inImg)
           outimg <- paste0(clim.outdir, unlist(strsplit(basename(inImg), "\\."))[1], "_albers_clip.tif")
           
           #clip/mask
           img.clipped <- clipProj(img,outimg,proj.def,clim.snapras)
         }# end inImg loop
    }#end params != "" if
} else if (clip.comboYN=="list" && dl == "l"){
    files <- read.csv(clim.in, header=FALSE, as.is=T)
    names(files) <- c("infile", "outdir")
    
    for (i in c(1:length(files[,1]))) {
      img <- brick(files$infile[i])
      clim.outdir2 <- files$outdir[i]
      outimg <- paste0(clim.outdir2, unlist(strsplit(basename(files$infile[i]), "\\."))[1], "_albers_clip.tif")
      
      #clip/mask
      img.clipped <- clipProj(img,outimg,proj.def,clim.snapras)
    }# end inImg loop
    

}#end combo if/else