library(raster)
#source parameter file for input and output directories
source("C:/Share/LCC-VP/scripts/R/LCC-VP_parameters.R")
source("C:/Share/LCC-VP/scripts/R/LCC-VP_functions.R")
# ###################### ONE-TIME to fix image extent issues ###################### 
# #one-time step to trim NA values from projections so they match the historical stuff and the biovars.
# #variables come from LCC-VP_parameters.R
# 
# #source parameter file for input and output directories
# source("C:/Share/LCC-VP/scripts/R/LCC-VP_parameters.R")
# 
# for (combo in allcombo) {
#   print(combo)
#   
#   #overwrite med and rcp variables that were passed with paramters script
#   #to reflect combo
#   med <- unlist(strsplit(combo, "_"))[1]
#   rcp <- unlist(strsplit(combo, "rcp"))[2]
#   
#   #create temp directory to store trimmed stacks
#   tmp <- paste0(bioclim.indir, med, "_rcp", rcp, "/temp/")
#   dir.create(tmp)
#   
#   #loop over climate vars
#   for (param in bioclim.params){
#     #create temp directory to store trimmed stacks
#     dir.create(paste0(tmp, param, "/"))
#     
#     #set new indir and outdir based on input dir and med and rcp values
#     bioclim.indir2 <- paste0(tmp, param, "/")
#     bioclim.outdir2 <- paste0(bioclim.indir, med, "_rcp", rcp, "/temp/", param, "/")
#     
#     #Get appropriate image list
#     img.list <- list.files(bioclim.indir2, pattern="*.tif$", full.names=T)
#     
#     #loop over imgs and trim
#     for (img in img.list) {
#       print(img)
#       outimg <- paste0(bioclim.outdir2, basename(img))
#       img.stk <- brick(img)
#       img.trim <- trim(img.stk)
#       writeRaster(img.trim,filename=outimg, overwrite=T)
#       
#     }#end img loop
#   }#end param loop
#   
#   #delete tmp dir - only do this if you're sure the trim worked as expected!!
#   #file.remove(tmp, recursive=T)
# }#end combo loop
# 
# 
# #Then copy images from temp dir into the appropriate combo folder within C:\Share\LCC-VP\Parks\[PARK]\climate_data\forecasts\adjusted_month_yearly\
# #################################################################################

#If all combos should be calculated, loop over serially
#would be nice to do on the cluster or in parallel, but I just want to get it
#running for now rather than dealing with moving data all around.
if (comboYN == "yes") {
  for (combo in allcombo) {
    print(combo)
    
    #overwrite med and rcp variables that were passed with paramters script
    #to reflect combo
    med <- unlist(strsplit(combo, "_"))[1]
    rcp <- unlist(strsplit(combo, "rcp"))[2]
    
    #loop over climate vars
    for (param in bioclim.params){
      #set new indir and outdir based on input dir and med and rcp values
      bioclim.indir2 <- paste(bioclim.indir, med, "_rcp", rcp, "/", param, "/", sep="")
      bioclim.outdir2 <- paste(bioclim.outdir, med, "_rcp", rcp, "/", param, "/", sep="")
      
      #HARDCODED - directory with historical PRISM rasters
      hx.dir <- paste0(bioclim.indir,"/","PRISM_historical/", param, "/")
      
      #HARDCODED list indices - in this case, we are calculating only certain years (see notes in evernote)
      #fore.list <- list.files(bioclim.indir2, pattern="*\\.tif$", full.names=T)[1:29]
      hx.list <- list.files(hx.dir, pattern="*\\.tif$", full.names=T)[1:111]
      
      #Combine lists 
      img.list <- hx.list
      
      #calc 30-yr averages
      calc30yr(img.list, bioclim.outdir2)
      
      
      #stk <- stack(img.list)
    }#end param loop 
  }#end combo loop
  
  #if comboYN is no, still looking at forecasts, but just the single one identified in parameters file.
} else if (comboYN == "no") {
  
  #loop over climate vars
  for (param in bioclim.params){
    #set new indir and outdir based on input dir and med and rcp values
    bioclim.indir2 <- paste(bioclim.indir, "/", param, "/", sep="")
    bioclim.outdir2 <- paste(bioclim.outdir,"/", param, "/", sep="")
    
    #HARDCODED - directory with historical PRISM rasters
    #hx.dir <- paste0(bioclim.indir,"/","PRISM_historical/", param, "/")
    
    #HARDCODED list indices - in this case, we are calculating only certain years (see notes in evernote)
    #fore.list <- list.files(bioclim.indir2, pattern="*\\.tif$", full.names=T)[1:29]
    hx.list <- list.files(bioclim.indir2, pattern="*\\.tif$", full.names=T)
    
    #Combine lists 
    #img.list <- c(hx.list, fore.list)
    img.list <- hx.list
    
    #calc 30-yr averages
    calc30yr(img.list, bioclim.outdir2)
    
  }#end param loop 
  
  #If comboYN is NA, then we are working with PRISM data and don't need to pass med and rcp variables  
} else {
  print("ComboYN must be 'yes' or 'no' for this script. Check your parameters.R file")
}#end if




