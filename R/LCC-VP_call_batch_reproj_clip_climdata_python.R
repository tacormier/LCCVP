#source parameter file for input and output directories
source("C:/Share/LCC-VP/scripts/R/LCC-VP_parameters.R")
#Reads in variables: park, clim.indir, clim.outdir, clim.clip

# redirect output to file (for selected variables) - FIX: sinking to log file
#is not appending - just overwriting. Not sure why.
#For now, decided to just write unique log file each time this script is run. 
#Be careful not to let logs pile up. 
date <- gsub(":", "", gsub(" ", "_", date()))
log.file <- file(paste(clim.outdir,"clim_reproj_clip_", date, "_log.txt",sep=""))
sink(log.file)
sink(log.file, type="message")
print("*********************************************************************")
print("******** LCC-VP_call_batch_reproj_clip_climdata_python.R Log ********")
print("*********************************************************************")

print(date)
print(park)
print(paste("today we are working on a ", dl, ": d for dir, l for list", sep=""))
print(paste("clim.in = ", clim.in, sep=""))
print(paste("params = ", paste(params,collapse=", "), sep=""))
print(paste("clim.outdir = ", clim.outdir, sep=""))
print(paste("clim.reproj = ", clim.reproj, sep=""))
print(paste("clim.clip = ", clim.clip, sep=""))
print(paste("clim.snap = ", clim.snap, sep=""))
print(paste("clim.fail = ", clim.fail, sep=""))

#If fail file doesn't exist, create it
if (!file.exists(clim.fail)) {
  file.create(clim.fail)
}

#If all combos should be calculated, loop over serially
#would be nice to do on the cluster or in parallel, but I just want to get it
#running for now rather than dealing with moving data all around.
if (clip.comboYN == "yes" && dl == "d") {
  for (combo in clip.allcombo) {
    print(combo)
    
    #overwrite med and rcp variables that were passed with paramters script
    #to reflect combo
    clip.med <- unlist(strsplit(combo, "_"))[1]
    clip.rcp <- unlist(strsplit(combo, "rcp"))[2]
    
    #loop over params as well
    for (clim in params) {
      print(clim)
      #set new indir/outdir based on input dir and med and rcp values
      clim.indir2 <- paste(clim.in, clip.med, "_rcp", clip.rcp, "/", clim, "/", sep="")
      clim.outdir2 <- paste(clim.outdir, clip.med, "_rcp", clip.rcp, "/", clim, "/", sep="") 
      
      #list files
      files <- list.files(clim.indir2, pattern="*.tif$", full.names=T)
      for (img in files) {
      #System call to python script that does the work!
      x <- system(paste("c:/Python27/ArcGISx6410.2/python C:/Share/LCC-VP/scripts/python/batch_reproject_clip_climdata.py", "-f", 
                        "-p", park, "-i", img, "-o", clim.outdir2, "-r", clim.reproj, "-cf", clim.clip, "-s", clim.snap, "-e", clim.fail, sep=" "))
      
      if (x != 0) stop("python script was not called successfully.")
      }#end img loop
    }#end clim loop    
  }#end combo loop
  
  #If running on a list of files
} else if (clip.comboYN == "list" && dl == "l") {
  #System call to python script that does the work!
  #loop over text file and submit to python
  txt <- as.vector(read.table(clim.in, header=FALSE, as.is=T)[,1])
  for (f in c(1:length(txt))) {
    #somewhat hard-coded. Assumes that outdir is either either in PACE or Parks directories somewhere.
    #Also assumes input file name is in this type of format: p25_tasmin_rcp45_2013_adj.tif
    out.split <- (unlist(strsplit(clim.outdir, "/")))
    in.split <- unlist(strsplit(txt[f], "/"))
    in.split.name <- in.split[length(in.split)]
    #park <- in.split[5]
    clip.med <- unlist(strsplit(in.split.name, "_"))[1]
    clip.rcp <- unlist(strsplit(in.split.name, "_"))[3]
    clim.type <- unlist(strsplit(in.split.name, "_"))[2]
    if (clim.type == "pr") {
      clim.var <- "ppt"
    } else if (clim.type == "tasmin") {
      clim.var <- "tmin"
    } else if (clim.type == "tasmax") {
      clim.var <- "tmax"
    } else {
      stop("clim type must be ppt, tmin, or tmax. Check wrapper to make sure input file structure is what is expected.")
    } #end clim.type if/else
    
    #set up outdir
    #fix to accomodate hx naming - see 2nd clim.outdir2.
    clim.outdir2 <- paste(paste(out.split[1:4], collapse="/"), "/", park, "/", paste(out.split[6:length(out.split)], collapse="/"),
                          "/", clip.med, "_", clip.rcp, "/", clim.var, "/", sep="")
    #clim.outdir2 <- paste(paste(out.split[1:4], collapse="/"), "/", park, "/", paste(out.split[6:length(out.split)], collapse="/"),
    #                "/",clim.var, "/", sep="")
                          
    
    x <- system(paste("c:/Python27/ArcGISx6410.2/python C:/Share/LCC-VP/scripts/python/batch_reproject_clip_climdata.py", "-f", 
                      "-p", park, "-i", txt[f], "-o", clim.outdir2, "-r", clim.reproj, "-cf", clim.clip, "-s", clim.snap, "-e", clim.fail, sep=" "))
    if (x != 0) stop("python script was not called successfully.")
  
    }#end file for loop
    
  #Finally, if running on PRISM or one quantile/rcp scenario
} else if (clip.comboYN == "na" || clip.comboYN == "no" && dl == "d") {
    #loop over params to submit current indir/outdir
    for (clim in params) {
      print(clim)
      #set new indir/outdir based on input dir and med and rcp values
      clim.indir2 <- paste(clim.in, clim, "/", sep="")
      clim.outdir2 <- paste(clim.outdir, clim, "/", sep="") 
      
      #System call to python script that does the work!
      x <- system(paste("c:/Python27/ArcGISx6410.2/python C:/Share/LCC-VP/scripts/python/batch_reproject_clip_climdata.py", "-d", 
                        "-p", park, "-i", clim.indir2, "-o", clim.outdir2, "-r", clim.reproj, "-cf", clim.clip, "-s", clim.snap, "-e", clim.fail, sep=" "))
      if (x != 0) stop("python script was not called successfully.")
      }#end clim loop
      
} else {
    stop(paste("clip.comboYN must be 'yes', 'no', 'na', or 'list'. If 'list', dl must be 'l'. Otherwise, dl must be 'd.' ",
        "You entered: clip.comboYN = ", clip.comboYN, " and dl = ", dl, sep=""))
}#end if 



#Restore message to console
sink()
sink(type="message")
