# User set variables here - this is not set up to use the LCC-VP_parameters.R file just yet.

# set the park - assuming for now that everything is happening at the park level.
park <- "GRSM"

# Root Output directory in which to store text files with var lists. If fstack.comboYN is yes,
# that subdirectory will be added on
text_dir <- "C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/biovarsGddSoilsTWIsrad_20150217/forecast_lists/"

# file root name - will have year appended - leave off file extension 
file_root <- "GRSM_biovarsGddSoilsTWIsrad_20150217_"

# output image directory where projected stacks will be saved (different from text_dir)
out.imgdir <- "C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/biovarsGddSoilsTWIsrad_20150217/forecast_imgs/"

#For forecast data, do you want to do all possible combinations? (note all lowercase - avoiding 
#regular expressions for now) "yes" or "no" 
fstack.comboYN <- "yes"
fstack.allcombo <- c('ea_rcp85', 'ea_rcp45')
#allcombo <- c('p50_rcp45','p50_rcp85','p25_rcp45', 'p25_rcp85','p75_rcp45','p75_rcp85')

#Otherwise, specify scenario here for forecast data.
#For forecast data:
#select median value: ea, p25, p50, or p75.  Enter NA for historical CMIP5 modeled 
#grids (no rcp). 
# med <- "p50"
# #select rcp value: 26, 45, 60, or 85
# rcp <- 85

#List range of years or specific years (e.g. years <- c(2021, 2050, 2086) or years <- c(2021:2086))
years <- c(2021:2086)


########################


#go through all climate scenarios?
if (fstack.comboYN == "yes") {
  for (combo in fstack.allcombo) {
    print(combo)
    
    #overwrite med and rcp variables that were passed with paramters scriptpor
    #to reflect combo
    fstack.med <- unlist(strsplit(combo, "_"))[1]
    fstack.rcp <- unlist(strsplit(combo, "rcp"))[2]
    
    #Set up text files with forecasted predictors - somewhat hardcoded based on our file structure
    k=1
    for (i in years) {
      print(i)
      print(k)
      txtFile <- paste(text_dir,"/",combo,"/", file_root, i, ".csv", sep="")
      #if the file already exists, delete it
      if (file.exists(txtFile)) {
        unlink(txtFile)
      }#end if
      
      #list of files - HARDCODED
      #First list files that have projections/forecasts
      #no biovars in hierarchy h2 - suitability layer takes its place - no longer trying hierarchical - so put biovars back in.
      biovars.dir <- paste0("C:/Share/LCC-VP/Parks/", park, "/climate_data/forecasts/ave_30yr_byYear/deltas/", combo, "/biovars/")
      #Find the year of interest (i)
      bp <- paste0("/",i, "_(.*)\\.tif$")
      biovars <- grep(bp, list.files(biovars.dir, full.names=T), value=T)
      #biovars <- list.files("C:/Share/LCC-VP/Parks/GRSM/analysis/Cove_Forest_all/outputs/hierarchical_test/forecasts/h1/p50_rcp45/", pattern="*.tif$", full.names=T)[k]
      gp.dir <- paste0("C:/Share/LCC-VP/Parks/", park, "/climate_data/forecasts/ave_30yr_byYear/deltas/", combo, "/GDD/")
      gp <- paste0("_",i, "_(.*)\\.tif$")
      GDD <- grep(gp, list.files(gp.dir, full.names=T), value=T)
#     GDD <- list.files("C:/Share/LCC-VP/Parks/GRSM/climate_data/forecasts/ave_30yr_byYear/deltas/p50_rcp45/GDD/", pattern="*.tif$", full.names=T)[k]
      
      #vpd - skipping this as of 2/17/15
#       vpd.dir <- paste0("C:/Share/LCC-VP/Parks/", park, "/climate_data/forecasts/ave_30yr_byYear/deltas/", combo, "/vpd/")
#       vpdp <- paste0("_",i, "\\.tif$")
#       # Spring, summer, fall, winter, annual
#       vpd.mam <- grep(vpdp, list.files(paste0(vpd.dir,"/mam/"), full.names=T), value=T)
#       vpd.jja <- grep(vpdp, list.files(paste0(vpd.dir,"/jja/"), full.names=T), value=T)
#       vpd.son <- grep(vpdp, list.files(paste0(vpd.dir,"/son/"), full.names=T), value=T)
#       vpd.djf <- grep(vpdp, list.files(paste0(vpd.dir,"/djf/"), full.names=T), value=T)
#       vpd.ann <- grep(vpdp, list.files(paste0(vpd.dir,"/ann/"), full.names=T), value=T)
      


      #Now static files
      bd <- paste0("C:/Share/LCC-VP/Parks/", park, "/ssurgo/muid_800_UNID_avg_bd_albers_clip_albers_clip.tif")
      clay <- paste0("C:/Share/LCC-VP/Parks/", park, "/ssurgo/muid_800_UNID_avg_clay_albers_clip_albers_clip.tif")
      ph <- paste0("C:/Share/LCC-VP/Parks/", park, "/ssurgo/muid_800_UNID_avg_ph_albers_clip_albers_clip.tif")
      sand <- paste0("C:/Share/LCC-VP/Parks/", park, "/ssurgo/muid_800_UNID_avg_sand_albers_clip_albers_clip.tif")
      silt <- paste0("C:/Share/LCC-VP/Parks/", park, "/ssurgo/muid_800_UNID_avg_silt_albers_clip_albers_clip.tif")
      awc <- paste0("C:/Share/LCC-VP/Parks/", park, "/ssurgo/muid_800_UNID_awc150_albers_clip_albers_clip.tif")
      bedDepth <- paste0("C:/Share/LCC-VP/Parks/", park, "/ssurgo/muid_800_UNID_bed_depth_albers_clip_albers_clip.tif")
      twi <- paste0("C:/Share/LCC-VP/Parks/", park, "/topography/TWI/TWI_easternUS_800m_clip.tif")
      srad <- paste0("C:/Share/LCC-VP/Parks/", park, "/srad/direct_irradiance_topo_shading_800m_contus_albers_clip_albers_clip.tif")
      
      #write lines to text file *** REWRITE THIS PIECE TO WRITE OUT A 2-COLUMN CSV - IN THE FORMAT EXPECTED BY LCC-VP_stack_predictors.R ***
      #imglist <- c(biovars, GDD,vpd.mam, vpd.jja, vpd.son, vpd.djf, vpd.ann, bd, clay, ph, sand, silt, awc, bedDepth, twi, srad)
      imglist <- c(biovars, GDD, bd, clay, ph, sand, silt, awc, bedDepth, twi, srad)
      outimgs <- rep(paste0(out.imgdir, "/",combo,"/"), length(imglist))
      df <- as.data.frame(cbind(imglist, outimgs),stringsAsFactors = F)
      names(df) <- NULL
      write.csv(df,file=txtFile,row.names=F,quote=F)

      
      k <- k+1
    
    
    } # end years loop
  } # end combo loop
} # end combo if (didn't write the rest yet - just faking it for now by only putting one dir in the list.)
