library(raster)
library(reshape2)
library(ggplot2)
library(rasterVis)
library(scales)
library(grid)
library(gridExtra)

#FUNCTIONS#
source("C:/Share/LCC-VP/scripts/R/LCC-VP_functions.R")

############ Variables ###############

# Enter the path to the analysis dir, including the subdirectory for the system you modeled
a.dir <- "C:/Share/LCC-VP/Parks/GRSM/analysis/Cove_Forest_GAP_GRSM/"

# Enter the name of the model run - these are the subdirectory names found inside the a.dir/inputs or a.dir/outputs.
# Enter just the name - not the whole path.
mr.subdir <- "biovarsGddSoilsTWIsrad_20150217_rngwideModel"

# Enter the name of the ecological system as you want it dispalyed on figures etc.
es.name <- "Cove Forest - Rangewide Model"

# Enter the mask value. Suitabilities less than or equal to this value in the observed raster will be set to 0 in all projections
maskval <- 0

# For which models do you want figures? This list must have the exact names as the subdirectories in the 
# a.dir/outputs/mr.subdir/forecasts directory
models <- c("KSModel", "TempModel")

# Model System - for graph titles
mod.sys <- "Cove Forest"

# What is the model extent? This text will be used in graph title
mod.extent <- "Rangewide"

######################################
# Figures will go into the model output director in their own subdir - if this dir doesn't exist, script creates it.
outdir <- paste0(a.dir, "outputs/", mr.subdir, "/figures/")
dir.create(outdir, showWarnings=FALSE)


# First, load the observed raster - 
es <- unlist(strsplit(a.dir, "/"))
es <- tail(es,1)
obs.dir <- paste0(a.dir, "inputs/", mr.subdir, "/")
obs.file <- paste0(obs.dir, "COVER_",es, "_inPRISM_800m_masked.tif")
#obs.file <- "C:/Share/LCC-VP/Parks/GRSM/analysis/spruce_fir_GAP_GRSM/inputs/biovarsGddSoilsTWIsrad_20150217_rngwideModel/COVER_spruce_fir_GAP_GRSM_inPRISM_800m_masked.tif"
obs <- raster(obs.file)
#set nodata to zero (TEMP FIX DUE TO PROBLEM WITH MISSING COVE FOREST PIXELS WHERE SUIT = 0 - only an issue for mapping)
#obs[is.na(obs)] <- 0
# Take one forecasted raster and use it to mask the obs - in case the observed is a rangewide raster :)
f.ras <- raster(list.files(paste0(a.dir, "outputs/", mr.subdir, "/forecasts/ea_rcp85/KSModel/"),pattern = "*.tif", full.names=T)[1])
obs.crop <- crop(obs, f.ras)
obs.mask <- mask(obs.crop, f.ras)

# Create mask where obs.mask == 0 (still playing with threshold..could be 20%? 10%...subjective.)
mask <- obs.mask
mask[mask <= maskval] <- 0

obs.mask[mask == 0] <- 0
o.mean <- mean(obs.mask[mask != 0])
o.sd <- sd(obs.mask[mask != 0])
               
# List projections
for (model in models) {
  proj.dir <- paste0(a.dir, "outputs/", mr.subdir, "/forecasts/ea_rcp85/", model, "/")
  proj.files <- list.files(proj.dir, pattern=".tif$", full.names = T)
  
  # Make a "mask" directory. NM - don't need to write these out as sep files just now. Maybe later.
  
  # loop over forecasts and mask
  fore.masked.ras <- list(obs.mask)
  fore.df <- as.data.frame(as.vector(obs.mask[mask != 0]))
  names(fore.df)[1] <- "1996"
  fore.mean <- o.mean
  fore.sd <- o.sd
  yrs <- "1996"
  
  for (fore in proj.files) {
    # only writing this as 2 variables to help visualize things while I'm working on the code.
    print(paste0("masking ", fore))
    f <- f.mask <- raster(fore)
    # mask f by mask
    f.mask[mask == 0] <- 0
    
    # Now take the mean and sd suitability in areas where currently suitable (mask != 0)
    # summarized data - also write out raw data (with no zeros) to fore.df
    f.mean <- mean(f.mask[f.mask !=0])
    f.sd <- sd(f.mask[f.mask != 0])
    
    # Add the values to their respective lists
    fore.masked.ras <- c(fore.masked.ras, f.mask)
    fore.df <- cbind(fore.df, as.data.frame(f.mask[mask != 0]))
    fore.mean <- c(fore.mean, f.mean)
    fore.sd <- c(fore.sd, f.sd)
    
    # Figure out year
    yr.pos <- regexpr("[0-9]{4}\\.", basename(fore))
    yr <- sub(pattern="\\.",replacement = "",regmatches(basename(fore),yr.pos))
    yrs <- c(yrs, yr)
    
    names(fore.df)[dim(fore.df)[2]] <- yr
    
  }#end forecast loop

  # Plot smoothed line with gray area showing standard error of the smoothed line fit. This is fitting the actual raw data points
  #ggplot(f.data.melt, (aes(variable,value))) + geom_smooth(aes(group=1),color="black") + geom_point()

  # This plot is plotting a smoothed line of the  mean suitabitily for each year. Shaded area is the standard deviation.
  x <- as.data.frame(cbind(yrs, fore.mean, fore.sd))
  x$fore.mean <- as.numeric(as.character(x$fore.mean))
  x$fore.sd <- as.numeric(as.character(x$fore.sd))
  x$yrs <- as.Date(x$yrs, "%Y")  

  sd1 <- fore.mean + fore.sd
  sdm1 <- fore.mean -fore.sd
  sdm1[sdm1 <0] <- 0
  eb <- aes(ymax = sd1, ymin = sdm1)  
  title <- paste0(mod.extent, " - ", model, "\n", mod.sys)
  
  suit.ts <- ggplot(x, aes(yrs, fore.mean)) + ylim(0,50) + ylab("Mean Suitability") + xlab("Year") + theme_bw() + 
      ggtitle(title) + geom_ribbon(eb, alpha=0.25) + geom_smooth(color="black", se=F) 
  print(suit.ts)

  
  # Maps of 1996 (observed current), 2025, 2050
  #color set up - to be uniform across maps
#  at <- seq(0,100,length=20)

#   # stack timesteps - skip maps for now.
#   # This needs to be B&W - figure out how to set par.settings=GrTheme and still keep my other par.settings
#   tstack <- stack(fore.masked.ras[[1]], fore.masked.ras[[6]], fore.masked.ras[[31]])
#   names(tstack) <- c("observed", "forecast 2025", "forecast 2050")
#   maps <- levelplot(tstack, margin=F, at=at, scales=list(draw=F), 
#                     colorkey=list(at=at, space="right", cex=0.85, height=.88, width=1.4), layout=c(3,1), col.regions = gray(0:100/100),
#                     ylab.right = "% suitability", par.settings = list(layout.widths = list(axis.key.padding = 0,ylab.right = 2)))

  # Use grid arrange function from gridExtra to layout all maps into one figure 
  outfig <- paste0(outdir, "forecast_trajectory_", es, "_", model, "_", mod.extent,".pdf")
  pdf(file=outfig,family="Times", width=10, height=7)
  print(suit.ts)
  dev.off()

} #End model loop