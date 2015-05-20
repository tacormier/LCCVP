library(raster)
library(ggplot2)

###################################################################################################
# user inputs

# raster of observed cover - current
obs.file <- "/Volumes/Share/LCC-VP/Parks/GRSM/analysis/Cove_Forest_all/inputs/hierarchical_test/COVER_Cove_Forest_all_inPRISM_800m.tif" 

# rf modeled hierarchical level 1, current
h1curr.file <- "/Volumes/Share/LCC-VP/Parks/GRSM/analysis/Cove_Forest_all/outputs/hierarchical_test/h1/Cove_Forest_all_current_suitability_rf.tif"

# rf modeled hierarchical level 2, current
h2curr.file <- "/Volumes/Share/LCC-VP/Parks/GRSM/analysis/Cove_Forest_all/outputs/hierarchical_test/Cove_Forest_all_current_suitability_rf.tif"

# rf modeled park-only, current - no hierarchy
parkcurr.file <- "/Volumes/Share/LCC-VP/Parks/GRSM/analysis/Cove_Forest_all/outputs/allVars/Cove_Forest_all_current_suitability_rf.tif"

# level 1 hierarchical forecast raster (single year for now)
h1proj.file <- "/Volumes/Share/LCC-VP/Parks/GRSM/analysis/Cove_Forest_all/outputs/hierarchical_test/forecasts/h1/p50_rcp85/p50_rcp85_2050_yearRange_2035-2064_delta_biovars_Cove_Forest_all.tif"

# level 2 hierarchical forecast raster (single year for now)
h2proj.file <- "/Volumes/Share/LCC-VP/Parks/GRSM/analysis/Cove_Forest_all/outputs/hierarchical_test/forecasts/h2/p50_rcp85/GRSM_var_files_ALLvars_forecast_h2_2050_Cove_Forest_all.tif"

# parkwide forecast - the one that doesn't use the whole range for the first level of modeling
parkproj.file <- "/Volumes/Share/LCC-VP/Parks/GRSM/analysis/Cove_Forest_all/outputs/allVars/forecasts/Cove_Forest_all_forecast_2050.tif"

#output directory for figures
outfigdir <- "/Volumes/Share/LCC-VP/Parks/GRSM/figures/"

###################################################################################################

# read in rasters
obs <- as.vector(raster(obs.file))

#current model suitability
h1curr <- as.vector(raster(h1curr.file))
h2curr <- as.vector(raster(h2curr.file))
parkcurr <- as.vector(raster(parkcurr.file))

#forecasted model suitability
h1proj <- as.vector(raster(h1proj.file))
h2proj <- as.vector(raster(h2proj.file))
parkproj <- as.vector(raster(parkproj.file))

# Use obs raster as a mask for NAs and zeros - slightly confusing because the nodata value for some of these IS reading as 0,
# which messes with masking for 0's. First, remove NAs based on observed. Then mask zeros.
h1curr <- h1curr[!is.na(obs)]
h2curr <- h2curr[!is.na(obs)]
parkcurr <- parkcurr[!is.na(obs)]

h1proj <- h1proj[!is.na(obs)]
h2proj <- h2proj[!is.na(obs)]
parkproj <- parkproj[!is.na(obs)]

#now remove NAs from obs before masking by 0
obs <- obs[!is.na(obs)]

#now mask my 0s
h1curr[obs==0] <- 0
h2curr[obs==0] <- 0
parkcurr[obs==0] <- 0

h1proj[obs==0] <- 0
h2proj[obs==0] <- 0
parkproj[obs==0] <- 0


# add a column that describes the scenario
#current obs and current models
obs.mod <- as.data.frame(cbind(obs, "observed current"), stringsAsFactors = F)
h1curr.mod <- as.data.frame(cbind(h1curr, "rangewide current"), stringsAsFactors = F)
#h2curr.mod <- as.data.frame(cbind(h2curr, "rangewide H2 - current"), stringsAsFactors = F)
parkcurr.mod <- as.data.frame(cbind(parkcurr, "park only current"), stringsAsFactors = F)

#projections
h1proj.mod <- as.data.frame(cbind(h1proj, "rangewide 2050"), stringsAsFactors = F)
#h2proj.mod <- as.data.frame(cbind(h2proj, "rangewide H2 - 2050"), stringsAsFactors = F)
parkproj.mod <- as.data.frame(cbind(parkproj, "park only 2050"), stringsAsFactors = F)

# assign some names for consistency before rbinding all of them!
names(h1curr.mod) <- names(h2curr.mod) <- names(parkcurr.mod) <- names(obs.mod) <- names(h1proj.mod) <- names(h2proj.mod) <- names(parkproj.mod) <- c("suitability", "model")

# combine into a single data frame - current
allcurr <- rbind(obs.mod, h1curr.mod, parkcurr.mod)
allcurr$suitability <- as.numeric(allcurr$suitability)
#

# combine into a single data frame - forecast
allproj <- rbind(obs.mod, h1proj.mod, parkproj.mod)
allproj$suitability <- as.numeric(allproj$suitability)
#allproj$model <- as.factor(allproj$model)

#find means per model category
allcurr.means <- as.data.frame(tapply(allcurr$suitability, allcurr$model, FUN = mean))
allproj.means <- as.data.frame(tapply(allproj$suitability, allproj$model, FUN = mean))
names(allcurr.means) <- names(allproj.means) <- "Mean Suitability"

#a little trick to add means to graph in the easiest way bc in a time crunch. Can later add it separately.
allcurr$model[allcurr$model == "observed current"] <- paste0("observed current - ", round(allcurr.means[1,],2), "%")
allcurr$model[allcurr$model == "rangewide current"] <- paste0("rangewide current - ", round(allcurr.means[3,],2), "%")
allcurr$model[allcurr$model == "park only current"] <- paste0("park only current - ", round(allcurr.means[2,],2), "%")
allcurr$model <- as.factor(allcurr$model)

allproj$model[allproj$model == "observed current"] <- paste0("observed current - ", round(allproj.means[1,],2), "%")
allproj$model[allproj$model == "rangewide 2050"] <- paste0("rangewide 2050 - ", round(allproj.means[3,],2), "%")
allproj$model[allproj$model == "park only 2050"] <- paste0("park only 2050 - ", round(allproj.means[2,],2), "%")
allproj$model <- as.factor(allproj$model)



###################################################################################################
# plotting
# use ggplot to plot histograms of all current obs and current models
outcurr <- paste0(outfigdir, "NASCB2014_cove_forests_compare_current_models.pdf")
pdf(file=outcurr, width=10, height=8)
p <- ggplot(allcurr, aes(x=suitability, fill=model)) + geom_density(alpha=.3)
p + ggtitle("Observed Suitability vs. Current Modeled Suitability")
#p + annotation_custom(grob=textGrob(label=))
dev.off()

# use ggplot to plot histograms of all current obs and projected (e.g., 2050) models
outproj <- paste0(outfigdir, "NASCB2014_cove_forests_compare_projected_models_noh2.pdf")
pdf(file=outproj, width=10, height=8)
p <- ggplot(allproj, aes(x=suitability, fill=model)) + geom_density(alpha=.3)
p + ggtitle("Observed Suitability vs. 2050 Modeled Suitability")
dev.off()

