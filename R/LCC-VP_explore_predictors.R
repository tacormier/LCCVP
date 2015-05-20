library(raster)
#library(rasterVis)
library(ggplot2)
library(Deducer)

#source("C:/Share/LCC-VP/scripts/R/ggcorplot.R")
source("C:/Share/LCC-VP/scripts/R/LCC-VP_functions.R")

#############################  SET VARIABLES HERE TO RUN MANUALLY #########################
#
#input pred stack
pred.file <- "C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/allVars/GRSM_AllVars_BiovarsGddVpdSoilsTwiSrad.tif"
#
#Text file containing names of predictors
pred.names <- "C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/GRSM_var_names_allVars_20141009_BiovarsGddVpdSoilsTwiSrad.txt"
  
#output directory for plots
pred.outdir <- "C:/Share/LCC-VP/Parks/GRSM/analysis/Cove_Forest_all/outputs/allVars_BiovarsGddVpdSoilsTWIsrad_20141009/var_explore"

##########################################################################################

#Read input raster
preds <- as.data.frame(stack(pred.file))

#names of predictors
var.names <- read.table(pred.names, sep="\n")[,1]
names(preds) <- var.names

#create correlation plot with bivariate scatter plots, histograms, and corr coefficients
pairs(preds, lower.panel=panel.smooth, cex = .8, pch = 21, bg="steelblue",
      diag.panel=panel.hist, cex.labels =.8, font.labels=2, upper.panel=panel.cor)

#create correlation/covariance plot
corr.mat <- cor.matrix(preds)
p <- ggcorplot(corr.mat1, data=preds, )

ggcorplot(preds, var_text_size = 15, cor_text_limits = c(10,10))

