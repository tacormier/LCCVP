library(raster)
library(reshape2)
library(ggplot2)
library(rasterVis)
library(scales)
library(grid)
library(gridExtra)
library(randomForest)

#FUNCTIONS#
source("C:/Share/LCC-VP/scripts/R/LCC-VP_functions.R")

############ Variables ###############
###CHECK line 85 - set to graph only to 2055 - hardcoded.
# Enter the path to the analysis dir
a.dir <- "C:/Share/LCC-VP/Parks/GRSM/analysis/"

# Enter the system name(s) (must be the same as the directory name)
s.name <- c("Cove_Forest_all", "spruce_fir", "Hemlock", "northern_hardwood", "Oak_Hickory", "Yellow_Pine")
#s.name <- c("spruce_fir","northern_hardwood")

# Enter the name(s) of the model run(s) - these are the subdirectory names found inside the a.dir/inputs or a.dir/outputs.
# Enter just the name - not the whole path. Must be in the same order as s.names
mr.subdir <- c("biovarsGddSoilsTWIsrad_20150304_parkModel","biovarsGddSoilsTWIsrad_20150304_parkModel", "biovarsGddSoilsTWIsrad_20150310_parkModel", "biovarsGddSoilsTWIsrad_20150310_parkModel","biovarsGddSoilsTWIsrad_20150310_parkModel","biovarsGddSoilsTWIsrad_20150310_parkModel")
#mr.subdir <- c("biovarsGddSoilsTWIsrad_20150304_parkModel", "biovarsGddSoilsTWIsrad_20150310_parkModel")

# Enter the name(s) of the system as you want it dispalyed on figures etc. Must be in
# same order as s.names
es.name <- c("Cove Forest", "Spruce-Fir", "Hemlock", "Northern Hardwood", "Oak-Hickory", "Yellow Pine")
#es.name <- c("Spruce-Fir", "Northern Hardwood")

# Enter the mask value. Suitabilities less than or equal to this value in the observed raster will be set to 0 in all projections
maskval <- 0

# Figure output directory
outdir <- "C:/Share/LCC-VP/Parks/GRSM/analysis/figures/"

# For which models do you want figures? This list must have the exact names as the subdirectories in the 
# a.dir/outputs/mr.subdir/forecasts directory
#models <- c("KSModel", "TempModel")

# Model System - for graph titles
#mod.sys <- "Cove Forest"

# What is the model extent? This text will be used in graph title
#mod.extent <- "Rangewide"

######################################
# Figures will go into the model output director in their own subdir - if this dir doesn't exist, script creates it.
#outdir <- paste0(a.dir, "outputs/", mr.subdir, "/figures/")
#dir.create(outdir, showWarnings=FALSE)

# Set up a list to hold results for plotting
#sys.list <- list()

# This df is for plotting a smoothed line of the  mean suitabitily for each year. Shaded area will be the standard deviation.
# includes a dummy row - I can rewrite this more elegantly later ;)
x <- as.data.frame(matrix(data = 0,nrow = 1, ncol=4,dimnames = list(NULL, c("yrs","fore.mean","fore.sd", "es"))), stringsAsFactors=F)
x$yrs <- as.character(x$yrs)
x$es <- as.character(x$es)

# First, need to create the datasets to plot for each system
for (i in c(1:length(s.name))) { 
  es <- s.name[i]
  obs.dir <- paste0(a.dir,s.name[i], "/inputs/", mr.subdir[i], "/")
  obs.file <- paste0(obs.dir, "COVER_",es, "_inPRISM_800m_masked.tif")
  #obs.file <- "C:/Share/LCC-VP/Parks/GRSM/analysis/spruce_fir_GAP_GRSM/inputs/biovarsGddSoilsTWIsrad_20150217_rngwideModel/COVER_spruce_fir_GAP_GRSM_inPRISM_800m_masked.tif"
  obs <- raster(obs.file)
  #set nodata to zero (TEMP FIX DUE TO PROBLEM WITH MISSING COVE FOREST PIXELS WHERE SUIT = 0 - only an issue for mapping)
  #obs[is.na(obs)] <- 0
  # Take one forecasted raster and use it to mask the obs - in case the observed is a rangewide raster :)
  f.ras <- raster(list.files(paste0(a.dir,es, "/outputs/", mr.subdir[i], "/forecasts/ea_rcp85/KSModel/"),pattern = "*.tif", full.names=T)[1])
  obs.crop <- crop(obs, f.ras)
  obs.mask <- mask(obs.crop, f.ras)
  
  # Create mask where obs.mask < 1 (still playing with threshold..could be 20%? 10%...subjective.)
  mask <- obs.mask
  mask[mask < maskval] <- 0
  
  obs.mask[mask == 0] <- 0
  o.mean <- mean(obs.mask[mask != 0])
  o.sd <- sd(obs.mask[mask != 0])
                                               
  # List projections - for now hardcoded to use results from variable selection model
  proj.dir <- paste0(a.dir, es,"/outputs/", mr.subdir[i], "/forecasts/ea_rcp85/TempModel/")
  proj.files <- list.files(proj.dir, pattern=".tif$", full.names = T)[1:35]
  
  # Make a "mask" directory. NM - don't need to write these out as sep files just now. Maybe later.
  
  # loop over forecasts and mask
  fore.masked.ras <- list(obs.mask)
  fore.df <- as.data.frame(as.vector(obs.mask[mask != 0]))
  names(fore.df)[1] <- "1996"
  fore.mean <- o.mean
  fore.sd <- o.sd
  yrs <- "1996"
  
  x <- rbind(x, c(yrs, fore.mean, fore.sd, es.name[i]))

  
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

    # Figure out year
    yr.pos <- regexpr("[0-9]{4}\\.", basename(fore))
    yr <- sub(pattern="\\.",replacement = "",regmatches(basename(fore),yr.pos))
    names(fore.df)[dim(fore.df)[2]] <- yr
    
    x <- rbind(x, c(yr,f.mean,f.sd, es.name[i]))
    
    
  }#end forecast loop
  
  # Some formatting and additional calcs
  x$fore.mean <- as.numeric(x$fore.mean)
  x$fore.sd <- as.numeric(x$fore.sd)
  x$sd1 <- x$fore.mean + x$fore.sd
  x$sdm1 <- x$fore.mean - x$fore.sd
  x$sdm1[x$sdm1 <0] <- 0
  #x$es <- es.name[i]
  #sys.list[[i]] <- x
  #names(sys.list)[i] <- es.name[i]
  
}# end system loop and set up of data for plotting

# remove dummy row
x <- x[-1,]

# Format date column
x$yrs <- as.Date(x$yrs, "%Y")

names(x)[names(x) == "es"] <- "System"
################## PLOTTING #################

# Plot smoothed line with gray area showing standard error of the smoothed line fit. This is fitting the actual raw data points
#ggplot(f.data.melt, (aes(variable,value))) + geom_smooth(aes(group=1),color="black") + geom_point()

# Some data formatting
#x.sub.mean <- x[,c(1,2,4)]
# x.m.melt <- melt(x, id.vars=c("yrs", "System"), measure.vars="fore.mean")
# x.upper.melt <- melt(x, id.vars=c("yrs", "System"), measure.vars="sd1")
# x.lower.melt <- melt(x, id.vars=c("yrs", "System"), measure.vars="sdm1")
# sd.rib <- aes(ymax=x.upper.melt, x.lower.melt, group="System")
# p <- ggplot(data=x.m.melt, aes(x=yrs, y=value, linetype=System)) + ylim(0,40) + ylab("Mean Suitability (%)") + xlab("Year") + theme_bw() +
#   geom_line() + geom_ribbon(sd.rib, alpha=0.5)
# p

#line type is set manually here - assuming 6 systems.
lt <- c("solid", "twodash", "longdash", "dotted", "12345678", "F1")
eb <- aes(ymax = sd1, ymin = sdm1, group=System)  
p <- ggplot(x, aes(yrs, fore.mean)) + ylim(0,40) + ylab("Mean Suitability") + xlab("Year") + theme_bw() + 
 geom_line(aes(group=System, linetype=System), lwd=0.5) + scale_linetype_manual(values=lt) + theme(legend.key.width=unit(2.5, "line"),
                                                                                                   legend.text = element_text(size = rel(0.65)),
                                                                                                   legend.title = element_text(size = rel(0.8))) 
                                                                                                   
p

#to add ribbon - but it looks to confusing
#+ geom_ribbon(eb, alpha=0.15) + geom_line(aes(yrs,sd1, linetype=System), alpha=0.5) + 
#geom_line(aes(yrs,sdm1, linetype=System),alpha=0.5)


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

# Write pdf
out.traj <- paste0(outdir, "forecast_trajectory_", paste(s.name, collapse="-"),".pdf")
pdf(file=out.traj,family="Times", width=6.5, height=3.25)
print(p)
dev.off()

########## Some more Figures - VarImpPlot and Partial Correlations ###############
for (j in c(1:length(s.name))){
  mod.dir <- paste0(a.dir,s.name[j], "/outputs/", mr.subdir[j], "/")
  # For now, using model with variable selection, which ends with "t.randfor.tif"
  mod.file <- list.files(mod.dir, pattern="*TempModel.RData", full.names=T)

  mod <- load(mod.file)
  mod <- get(mod[1])
  
  r2 <- round(1 - sum((mod$y-mod$predicted)^2)/sum((mod$y-mean(mod$y))^2), digits=2)
  rmse <- round(sqrt(sum((mod$y-mod$predicted)^2)/length(mod$y)), digits=2)
  #t.nvar <- length(t.varsel03$SELVARS)
  
  
  #Variable importance plot set up
  
  leg.txt2 <- c(as.expression(bquote(r^2 == ~.(sprintf("%.2f", r2)))), 
                as.expression(bquote(rmse == .(sprintf("%.2f", rmse)))))
  outimp <- paste0(outdir, "varImp_", s.name[j],".pdf")
  pdf(file=outimp,family="Times", width=7, height=7)
  varImpPlot(mod, pch=16, type=1,main=paste("Variable Importance \n", es.name[j], sep=' '))
  legend("bottomright", legend=leg.txt2, bty='n', cex=0.85)
  dev.off()
  
  ########## Partial Correlations ##############
  #pred stack
  pred.dir <- paste0(a.dir,s.name[j], "/inputs/", mr.subdir[j], "/")
  pred.file <- list.files(pred.dir, pattern="predVars", full.names=T)
  pred.file <- pred.file[grep(".tif$", pred.file)]
  pred.stk <- as.data.frame(brick(pred.file))
  v.names <- read.table("C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/GRSM_var_names_Vars_20150217_BiovarsGddSoilsTwiSrad_SHORT.txt", sep="\n")[,1]
  names(pred.stk) <- v.names
  
  #
  impt.vals <- sort(importance(mod, type=1)[,1], dec=T)[1:5]
  impt.names <- names(impt.vals)
  
  prednames <- names(pred.stk)
  pp.imp <-list()
  pp.preds <- list()
  
  #### Loop through variables and calculate partial dependency for each
  outparcor <- paste0(outdir, "PartialCorr_TOP5_", s.name[j],".pdf")
  outtest <- paste0(outdir, "PartialCorr_TOP5_indivPlots_", s.name[j],".pdf")
    
  pdf(file=outtest, width=6, height=6)
  for (var in (impt.names)){
    #if(!is.numeric(pred.stk[, j])){ next }
    #if(length(unique(pred.stk[, j])) <= 10){ next }
    
    # try removing pixels from pred stack where response image is 0 - since we are only reporting on where the community is 
    # currently present
    pred.stk.na <- na.omit(pred.stk)
    pred.mask <- pred.stk.na[mod$y > 0,]
    pp <- eval(bquote(partialPlot(mod, pred.mask, .(var), plot=F)))
    
    # We want to plot individual partial correlations as well as the top 5 together -     
    pi <- ggplot(data=as.data.frame(pp), aes(x=x/100, y=y)) + geom_line(col="#000099") + xlab(var) + ylab("mean suitability")
    print(pi)    
      
    
    #This is for plotting together with normalized/standardized x axis
    pp.imp[[length(pp.imp) + 1]] <- pp
    names(pp.imp)[length(pp.imp)]<- var
  }#end var pd loop
  dev.off()
  
  # Plots each individual frame of the partial dependency plots
  #cols <- brewer.pal(length(pp.imp), "Set1")
  ylim1 <- sapply(pp.imp, function(d){ return(c(min(d$y), max(d$y)))})
  ylim <- c(min(ylim1[1,]-1.5), max(ylim1[2,]))

  #par(mar=c(10.1, 4.1, 4.1, 2), xpd=TRUE)
  par(mar=c(5.1, 6.1, 4.1, 2.1))
  
#   #set up plotting area
#   if (mod == "randfor") {
#     mod.text <- "Kitchen Sink Model"
#   } else if (mod == "t.randfor") {
#     mod.text <- "Temperature Model"
#   } else if (mod == "v.randfor") {
#     mod.text <- "VPD Model"
#   } else {
#     warning("Model types are currently hard coded and must be 'Kitchen Sink,' 'Temperature', or 'VPD.' Please check your model type.")
#   }

  pdf(file = outparcor, width=6, height=6)
  plot(c(0, 1), ylim, type="n", xlab="Percentile of Predictor Data",ylab=paste0(es.name[j], "\nFractional Cover (%)"),  
       cex.axis=1, cex.lab=1, main=paste0("Partial Dependency\n", es.name[j]))
  
  #Plot lines for top 5 variables
  for (k in 1:length(pp.imp)){
    Fn <- ecdf(pp.imp[[k]]$x)
    lines(Fn(pp.imp[[k]]$x), pp.imp[[k]]$y, lty=k)
  }
  
  #ypos <- ylim[1] - (ylim[2]-ylim[1])*0.3
  #ypos <- ylim[1] - ylim[1]*.5 
  legend("bottom", names(pp.imp), lty=c(1:length(pp.imp)),ncol=3, cex=0.85, bty="n", xjust=0.5, yjust=1)
  dev.off()
}# end j loop

################ Quick and Dirty Section to Find out How Many Samples Per Model ##########################

# 
for (k in c(1:length(s.name))) { 
  es <- s.name[k]
  obs.dir <- paste0(a.dir,s.name[k], "/inputs/", mr.subdir[k], "/")
  obs.file <- paste0(obs.dir, "COVER_",es, "_inPRISM_800m_masked.tif")
  #obs.file <- "C:/Share/LCC-VP/Parks/GRSM/analysis/spruce_fir_GAP_GRSM/inputs/biovarsGddSoilsTWIsrad_20150217_rngwideModel/COVER_spruce_fir_GAP_GRSM_inPRISM_800m_masked.tif"
  obs <- na.omit(getValues(raster(obs.file)))
  
  assign(s.name[k],obs)  
}# end K loop


cf <- as.data.frame(cbind(Cove_Forest_all, "Cove Forest"),stringsAsFactors=F)
names(cf) <- c("suitability", "community")
sf <- as.data.frame(cbind(spruce_fir, "Spruce-Fir"),stringsAsFactors=F)
names(sf) <- c("suitability", "community")
hem <- as.data.frame(cbind(Hemlock, "Hemlock"),stringsAsFactors=F)
names(hem) <- c("suitability", "community")
nh <- as.data.frame(cbind(northern_hardwood, "Northern Hardwood"),stringsAsFactors=F)
names(nh) <- c("suitability", "community")
oh <- as.data.frame(cbind(Oak_Hickory, "Oak-Hickory"),stringsAsFactors=F)
names(oh) <- c("suitability", "community")
yp <- as.data.frame(cbind(Yellow_Pine, "Yellow Pine"),stringsAsFactors=F)
names(yp) <- c("suitability", "community")

suit <- rbind(cf, sf, hem, nh, oh, yp)
suit$suitability <- as.numeric(suit$suitability)
suit <- suit[suit$suitability >=1]

m <- ggplot(suit, aes(x=suitability, y= ..density..))
m <- m + geom_histogram(binwidth = 5)
m <- m + facet_wrap(~ community)
m

outhist <- paste0(outdir, "ForestType_Histograms.pdf")
pdf(file=outhist,family="Times", width=10, height=8)
print(m)
dev.off()
