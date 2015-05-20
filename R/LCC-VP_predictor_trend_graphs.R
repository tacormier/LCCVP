library(raster)
library(ggplot2)

# Looking at predictor trends
pr.dir <- "C:/Share/LCC-VP/Parks/GRSM/climate_data/forecasts/ave_30yr_byYear/deltas/ea_rcp85/ppt/"
tmin.dir <- "C:/Share/LCC-VP/Parks/GRSM/climate_data/forecasts/ave_30yr_byYear/deltas/ea_rcp85/tmin/"
tmax.dir <- "C:/Share/LCC-VP/Parks/GRSM/climate_data/forecasts/ave_30yr_byYear/deltas/ea_rcp85/tmax/"

biovar.dir <- "C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/biovarsGddSoilsTWIsrad_20150217/forecast_imgs/ea_rcp85/"
biovars.names <- "C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/GRSM_var_names_Vars_20150217_BiovarsGddSoilsTwiSrad_SHORT.txt"

outdir <- "C:/Share/LCC-VP/Parks/GRSM/analysis/figures/"

# for (clim in c(pr.dir, tmin.dir, tmax.dir)) {
  clim.files <- list.files(clim, pattern="*.tif$", full.names=T)
  
  ann.mean <- c(1:length(clim.files))
  year <- c(1:length(clim.files))
  
  if (clim != "biovar.dir") {
    for (i in c(1:length(clim.files))) {
      print(clim.files[i])
      yr.pos <- regexpr("[0-9]{4}", basename(clim.files[i]))
      yr <- as.numeric(sub(pattern="\\.",replacement = "",regmatches(basename(clim.files[i]),yr.pos)))
      
      # Ok, so this takes the mean of the stack (inner mean) to get an annual raster, then we get those values as a vector,
      # remove the NAs, and take the mean of the vector to get one mean of all the pixels for the park.
      img.mean <- mean(na.omit(getValues(mean(brick(clim.files[i])))))/100
      
      ann.mean[i] <- img.mean
      year[i] <- yr
      
    }#end clim.files loop

  df <- as.data.frame(cbind(year, ann.mean))
  
  #labeling & file naming currently hard coded. Can write in some logic if we need to use this script more.
  p <- ggplot(df, aes(x=year, y=ann.mean, group=1)) + ylim(5,25)
  p <- p + stat_smooth(se = T, method = "lm", formula = y ~ poly(x, 20)) + ylab(expression(paste("Temperature (", degree, "C)"))) +
    stat_smooth(data=df.tmin, se=T, method="lm", formula=y ~ poly(x, 20)) #This is added to get both temp lines on one graph.
  #p + geom_point()
  p

#ylab("Precipitation (mm)")

  outAnn <- paste0(outdir, "ann_temp_projections_ea_rcp85.pdf")
  pdf(file=outAnn,family="Times", width=6.5, height=3.25)
  print(p)
  dev.off()

  } else {
      b.names <- read.table(biovars.names, sep="\n",as.is = T)[,1]
      biovar.means <- as.data.frame(matrix(data = NA, nrow = length(clim.files), ncol = length(b.names)+1))
      names(biovar.means) <- c(b.names, "Year")
      
      for (j in c(1:length(clim.files))) {
        yr.pos <- regexpr("_[0-9]{4}\\.tif", basename(clim.files[j]))
        yr <- as.numeric(sub(pattern="_", replacement="", sub(pattern="\\.tif",replacement = "",regmatches(basename(clim.files[j]),yr.pos))))
        
        img.mean <- mean(na.omit(getValues(brick(clim.files[j]))))/100
        img <- as.data.frame(getValues(brick(clim.files[j])/100))
        names(img) <- b.names
        
        #calculate column means
        c.means <- colMeans(img, na.rm=T)
        c.means <- c(c.means, yr)
        biovar.means[j,] <- c.means        
        
      }#end biovars clim.files loop
      
      biovar.means[,c(3,15)] <- biovar.means[,c(3,15)]*100
      
      for (v in c(1:(ncol(biovar.means)-1))) {
        var <- names(biovar.means[v])
        z <- ggplot(biovar.means, aes(x=Year, y=biovar.means[[var]], group=1)) 
        z <- z + stat_smooth(se = T, method = "lm", formula = y ~ poly(x, 20)) + ylab(var) 
        #z <- z + geom_point()
        z
        
        outvar <- paste0(outdir, var, "_projections_ea_rcp85.pdf")
        pdf(file=outvar, family="Times", width=6.5, height=3.25)
        print(z)
        dev.off()
        
      }#end biovar column loop
        
      
  }#end biovar.dir if
  
# }# end clim loop