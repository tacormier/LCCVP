#Script Name: LCC-VP_soils_prep_statsgo.R 
#Author: Tina Cormier
#Date: 05/10/2013
#
#Purpose: This script uses STATSGO soils raster data from Dave Theobald, resampled
#to 800m.  Data originated from: http://www.soilinfo.psu.edu/index.cgi?soil_data&conus&data_cov.
#
#Calculates avg sand, clay, silt, bulk density, and ph based on a weighted average
#of the first 9 layers (150cm) - weighted by layer depth. 



library(raster)

in.ras <- "C:/Share/LCC-VP/US_soils/Theobald_STATSGO/STATSGO_PS/muid_270y_allMetrics.tif"
outdir <- "C:/Share/LCC-VP/US_soils/Theobald_STATSGO/STATSGO_PS/"

#Read in soils data
soils <- raster(in.ras)

#is.factor(soils)
#x <- levels(soils)[[1]][1]
#rat <- as.data.frame(soils@data@attributes)


#calculate weighted avg. of soil variables based on depth of layer (found in metadata:C:\Share\LCC-VP\US_soils\Theobald_STATSGO\STATSGO_PS\metadata\Sand Silt Clay Fractions.mht)
weights <- c(5,5,10,10,10,20,20,20,50)


attach(soils@data@attributes[[1]])
#soils@data@attributes[[1]]$ID <- as.factor(soils@data@attributes[[1]]$ID)
#attach(rat)
sand <- cbind(muid_270y.vat.SAND_L1, muid_270y.vat.SAND_L2, muid_270y.vat.SAND_L3, muid_270y.vat.SAND_L4,
          muid_270y.vat.SAND_L5, muid_270y.vat.SAND_L6, muid_270y.vat.SAND_L7, muid_270y.vat.SAND_L8,
          muid_270y.vat.SAND_L9)
clay <- cbind(muid_270y.vat.CLAY_L1, muid_270y.vat.CLAY_L2, muid_270y.vat.CLAY_L3, muid_270y.vat.CLAY_L4,
              muid_270y.vat.CLAY_L5, muid_270y.vat.CLAY_L6, muid_270y.vat.CLAY_L7, muid_270y.vat.CLAY_L8,
              muid_270y.vat.CLAY_L9)
silt <- cbind(muid_270y.vat.SILT_L1, muid_270y.vat.SILT_L2, muid_270y.vat.SILT_L3, muid_270y.vat.SILT_L4,
              muid_270y.vat.SILT_L5, muid_270y.vat.SILT_L6, muid_270y.vat.SILT_L7, muid_270y.vat.SILT_L8,
              muid_270y.vat.SILT_L9)
bd <- cbind(muid_270y.vat.L1_BD, muid_270y.vat.L2_BD, muid_270y.vat.L3_BD, muid_270y.vat.L4_BD, 
            muid_270y.vat.L5_BD, muid_270y.vat.L6_BD, muid_270y.vat.L7_BD, muid_270y.vat.L8_BD,
            muid_270y.vat.L9_BD)
ph <- cbind(mu_ph.L1_PH, mu_ph.L2_PH, mu_ph.L3_PH, mu_ph.L4_PH, mu_ph.L5_PH, mu_ph.L6_PH, mu_ph.L7_PH,
            mu_ph.L8_PH, mu_ph.L9_PH)
#awc <- muid_270y.vat.AWC_150
#rockdepth <- muid_270y.vat.ROCKDEPM

#Calculate new column that is the weighted average of the 9 soil layers above 150cm.
#Multiply by 100 and round off decimal - to retain some precision but avoid float.
soils@data@attributes[[1]]$sand_avg <- round(apply(sand, 1, weighted.mean, weights)*100, digits=0)
soils@data@attributes[[1]]$clay_avg <- round(apply(clay, 1, weighted.mean, weights)*100, digits=0)
soils@data@attributes[[1]]$silt_avg <- round(apply(silt, 1, weighted.mean, weights)*100, digits=0)
soils@data@attributes[[1]]$bd_avg <- round(apply(bd, 1, weighted.mean, weights)*100, digits=0)
soils@data@attributes[[1]]$ph_avg <- round(apply(ph, 1, weighted.mean, weights)*100, digits=0)
soils@data@attributes[[1]]$awc_150 <- muid_270y.vat.AWC_150
soils@data@attributes[[1]]$bedrock_depth <- muid_270y.vat.ROCKDEPM

#write out raster with new variables calculated
#dt <- dataType(soils)

#list of column names from RAT to write to new raster
cols <- c("UNID","MUID","sand_avg", "clay_avg", "silt_avg", "bd_avg", "ph_avg", "awc_150", "bedrock_depth")

tbl <- as.data.frame(cbind(soils@data@attributes[[1]]$muid_270y.vat.UNID, soils@data@attributes[[1]]$muid_270y.vat.MUID, soils@data@attributes[[1]]$sand_avg, 
             soils@data@attributes[[1]]$clay_avg, soils@data@attributes[[1]]$silt_avg, soils@data@attributes[[1]]$bd_avg, 
             soils@data@attributes[[1]]$ph_avg, soils@data@attributes[[1]]$awc_150, soils@data@attributes[[1]]$bedrock_depth))

names(tbl) <- cols

outTbl <- paste(outdir, "avg_calcs.csv", sep="")
write.csv(tbl, file=outTbl, quote=F, row.names=F)


detach(soils@data@attributes[[1]])