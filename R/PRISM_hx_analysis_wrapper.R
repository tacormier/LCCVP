library(raster)
library(maptools)
library(rasterVis)
library(RColorBrewer)

source("C:/Share/LCC-VP/scripts/R/PRISM_hx_analysis.R")
#####################################
#Set variables here
#vars <- c("ppt", "tmax", "tmin", "tmean")
vars <- c("tmax", "tmean")
PACE.path <- "C:/Share/LCC-VP/ALCC_PACE_Park_boundaries/GRSM_pace_wgs72.shp"
park.path <- "C:/Share/LCC-VP/ALCC_PACE_Park_boundaries/GRSM_boundary_wgs72.shp"
hsPath <- "C:/Share/LCC-VP/RangeWide/ned/clipped/GRSM_pace_ned_hillshade_120m_wgs72.tif"
outpath <- "C:/Share/LCC-VP/ClimatePrimer/figs/"
b.year <- 1895
e.year <- 2010
#moving window size (in years) for plotting trends
mws <- 3

#optional - climate station points to plot over maps
#c.stations <- "C:/Share/LCC-VP/ClimateStation/GRSM_climate_db_updated_2011/GRSM_Climate_stations_Fridley_wgs72.shp"
#####################################

#read in polygons
PACE <- readShapePoly(PACE.path)
park <- readShapePoly(park.path)
#optional
#c.stations <- readShapePoints(c.stations)
# 
#Plot historical trend maps
for (cvar in vars) {
  adata <- annualgrid(cvar, PACE)
  #x <- stack(adata)
  sp.trends <- temporalgradient(adata, 1895, 2010)
  
  #create a single layer raster with same dims as adata - to house trend data as raster.
  trend.ras <- raster(adata)
  trend.ras <- setValues(trend.ras, sp.trends)

  #Rewrote annualgrid function to do conversion to deg f - no need to do it here
  #We want to change deg C to deg F (for temp) and *10 to get 10 year trends
  #coeff <- 1.8
  trend.ras.con <- trend.ras*10
#   if (cvar == "tmax" | cvar == "tmin" | cvar == "tmean") {
#     #deg F/decade
#     trend.ras.con <- trend.ras * 1.8 * 10
#   } else if (cvar == "ppt") {
#     #mm/decade
#     trend.ras.con <- trend.ras *10
#   } else {
#     stop(paste("invalid 'cvar': ", var, ". Must be one of 'ppt', 'tmax', 'tmean', or 'tmin.'", sep=""))
#   }#end if
  
  trend.ras <- trend.ras.con
  
  #Generate historical trend map
  #outfile
  print(paste("plotting historical prism trends - ", cvar, sep=""))
  outfile <- paste(outpath, cvar, "_hx_trend_map_revSym.png", sep="")
  png(file=outfile, 7,5, units="in", pointsize=12, res=300)
  print(plotPRISMgrad(trend.ras, PACE, park, hsPath, cvar))
  dev.off()
#   
#   #Plot historical trend graphs
#   #Generate historical trend graphs
#   #outfile
#   print(paste("plotting historical prism trend graphs - ", cvar, sep=""))
#   #outfile <- paste(outpath, cvar, "_hx_trend_graphs_stations.png", sep="")
#   outfile <- paste(outpath, cvar, "_annual_anomaly.png", sep="")
#   png(file=outfile, 7,5, units="in", pointsize=12, res=300)
#   print(plotAnom(adata, cvar, mws, b.year,e.year))
#   dev.off()
#     
#   
# }#end for loop


###########################
#Plot forecast trend graphs
for (cvar in vars) {
  
  adata <- annualgridForecasts(cvar, PACE)
  #Plot forecast trend graphs
  #Generate forecast trend graphs
  #outfile
  print(paste("plotting forecast trend graphs - ", cvar, sep=""))
  #outfile <- paste(outpath, cvar, "nexdcp30_hx_trend_graphs_stations.png", sep="")
  outfile <- paste(outpath, cvar, "nexdcp30_annual_anomaly.png", sep="")
  png(file=outfile, 7,5, units="in", pointsize=12, res=300)
  print(plotAnomForecasts(adata, cvar, mws, b.year,e.year))
  dev.off()
  
  
}#end for loop
