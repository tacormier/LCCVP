# prism mask by eatern pace
# mask PRISM data by the following PACEs: GRSM, SHEN, DEWA
# by  Kevin Guay (kguay@whrc.org)
# on  11-13-2014
# mod 11-13-2014

# @desc This script produces a map of PACE and Park boundaries for the DEWA, SHEN and GRSM
#  parks in the Eastern US. The output map has the following values:

#  DEWA PACE: 1
#  DEWA Park: 10
#  SHEN PACE: 2
#  SHEN Park: 20
#  GRSM PACE: 3
#  GRSM Park: 30

require(raster)
require(maptools)
require(sp)

# load boundary files
bounds.pace <- readShapePoly('/mnt/arctic/c/Share/LCC-VP/ALCC_Bounds/ALL_pace_albers')

bounds.dewa <- readShapePoly('/mnt/arctic/c/Share/LCC-VP/ALCC_Bounds/DEWA_boundary_albers')
bounds.shen <- readShapePoly('/mnt/arctic/c/Share/LCC-VP/ALCC_Bounds/SHEN_boundary_albers')
bounds.grsm <- readShapePoly('/mnt/arctic/c/Share/LCC-VP/ALCC_Bounds/GRSM_boundary_albers')

# load PRISM file
prism <- prism <- raster('/mnt/arctic/c/Share/LCC-VP/US_PRISM/normals_1981-2010/tmin/original/PRISM_tmin_30yr_normal_800mM2_stack.tif')
# set PRISM data to 1's (since this will be a mask, real data is not needed)
prism[] <- 0

# remind bounds of its projection
projection(bounds.pace) <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0'
projection(bounds.dewa) <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0'
projection(bounds.shen) <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0'
projection(bounds.grsm) <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0'

# project the boundary shapefile to lat/lon (same as PRISM)
bounds.pace <- spTransform(bounds.pace, CRS('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'))
bounds.dewa <- spTransform(bounds.dewa, CRS('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'))
bounds.shen <- spTransform(bounds.shen, CRS('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'))
bounds.grsm <- spTransform(bounds.grsm, CRS('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'))

# crop and mask (i.e. clip) PRISM by the boundaries
prism.color <- prism
prism.color[] <- 1:ncell(prism.color)
prism.pace <- mask(crop(prism.color, bounds.pace), bounds.pace)
prism.pace[prism.pace > 0 & prism.pace < 8500000] <- 1
prism.pace[prism.pace > 8500000 & prism.pace < 11000000] <- 2
prism.pace[prism.pace > 11000000] <- 3


prism.dewa <- mask(crop(prism, bounds.dewa), bounds.dewa)
prism.shen <- mask(crop(prism, bounds.grsm), bounds.shen)
prism.grsm <- mask(crop(prism, bounds.shen), bounds.grsm)

pp.dewa <- mask(prism.pace, bounds.dewa)
pp.shen <- mask(prism.pace, bounds.shen)
pp.grsm <- mask(prism.pace, bounds.grsm)

prism.pace[pp.dewa] <- 10
prism.pace[pp.shen] <- 20
prism.pace[pp.grsm] <- 30

# write the final raster to disk
writeRaster(prism.pace, '/mnt/arctic/c/Share/LCC-VP/ALCC_Bounds/ALL_pace_park_latlon_prism.tif', format='GTiff', overwrite=T)