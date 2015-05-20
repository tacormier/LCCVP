# Make point shapefiles for tree species

# Vegbank Fields
# WGS84 is the datum used for plot coordinates

# plot_env.csv table
# observation_id - plot id
# obsstartdate_vb - start of observations
# confidentialitystatus - 0=exact, 1=fuzzing
# latitude - latitude
# longitude - longitude
# locationaccuracy - Estimated accuracy of the location of the plot. Plot origin has a 95% or greater probability of being within this many meters of the reported location. 
# shape_vb - text string describing plot shape = Rectangular, Plotless, Square, etc.
# area - square meters
# topoposition_vb - topographic position = Lowslope, Toeslope, Midslope, High slope, etc.
# landform_vb - landform = slope, toe slope, ridge, mid slope, slope, alluvial terrace, scour, etc.
# stemsizelimit
# stemsamplemethod_vb - Full census or full census or Full Census, etc.

# effortlevel_vb
# plotvalidationlevel
# floristicquality_vb

# plot_taxa.csv table
# currenttaxoninterp_scientificname
# e.g. Sugar Maple - Acer saccharum Marsh.
# cover - percent cover of spp.
# currenttaxoninterp_usdacode - usda spp. code

# Packages
require(sp)
require(rgdal)
# Set working directory
wd <- "C:/Share/LCC-VP/RangeWide/Vegbank/"
setwd(wd)

# Read in plot table
plot.env <- read.csv("plot_env_all.csv")
# Grab plots with lat and lon
plot.env <- plot.env[!is.na(plot.env$latitude) & !is.na(plot.env$longitude),]
# Grab plots that are not confidential
plot.env <- plot.env[which(plot.env$confidentialitystatus == 0),]
# Count decimal places in latitude field (~3 decimal places is ~ 100m precision)
dpees <- unlist(lapply(plot.env$latitude,decimalplaces))
# Grab plots with location accuracy at least as good as pixel size or with at least 3 decimal
# points of latitude precision
plot.env <- plot.env[which(plot.env$locationaccuracy <= 800 | (is.na(plot.env$locationaccuracy) & dpees >= 3)),]

# Convert to a point shapefile
vbank.pts <- SpatialPointsDataFrame(plot.env[,c("longitude","latitude")],data.frame(plot.env[,c("observation_id","longitude","latitude","stateprovince_vb")]),
                                  proj4string=CRS("+proj=longlat +datum=WGS84"))
# Write to file
writeOGR(vbank.pts,"C:/Share/LCC-VP/RangeWide/Vegbank","vbank_pts",driver="ESRI Shapefile")
