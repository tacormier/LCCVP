


lith <- raster("C:/Share/LCC-VP/Theobald/LandFacets/ergo5/ergo5_20140416/us_lith_90s")

# Read in points


# Reproject points to match raster proj
atest2 <- spTransform(pts,CRS(projection(lith)))

# This should proably be done within the spp. range
# Extract lithology to points
atest <- extract(lith,atest2[atest2$PRES==1,])

# Calculate proportion of presences on a particular lithology
tapply(rep(1,length(atest)),atest,sum)/length(atest)

