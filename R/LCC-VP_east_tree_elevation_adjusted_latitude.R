
# Calculate elevation adjusted latitude for each tree species

adj.el.fun <- function(dem,wd,pts) {
  print(dem)
  # Get elevation raster
  aras <- raster(dem)
  # Get raster projection
  aproj <- CRS(projection(aras))
  # Set point layer
  ptl=pts
  # Transform point layer to raster projection
  ptl <- spTransform(ptl,aproj)
  # Extract elevation values to points
  ex <- extract(aras, ptl, df=T)
  # Divide by 100 to get scaled values back to meters and divide by lat/elevation scaling factor
  # taken from Jump et al. 2009. 167m = 1degC temp change = 145km latitude
  # 1/167degC corresponds to 1m which corresponds to 145/167 km. So the ratio is 0.87km per meter
  # to adjust latitude based on elevation
  # 0.868263473
  
  # Elevation (m)
  ex$elev <- ex[,2]/10
  # Adjusted elevation
  ex$elev_adj <- ex[,2]/10*0.868263473
  # Get rid of scaled elevation column
  ex <- ex[,-c(2)]
  return(ex)
  
}

# Get elevation files
wd <- 'C:/Share/LCC-VP/RangeWide/ned/elev_only'
af <- list.files(path='C:/Share/LCC-VP/RangeWide/ned/elev_only',pattern="*img")
af <- paste0(wd,'/',af)

# Add two elevation tiles that aren't in the folder
af <- c(af,"C:/Share/LCC-VP/RangeWide/ned/ned_west/MZ26_ned_alb_ecog.img",
        "C:/Share/LCC-VP/RangeWide/ned/ned_west/MZ30_ned_alb_ecog.img")

# Read in points shapefile (doesn't matter which one, just need the geometry)
pts <- readOGR("C:/Share/LCC-VP/RangeWide/FIA/bySpecies","sp_261_pts")

# Extract elevation to points and convert to latitude equivalent 
ptlist <- lapply(af, adj.el.fun, wd, pts)

# Grab data column of each list and make a data frame
# Take mean of overlapping points to get elevation adjusted latitude shift scalar for each point
ptlist2 <- lapply(ptlist, function(x) x[,2])
ptlist2 <- do.call("cbind",ptlist2)
ptel <- apply(ptlist2, 1, mean, na.rm=T)
# Do the same for latitude adjustment factor based on elevation
ptlist2 <- lapply(ptlist, function(x) x[,3])
ptlist2 <- do.call("cbind",ptlist2)
# This is the latutude adjustment factor in kilometers
ptel.adj <- apply(ptlist2, 1, mean, na.rm=T)

rvec <- c(17077,18427)
ptel.adj <- ptel.adj[-c(which(is.na(ptel.adj)))]



# Southernmost point is 24.631535, -81.743596
sco <- data.frame(LON=-81.743596, LAT=24.631535)
# Northernmost point is 49.348905, -95.081388
nco <- data.frame(LON=-95.081388, LAT=49.348905)

# Calculate distance between southernmost point and all other points
# assuming the same longitude (i.e. get meters(or kilometers) north of southernmost point)
require(geosphere)

# Create list of new coordinates with same longitude
newco <- data.frame(LON=-81.743596,LAT=pts$LAT)

# Get great circle distance in kilometers from each point to the southernmost point
gcdfun <- function(newpoints,southpoint) {
  gcdist <- distVincentyEllipsoid(southpoint, newpoints, a=6378137, b=6356752.3142, f=1/298.257223563)/1000
}

xy.list <- split(newco, rownames(newco))

# Calculate distances north of southernmost point
dnorth <- lapply(xy.list,gcdfun,sco)
dnorth2 <- do.call("rbind",dnorth)

# Adjust latitude
latadj <- dnorth2+ptel.adj

# SPP CALCULATIONS
# Loop through species and calculate average latitude of presences
# Get species codes
scodes <- c(12,16,97,110,111,121,123,126,129,131,132,261,315,316,317,318,319,332,371,403,407,408,409,412,531,541,602,611,621,746,762,802,824,832,833,835,837,951,971,972)
scodes <- paste0("sp_",scodes,"_pts")

# First calculate Albers equal area coordinates (in meters)
xyco <- as.matrix(pts@data[,c("LON","LAT")])
aeaproj  <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
aeaco <- rgdal::project(xyco,as.character(aeaproj))

newcolist <- list()

for (i in scodes) {
  print(i)
  # Read in point file
  ptfile <- readOGR("C:/Share/LCC-VP/RangeWide/FIA/bySpecies",i)
  ptfile <- ptfile[-c(rvec),]
  llproj <- CRS(projection(ptfile))
  
  atest <- spTransform(ptfile,aeaproj)
  newco <- cbind(coordinates(atest)[,1],coordinates(atest)[,2]+ptel.adj*1000)
  colnames(newco) <- c("coords.x1","coords.x2")
  atest <- as(atest, "data.frame")
  coordinates(atest) <- newco
  projection(atest) <- aeaproj
  atest2 <- spTransform(atest,llproj)
  
  madjlat <- mean((coordinates(atest2)[,2][ptfile@data$PRES==1]),na.rm=T)
  madjlatmin <- min((coordinates(atest2)[,2][ptfile@data$PRES==1]),na.rm=T)
  madjlatmax <- max((coordinates(atest2)[,2][ptfile@data$PRES==1]),na.rm=T)
  originallat <- mean((coordinates(ptfile)[,2][ptfile@data$PRES==1]),na.rm=T)
  
  mlon <- mean((coordinates(atest2)[,1][ptfile@data$PRES==1]),na.rm=T)
  newcolist <- rbind(newcolist,cbind(mlon,madjlat,madjlatmin,madjlatmax,originallat))
  
}

newcolist4 <- data.frame(lon=unlist(newcolist[,1]),meanlat=unlist(newcolist[,2]),minlat=unlist(newcolist[,3]),
                         maxlat=unlist(newcolist[,4]),orlat=unlist(newcolist[,5]))

# Calculate derived quantities
newcolist3 <- data.frame(lon=unlist(newcolist[,1]),meanlat=unlist(newcolist[,2]),minlat=unlist(newcolist[,3]),
                         maxlat=unlist(newcolist[,4]))
newcolist3$latrange <- newcolist3$maxlat - newcolist3$minlat
newcolist3$midpointlat <- ((newcolist3$maxlat - newcolist3$minlat)/2)+newcolist3$minlat

# Calculate mean adjusted latitude for all eastern FIA points
mean.east.lat.adj <- mean(coordinates(atest2)[,2])
# Calcualte lower and upper thirds of adjusted latitude for the eastern U.S.
# to classify species
southern.threshold <- quantile(coordinates(atest2)[,2],probs=c(0.33,0.66))[1]
northern.threshold <- quantile(coordinates(atest2)[,2],probs=c(0.33,0.66))[2]

# NORTHERN SPP
balsam fir
red spruce
eastern white pine
eastern hemlock
striped maple
mountain maple
yellow birch
quaking aspen
American basswood

# CENTRAL SPP
Fraser fir
shortleaf pine
table mountain pine
pitch pine
Virginia pine
red maple
silver maple
sugar maple
yellow buckeye
pignut hickory
shagbark hickory
black hickory
mockernut hickory
American beech
white ash
black walnut
yellow-poplar 
black cherry
white oak
chestnut oak
northern red oak
post oak
black oak
American elm

# SOUTHERN SPP
slash pine
longleaf pine
loblolly pine
red hickory
sweetgum 
blackjack oak
winged elm













# SCRAPS
# Calculate great circle distance assuming WGS84 ellipsoid and divide by 1000 to get km
atest <- distVincentyEllipsoid(sco, newco[1,], a=6378137, b=6356752.3142, f=1/298.257223563)/1000

pts <- readOGR("C:/Share/LCC-VP/RangeWide/FIA/bySpecies","sp_261_pts")

cc <- raster(af[[28]])
aproj <- CRS(projection(cc))
pts <- spTransform(pts,aproj)
ex <- extract(cc, pts, df=T)


ex <- extract(raster(af[[28]]),pts,df=T)

