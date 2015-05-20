# This appears to be some sort of manipulation for Little's Range Maps

setwd("C:/Share/LCC-VP/RangeWide/RFoutput")

aa <- list.files(path=".",pattern="modbesttemp_rfobject")

for (i in aa) {
  bb <- load(i)
  cc <- row.names(get(bb)$importance)
  if ('gdd' %in% cc) {print(i)}
}

setwd("C:/Share/LCC-VP/RangeWide/LittleRange")

# 16, 412
for (i in list(12,97,110,111,121,123,126,129,131,132,261,315,316,317,318,319,332,371,403,407,408,409,531,541,602,611,621,746,762,802,824,832,833,835,837,951,971,972)) {
  aline <- paste0("http://www.fs.fed.us/nrs/atlas/littlefia/litt",i,"av.zip")
  temp <- tempfile()
  download.file(aline,temp)
  unzip(temp, exdir=getwd())
}


sproj <- CRS("+proj=longlat +ellps=clrk66 +datum=NAD27 +no_defs")


for (i in list(12,97,110,111,121,123,126,129,131,132,261,315,316,317,318,319,332,371,403,407,408,409,531,541,602,611,621,746,762,802,824,832,833,835,837,951,971,972)) {
  
  aa <- readOGR(getwd(),paste0("litt",i,"av"))
  projection(aa) <- sproj
  bb <- spTransform(aa,canomproj)
  
}

aa <- readOGR("C:/Share/LCC-VP/LittleRange","abiebals")


setwd("C:/Share/LCC-VP/RangeWide/LittleRange")
snum <- c("12,16,97,110,111,121,123,126,129,131,132,261,315,316,317,318,319,332,371,403,407,408,409,531,541,602,611,621,746,762,802,824,832,833,835,837,951,971,972")
snum <- unlist(strsplit(snum,","))
# changed aesculus flava to aesculus octandra
# carya alba to carya tomentosa
sn <- c("Abies balsamea,Abies fraseri,Picea rubens,Pinus echinata,Pinus elliottii,Pinus palustris,Pinus pungens,Pinus rigida,Pinus strobus,Pinus taeda,Pinus virginiana,Tsuga canadensis,Acer pensylvanicum,Acer rubrum,Acer saccharinum,Acer saccharum,Acer spicatum,Aesculus octandra,Betula alleghaniensis,Carya glabra,Carya ovata,Carya texana,Carya tomentosa,Fagus grandifolia,Fraxinus americana,Juglans nigra,Liquidambar styraciflua,Liriodendron tulipifera,Populus tremuloides,Prunus serotina,Quercus alba,Quercus marilandica,Quercus prinus,Quercus rubra,Quercus stellata,Quercus velutina,Tilia americana,Ulmus alata,Ulmus americana")
sn <- unlist(strsplit(sn,","))

for (i in seq(1,length(sn),1)) {
  a1 <- substr(tolower(unlist(strsplit(sn[i]," ")))[1],1,4)
  a2 <- substr(unlist(strsplit(sn[i]," "))[2],1,4)
  a3 <- paste0("http://esp.cr.usgs.gov/data/little/",a1,a2,".zip")
  temp <- tempfile()
  download.file(a3,temp)
  unzip(temp,exdir=getwd()) 
}


setwd("C:/Share/LCC-VP/LittleRange")
sproj <- CRS("+proj=longlat +ellps=clrk66 +datum=NAD27 +no_defs")
for (i in seq(1,length(sn),1)) {
  print(sn[i])
  a1 <- substr(tolower(unlist(strsplit(sn[i]," ")))[1],1,4)
  a2 <- substr(unlist(strsplit(sn[i]," "))[2],1,4)
  inpath <- paste0(getwd())
  aa <- readOGR(inpath,paste0(a1,a2))
  projection(aa) <- sproj
  bb <- spTransform(aa,canomproj)
  
  outpath <- inpath
  outname <- paste0("s",snum[i],"_little")
  writeOGR(bb,outpath,outname,driver="ESRI Shapefile")
}

