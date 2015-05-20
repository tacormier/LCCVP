require(OSfun)


param <- list()
param$input <- list()
param$data <- list()
param$output <- list()


# general parameters
####################
# path to maxent.jar
param$maxent.jar <- "Maxent/maxent.jar"
# environmental layer projection information
param$proj4string <- proj4strings["Albers-NAD83"]
# occurrences observed during or after this year are considered recent enough to be meaningful
param$recentOccurrenceYear <- 1960
# number of chunks to process tile values in when aggregating rasters
param$rasterAggChunks <- 1000000


# import species list
#####################
# [input] species list as a text file
param$input$species <- "input/species.txt"
# [data] species R object
param$data$species <- "data/species.Rdata"


# subset CNDDB by species list
##############################
# CNDDB (see notes for version information)
param$CNDDB3 <- list()
param$CNDDB3$path <- "fragileInput/CNDDB"
param$CNDDB3$cnddb.shp <- file.path(param$CNDDB3$path, "gis", "cnddb.shp")
# synonyms
param$CNDDB3$synonyms <- list()
param$CNDDB3$synonyms$"Ovis canadensis nelsoni DPS" <- "Ovis canadensis nelsoni"
# [output] CNDDB subset
param$output$CNDDB_subset.shp <- "output/CNDDB_subset.shp"
# [data] CNDDB subset SpatialPolygonsDataFrame R object
param$data$CNDDB_subset <- "data/CNDDB_subset.Rdata"
# CNDDB subset feature layer name
param$CNDDB_subset_fl <- "CNDDB_subset"
# [input] imported CNDDB symbology layer (note this was manually adjusted for what I think are ArcGIS version problems)
param$input$CNDDB.lyr <- "input/CNDDB.lyr"
# [output] CNDDB subset symbology layer
param$output$CNDDB_subset.lyr <- "output/CNDDB_subset.lyr"


# subset Dudek occurrence data by species list
##############################################
# [input] Dudek occurrence data (zipped)
param$input$Dudek_occurrence <- list()
param$input$Dudek_occurrence$zip <- "input/DRECP_Species_20111214_Draft.zip"
param$input$Dudek_occurrence$shp <- "DRECP_Species_20111214_Draft.shp"
# synonyms
param$Dudek_occurrence$synonyms <- list()
param$Dudek_occurrence$synonyms$"Ovis canadensis nelsoni DPS" <- "Ovis canadensis nelsoni"
# [output] Dudek occurrence subset
param$output$Dudek_occurrence_subset.shp <- "output/Dudek_occurrence_subset.shp"
# [data] Dudek occurrence subset SpatialPointsDataFrame R object
param$data$Dudek_occurrence_subset <- "data/Dudek_occurrence_subset.Rdata"


# subset US FWS CFWO occurrence data by species list
####################################################
# [input] US FWS CFWO occurrence data
param$input$US_FWS_CFWO_occurrence <- list()
# [input] initial zipped input
param$input$US_FWS_CFWO_occurrence$zip <- "input/US FWS CFWO SppOcc.zip"
# [input] e00 files contained in the zip
param$input$US_FWS_CFWO_occurrence$e00 <- list()
# [input] shape data and attribute tables
param$input$US_FWS_CFWO_occurrence$e00$shapes <- "cfwo_12-07.e00"
# [input] species code explanations table
param$input$US_FWS_CFWO_occurrence$e00$species <- "species.e00"
# [input] coverage (imported e00) region containing occurrence data
param$input$US_FWS_CFWO_occurrence$region <- "allpcode"
# [output] US FWS CFWO occurrence subset
param$output$US_FWS_CFWO_occurrence_subset.shp <- "output/US_FWS_CFWO_occurrence_subset.shp"
# [data] US FWS CFWO occurrence subset SpatialPolygonsDataFrame R object
param$data$US_FWS_CFWO_occurrence_subset <- "data/US_FWS_CFWO_occurrence_subset.Rdata"


# subset JT CA endemics data by species list
############################################
# [input] JT CA endemics data (zipped)
param$input$JT_occurrence <- list()
param$input$JT_occurrence$zip <- "input/CA_Endemics_SLThrasher.zip"
param$input$JT_occurrence$mdb <- "CA_Endemics_SLThrasher.mdb"
# JT CA endemics
param$JT_occurrence <- list()
# synonyms
param$JT_occurrence$synonyms <- list()
param$JT_occurrence$synonyms$"Astrolepis cochisensis" <- "Astrolepis cochisensis cochisensis"
param$JT_occurrence$synonyms$"Arabis shockleyi" <- "Boechera shockleyi"
param$JT_occurrence$synonyms$"Escobaria vivipara alversonii" <- "Coryphantha alversonii"
param$JT_occurrence$synonyms$"Achnatherum aridum" <- "Stipa arida"
# [output] JT CA endemics occurrence subset
param$output$JT_occurrence_subset.shp <- "output/JT_occurrence_subset.shp"
# [data] JT CA endemics occurrence subset SpatialPointsDataFrame R object
param$data$JT_occurrence_subset <- "data/JT_occurrence_subset.Rdata"


# subset eBird data by species list
###################################
# eBird
param$eBird <- list()
param$eBird$path <- "fragileInput/eBird"
param$eBird$pattern <- "^ebd_.*_rel.*\\.txt$"
# synonyms
param$eBird$synonyms <- list()
param$eBird$synonyms$"Laterallus jamaicensis" <- "Laterallus jamaicensis coturniculus"
# fields to extract and the data type to extract into
param$eBird$fields <- list()
param$eBird$fields$"GLOBAL UNIQUE IDENTIFIER" <- "character"
param$eBird$fields$"SCIENTIFIC NAME" <- "factor"
param$eBird$fields$"SUBSPECIES SCIENTIFIC NAME" <- "factor"
param$eBird$fields$"LATITUDE" <- "numeric"
param$eBird$fields$"LONGITUDE" <- "numeric"
param$eBird$fields$"OBSERVATION DATE" <- "factor"
# [output] eBird occurrence subset
param$output$eBird_occurrence_subset.shp <- "output/eBird_occurrence_subset.shp"
# [data] eBird occurrence subset SpatialPointsDataFrame R object
param$data$eBird_occurrence_subset <- "data/eBird_occurrence_subset.Rdata"


# merge occurrence datasets
###########################
# things not to extract from the Dudek data
param$Dudek_exclude <- c("CDFG CNDDB DB 20111101", "USFWS (20110125)", "EBIRD DB 20110503")
# occurrence accuracy thresholds
param$accuracy <- list()
# CNDDB accuracy
param$accuracy$CNDDB <- c("specific area", "80 meters", "1/10 mile", "1/5 mile")
# Dudek accuracy (I don't yet know what the codes mean)
param$accuracy$Dudek <- NULL
# US FWS CFWO accuracy
param$accuracy$US_FWS_CFWO <- 1:3
# JT CA endemics accuracy (no such information)
param$accuracy$JT <- NULL
# eBird accuracy (no such information)
param$accuracy$eBird <- NULL
# source version labels
param$sources <- list()
param$sources$CNDDB <- "CNDDB 20121204"
param$sources$Dudek <- "Dudek 20111214"
param$sources$US_FWS_CFWO <- "US FWS CFWO 20120706"
param$sources$JT <- "JT CA endemics 20090722"
param$sources$eBird <- "eBird 201208"
# [output] polygon occurrence data
param$output$occurrencePolygons.shp <- "output/occurrencePolygons.shp"
# [output] point occurrence data
param$output$occurrencePoints.shp <- "output/occurrencePoints.shp"
# [data] polygon occurrence data SpatialPolygonsDataFrame R object
param$data$occurrencePolygons <- "data/occurrencePolygons.Rdata"
# [data] point occurrence data SpatialPointsDataFrame R object
param$data$occurrencePoints <- "data/occurrencePoints.Rdata"


# create model species
######################
# [data] list of model species data.frame R object
param$data$modSpecies <- "data/modSpecies.Rdata"
# [output] model species polygon occurrence data
param$output$modSpeciesOccurrencePolygons.shp <- "output/modSpeciesOccurrencePolygons.shp"
# [output] model species point occurrence data
param$output$modSpeciesOccurrencePoints.shp <- "output/modSpeciesOccurrencePoints.shp"
# [data] model species polygon occurrence data SpatialPolygonsDataFrame R object
param$data$modSpeciesOccurrencePolygons <- "data/modSpeciesOccurrencePolygons.Rdata"
# [data] model species point occurrence data SpatialPointsDataFrame R object
param$data$modSpeciesOccurrencePoints <- "data/modSpeciesOccurrencePoints.Rdata"


# create maxent species names
#############################
# [data] Maxent species names crosswalk data.frame R object
param$data$maxentSpeciesNames <- "data/maxentSpeciesNames.Rdata"


# reproject analysis regions
############################
# [input] niche modeling analysis region (zipped)
param$input$currentRegion <- list()
param$input$currentRegion$zip <- "input/DRECP_Bdy_20110128_40kBufferCAonly.zip"
param$input$currentRegion$shp <- "DRECP_Bdy_20110128_40kBufferCAonly.shp"
# [input] future projection analysis region (zipped)
param$input$futureRegion <- list()
param$input$futureRegion$zip <- "input/DRECP_Bdy_20110128_40kBufferclipWhole.zip"
param$input$futureRegion$shp <- "DRECP_Bdy_20110128_40kBufferclipWhole.shp"
# [data] reprojected niche modeling analysis region
param$data$currentRegion.shp <- "data/currentRegion.shp"
# [data] reprojected future projection analysis region
param$data$futureRegion.shp <- "data/futureRegion.shp"
# [data] reprojected niche modeling analysis region SpatialPolygonsDataFrame R object
param$data$currentRegion <- "data/currentRegion.Rdata"
# [data] reprojected future projection analysis region SpatialPolygonsDataFrame R object
param$data$futureRegion <- "data/futureRegion.Rdata"


# identify CWHR codes
#####################
# CWHR
param$CWHR <- list()
param$CWHR$path <- "fragileInput/CWHR"
# lookup table
param$CWHR$lookup <- list()
param$CWHR$lookup$xls <- file.path(param$CWHR$path, "lookup.xls")
param$CWHR$lookup$sheet <- "SPECIES"
# manual crosswalks
param$CWHR$lookup$manual <- list()
param$CWHR$lookup$manual$"Phrynosoma blainvillii" <- "Phrynosoma coronatum"
param$CWHR$lookup$manual$"Xerospermophilus mohavensis" <- "Spermophilus mohavensis"
# [data] CWHR species/code crosswalk data.frame R object
param$data$CWHRLookup <- "data/CWHRLookup.Rdata"


# clip occurrences to analysis region
#####################################
# [output] clipped model species polygon occurrence data
param$output$clippedModSpeciesOccurrencePolygons.shp <- "output/clippedModSpeciesOccurrencePolygons.shp"
# [output] clipped model species point occurrence data
param$output$clippedModSpeciesOccurrencePoints.shp <- "output/clippedModSpeciesOccurrencePoints.shp"
# [data] clipped model species polygon occurrence data SpatialPolygonsDataFrame R object
param$data$clippedModSpeciesOccurrencePolygons <- "data/clippedModSpeciesOccurrencePolygons.Rdata"
# [data] clipped model species point occurrence data SpatialPointsDataFrame R object
param$data$clippedModSpeciesOccurrencePoints <- "data/clippedModSpeciesOccurrencePoints.Rdata"



# define per-species domains
############################
# CA Ecoregion Subregions
param$CAEcoregionSubregions <- list()
param$CAEcoregionSubregions$dataset <- "fragileInput/CA Ecoregion Subregions/EcoregionsCalifornia07_3.mdb"
param$CAEcoregionSubregions$layer <- "EcoregionsCalifornia07_3"
param$CAEcoregionSubregions$renameFields <- list()
param$CAEcoregionSubregions$renameFields$ECOREGION_ <- list(pattern = "ECOREGION_", replacement = "")
param$CAEcoregionSubregions$renameFields$SHAPE_ <- list(pattern = "SHAPE_", replacement = "")
# range maps
param$CWHR$rangeMaps <- list()
param$CWHR$rangeMaps$path <- file.path(param$CWHR$path, "CWHR_GIS2008")
# CWHR domain method buffer
param$CWHRMethodBuffer <- "10 kilometers"
# species modeled over the entire DRECP region
param$domainDRECP <- "Buteo swainsoni"
# [output] per-species domains
param$output$speciesDomains <- "output/speciesDomains"
# [data] per-species domains R SpatialPolygonsDataFrame object
param$data$speciesDomains <- "data/speciesDomains.Rdata"


# extract environmental layers
##############################
# [input] niche modeling layers (zipped)
param$input$currentEnvLayers <- "input/envLayers/PRISM_1971_2000.zip"
# [input] future projection layers (zipped)
param$input$futureEnvLayers <- list()
param$input$futureEnvLayers$GFDL_A2_2071_2100 <- "input/envLayers/GFDL_A2_2071_2100.zip"
param$input$futureEnvLayers$GFDL_B1_2071_2100 <- "input/envLayers/GFDL_B1_2071_2100.zip"
param$input$futureEnvLayers$PCM_A2_2070_2099 <- "input/envLayers/PCM_A2_2070_2099.zip"
param$input$futureEnvLayers$PCM_B1_2070_2099 <- "input/envLayers/PCM_B1_2070_2099.zip"
# paths within tempdir() to extract environmental layers to
param$tempdirCurrentEnvLayers <- "currentEnvLayers"
param$tempdirFutureEnvLayers <- "futureEnvLayers"


# generate environmental layer masks
####################################
# [data] niche modeling masks
param$data$currentMask.tif <- "data/currentMask.tif"
# [data] future projection mask
param$data$futureMask.tif <- "data/futureMask.tif"


# clip environmental layers
###########################
# paths within tempdir() to extract clipped environmental layers to
param$tempdirCurrentEnvLayersClipped <- "currentEnvLayersClipped"
param$tempdirFutureEnvLayersClipped <- "futureEnvLayersClipped"
# [input] environmental layer projection file
param$input$envLayerProjectionFile <- "input/NAD_1983_Albers.prj"


# convert environmental layers
##############################
# [data] paths to write clipped and converted environmental layers to
param$data$currentEnvLayers <- "data/currentEnvLayers"
param$data$futureEnvLayers <- "data/futureEnvLayers"


# generate polygon mask
#######################
# [data] identified pixels
param$data$envMask.tif <- "data/envMask.tif"
# [data] identified pixel polygons
param$data$envMaskPolys.shp <- "data/envMaskPolys.shp"


# extract NHD datasets
######################
# WBD
param$WBD <- list()
param$WBD$path <- "fragileInput/WBD"
param$WBD$pattern <- "\\.shp$"
# NHD
param$NHD <- list()
param$NHD$path <- "fragileInput/NHD"
# NHD layers
param$NHD$patterns <- list()
param$NHD$patterns$Point <- "nhd24kpt_p_[0-9]{8}\\.shp$"
param$NHD$patterns$Line <- "nhd24kli_l_[0-9]{8}\\.shp$"
param$NHD$patterns$Flowline <- "nhd24kst_l_[0-9]{8}\\.shp$"
param$NHD$patterns$Area <- "nhd24kar_a_[0-9]{8}\\.shp$"
param$NHD$patterns$Waterbody <- "nhd24kwb_a_[0-9]{8}\\.shp$"
# NHD extracts
param$NHD$extracts <- list()
# NHD extract: playa
param$NHD$extracts$NHD_playa <- 36100
# NHD extract: perennially wet features (points, lines, and areas, excluding unindicated)
param$NHD$extracts$NHD_perennial <- c(39004, 39009, 39010, 39011, 39012, 45800, 46006, 46602)
# [data] path to NHD extracts
param$data$NHDFeatureExtracts <- "data/NHDFeatureExtracts"


# rasterize NHD datasets
########################
# [output] NHD extracts
param$output$NHDExtracts.tif <- file.path("output/NHDExtracts", paste0(names(param$NHD$extracts), ".tif"))


# clip and convert NHD datasets
###############################


# generate water mask
#####################
# [data] water mask
param$data$waterMask.tif <- "data/waterMask.tif"


# identify NED30 tiles
######################
# NED30
param$NED30 <- list()
param$NED30$path <- "fragileInput/NED30"
param$NED30$pattern <- "(n(\\d*)w(\\d*))"
param$NED30$grid.pattern <- paste0("grd", param$NED30$pattern, "_1")
# [data] relevant NED30 tiles data.frame R object
param$data$NED30 <- "data/NED30.Rdata"
# the number of cells already buffering the NED dataset
param$NED30$existingBuffer <- 6


# generate NED30 tiles for solar radiation
##########################################
# the desired buffer size
param$NED30$solarRadBuffer <- 10
# path to temporary location of NED30 tiles buffered for solar radiation
param$tempdirSolarRadNED30Tiles <- "solarRadNED30Tiles"


# run solar radiation model
###########################
# solar radiation modeling
param$solarRad <- list()
param$solarRad$dates <- list()
param$solarRad$dates$winter <- list(start = 334, end = 59)
param$solarRad$dates$spring <- list(start = 60, end = 151)
param$solarRad$dates$summer <- list(start = 152, end = 243)
param$solarRad$diffuse_proportion <- 0.1
param$solarRad$transmittivity <- 0.8
param$solarRad$z_factor <- 0.00001100
# path to temporary location of buffered solar radiation tiles
param$tempdirBufferedSolarRadTiles <- "bufferedSolarRadTiles"


# clip solar radiation outputs
##############################
# [data] path to solar radiation tiles
param$data$solarRadTiles <- "data/solarRadTiles"


# aggregate solar radiation data
################################
# solar radiation aggregates
param$solarRad$aggregates <- list()
param$solarRad$aggregates$mean <- base::mean
param$solarRad$aggregates$min <- base::min
param$solarRad$aggregates$max <- base::max
# [output] solar radiation aggregates
param$output$solarRadAgg.tif <- file.path("output/solarRadAgg", paste0("sr_", outer(names(param$solarRad$dates), names(param$solarRad$aggregates), paste, sep = "_"), ".tif"))


# clip and convert solar radiation data
#######################################


# identify HydroSHEDS tiles
###########################
# HydroSHEDS
param$HydroSHEDS <- list()
param$HydroSHEDS$path <- "fragileInput/HydroSHEDS"
param$HydroSHEDS$pattern <- "(n(\\d*)w(\\d*))_dir"
# [data] relevant HydroSHEDS tiles data.frame R object
param$data$HydroSHEDS <- "data/HydroSHEDS.Rdata"


# HydroSHEDS flow accumulation
##############################
# [data] HydroSHEDS accumulated flow
param$data$HydroSHEDSFlowAcc.tif <- "data/HydroSHEDSFlowAcc.tif"


# aggregate HydroSHEDS accumulated flow
#######################################
# HydroSHEDS accumulated flow aggregates
param$HydroSHEDS$aggregates <- list()
param$HydroSHEDS$aggregates$mean <- base::mean
param$HydroSHEDS$aggregates$min <- base::min
param$HydroSHEDS$aggregates$max <- base::max
param$HydroSHEDS$aggregates$meanLog1 <- function(x) mean(log(x + 1))
param$HydroSHEDS$aggregates$minLog1 <- function(x) min(log(x + 1))
param$HydroSHEDS$aggregates$maxLog1 <- function(x) max(log(x + 1))
# [output] HydroSHEDS accumulated flow aggregates
param$output$HydroSHEDSFlowAccAgg.tif <- file.path("output/HydroSHEDSFlowAccAgg", paste0("fa_hs_", names(param$HydroSHEDS$aggregates), ".tif"))


# clip and convert HydroSHEDS accumulated flow
##############################################


# identify soil databases
#########################
# SSURGO
param$SSURGO <- list()
param$SSURGO$path <- "fragileInput/SSURGO"
param$SSURGO$pattern <- "^soil_(.*)$"
# SSURGO soil survey areas
param$SSURGO$ssa.shp <- file.path(param$SSURGO$path, "soilsa_a_nrcs.shp")
# STATSGO
param$STATSGO <- list()
param$STATSGO$path <- "fragileInput/STATSGO"
param$STATSGO$pattern <- "^gsmsoil_(.*)$"
# [data] intersecting STATSGO map units
param$data$intersectingSTATSGO.shp <- "data/intersectingSTATSGO.shp"
# [data] relevant soil datasets data.frame R object
param$data$soilDatasets <- "data/soilDatasets.Rdata"


# import soil data
##################
# SSURGO/STATSGO template database
param$soilTemplateDB.mdb <- file.path(param$SSURGO$path, "soildb_CA_2003_OSauto.mdb")
# [data] imported soil data tables list of data.frame R object
param$data$soilTables <- "data/soilTables.Rdata"


# soil calculations
###################
# soil parameters
param$soil <- list()
# muaggatt parameters (column name)
param$soil$muaggatt <- list()
param$soil$muaggatt$soil_aws025wta <- "aws025wta"
param$soil$muaggatt$soil_aws050wta <- "aws050wta"
param$soil$muaggatt$soil_aws0100wta <- "aws0100wta"
param$soil$muaggatt$soil_aws0150wta <- "aws0150wta"
# pH parameters (maximum depth in hzdep[tb]_r units, cm.)
param$soil$pH <- list()
param$soil$pH$soil_ph025wta <- 25
param$soil$pH$soil_ph050wta <- 50
param$soil$pH$soil_ph0100wta <- 100
param$soil$pH$soil_ph0150wta <- 150
# [data] soil map unit data data.frame R object
param$data$soilmuTable <- "data/soilmuTable.Rdata"


# burn SSURGO into STATSGO
##########################
# [data] SSURGO burned into STATSGO
param$data$soils.shp <- "data/soils.shp"


# rasterize soil data
#####################
# [output] rasterized soil data
param$output$soils.tif <- file.path("output/soils", paste0(unlist(lapply(param$soil, names)), ".tif"))


# clip and convert soil data
############################


# extract MRDS data
###################
# MRDS
param$MRDS <- list()
param$MRDS$path <- "fragileInput/MRDS"
# [data] abandoned mines
param$data$abandonedMines.shp <- "data/abandonedMines.shp"
# [data] abandoned mines R SpatialPointsDataFrame object
param$data$abandonedMines <- "data/abandonedMines.Rdata"


# rasterize MRDS data
#####################
# [output] rasterized abandoned mines
param$output$abandonedMines.tif <- "output/abandonedMines.tif"


# clip and convert MRDS data
############################


# prepare relief layer
######################
# [input] relief layer (zipped)
param$input$relief <- list()
param$input$relief$zip <- "input/envLayers/relief.zip"
param$input$relief$grid <- "relief270"
param$input$relief$prj <- "relief.prj"
# [output] topographic relief
param$output$relief.tif <- "output/relief.tif"


# clip and convert relief layer
###############################


# resample CWHR suitability
###########################
# [input] CWHR suitability (zipped)
param$input$CWHRSuitability <- list()
param$input$CWHRSuitability$zip <- "input/envLayers/CWHRSuitability.zip"
param$input$CWHRSuitability$pattern <- "^([abmr][0-9]{3})whrb?_(cl|drc)$"
# [data] relevant CWHR suitability datasets data.frame R object
param$data$CWHRSuitability <- "data/CWHRSuitability.Rdata"
# [output] CWHR suitability maps
param$output$CWHRSuitability <- "output/CWHRSuitability"


# clip and convert CWHR suitability
###################################


# resample vegmap dunes and playas
##################################
# [input] vegmap dunes (zipped)
param$input$vegmapDunes <- list()
param$input$vegmapDunes$zip <- "input/envLayers/dunes.zip"
param$input$vegmapDunes$tif <- "veg_dunes270.tif"
# [input] vegmap playas (zipped)
param$input$vegmapPlayas <- list()
param$input$vegmapPlayas$zip <- "input/envLayers/playas.zip"
param$input$vegmapPlayas$grid <- "veg_playa270"
# [output] vegmap dunes
param$output$vegmapDunes.tif <- "output/vegmapDunes.tif"
# [output] vegmap playas
param$output$vegmapPlayas.tif <- "output/vegmapPlayas.tif"


# clip and convert vegmap dunes and playas
##########################################


# merge playas layers
#####################
# [output] merged playas
param$output$playasMerged.tif <- "output/playasMerged.tif"


# clip and convert merged playas layers
#######################################


# generate species domain masks
###############################
# [data] relevant species domain masks data.frame R object
param$data$speciesMasks <- "data/speciesMasks.Rdata"
# [output] species domain mask path and prefix
param$output$speciesMasks <- "output/speciesMasks/m_"


# convert species domain masks
##############################


# prepare occurrence data
#########################
# [output] clipped model species polygon occurrence data intersected with environmental layers grid
param$output$clippedModSpeciesOccurrencePolygonsSplit.shp <- "output/clippedModSpeciesOccurrencePolygonsSplit.shp"
# [data] clipped model species polygon occurrence data intersected with environmental layers grid as a SpatialPolygonsDataFrame R object
param$data$clippedModSpeciesOccurrencePolygonsSplit <- "data/clippedModSpeciesOccurrencePolygonsSplit.Rdata"
# [output] Maxent occurrence data
param$output$occurrences <- "output/occurrences"
# [data] Maxent occurrence data data.frame R object
param$data$occurrences <- "data/occurrences.Rdata"


# prepare environmental layers lists
####################################
# [input] environmental layers to use for each species
param$input$speciesEnvVars <- list()
param$input$speciesEnvVars$xlsx <- "input/ucsb_cec_spp_envars.xlsx"
param$input$speciesEnvVars$sheet <- "model inputs"
# headers to manually rename
param$input$speciesEnvVars$headerNames <- list()
param$input$speciesEnvVars$headerNames$"*###whr270_cl" <- "CWHR"
param$input$speciesEnvVars$headerNames$"*###whr270_drc" <- "CWHR buffered range"
param$input$speciesEnvVars$headerNames$F7 <- "Hydrology"
param$input$speciesEnvVars$headerNames$"solar radiation" <- "win_rad"
param$input$speciesEnvVars$headerNames$F16 <- "spr_rad"
param$input$speciesEnvVars$headerNames$F17 <- "sum_rad"
# correspond headers to environmental layers
param$input$speciesEnvVars$envLayers <- list()
param$input$speciesEnvVars$envLayers$Hydrology <- c("fa_hs_maxLog1", "NHD_perennial")
param$input$speciesEnvVars$envLayers$Soil <- c("ca_fc0_v3", "ca_por0_v3", "ca_thck6_v3", "ca_wp0_v3", "soil_aws050wta", "soil_ph050wta")
param$input$speciesEnvVars$envLayers$"temp seasonality" <- "bio_4"
param$input$speciesEnvVars$envLayers$"maxt warmest mo" <- "bio_5"
param$input$speciesEnvVars$envLayers$"mint coldest mo" <- "bio_6"
param$input$speciesEnvVars$envLayers$"ann ppt" <- "bio_12"
param$input$speciesEnvVars$envLayers$"ppt warm qtr" <- "bio_18"
param$input$speciesEnvVars$envLayers$GDD5 <- "bio_20"
param$input$speciesEnvVars$envLayers$win_rad <- "sr_spring_mean"
param$input$speciesEnvVars$envLayers$spr_rad <- "sr_spring_mean"
param$input$speciesEnvVars$envLayers$sum_rad <- "sr_spring_mean"
param$input$speciesEnvVars$envLayers$CWD <- "bio_24"
param$input$speciesEnvVars$envLayers$relief <- "relief"
param$input$speciesEnvVars$envLayers$dune <- "vegmapDunes"
param$input$speciesEnvVars$envLayers$playa <- "playasMerged"
# [data] table of environmental variables to use to model each species data.frame R object
param$data$envVars <- "data/envVars.Rdata"
# [data] lists of environmental layers to use to model each species R object
param$data$envLayers <- "data/envLayers.Rdata"


# prepare Maxent commands
#########################
# [data] Maxent outputs
param$data$maxentOutputs <- "data/maxentOutputs"
# Java memory limit when running Maxent
param$javaMaxentMemory <- "4g"
# number of concurrent Maxent instances
param$maxentThreads <- "max"
# Maxent parameters
param$maxent <- list()
param$maxent$environmentallayers <- param$data$currentEnvLayers
param$maxent$askoverwrite <- "false"
param$maxent$jackknife <- "true"
param$maxent$autorun <- "true"
param$maxent$responsecurves <- "true"
param$maxent$randomtestpoints <- 30
param$maxent$replicates <- 10
param$maxent$replicatetype <- "Bootstrap"
param$maxent$randomseed <- "true"
param$maxent$visible <- "true"
param$maxent$warnings <- "false"
param$maxent$writeclampgrid <- "false"
param$maxent$writemess <- "false"
param$maxent$outputgrids <- "false"
param$maxent$outputfiletype <- "mxe"
param$maxent$prefixes <- "false"
# flag certain environmental layers as being categorical
param$maxent$togglelayertype <- c("abandonedMines", names(param$NHD$extracts), "vegmapDunes", "vegmapPlayas", "playasMerged")
# disable all environmental layers
param$maxent$togglelayerselected <- "\"\""
# [data] Maxent calls
param$data$maxentCalls <- "data/maxentCalls.Rdata"
# [data] per-species output directories
param$data$maxentSpeciesOutputs <- "data/maxentSpeciesOutputs.Rdata"


# run Maxent
############


# extract Maxent summary
########################
# [data] maxentResults summary data.frame R object
param$data$maxentResultsSummary <- "data/maxentResultsSummary.Rdata"
# [output] Maxent summary
param$output$maxentSummary.csv <- "output/maxentSummary.csv"


# convert projected grids
#########################
# [output] Maxent projected grids
param$output$maxentProjGrids <- "output/maxentProjGrids"


# threshold projected grids
###########################
# [output] thresholded Maxent projections
param$output$maxentBinProj <- "output/maxentBinProj"


# top contributing variables tables
###################################
# [data] top contributing variables tables as list of data.frame R object
param$data$maxentTopVarsTables <- "data/maxentTopVarsTables.Rdata"
# [output] top contributing variables tables
param$output$maxentTopVarsTables <- "output/maxentTopVarsTables"


# presentable maps of Maxent projections
########################################
# [input] projections map file
param$input$projections.mxd <- "input/projections.mxd"
# [output] projections maps
param$output$maxentProjMaps <- "output/maxentProjMaps"


# presentable maps of binarized Maxent projections
##################################################
# [input] binarized projections map file
param$input$binarized.mxd <- "input/binarized.mxd"
# [output] binarized projections maps
param$output$maxentBinMaps <- "output/maxentBinMaps"


# export environmental layers
#############################
# [export] utilized niche modeling environmental layers
param$export$currentEnvLayers <- "export/currentEnvLayers"


# export Maxent projections
###########################
# [export] continuous Maxent projections
param$export$maxentProjGrids <- "export/maxentProjGrids"
# [export] binarized Maxent projections
param$export$maxentBinProj <- "export/maxentBinProj"


# export
########
save(param, file = "data/param.Rdata")
