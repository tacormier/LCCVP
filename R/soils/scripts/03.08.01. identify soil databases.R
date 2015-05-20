require(OSfun)
require(foreign)

# load general parameters
load("data/param.Rdata")



# switching separators makes the python code simpler
tempdir <- gsub("\\\\", "/", tempdir())



# all SSURGO soil survey areas
SSURGO <- data.frame(db = dir(param$SSURGO$path, param$SSURGO$pattern, full.names = TRUE), stringsAsFactors = FALSE)
SSURGO$name <- sub(param$SSURGO$pattern, "\\1", basename(SSURGO$db))
SSURGO$source <- "SSURGO"

# all STATSGO states
STATSGO <- data.frame(db = dir(param$STATSGO$path, param$STATSGO$pattern, full.names = TRUE), stringsAsFactors = FALSE)
STATSGO$name <- sub(param$STATSGO$pattern, "\\1", basename(STATSGO$db))
STATSGO$source <- "STATSGO"


# determine intersecting SSURGO soil survey areas
#################################################
# intersecting soil survey areas
intersectingSSURGO.shp <- file.path(tempdir, "intersectingSSURGO.shp")
# determine intersecting soil survey areas
script.py <- file.path(tempdir(), "intersectBdySSURGO.py")
pycmd <- c(
	"import arcpy, sys", 
	"arcpy.env.overwriteOutput = True", 
	"", 
	"try: ", 
	paste0("\tarcpy.Clip_analysis(\"", param$SSURGO$ssa.shp, "\", \"", param$data$futureRegion.shp, "\", \"", intersectingSSURGO.shp, "\")"), 
	"except: ", 
	"\tprint arcpy.GetMessages()", 
	"\tsys.exit(1)", 
	""
)
writeLines(pycmd, script.py)
stopifnot(0 == system(paste("python", script.py)))
# extracting intersecting soil survey area names
intersectingSSURGO <- as.character(read.shp(intersectingSSURGO.shp, attrOnly = TRUE)$AREASYMBOL)
# determine corresponding soil survey areas
SSURGO <- subset(SSURGO, toupper(name) %in% intersectingSSURGO)


# determine intersecting STATSGO states
#######################################
# STATSGO mapunit shapefiles
muSTATSGO.shp <- file.path(STATSGO$db, "spatial", paste0("gsmsoilmu_a_", STATSGO$name, ".shp")) # intersecting STATSGO mapunits
# path to temporary location of intersecting STATSGO map units
tempdirIntersectingSTATSGO.shp <- file.path(tempdir, "intersectingSTATSGO", paste0(STATSGO$name, ".shp"))
dir.create(unique(dirname(tempdirIntersectingSTATSGO.shp)), FALSE, TRUE)
# determine intersecting soil survey areas
script.py <- file.path(tempdir(), "intersectBdySTATSGO.py")
pycmd <- c(
	"import arcpy, sys", 
	"arcpy.env.overwriteOutput = True", 
	"", 
	"try: ", 
	paste0("\tarcpy.Clip_analysis(\"", muSTATSGO.shp, "\", \"", param$data$futureRegion.shp, "\", \"", tempdirIntersectingSTATSGO.shp, "\")"), 
	paste0("\tarcpy.Merge_management([", paste0("\"", tempdirIntersectingSTATSGO.shp, "\"", collapse = ", "), "], \"", param$data$intersectingSTATSGO.shp, "\")"), 
	"except: ", 
	"\tprint arcpy.GetMessages()", 
	"\tsys.exit(1)", 
	""
)
writeLines(pycmd, script.py)
stopifnot(0 == system(paste("python", script.py)))
# number of intersecting mapunits for each STATSGO state dataset
intersectingSTATSGO <- sapply(lapply(tempdirIntersectingSTATSGO.shp, read.shp, attrOnly = TRUE), nrow)
# determine intersecting STATSGO states
STATSGO <- STATSGO[intersectingSTATSGO > 0, ]



# merge STATSGO and SSURGO
soilDatasets <- rbind(SSURGO, STATSGO)
# export list of relevant soil datasets
save(soilDatasets, file = param$data$soilDatasets)
