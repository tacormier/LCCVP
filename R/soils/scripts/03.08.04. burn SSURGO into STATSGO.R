require(OSfun)
require(foreign)

# load general parameters
load("data/param.Rdata")



# load relevant soil datasets
load(param$data$soilDatasets)
# load soil map unit data
load(param$data$soilmuTable)

# switching separators makes the python code simpler
tempdir <- gsub("\\\\", "/", tempdir())



# relevant SSURGO datasets
SSURGO <- subset(soilDatasets, source == "SSURGO")
# SSURGO map units
SSURGO.shp <- file.path(SSURGO$db, "spatial", paste0("soilmu_a_", SSURGO$name, ".shp"))
# merged SSURGO map units
mergedSSURGO.shp <- file.path(tempdir, "mergedSSURGO.shp")
# clipped SSURGO map units
clippedSSURGO.shp <- file.path(tempdir, "clippedSSURGO.shp")
# subset SSURGO map units with tabular data
subsetSSURGO.shp <- file.path(tempdir, "subsetSSURGO.shp")

# merge and subset SSURGO
script.py <- file.path(tempdir(), "SSURGO.py")
pycmd <- c(
	"import arcpy, sys", 
	"arcpy.env.overwriteOutput = True", 
	"", 
	"try: ", 
	paste0("\tarcpy.Merge_management([", paste0("\"", SSURGO.shp, "\"", collapse = ", "), "], \"", mergedSSURGO.shp, "\")"), 
	paste0("\tarcpy.Clip_analysis(\"", mergedSSURGO.shp, "\", \"", param$data$futureRegion.shp, "\", \"", clippedSSURGO.shp, "\")"), 
	paste0("\tarcpy.MakeFeatureLayer_management(\"", clippedSSURGO.shp, "\", \"clippedSSURGO\", \"\"\" \"MUSYM\" <> 'NOTCOM' \"\"\")"), 
	paste0("\tarcpy.CopyFeatures_management(\"clippedSSURGO\", \"", subsetSSURGO.shp, "\")"), 
	"except: ", 
	"\tprint arcpy.GetMessages()", 
	"\tsys.exit(1)", 
	""
)
writeLines(pycmd, script.py)
stopifnot(0 == system(paste("python", script.py)))

# extract attribute tables field names
subsetSSURGONames <- names(read.shp(subsetSSURGO.shp, attrOnly = TRUE))
intersectingSTATSGONames <- names(read.shp(param$data$intersectingSTATSGO.shp, attrOnly = TRUE))
# generate field_info tables
auto_field_info <- c("FID", "Shape")
auto_field_info <- data.frame(FieldName = auto_field_info, NewFieldName = auto_field_info, Visible = "HIDDEN", "Use Ratio Policy" = "NONE", stringsAsFactors = FALSE, check.names = FALSE)
subsetSSURGO_field_info <- data.frame(FieldName = subsetSSURGONames, NewFieldName = subsetSSURGONames, Visible = "HIDDEN", "Use Ratio Policy" = "NONE", stringsAsFactors = FALSE, check.names = FALSE)
subsetSSURGO_field_info <- rbind(auto_field_info, subsetSSURGO_field_info)
intersectingSTATSGO_field_info <- data.frame(FieldName = intersectingSTATSGONames, NewFieldName = intersectingSTATSGONames, Visible = "HIDDEN", "Use Ratio Policy" = "NONE", stringsAsFactors = FALSE, check.names = FALSE)
intersectingSTATSGO_field_info <- rbind(auto_field_info, intersectingSTATSGO_field_info)
# rename MUKEYs and set visibility
subsetSSURGO_field_info[subsetSSURGO_field_info$FieldName == "MUKEY", c("NewFieldName", "Visible")] <- c("SSUR_MUK", "VISIBLE")
intersectingSTATSGO_field_info[intersectingSTATSGO_field_info$FieldName == "MUKEY", c("NewFieldName", "Visible")] <- c("STATS_MUK", "VISIBLE")
# flatten
subsetSSURGO_field_info <- do.call(paste, c(subsetSSURGO_field_info, list(collapse = ";")))
intersectingSTATSGO_field_info <- do.call(paste, c(intersectingSTATSGO_field_info, list(collapse = ";")))


# burn SSURGO into STATSGO
script.py <- file.path(tempdir(), "SSURGO+STATSGO.py")
pycmd <- c(
	"import arcpy, sys", 
	"arcpy.env.overwriteOutput = True", 
	"", 
	"try: ", 
	paste0("\tarcpy.MakeFeatureLayer_management(\"", subsetSSURGO.shp, "\", \"subsetSSURGO\", \"#\", \"#\", \"", subsetSSURGO_field_info, "\")"), 
	paste0("\tarcpy.MakeFeatureLayer_management(\"", param$data$intersectingSTATSGO.shp, "\", \"intersectingSTATSGO\", \"#\", \"#\", \"", intersectingSTATSGO_field_info, "\")"), 
	paste0("\tarcpy.Union_analysis([\"subsetSSURGO\", \"intersectingSTATSGO\"], \"", param$data$soils.shp, "\", \"NO_FID\")"), 
	"except: ", 
	"\tprint arcpy.GetMessages()", 
	"\tsys.exit(1)", 
	""
)
writeLines(pycmd, script.py)
stopifnot(0 == system(paste("python", script.py)))
