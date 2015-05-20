require(OSfun)
require(RODBC)

# load general parameters
load("data/param.Rdata")



# load table of relevant soil datasets
load(param$data$soilDatasets)


# soil tables
tables.txt <- list()
tables.txt$chorizon <- file.path(soilDatasets$db, "tabular", "chorizon.txt")
tables.txt$component <- file.path(soilDatasets$db, "tabular", "comp.txt")
tables.txt$mapunit <- file.path(soilDatasets$db, "tabular", "mapunit.txt")
tables.txt$muaggatt <- file.path(soilDatasets$db, "tabular", "muaggatt.txt")

# import tables from template database
templates <- list()
templates$chorizon <- read.mdb(param$soilTemplateDB.mdb, "chorizon")
templates$component <- read.mdb(param$soilTemplateDB.mdb, "component")
templates$mapunit <- read.mdb(param$soilTemplateDB.mdb, "mapunit")
templates$muaggatt <- read.mdb(param$soilTemplateDB.mdb, "muaggatt")

# primary keys
pkeys <- list()
pkeys$chorizon <- "chkey"
pkeys$component <- "cokey"
pkeys$mapunit <- "mukey"
pkeys$muaggatt <- "mukey"

# read soil data tables
soilTables <- mapply(function(template, tables.txt, pkey) {
	tables <- lapply(tables.txt, read.table, sep = "|", col.names = names(template), colClasses = sapply(template, class))
	tables <- do.call(rbind, tables)
	uniqueKeys <- unique(tables[[pkey]])
	tables[match(uniqueKeys, tables[[pkey]]), ]
}, templates, tables.txt, pkeys, SIMPLIFY = FALSE)

# export imported soil data tables
save(soilTables, file = param$data$soilTables)
