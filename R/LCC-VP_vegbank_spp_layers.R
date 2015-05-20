
# Load packages
require(rgdal)
require(data.table)

# Read in VegBank points
vbank.pts <- readOGR("C:/Share/LCC-VP/RangeWide/Vegbank","vbank_pts")
names(vbank.pts@data)[names(vbank.pts@data) == "obsrvt_"] <- "observation_id"

# Read in taxa table
vbank.taxa <- fread("C:/Share/LCC-VP/RangeWide/Vegbank/plot_taxa_all.csv")

# Join taxa table to points data, subsetting taxa by a particular species
# Use the USDA PLANTS database code for species references

# Sugar maple - ACSA3
acsa3 <- vbank.taxa[vbank.taxa$currenttaxoninterp_usdacode=="ACSA3",]
vbank.pts@data$acsa3 <- as.numeric(vbank.pts$observation_id %in% acsa3$observation_id)
# Write to file
writeOGR(vbank.pts,"C:/Share/LCC-VP/RangeWide/Vegbank","acsa3_pts",driver="ESRI Shapefile")
