# This script unzips vegbank plot data files to state level directories
# It then loops through directories and concatenates all files of the same name
# e.g. "observation_contributor.csv","plot_community.csv","plot_env.csv","plot_taxa.csv","stems.csv","strata.csv"
# Files are then written to disk

# Set working directory
wd <- "C:/Share/LCC-VP/RangeWide/Vegbank/"
setwd(wd)

# List zip files
zlist <- list.files(pattern="zip")
ulfun <- function(x) {
  # Get state code
  nmz <- rev(unlist(strsplit(unlist(strsplit(x,"\\."))[1],"_")))[1]
  # Unzip to state directory
  unzip(x,junkpaths=T,exdir=nmz)  
}

# Run unzip function
zips <- lapply(zlist,ulfun)

# Concatenate tables and write to files
alist <- lapply(list.files(pattern="observation_contributor",recursive=T,full.names=T),read.csv,stringsAsFactors=FALSE)
observation.contributor <- do.call("rbind",alist)
write.csv(observation.contributor,file="observation_contributor_all.csv")
alist <- lapply(list.files(pattern="plot_community",recursive=T,full.names=T),read.csv,stringsAsFactors=FALSE)
plot.community <- do.call("rbind",alist)
write.csv(plot.community,file="plot_community_all.csv")
alist <- lapply(list.files(pattern="plot_env",recursive=T,full.names=T),read.csv,stringsAsFactors=FALSE)
plot.env <- do.call("rbind",alist)
write.csv(plot.env,file="plot_env_all.csv")
alist <- lapply(list.files(pattern="plot_taxa",recursive=T,full.names=T),read.csv,stringsAsFactors=FALSE)
plot.taxa <- do.call("rbind",alist)
write.csv(plot.taxa,file="plot_taxa_all.csv")
alist <- lapply(list.files(pattern="stems",recursive=T,full.names=T),read.csv,stringsAsFactors=FALSE)
stems <- do.call("rbind",alist)
write.csv(stems,file="stems_all.csv")
alist <- lapply(list.files(pattern="strata",recursive=T,full.names=T),read.csv,stringsAsFactors=FALSE)
strata <- do.call("rbind",alist)
write.csv(strata,file="strata_all.csv")

