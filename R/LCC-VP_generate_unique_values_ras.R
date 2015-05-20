#Load source files:
#parameter file for input and output directories
source("C:/Share/LCC-VP/scripts/R/LCC-VP_parameters.R")
#Reads in variables park, existingRas, and uni.outdir

#Load functions
source("C:/Share/LCC-VP/scripts/R/LCC-VP_functions.R")

# redirect output to file (for selected variables) - FIX: sinking to log file
#is not appending - just overwriting. Not sure why.
#For now, decided to just write unique log file each time this script is run. 
#Be careful not to let logs pile up. 
date <- gsub(":", "", gsub(" ", "_", date()))
log.file <- file(paste(uni.outdir, unlist(strsplit(basename(existingRas), "\\."))[1],"_", date, "_uniqueRas_log.txt",sep=""))
sink(log.file)
sink(log.file, type="message")
print("*********************************************************************")
print("******** LCC-VP_generate_unique_values_ras.R Log ********")
print("*********************************************************************")

print(date)
print(park)
print(paste("existingRas = ", existingRas, sep=""))
print(paste("uni.outdir = ", uni.outdir, sep=""))


#execute uniqueRas function to generate raster of unique values
uniRas <- uniqueRas(existingRas, uni.outdir)

#Restore message to console
sink()
sink(type="message")
