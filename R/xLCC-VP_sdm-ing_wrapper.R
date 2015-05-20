#source parameter file for input and output directories
source("C:/Share/LCC-VP/scripts/R/LCC-VP_parameters.R")


# redirect output to file (for selected variables) - FIX: sinking to log file
#is not appending - just overwriting. Not sure why.
#For now, decided to just write unique log file each time this script is run. 
#Be careful not to let logs pile up. 
date <- gsub(":", "", gsub(" ", "_", date()))
log.file <- file(paste(path,"modeling", date, "_log.txt",sep=""))
sink(log.file)
sink(log.file, type="message")
print("*********************************************************************")
print("******** LCC-VP_sdm-ing_wrapper.R Log ********")
print("*********************************************************************")

print(date)
print(park)
print (res.img)
print(v.stk)
print(map)
print(outDir)
print(mod.name)

result_filename <- paste(outDir, mod.name, "_R_out.txt", sep="")

line1 <- paste("res.img <- ", "\"", res.img,"\"", sep="")
line2 <- paste("v.stk <- ", "\"", v.stk,"\"", sep="")
line3 <- paste("map <- ", "\"", map, "\"", sep="")
line4 <- paste("outDir <- ", "\"", outDir, "\"", sep="")
line5 <- paste("mod.name <- ", "\"", mod.name, "\"", sep="")

lines <- c(line1, line2, line3, line4, line5)
paramsFile <- paste(dirname(res.img), "/", mod.name, "_R_params.txt", sep="")
write.table(lines, file=paramsFile, quote=F, sep="\n", col.names=F, row.names=F)

cmd = "C:\\Program Files\\R\\R-2.15.1\\bin\\R.exe --vanilla --args " + paramsFile + " < " + 
  script_filename + " > "+ result_filename

x <- system(paste("C:\\Program Files\\R\\R-2.15.2\\bin\\R.exe --vanilla --args ", paramsFile, 
                  " < C:/Share/LCC-VP/scripts/R/LCC-VP_sdm-ing.R > ", result_filename, sep="")) 

x <- system(paste("C:\\Program Files\\R\\R-2.15.2\\bin\\R.exe C:/Share/LCC-VP/scripts/R/LCC-VP_sdm-ing.R ", paramsFile, " > ", result_filename, sep=""))

if (x != 0) stop("R script was not called successfully.")