#script you want to run
script <- "C:/Share/LCC-VP/scripts/R/process_biovar_forecasts_yearly.R"

#Log file
log.file <- file("C:/Share/LCC-VP/PACE/GPE/climate_data/forecasts/ave_30yr/biovars/calc_30yr_biovars_log.txt")

#redirect input and errors to log file
sink(log.file, append=TRUE)
sink(log.file, append=TRUE, type="message")

#call desired script and write output to logfile
Source(script, echo=T, max.deparse.length=10000)

#restore output to console
sink()
sink(type="message")

#Look at log file
cat(readLines(log.file), sep="\n")