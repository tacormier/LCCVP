#!/bin/bash

#User set variables here
#Output directory in which to store text files with var lists
text_dir="/mnt/arctic/share/LCC-VP/Parks/GRSM/analysis/common_inputs/forecast_var_lists/"

#file root name - will have year appended - leave off file extension 
file_root="GRSM_var_files_ALLvars_forecast_"

#Enter start year
start=2021
#Enter end year
end=2086

########################



for i in $(seq $start $end)	
do
	$(echo ${i})
	file=${file_root}{i}".txt"
	
	
done	
 
