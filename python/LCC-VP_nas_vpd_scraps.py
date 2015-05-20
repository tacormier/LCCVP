# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

prism_vpd_east_2005_08.nc

earcp85_vpd_east_new_2032_07.nc

prism_vpd_east_${x}_${y}.nc earcp85_vpd_east_cls_${x}_${y}.nc

flistold = ["earcp85_vpd_east_new_"+str(x)+"_"+y+".nc" for x in range(1982,2036,1) for y in ["01","02","03","04","05","06","07","08","09","10","11","12"]]
flistnew = ["earcp85_vpd_east_new_"+y+"_"+str(x)+".nc" for x in range(1982,2036,1) for y in ["01","02","03","04","05","06","07","08","09","10","11","12"]]

flistold = ["earcp85_vpd_east_new_"+str(x)+"_"+y+".nc" for x in range(2006,2036,1) for y in ["01","02","03","04","05","06","07","08","09","10","11","12"]]
flistnew = ["earcp85_vpd_east_new_"+y+"_"+str(x)+".nc" for x in range(2006,2036,1) for y in ["01","02","03","04","05","06","07","08","09","10","11","12"]]

import os
os.getcwd()

os.chdir('//ARCTIC/C_Drive/Share/LCC-VP/TOPS/EAST/Projections')

flist = os.listdir('.')

[os.rename(i,i.replace("_mam","").replace(".nc","_mam.nc")) for i in flist if "earcp85_vpd_east_mam" in i]

[os.rename("earcp85_vpd_east_new_" + str(i) + "_" + j + ".nc","earcp85_vpd_east_new_" + j + "_" + str(i) + ".nc") for i in range(2036,2079,1) for j in ["01","02","03","04","05","06","07","08","09","10","11","12"]]