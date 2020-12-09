

/* 
Ayse Imrohoroglu and Selale Tuzel, 6/27/2015

This SAS program takes Compustat "FUNDA" and "NAMES" SAS datafiles and prepares inputs for the TFPCompute_ImrohorogluTuzel.do Stata program.
FUNDA and NAMES can be downloaded from WRDS via FTP from the directory: /mnt/comp.wrds3/sasdata/naa.

*/

libname maindata '/project/econdept/pak/firmLevelData/TFP/YourWRDSDataFolder';


data compustat_data(keep= fyear gvkey PPEGT AT SALE OIBDP DP EMP CAPX DPACT  ); 
set maindata.funda; 
where DATAFMT='STD' and POPSRC='D' and CONSOL='C' and INDFMT='INDL' and CURCD='USD';
run;

proc export data=compustat_data outfile="compustat_data.dta";
run;

data compustat_names(keep= gvkey sic naics year1 year2); 
set maindata.names; 
run;

proc export data=compustat_names outfile="compustat_names.dta";
run;


