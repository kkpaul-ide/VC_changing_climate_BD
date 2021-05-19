# Vectorial capacity of Aedes aegypti mosquitoes in Bangladesh

This repository contains all the code and processed climate data (except observed weather station data) to calculate vectorial capacity of Aedes aegypti mosquitoes in Bangladesh for the period 1950-2099. 

[![DOI](https://zenodo.org/badge/365786828.svg)](https://zenodo.org/badge/latestdoi/365786828)

The results are reported in the manuscript: KK Paul et. al., *Dengue transmission risk in a changing climate: Bangladesh could experience a longer dengue fever season in the future* submitted to the journal of Environmental Research Letters (ERL).

All codes were created and run using R statistical software version 4.0.2 and Rstudio version 1.3.1056 with associated packages (ncdf4 1.17; tidyverse 1.3.0; reshape2 1.4.4; raster 3.3-7; RColorBrewer 1.1-2; grid 4.0.2; rgdal 1.5-12; cowplot 1.1.1). Some of the data files required to run the scripts are not publicly available and have been excluded from the repository (but can be provided on request). Please report an issue if you have any problems running the code or contact either Kpaul@kirby.unsw.edu.au or Rgray@kirby.unsw.edu.au.

# Aims

The aim of this study was to understand future dengue epidemic potential given changes in climate of Bangladesh. 

# Contributors

The main developers for the project code are Dr Kishor K. Paul and Dr Richard T. Gray who are in the Surveillance, Evaluation and Research Program (SERP) at the The Kirby Institute, UNSW Sydney, Sydney, NSW, Australia. Kishor Paul was the primarily developer of the data analysis and calculation scripts under the supervision of Dr Gray. Both maintain the repository.

# Project organization

*Main directory sub-directories*

## code
Contains specific R functions and scripts used for data processing, VC calculation script, reshaping ISIMIP netCDF data files to run VC calculation script.

## data
Contains some raw data used in the analysis. Temperature output of ISIMIP models are available as netCDF files at https://www.isimip.org/outputdata/isimip-data-on-the-esgf-server/ . Data for weather station locations need to be extraced first with the help of Climate Data Operator (CDO) (https://doi.org/10.5281/zenodo.3539275), a collection of command line operators, to be able to use user defined functions and analysis scripts. Subnational level shape files necessary to create maps of Bangladesh are available from https://data.humdata.org/dataset/administrative-boundaries-of-bangladesh-as-of-2015

*isimip_data_station.R
Script to compare ISIMIP data with observed temperature data for validity checking, calculate VC and create plots with ISIMIP data

*plot_maps_contour.R
Script to plot background map, VC change map and calculate VC for a range of temp and dtr to create contour plot. 

*descriptive.R
Script to extract descriptive stats from observed and ISIMIP data.

*observed_data.R
Script to calculate VC with observed temperature data and associated plots.

