# Overdose crisis data visualization 

2020-04-10

## Overview
The goal of this project is to develop an interactive online visualization tool to allow users to understand the various legal, health, and social policies internationally that have been taken to address the overdose crisis. 

The project is based on the work of a research team led by Dr. Jane Buxton, Professor of the School of Population and Public Health, University of British Columbia (UBC) and Medical Director of Harm Reduction, BC Centre for Disease Control. The research team conducted the literature search, data extraction, and data analysis. 

The resuls of the work can be explored on [this interactive website](https://meenrz.shinyapps.io/overdose-crisis-data-viz_shiny-app/_w_b9597691/_w_2401cab7/). The code for building the interactive visualization is hosted in a separate repository, [here](https://github.com/nayefahmad/overdose-crisis-data-viz_shiny-app)

## Repository structure
1. **data**: this folder contains the original data collected by the research team 
2. **src**: this folder contains the code that transforms the original data into the format that is used in the visualization 
    1. `functions.R`: this file contains the main functions that are used to transform the data   
    2. `interactive.R`: this file is for allowing users to interactively run the functions in the file `functions.R`. It is primarily for testing or exploratory analysis. The main pipeline is run via the file `make.R`. 
    3. `make.R`: this file defines an automated pipeline for taking in the original data, running the functions in `functions.R` in the correct order, and saving the outputs in the **dst** folder. When the file is run, the pipeline will first check whether there have been any changes in the data or the functions; if any changes were made, then the appropriate parts of the pipeline will be rerun. This makes it possible to check that all the output files are up to date with the code, without having to always rerun the whole pipeline.  
3. **dst**: this folder contains the output data that is used in the visualization. The main outptut file is `05_join_pop_mod_cor_file.csv`



