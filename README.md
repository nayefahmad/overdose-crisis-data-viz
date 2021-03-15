# Overdose crisis data visualization 

## Overview
The goal of this project is to develop an interactive online visualization tool to allow users to understand the various legal, health, and social policies internationally that have been taken to address the overdose crisis. 

The project is based on the work of a research team led by Dr. Jane Buxton, Professor of the School of Population and Public Health, University of British Columbia (UBC) and Medical Director of Harm Reduction, BC Centre for Disease Control. The research team conducted the literature search, data extraction, and data analysis. 

The results of the work can be explored on [this interactive website](https://meenrz.shinyapps.io/overdose-crisis-data-viz_shiny-app/_w_b9597691/_w_2401cab7/). The code for building the interactive visualization is hosted in a separate repository, [here](https://github.com/nayefahmad/overdose-crisis-data-viz_shiny-app). 

## Repository structure
1. **data**: this folder contains the original data collected by the research team 
2. **src**: this folder contains the code that transforms the original data into the format that is used in the visualization 
    1. `functions.R`: this file contains the main functions that are used to transform the data   
    2. `interactive.R`: this file is for allowing users to interactively run the functions in the file `functions.R`. It is primarily for testing or exploratory analysis. The main pipeline is run via the file `make.R`. 
    3. `make.R`: this file defines an automated pipeline for taking in the original data, running the functions in `functions.R` in the correct order, and saving the outputs in the **dst** folder. When the file is run, the pipeline will first check whether there have been any changes in the data or the functions; if any changes were made, then the appropriate parts of the pipeline will be rerun. This makes it possible to check that all the output files are up to date with the code, without having to always rerun the whole pipeline.  
3. **dst**: this folder contains the output data that is used in the visualization. The main outptut file is `05_join_pop_mod_cor_file.csv`

## Making changes to the data processing functions
If you want to make changes to the data processing that was done in this repository, and ensure that those changes are reflected on the interactive website, here are the steps you need to take: 

### 1. Changes in data processing repository 
1.  Download the latest version of this repository. [This page](https://www.itprotoday.com/mobile-management-and-security/how-do-i-download-files-github) has instructions on how to download repositories from GitHub.
2. Once you have downloaded the data processing repository, make any changes locally in RStudio. Then run the code and save the output files. You must save the new data in a publicly accessible location, such as GitHub, in order for the Shiny app to be able to read it in. For example, the current output is saved [here](https://raw.githubusercontent.com/nayefahmad/overdose-crisis-data-viz/master/dst/05_join_pop_mod_cor_file.csv). 

### 2. Changes in Shiny app repository 
1. Download the latest version of the repository for building the Shiny app, which is located [here](https://github.com/nayefahmad/overdose-crisis-data-viz_shiny-app). 
2. The shiny app reads in a single file that is an output from the data processing repository - currently, this file is named `05_join_pop_mod_cor_file.csv` and it is saved in the data processing repo [here](https://raw.githubusercontent.com/nayefahmad/overdose-crisis-data-viz/master/dst/05_join_pop_mod_cor_file.csv). If you have made changes in the data processing repo, then you will have to change the code in the `data_preprocessing.R` file to read in the new data. Specifically, you have to change line 20. 
3. Once you've read in the new data, click the "Run App" button in RStudio. This will launch the app on your computer, but will not change the live version of the app.
4. When the app runs locally in RStudio, there will be an option to publish the app in the top right corner of the window. Click that button, and you will be asked to log in to a shinyapps.io account. After logging in to the account you want to use, you can re-publish the app. This will ensure that your changes are reflected in the live version of the app.





