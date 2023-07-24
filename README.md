# Minimum Inhibitory Concentration Analysis by Group (MICAG) Data Challenge

This repo contains the tool for looking at MICAG, as well as all the code to reproduce the analysis in the Data Challenge report. 

Main scripts are: 

DATA CHALLENGE SPECIFIC
- 0_preparation_data_cleaning.R : This cleans and synthesises the datasets used in the Data Challenge. Must be run first in order to recreate the analysis.

GENERIC TOOL
- 1a_TOOL_screening_plots.R : Creates MIC distribution plots by Group for specified characteristics and bacteria/antibiotic combinations.
- 1b_TOOL_screening_plots_time.R : As above, but includes the extra dimension of time.
- 2_TOOL_regression.R : Runs the regresssion analysis to check for confounders on the specified bacteria/antibiotic combination.
- 3_TOOL_index_analysis : Creates plots with the index analysis for all bacteria/antibiotics included in the dataset.

DATA CHALLENGE SPECIFIC
- 4_Figures_data_challenge.R : This recreates the specific figures for our data challenge entry.

Other files in the repro are: 
- Folder initial_testing_not_used : These are scripts written in the process of developing the tool, but are no longer used.
- overlapping_drugs.R : This file creates a list of overlapping drugs between two bacteria, but is not required to run the analysis.
- income.csv : this contains mapping from the countries to World Bank income group.
- who-regions.csv : this contains the mapping from countries to WHO regions.

#The TOOL can also be used with other data. In order to do this a cleaned dataset must be provided in a subfolder: data/full_data.csv
Columns must include: gender, organism, antibiotic, mic, and any other characteristics the user wants to split the data by. The mic column must be numeric, and the other columns either character or factor. 
