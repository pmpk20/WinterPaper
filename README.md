#### ERC-Funded RELATE Project Work Package Five ####
- Repo for the data and code for the "Stated preferences for the colours, smells and sounds of biodiversity outweigh those for ecological function. " from the RELATE ERC-funded project.
- Author: Peter King (p.m.king@kent.ac.uk)
- Last Edited: 30/01/2023.
- Guide to replicating the paper from survey data


####  Comments: 


- This is a work in progress as I triple-check every line of every script and decide what needs uploading
- I'm also double-checking filenames and versions to make sure only the latest is available.
- Current: Finished Winter_CleanSurveyData.R
- ToDo: check 'SpatialSetup.R', and other setup files. Then upload choice modelling work, spatial work, and paper files.

------------------------------
#### File Structure: 

- CEModelData: Inputs for estimated models.
- CEModelScripts: Scripts to estimate choice models.
- CEoutput: Outputs from estimated models, split into models one and two.
- OtherData: Inputs to plotting, tables, cleaning and other scripts. Also 'Winter_dataframe_Step4.csv' which is the final output
- OtherScripts: R scripts to manipulate data, spatial analysis, present outputs, consumer surplus


------------------------------
#### How to replicate:

Tl:Dr; just run StepByStep.rmd to follow the whole process.

1) Start from 'Winter_CleanSurveyData.R': This transforms 'Winter_SurveyData.xlsx' (data downloaded from Qualtrics) to two new files:
- Winter_dataframe_Step1.csv (Long format: One row per respondent. Useful for individual-level analysis.)
- database_Winter_Step1.csv (Wide format: One row per respondent choice. Required for APOLLO models).


2) Run 'Winter_AddSpatialData.R': This adds spatial elements to the datasets mentioned above.
- Spatial elements include coordinates/postcodes/regions/etc.
- Performs spatial joins with 'Winter_dataframe_Step1.csv' and 'Counties_and_Unitary_Authorities_(December_2019)_Boundaries_UK_BUC.shp' to create
'GB_Winter_Step4.gpkg'


3) Choice Models:
- See 'CEModelScripts': 
-- Estimates basic mixed logits with two levels for each non-monetary attribute
-- Run 'ModelOne', 'ModelOne_PrefSpace' and 'ModelTwo' 
-- Summarise outputs: '_MXLSummary'
-- Add conditionals to the 'Winter' and 'dataframe' objects in '_MXLSummary' 
-- Validity and Robustness Checks: 'Replication_WP5_Winter_WTPValidity.R'


4) Spatial Analysis: Necessarily after choice models as we're interested in spatial patterns of WTP.
-- See: 'Replication_WP5_Winter_SpatialPlots.R', 'WP5_Winter_SpatialLagModels.R', 'Winter_SpatialClusters.R'

5) Plots:
-- /OtherScripts/Figures: Boxplot of unconditional WTP and distributions of WTP by impairment, visit frequency

------------------------------
#### Any Questions: p.m.king@kent.ac.uk 
