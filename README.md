#### ERC-Funded RELATE Project Work Package Five ####
- Repo for the data and code for the "Stated preferences for the colours, smells and sounds of biodiversity outweigh those for ecological function. " from the RELATE ERC-funded project.
- Author: Peter King (p.m.king@kent.ac.uk)
- Last Edited: 27/01/2023.
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
- CEModelOutput: Outputs from estimated models.
- OtherData: Survey data, transformed data, and outputs.
- OtherScripts: R scripts to manipulate data, spatial analysis, present outputs


------------------------------
#### How to replicate:


1) Start from 'Winter_CleanSurveyData.R': This transforms 'Winter_SurveyData.xlsx' (data downloaded from Qualtrics) to two new files:
- Winter_dataframe.csv (Long format: One row per respondent. Useful for individual-level analysis.)
- database_Winter.csv (Wide format: One row per respondent choice. Required for APOLLO models).


2) Run 'RWinter_SpatialSetup.R': This adds spatial elements to the datasets mentioned above.
- Spatial elements include coordinates/postcodes/regions/etc.
- Performs spatial joins with 'Winter_dataframe.csv' and 'Counties_and_Unitary_Authorities_(December_2019)_Boundaries_UK_BUC.shp' to create
'GB_Winter.gpkg'


3) Choice Models:
- See 'CEModelScripts': 
-- Estimate a basic mixed logit with two levels for each non-monetary attribute.
-- Validity and Robustness Checks: 'Replication_WP5_Winter_WTPValidity.R'


4) Spatial Analysis: Necessarily after choice models as we're interested in spatial patterns of WTP.
- See: 'Replication_WP5_Winter_SpatialPlots.R', 'WP5_Winter_SpatialLagModels.R', 'Winter_SpatialClusters.R'

5) An anonymised and variables explained excel worksheet is available with all final variables: "Winter_dataframe_Anonymised.csv"

------------------------------
#### Any Questions: p.m.king@kent.ac.uk 
