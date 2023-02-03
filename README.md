------------------------------
### ERC-Funded RELATE Project Work Package Five ####
- Repo for the data and code for the "Stated preferences for the colours, smells and sounds of biodiversity outweigh those for ecological function. " from the RELATE ERC-funded project.
- Author: Peter King (p.m.king@kent.ac.uk)
- Last Edited: 03/02/2023.
- Guide to replicating the paper from survey data


#####  Comments: 
- This works locally on my machine and HPC so hopefully does for you too.
- Current issues: 
  - The *model.rds* and *unconditionalWTP* are too large for github so do not appear here. 

------------------------------
##### File Structure: 

- CEModelData: Inputs for estimated models.
- CEModelScripts: Scripts to estimate choice models.
- CEoutput: Outputs from estimated models, split into models one and two.
- OtherData: Inputs to plotting, tables, cleaning and other scripts. Also *Winter_dataframe_Step4.csv* which is the final output
- OtherScripts: R scripts to manipulate data, spatial analysis, present outputs, consumer surplus
- OutherOutput: Misc tables and figures.


------------------------------
#### How to replicate:

> Tl:Dr; just run StepByStep.rmd to follow the whole process.

##### 1) Start from *Winter_CleanSurveyData.R*: This transforms *Winter_SurveyData.xlsx* (data downloaded from Qualtrics) to two new files:

-   Winter_dataframe_Step1.csv (Long format: One row per respondent. Useful for individual-level analysis.)
-   database_Winter_Step1.csv (Wide format: One row per respondent choice. Required for APOLLO models).

##### 2) Run *Winter_AddSpatialData.R*: This adds spatial elements to the datasets mentioned above.

-   Spatial elements include coordinates/postcodes/regions/etc.
-   Performs spatial joins with *Winter_dataframe_Step1.csv* and \*Counties_and_Unitary_Authorities\_(December_2019)\_Boundaries_UK_BUC.shp\* to create *GB_Winter_Step4.gpkg*

##### 3) Choice Models:

-   See *CEModelScripts*:
-   Estimates basic mixed logits with two levels for each non-monetary attribute
-   Run *ModelOne*, *ModelOne_PrefSpace* and *ModelTwo*
-   Must run *OtherScripts/Winter_MergeSurveyAndWTP.R* afterwards for later analysis to work!
-   Try running *OtherScripts/Tables/Winter_TableX_ModelOutputs.R* to see the model outputs nicely.

##### 3B) Analyse WTP:

-   *Winter_SarriasTests.R* is a nice tidy script to run Sarrias (2020) three checks of WTP.
-   *Winter_ConsumerSurplus.R* quickly calculates some simple CS measures.
-   Scripts in *OtherScripts/Figures* can plot you the unconditionals by attribute and other factors of interest.

##### 4) Spatial Analysis: Necessarily after choice models as we\*re interested in spatial patterns of WTP.

-   Three checks: Global Moran, Local Moran, and Spatial Lag.
-   */OtherScripts/Spatial/Winter\_* have scripts for each.
-   Two scripts for spatial lag models, one to repeat models for each of the unconditional (*Winter_SpatialLagModels_Simulator.R*), and one to spit out the model presented in-text (*Winter_SpatialLagModels.R*)
-   Results from each step should be in */OtherOutput/*
-   I use *GB_Winter_Final.gpkg* for most of these.

------------------------------------------------------------------------

#### Any Questions: [p.m.king\@kent.ac.uk](mailto:p.m.king@kent.ac.uk)
