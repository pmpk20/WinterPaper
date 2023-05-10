#### RELATE WP5: Full run through of how to replicate the paper  ###############
# Function: To list all files in one go
# Author: Dr Peter King (p.m.king@kent.ac.uk)
# Last Edited: 10/05/2023


# **********************************************************************************
#### Section 0: Setting up ####
## NOTES: This is just importing packages.
# **********************************************************************************


## sessionInfo()-----------------------------------------------------------------
# R version 4.2.0 (2022-04-22 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19044)
#
# Matrix products: default
#
# locale:
#   [1] LC_COLLATE=English_United Kingdom.utf8  LC_CTYPE=English_United Kingdom.utf8    LC_MONETARY=English_United Kingdom.utf8
# [4] LC_NUMERIC=C                            LC_TIME=English_United Kingdom.utf8
#
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:
#   [1] udunits2_0.13.2.1  PostcodesioR_0.3.1 geosphere_1.5-18   psych_2.2.9        dplyr_1.0.10       magrittr_2.0.3     readxl_1.4.1
# [8] here_1.0.1
#
# loaded via a namespace (and not attached):
#   [1] Rcpp_1.0.9       cellranger_1.1.0 pillar_1.8.1     compiler_4.2.0   tools_4.2.0      digest_0.6.31    evaluate_0.20    lifecycle_1.0.3
# [9] tibble_3.1.8     nlme_3.1-157     lattice_0.20-45  pkgconfig_2.0.3  rlang_1.0.6      cli_3.6.0        DBI_1.1.3        rstudioapi_0.14
# [17] yaml_2.3.6       parallel_4.2.0   xfun_0.36        fastmap_1.1.0    httr_1.4.4       knitr_1.41       generics_0.1.3   vctrs_0.5.1
# [25] rprojroot_2.0.3  grid_4.2.0       tidyselect_1.2.0 glue_1.6.2       R6_2.5.1         fansi_1.0.3      rmarkdown_2.20   sp_1.6-0
# [33] htmltools_0.5.4  assertthat_0.2.1 mnormt_2.1.1     utf8_1.2.2


## Libraries here: -----------------------------------------------------------------
## Setting up libraries in order of use in the script
library(here)
library(magrittr)
library(data.table)
library(tidyverse)




# **********************************************************************************
#### Section Zero: Prepare Data For Analysis ####
# **********************************************************************************


## Start by converting raw "SurveyData.xlsx" to cleaned "Winter_dataframe_Step1.csv"
here("OtherScripts/Setup", "01_Winter_CleanSurveyData.R") %>% source()


## Add spatial data to make "Winter_dataframe_Step3.csv"
here("OtherScripts/Setup", "02_Winter_AddSpatialData.R") %>% source()


# History on how to make "AdditionalInfo.gpkg" that is used to make "GB_Winter_Step5.gpkg"
# here("OtherScripts/Spatial","Winter_Spatial_AddTrees.R") %>% source()




# **********************************************************************************
#### Section One: Run all choice models ####
# **********************************************************************************


## Estimate basic attributes only model in WTP-space
here("CEModelScripts", "03_Winter_MXL_ModelOne.R") %>% source()


## Estimate basic attributes only model in preference-space
here("CEModelScripts", "04_Winter_MXL_ModelOne_PrefSpace.R") %>% source()


## Add covariates
here("CEModelScripts", "05_Winter_MXL_ModelTwo.R") %>% source()


## MUST RUN THIS FOR THE REST TO WORK:
here("OtherScripts/Setup", "06_Winter_MergeSurveyAndWTP.R") %>% source()



# **********************************************************************************
#### Section Two: Create all outputs from the choice models ####
# **********************************************************************************



## Create table 1 which breaks down and tests the sample versus population
here("OtherScripts/Tables","07_Winter_Table1_SampleTests.R") %>% source()


## Present the Choice Model outputs nicely
here("OtherScripts/Tables","08_Winter_Table2_ModelOutputs.R") %>% source()
here("OtherScripts/Tables","09_Winter_Table3_ModelOutputs.R") %>% source()


## Output the WTP nicely
here("OtherScripts/Tables","21_Winter_Table3_WTPSummary.R") %>% source()


## Check WTP distributions:
here("OtherScripts/Tables","10_Winter_SarriasTests.R") %>% source()


## Calculate CS measures:
here("OtherScripts","11_Winter_ConsumerSurplus.R") %>% source()



## Plot distribution of WTP
here("OtherScripts/Figures",
     "12_Winter_Figure2_Boxplot_Unconditionals_Parellel.R") %>% source()


## Plot distribution of WTP by impairments
here("OtherScripts/Figures", "13_Winter_Figure2_Impairments.R") %>% source()
here("OtherScripts/Figures", "14_Winter_Figure2_Impairments_PoeTestsOnly.R") %>% source()


## Plot distribution of WTP by visit frequency
here("OtherScripts/Figures", "15_Winter_Figure2_VisitFrequency.R") %>% source()




# **********************************************************************************
#### Section Three: All spatial modelling ####
# **********************************************************************************


## Calculate global moran I stat for T3 in the appendix
here("OtherScripts/Spatial","16_Winter_GlobalMorans_Table.R") %>% source()


## Local Moran's I for autocorrelation
here("OtherScripts/Spatial","17_Winter_LocalMorans_Plots.R") %>% source()
here("OtherScripts/Spatial","18_Winter_LocalMorans_Table.R") %>% source()


## Output Table 5 in the paper using the spatial lag models
here("OtherScripts/Spatial","19_Winter_SpatialLagModels_Table.R") %>% source()


## Spatial Lag Model simulate P values
here("OtherScripts/Spatial","20_Winter_SpatialLagModels_Simulator_Plot.R") %>% source()





#### That's all! ####
# **********************************************************************************
