#### RELATE WP5: Winter Paper  ###############
# Script author: Dr Peter King (p.m.king@kent.ac.uk)
# Last Edited: 14/05/23
# Change: Add the WTP to the survey data


#----------------------------------------------------------------------------------------------------------
#### Section 0: Setting up ####
## NOTES: This is just importing packages.
#----------------------------------------------------------------------------------------------------------


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
rm(list=ls())
library(here)
library(magrittr)
library(dplyr)
library(data.table)
library(tidyverse)




# ***************************************************************************
#### Section A: Importing Data ####
# ***************************************************************************



# Data Import: ----
Winter <- here("OtherData","Winter_dataframe_Step4.csv") %>% fread() %>% data.frame()
Draws <- here("CEoutput/ModelTwo","Winter_MXL_ModelTwo_UnconWTP.csv") %>% fread() %>% data.frame()



## Drop rows that have missing data in any of the following we use in the models:
Winter <- Winter %>% drop_na(Colour_WTP_Medium,WoodlandsScore,
                             MilesDistance,MostRecentVisit,
                             DummyAge,Gender,
                             IncomeDummy, Impairment,
                             GDHI,Density,Area_ha_median)



Data_Combined <- bind_cols(
  Winter, Draws
)

# GB_Winter <- st_read("GB_Winter_2022_07_30.gpkg")

Data <- Data_Combined
Data_Winter <- Data_Combined
