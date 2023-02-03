#### RELATE Winter Paper ####
## Function: Adds all variables into the places they need to be
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 02/02/2023
## Changes: Streamlined code and made sure each df has each variable


#------------------------------
# Replication Information: ####
#------------------------------


# R version 4.2.0 (2022-04-22 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19044)
# Matrix products: default
# locale:
#   [1] LC_COLLATE=English_United Kingdom.utf8  LC_CTYPE=English_United Kingdom.utf8
# [3] LC_MONETARY=English_United Kingdom.utf8 LC_NUMERIC=C
# [5] LC_TIME=English_United Kingdom.utf8
#
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:
#   [1] Rfast_2.0.6          RcppZiggurat_0.1.6   Rcpp_1.0.9           distributional_0.3.1 ggdist_3.2.1
# [6] data.table_1.14.6    here_1.0.1           ggridges_0.5.4       reshape2_1.4.4       apollo_0.2.8
# [11] magrittr_2.0.3       forcats_0.5.2        stringr_1.5.0        dplyr_1.0.10         purrr_1.0.1
# [16] readr_2.1.3          tidyr_1.2.1          tibble_3.1.8         ggplot2_3.4.0        tidyverse_1.3.2
#
# loaded via a namespace (and not attached):
#   [1] mcmc_0.9-7          matrixStats_0.63.0  fs_1.6.0            lubridate_1.9.0     RColorBrewer_1.1-3
# [6] httr_1.4.4          rprojroot_2.0.3     numDeriv_2016.8-1.1 tools_4.2.0         backports_1.4.1
# [11] utf8_1.2.2          R6_2.5.1            DBI_1.1.3           colorspace_2.0-3    withr_2.5.0
# [16] tidyselect_1.2.0    mnormt_2.1.1        compiler_4.2.0      cli_3.6.0           rvest_1.0.3
# [21] quantreg_5.94       SparseM_1.81        xml2_1.3.3          sandwich_3.0-2      labeling_0.4.2
# [26] scales_1.2.1        mvtnorm_1.1-3       digest_0.6.31       RSGHB_1.2.2         MCMCpack_1.6-3
# [31] pkgconfig_2.0.3     dbplyr_2.3.0        rlang_1.0.6         readxl_1.4.1        rstudioapi_0.14
# [36] generics_0.1.3      farver_2.1.1        zoo_1.8-11          jsonlite_1.8.4      googlesheets4_1.0.1
# [41] Matrix_1.5-3        munsell_0.5.0       fansi_1.0.3         lifecycle_1.0.3     stringi_1.7.12
# [46] MASS_7.3-56         plyr_1.8.8          grid_4.2.0          parallel_4.2.0      crayon_1.5.2
# [51] lattice_0.20-45     haven_2.5.1         splines_4.2.0       hms_1.1.2           pillar_1.8.1
# [56] randtoolbox_2.0.3   reprex_2.0.2        glue_1.6.2          modelr_0.1.10       vctrs_0.5.1
# [61] tzdb_0.3.0          miscTools_0.6-26    MatrixModels_0.5-1  cellranger_1.1.0    gtable_0.3.1
# [66] assertthat_0.2.1    broom_1.0.2         rngWELL_0.10-9      coda_0.19-4         survival_3.3-1
# [71] googledrive_2.0.0   gargle_1.2.1        maxLik_1.5-2        timechange_0.2.0    ellipsis_0.3.2



#----------------------------------------------------------------------------------------------------------
# Setup Environment: ####
#----------------------------------------------------------------------------------------------------------


library(tidyverse)
library(magrittr)
library(dplyr)
library(reshape2)
library(here)
library(data.table)
library(Rfast)
library(sf)
library(readxl)


#----------------------------------------------------------------------------------------------------------
# Section 1: Import Data ####
#----------------------------------------------------------------------------------------------------------


# Survey Data ----------------------------------------------
## For Winter we need the survey data (Step3) and the WTP (Model two conditionals)
Winter <- data.frame(fread(here("OtherData","Winter_dataframe_Step3.csv")))
WTP <- data.frame(fread(here("CEoutput/ModelTwo","Winter_MXL_ModelTwo_ConWTP.csv")))


# GB_Shapefile ----------------------------------------------

## Start by reading in the new shapefile
### Available here: https://geoportal.statistics.gov.uk/datasets/ons::counties-and-unitary-authorities-december-2019-boundaries-uk-buc/about
GB <- st_read(here("OtherData","Counties_and_Unitary_Authorities_(December_2019)_Boundaries_UK_BUC.shp"))
GB <- st_transform(GB,crs=4326) ## Comes in BNG so convert to LatLon


## Data on tree cover:
### Previously added to spatial using QGIS but I exported it to csv
Trees <- here("OtherData","AdditionalData_Trees.csv") %>% fread() %>% data.frame()


## Add in extra data on population density and GDP per capita
PopDensity <- read_excel(here("OtherData","AdditionalData.xlsx"),
                         sheet = "Pop.Density")

GDHI <- read_excel(here("OtherData","AdditionalData.xlsx"),
                   sheet = "Income")


#----------------------------------------------------------------------------------------------------------
# Section 2: Clean Data for Winter_dataframe_Step4.csv ####
#----------------------------------------------------------------------------------------------------------


## Trim missing data to make it consistent with the data used in the choice models
Winter<- Winter[!is.na(Winter$MilesDistance),] ## Drop missing distances
Winter<- Winter[!is.na(Winter$Overall),] ## Drop respondents not completing BIOWELL


## If the conditionals are imported then run this to recover only useful variables
WTP <- WTP[WTP %>% select(-ends_with(c(".ID",".post.sd"))) %>% colnames()] %>% data.frame()


## Renaming variables here to make more sense:
colnames(WTP) <- WTP %>% colnames() %>%
  c() %>%
  str_replace_all(c(beta_Tax="Tax",
                    b_="",
                    "2.post.mean"="_WTP_High",
                    ".post.mean"="_WTP_Medium"))


## Add impairment which keeps disappearing
Winter$Impairment <- ifelse((Winter$SightIssues==1)|
                                (Winter$SmellIssues==1)|
                                (Winter$HearingIssues==1),1,0)


## Adding calculated WTP to the participant survey data
Winter <- cbind(Winter,
                WTP)


## Export
fwrite(Winter %>% data.frame(),
       sep=",",
       here("OtherData","Winter_dataframe_Step4.csv"))


#----------------------------------------------------------------------------------------------------------
# Section 3: Clean Data for GB shapefile ####
#----------------------------------------------------------------------------------------------------------


## Import furthest version:
Winter <- data.frame(fread(here("OtherData","Winter_dataframe_Step4.csv")))


## Join county name to county name
GB_Winter <- left_join(x = GB,Winter,by=c("ctyua19nm"="County"))


## Join dataframes by NUTS codes
GB_Winter_Joined <- left_join(x = GB_Winter,PopDensity,by=c("NUTS"="NUTS"))
GB_Winter_Joined <- left_join(x = GB_Winter_Joined,GDHI,by=c("NUTS"="NUTS"))


## Little column renaming to make things easier
colnames(GB_Winter_Joined)[which(names(GB_Winter_Joined)==colnames(GB_Winter_Joined)[291])] <- "Density"


## Easiest way to add data on the trees:
GB_Winter_Joined <- cbind(GB_Winter_Joined,as.data.frame(Trees)[,1:4])


## Export
st_write(GB_Winter_Joined,
         here("OtherData","GB_Winter_Final.gpkg"),
         append=FALSE)


#----------------------------------------------------------------------------------------------------------
#### END OF SCRIPT ####
#----------------------------------------------------------------------------------------------------------
