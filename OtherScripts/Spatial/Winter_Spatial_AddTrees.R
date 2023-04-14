#### RELATE WP5: Winter Paper  ###############
# Script author: Dr Peter King (p.m.king@kent.ac.uk)
# Last Edited: 14/05/23
# Change: Okay full disclosure:
# I had a huge external trees data from the National Forest Inventory.
# Too large to wrangle in R, I used QGIS.
# However, that project save file was corrupted so I had to find a workaround
# that would have the correct areas_* columns.
# I have this in GB_Winter_2021 but the other columns in that are outdated.
# SO this script stitched the correct columns of everything together.
# Now you can just run Winter_AddSpatialData.R and be fine but this is the backstory
# to AdditionalInfo.gpkg

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
library(readxl)
library(magrittr)
library(psych)
library(dplyr)
library(geosphere)
library(PostcodesioR)
library(udunits2)
library(sf)
library(stringi)
library(stringr)
library(lubridate)
library(data.table)




# ***************************************************************************
#### Section A: Importing Data ####
# ***************************************************************************


## Latest GPKG
GB_Winter <- st_read(here("OtherData","GB_Winter_Step4.gpkg"))


## Old version that has the QGIS work but the WRONG WTP
Test <- st_read(here("OtherData","GB_Winter_2021-11-26.gpkg"))


# ***************************************************************************
#### Section B: Truncating Data ####
# ***************************************************************************


GB_Winter_2 <- GB_Winter[GB_Winter$ID %in% Test$ID,]
Test2 <- Test[Test$ID %in% GB_Winter_2$ID,]
Test3 <- Test2[-1,]


Additions <- Test3[,239:253]


# ***************************************************************************
#### Section C: Adding Trees Data ####
# ***************************************************************************

GB_Winter_2$Area..sq.km. <- Additions$Area..sq.km.
GB_Winter_2$Area_ha_mean <- Additions$Area_ha_mean
GB_Winter_2$Area_ha_median <- Additions$Area_ha_median
GB_Winter_2$Area_ha_stddev <- Additions$Area_ha_stddev

GB_Winter_2 %>%
  st_write(here("OtherData","GB_Winter_Step5.gpkg"),append=FALSE)



# ***************************************************************************
#### Section D: Saving useful Data ####
# ***************************************************************************


Additions <- Test3[,239:253]
Additions <- cbind(Additions,
  Test3[,1:10])

Additions$Code <- Test3$Code
Additions$ID <- Test3$ID



Additions %>%
  st_write(here("OtherData","AdditionalInfo.gpkg"),append=FALSE)

# End of script **************************************************************
