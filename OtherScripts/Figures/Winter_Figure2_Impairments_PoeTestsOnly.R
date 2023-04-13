#### RELATE Winter Paper ####
## Function: Calculate Poe tests between impairments
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 13/04/2023
## Change: Format script nicely
## TODO: Maybe there's a better way.


# ******************************
# Replication Information: ####
# ******************************


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



# **********************************************************
# Setup Environment: ####
# **********************************************************


library(tidyverse)
library(magrittr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggridges)
library(here)
library(data.table)
library(Rfast)


# **********************************************************
# Section 1: Import Data ####
# **********************************************************


## Import Respondents Data
Winter <- data.frame(fread(here("OtherData","Winter_dataframe_Step3.csv")))


## Import WTP
WTP <- data.frame(fread(here("CEoutput/ModelTwo","Winter_MXL_ModelTwo_ConWTP.csv")))




# **********************************************************
# Section 2: Truncate Data ####
# **********************************************************


## Drop missing rows
Winter <-  Winter[!is.na(Winter$MilesDistance), ] ## Drop missing distances
Winter <-  Winter[!is.na(Winter$Overall), ] ## Drop respondents not completing BIOWELL


## If the conditionals are imported then run this to recover only useful variables
WTP <- WTP[WTP %>% select(-ends_with(c(".ID", ".post.sd"))) %>% colnames()] %>% data.frame()


## Change names for ease of reference
colnames(WTP) <- WTP %>%
  colnames() %>%
  gsub(pattern = ".post.mean", replacement = "")


## Merge data here
Winter <- cbind(Winter,WTP)




# **********************************************************
# Section 3: Calculate test results ####
# **********************************************************


## Colours:
OutputColours <- paste0("of colours (p = ", c(mded(Winter[, "b_Colour"][Winter$SightIssues == 0],
                      Winter[, "b_Colour"][Winter$SightIssues == 1])$stat) %>% round(3),
       " and ", c(mded(Winter[, "b_Colour2"][Winter$SightIssues == 0],
                       Winter[, "b_Colour2"][Winter$SightIssues == 1])$stat) %>% round(3),
       " for the ‘medium’ and ‘high’ levels of colours respectively)"
)


## Smell:
OutputSmells <- paste0(", smells (p = ", c(mded(Winter[,"b_Smell"][Winter$SmellIssues==0],
                      Winter[,"b_Smell"][Winter$SmellIssues==1])$stat) %>% round(3),
       " and ", c(mded(Winter[,"b_Smell2"][Winter$SmellIssues==0],
                       Winter[,"b_Smell2"][Winter$SmellIssues==1])$stat) %>% round(3),
       ")")


## Sounds:
OutputSounds <- paste0(", or sounds (p = ", c(mded(Winter[,"b_Sound"][Winter$HearingIssues==0],
                      Winter[,"b_Sound"][Winter$HearingIssues==1])$stat) %>% round(3),
       " and ", c(mded(Winter[,"b_Sound2"][Winter$HearingIssues==0],
                       Winter[,"b_Sound2"][Winter$HearingIssues==1])$stat) %>% round(3),
       ")")




# **********************************************************
# Section 4: Export results ####
# **********************************************************


## Output here
paste0("We found no statistical difference in WTP for increases to the variety of ",
  OutputColours,
  OutputSmells,
  OutputSounds,
  " between participants with and without sensory impairments. "
) %>% noquote()


# End Of Script ****************************************************
