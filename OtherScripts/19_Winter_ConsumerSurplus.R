#### Stated preferences for the colours, smells and sounds of biodiversity  ###############
# Project: ERC Relate Project. Work Package 5
# Author: Dr Peter King (p.king1@leeds.ac.uk)
# Function: To calculate the correct consumer surplus in the text
# Notes: Bit inelegant but it works
# Last Edited: 28/09/2024
# - double-checking post R2
# - updating sessioninfo()


# ************************************************************
#### Section 0: Setup and estimate models ####
# ************************************************************


## sessionInfo() ---------------------------------------------------------------
# R version 4.3.1 (2023-06-16 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 11 x64 (build 22631)
# Matrix products: default
# locale:
#   [1] LC_COLLATE=English_United Kingdom.utf8
# [2] LC_CTYPE=English_United Kingdom.utf8
# [3] LC_MONETARY=English_United Kingdom.utf8
# [4] LC_NUMERIC=C
# [5] LC_TIME=English_United Kingdom.utf8
# time zone: Europe/London
# tzcode source: internal
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:
#   [1] dgof_1.4              microbenchmark_1.4.10 lubridate_1.9.3
# [4] forcats_1.0.0         purrr_1.0.2           readr_2.1.5
# [7] tidyr_1.3.1           tibble_3.2.1          ggplot2_3.5.1
# [10] tidyverse_2.0.0       Rfast_2.1.0           RcppParallel_5.1.7
# [13] RcppZiggurat_0.1.6    Rcpp_1.0.12           stringr_1.5.1
# [16] data.table_1.15.4     here_1.0.1            mded_0.1-2
# [19] reshape2_1.4.4        magrittr_2.0.3        dplyr_1.1.4
# [22] apollo_0.3.1
# loaded via a namespace (and not attached):
#   [1] gtable_0.3.5        rstatix_0.7.2       lattice_0.22-6
# [4] tzdb_0.4.0          numDeriv_2016.8-1.1 vctrs_0.6.5
# [7] tools_4.3.1         generics_0.1.3      parallel_4.3.1
# [10] sandwich_3.1-0      fansi_1.0.6         pkgconfig_2.0.3
# [13] Matrix_1.6-5        lifecycle_1.0.4     truncnorm_1.0-9
# [16] compiler_4.3.1      maxLik_1.5-2.1      MatrixModels_0.5-3
# [19] mcmc_0.9-8          munsell_0.5.1       mnormt_2.1.1
# [22] codetools_0.2-20    carData_3.0-5       SparseM_1.81
# [25] RSGHB_1.2.2         quantreg_5.97       bgw_0.1.2
# [28] Rsolnp_1.16         pillar_1.9.0        car_3.1-2
# [31] MASS_7.3-60.0.1     randtoolbox_2.0.4   multcomp_1.4-25
# [34] abind_1.4-5         tidyselect_1.2.1    digest_0.6.35
# [37] mvtnorm_1.2-4       stringi_1.8.3       splines_4.3.1
# [40] miscTools_0.6-28    rprojroot_2.0.4     grid_4.3.1
# [43] colorspace_2.1-0    cli_3.6.2           survival_3.6-4
# [46] utf8_1.2.4          TH.data_1.1-2       broom_1.0.5
# [49] withr_3.0.0         scales_1.3.0        backports_1.4.1
# [52] timechange_0.3.0    matrixStats_1.3.0   rngWELL_0.10-9
# [55] hms_1.1.3           zoo_1.8-12          coda_0.19-4.1
# [58] rlang_1.1.3         MCMCpack_1.7-0      glue_1.7.0
# [61] rstudioapi_0.16.0   R6_2.5.1            plyr_1.8.9


## Libraries: ---------------------------------------------------------------
library(data.table)
library(tidyverse)
library(here)
library(magrittr)

# ************************************************************
#### Section 1: Importing Model,  Estimates,  and WTP for FULL SAMPLE models ####
# ************************************************************


## Estimates model in Preference-Space to allow calculation of consumer surplus:
Model <-
  readRDS(here(
    "CEoutput/ModelOne",
    "Winter_MXL_ModelOne_AllCorrelations_model.rds"
  ))


# ************************************************************
#### Section 2: Consumer Surplus ####
# ************************************************************


## Utility for changing from SQ to medium levels:
V_SQToMedium <-
  -Model$estimate["asc_C"] +  Model$estimate["mu_Sound"] + Model$estimate["mu_Smell"] +
  Model$estimate["mu_Colour"] + Model$estimate["mu_Deadwood"]

## Utility for changing from SQ to high levels:
V_SQToHigh <-
  -Model$estimate["asc_C"] +  Model$estimate["mu_Sound2"] + Model$estimate["mu_Smell2"] +
  Model$estimate["mu_Colour2"] + Model$estimate["mu_Deadwood2"]

## Utility for SQ
V_SQ <- 0


## Using logsum approach from: https://link.springer.com/content/pdf/10.1007%2F978-3-030-62669-3.pdf
# CS_SQToMedium <- -log(V_SQToMedium - V_SQ)
# CS_SQToHigh <- -log(V_SQToHigh - V_SQ)



# ************************************************************
#### Section 3: Output for paper version ####
# ************************************************************


paste0("Using the WTP estimates from Model One for simplicity, we calculated across all the biodiversity attributes a positive consumer surplus (status quo to ‘medium’=£",
       V_SQToMedium %>% round(2) %>% sprintf("%.3f", .),
       "; status quo to ‘high’=£",
       V_SQToHigh %>% round(2) %>% sprintf("%.3f", .),
       ") showing that welfare improves with the changes in attribute levels.")


# **********************************************************************************
#### END OF SCRIPT ####
## Next step: 20_Winter_TableB11_MWTests_Impairments.R
# **********************************************************************************
