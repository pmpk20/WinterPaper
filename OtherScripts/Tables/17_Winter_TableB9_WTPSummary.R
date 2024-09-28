#### Stated preferences for the colours, smells and sounds of biodiversity  ###############
# Project: ERC Relate Project. Work Package 5
# Author: Dr Peter King (p.king1@leeds.ac.uk)
# Function: To compare all the WTPs to satisfy a reviewer, who were really good!
# Notes: should work
# Last Edited: 28/09/2024
# Changes:
# - newer WTP
# - added means too
# - double-checking post R2
# - updating sessioninfo()


# *****************************
# Replication Information: ####
# *****************************


# R version 4.4.1 (2024-06-14 ucrt)
# Platform: x86_64-w64-mingw32/x64
# Running under: Windows 11 x64 (build 22631)
# Matrix products: default
# locale:
#   [1] C
# system code page: 65001
# time zone: Europe/London
# tzcode source: internal
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base
# other attached packages:
#   [1] ggdist_3.3.2       stringi_1.8.4      sf_1.0-16
# [4] udunits2_0.13.2.1  PostcodesioR_0.3.1 geosphere_1.5-18
# [7] psych_2.4.3        readxl_1.4.3       coin_1.4-3
# [10] survival_3.7-0     Rfast_2.1.0        RcppParallel_5.1.7
# [13] RcppZiggurat_0.1.6 Rcpp_1.0.12        lubridate_1.9.3
# [16] forcats_1.0.0      stringr_1.5.1      purrr_1.0.2
# [19] readr_2.1.5        tidyr_1.3.1        tibble_3.2.1
# [22] tidyverse_2.0.0    RColorBrewer_1.1-3 data.table_1.15.4
# [25] here_1.0.1         mded_0.1-2         reshape2_1.4.4
# [28] ggridges_0.5.6     ggplot2_3.5.1      magrittr_2.0.3
# [31] dplyr_1.1.4        apollo_0.3.3
# loaded via a namespace (and not attached):
#   [1] DBI_1.2.3            mnormt_2.1.1         sandwich_3.1-0
# [4] rlang_1.1.4          multcomp_1.4-25      e1071_1.7-14
# [7] matrixStats_1.3.0    compiler_4.4.1       systemfonts_1.1.0
# [10] vctrs_0.6.5          quantreg_5.98        pkgconfig_2.0.3
# [13] backports_1.5.0      mcmc_0.9-8           utf8_1.2.4
# [16] tzdb_0.4.0           miscTools_0.6-28     ragg_1.3.2
# [19] MatrixModels_0.5-3   modeltools_0.2-23    Deriv_4.1.3
# [22] broom_1.0.6          parallel_4.4.1       R6_2.5.1
# [25] Rsolnp_1.16          car_3.1-2            cellranger_1.1.0
# [28] numDeriv_2016.8-1.1  zoo_1.8-12           rngWELL_0.10-9
# [31] Matrix_1.7-0         splines_4.4.1        timechange_0.3.0
# [34] tidyselect_1.2.1     rstudioapi_0.16.0    abind_1.4-5
# [37] maxLik_1.5-2.1       codetools_0.2-20     lattice_0.22-6
# [40] plyr_1.8.9           withr_3.0.0          coda_0.19-4.1
# [43] RSGHB_1.2.2          units_0.8-5          proxy_0.4-27
# [46] pillar_1.9.0         carData_3.0-5        KernSmooth_2.23-24
# [49] stats4_4.4.1         distributional_0.4.0 generics_0.1.3
# [52] rprojroot_2.0.4      sp_2.1-4             truncnorm_1.0-9
# [55] hms_1.1.3            munsell_0.5.1        scales_1.3.0
# [58] randtoolbox_2.0.4    class_7.3-22         glue_1.7.0
# [61] tools_4.4.1          SparseM_1.83         mvtnorm_1.2-5
# [64] grid_4.4.1           MCMCpack_1.7-0       libcoin_1.0-10
# [67] colorspace_2.1-0     nlme_3.1-165         cli_3.6.3
# [70] textshaping_0.4.0    fansi_1.0.6          gtable_0.3.5
# [73] rstatix_0.7.2        digest_0.6.35        classInt_0.4-10
# [76] bgw_0.1.3            TH.data_1.1-2        farver_2.1.2
# [79] lifecycle_1.0.4      httr_1.4.7           MASS_7.3-61

# *****************************
# Setup Environment: ####
# *****************************


library(magrittr)
library(dplyr)
library(apollo)
library(reshape2)
library(here)
library(data.table)

# *****************************
# Section 1: Import Data ####
# *****************************


## Conditional WTP from model two
WTP <-
  here("CEoutput/ModelTwo",
       "Winter_MXL_ModelTwo_AllCorrelations_ConWTP.csv") %>%
  fread() %>%
  data.frame()



# *****************************
# Section 2: Arrange Data ####
# *****************************


## Define a function that summarises median, SD, and 95% by variable
calculate_summary_table <- function(data, columns) {
  Upper <- WTP[, paste0(i, "post.mean")] %>% quantile(c(0.975)) %>% round(3) %>% sprintf("%.3f", .)
  Lower <- WTP[, paste0(i, "post.mean")] %>% quantile(c(0.025)) %>% round(3) %>% sprintf("%.3f", .)


  cbind(
    "Variable" = columns,
    "Mean" = WTP[, paste0(i, "post.mean")] %>% mean() %>% round(3) %>% sprintf("%.3f", .),
    "Median" = WTP[, paste0(i, "post.mean")] %>% median() %>% round(3) %>% sprintf("%.3f", .),
    "SD" = WTP[, paste0(i, "post.sd")] %>% median() %>% round(3) %>% sprintf("%.3f", .),
    "95% C-I" = paste0("(",Lower, ") - (", Upper, ")")
) %>% data.frame() %>% return()
}


## List of attributes to loop through
Variables <- c(
  "beta_Tax.",
  "b_Colour2.",
  "b_Colour.",
  "b_Smell2.",
  "b_Smell.",
  "b_Sound2.",
  "b_Sound.",
  "b_Deadwood2.",
  "b_Deadwood."
)



# Initialize an empty list to store the results
summary_results <- list()

# The actual loop
for (i in Variables){
  summary_results[[i]] <- calculate_summary_table(WTP, i)
}


## Rearrange here
Table3 <- do.call(rbind, summary_results)


Table3$Variable <-
  c(
    "Tax",
    "Colour: high",
    "Colour: medium",

    "Smell: high",
    "Smell: medium",

    "Sound: high",
    "Sound: medium",

    "Deadwood: high",
    "Deadwood: medium"
    )




# *****************************
# Section 3: Export Data ####
# *****************************


## Output to console for easy ctrl C+V
write.csv(Table3, quote = FALSE)

TableB9 <- Table3
# Output to correct location
TableB9 %>% fwrite(
  sep = ",",
  here("OtherOutput/Tables", "TableB9_WTPSummary.txt"),
  row.names = TRUE,
  quote = FALSE
)



# **********************************************************************************
#### END OF SCRIPT ####
## Next step: 18_Winter_TableB10_Sarrias.R
# **********************************************************************************
