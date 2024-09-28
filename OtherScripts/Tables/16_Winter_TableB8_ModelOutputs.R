#### Stated preferences for the colours, smells and sounds of biodiversity  ###############
# Project: ERC Relate Project. Work Package 5
# Author: Dr Peter King (p.king1@leeds.ac.uk)
# Function: To output Table B8 with model outputs
# Notes: should work
# Last Edited: 28/09/2024
# Changes:
# - Using conditional WTP from correlated model
# - Using Mann-Whitney means test
# - double-checking post R2
# - updating sessioninfo()


# ******************************
# Replication Information: ####
# ******************************


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
Winter <- here("OtherData","Winter_dataframe_Step4.csv") %>% fread() %>% data.frame()


## Import new WTP
WTP <-
  here("CEoutput/ModelTwo",
       "Winter_MXL_ModelTwo_AllCorrelations_ConWTP.csv") %>% fread() %>% data.frame()


# **********************************************************
# Section 2: Truncate Data ####
# **********************************************************


## Drop missing rows
Winter <-  Winter[!is.na(Winter$MilesDistance), ] ## Drop missing distances
Winter <-  Winter[!is.na(Winter$Overall), ] ## Drop respondents not completing BIOWELL


## If the conditionals are imported then run this to recover only useful variables
WTP <- WTP[WTP %>% dplyr::select(-ends_with(c(".ID", ".post.sd"))) %>% colnames()] %>% data.frame()



WTP <-
  bind_cols(
    "Colour_WTP_High" = WTP$b_Colour2.post.mean,
    "Colour_WTP_Medium" = WTP$b_Colour.post.mean,
    "Smell_WTP_High" = WTP$b_Smell2.post.mean,
    "Smell_WTP_Medium" = WTP$b_Smell.post.mean,
    "Sound_WTP_High" = WTP$b_Sound2.post.mean,
    "Sound_WTP_Medium" = WTP$b_Sound.post.mean,
    "Deadwood_WTP_High" = WTP$b_Deadwood2.post.mean,
    "Deadwood_WTP_Medium" = WTP$b_Deadwood.post.mean
  )



Data <- cbind(
  Winter[1:217],
  WTP
)


# **********************************************************
# Section 3: Calculate test results ####
## Here we test whether WTP without impairment (==0) is greater
## than WTP with an impairment (==1)
# **********************************************************


## Function that adds significance stars
AddStars <- function(Data) {
  ifelse(
    Data < 0.01,
    paste0(Data %>% round(3) %>% sprintf("%.3f", .), "***"),
    ifelse(
      Data < 0.05,
      paste0(Data %>% round(3) %>% sprintf("%.3f", .), "**"),
      ifelse(
        Data < 0.1,
        paste0(Data %>% round(3) %>% sprintf("%.3f", .), "*"),
        paste0(Data %>% round(3) %>% sprintf("%.3f", .), " "))))
}


Variables <-  c(
"Colour_WTP_High",
"Colour_WTP_Medium",
"Smell_WTP_High",
"Smell_WTP_Medium",
"Sound_WTP_High",
"Sound_WTP_Medium",
"Deadwood_WTP_High",
"Deadwood_WTP_Medium"
)


Formatter <- function(data.frame){
  data.frame %>%
    summarise(Result = paste0(statistic %>%
                                round(3) %>%
                                sprintf("%.3f", .),
                              " (",
                              p.value %>% AddStars(),
                              ")"))
}


## Initialise vectors to store loop outputs in
Loop_InnerResults <- vector("list", length = 8)
Loop_OuterResults  <- vector("list", length = 8)
Headers <- Variables

## Loop through variables
for (V in Variables){
  for (V2 in Variables){

    C <- wilcox.test(Data[, V],
                  Data[, V2])[c("statistic", "p.value")] %>%
    data.frame() %>%
    Formatter()

      Loop_InnerResults[match(V2, Variables)] <- C
  }

  Headers[[grep(V, unlist(Variables))]] <- Data[, V] %>%
    data.frame() %>%
    summarise(paste0(V %>%
                       gsub(pattern = "_WTP_", replacement = "s: "),
                     " (Mean: ",
                     mean(.) %>%
                       round(3) %>%
                       sprintf("%.3f", .),
                     ", SD: ",
                     sd(.) %>%
                       round(3) %>%
                       sprintf("%.3f", .), ")")) %>%
    as.character()

  Loop_OuterResults[[grep(V, unlist(Variables))]] <- do.call(bind_cols, Loop_InnerResults)
}


## Stitch together into data.frame
Results <- rbindlist(use.names = FALSE, idcol = TRUE, Loop_OuterResults) %>% data.frame()

## Append ID column
Results$.id <- Headers

## Correct column names
colnames(Results) <- c("Variable", Variables)


## Add easier row labels
Results <- rbind(
Attribute <- c("Variable", "Colours", "Colours", "Smells", "Smells", "Sounds", "Sounds", "Deadwood for decomposition", "Deadwood for decomposition"),
Level <- c("Variable", "High", "Medium", "High", "Medium", "High", "Medium", "High", "Medium"),
Results)


# **********************************************************
# Section 4: Export results ####
# **********************************************************


TableB8 <- Results


## Output here
TableB8 %>% fwrite(
  sep = "#",
  here("OtherOutput/Tables",
       "TableB8_AllWTPTests.txt"),
  row.names = TRUE,
  quote = FALSE
)


# **********************************************************************************
#### END OF SCRIPT ####
## Next step: 17_Winter_TableB9_WTPSummary.R
# **********************************************************************************
