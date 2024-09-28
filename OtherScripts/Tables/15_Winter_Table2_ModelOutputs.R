#### Stated preferences for the colours, smells and sounds of biodiversity  ###############
# Project: ERC Relate Project. Work Package 5
# Author: Dr Peter King (p.king1@leeds.ac.uk)
# Function: To output Table2, or most of its information at least
# Notes: I added useful text in the manuscript and should have done here
# Last Edited: 28/09/2024
# Changes:
# - Changed to correlated models
# - Misc refactoring using chatgpt to make more legible
# - Changed hard coding to dynamic subsetting by "beta"
# - double-checking post R2
# - updating sessioninfo()

 # *************************************************************************
#### Section 0: Setup and estimate models ####
 # *************************************************************************

# Replication information: ------------------------------------------------
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

# Libraries: ----------------------------
library(stats)
library(data.table)
library(here)
library(tidyverse)
library(magrittr)

 # *************************************************************************
#### Section 1: Importing Model,  Estimates,  and WTP for FULL SAMPLE models ####
 # *************************************************************************


## Read in participant data here:
Winter <- here("OtherData", "Winter_dataframe_Step4.csv") %>% fread() %>% data.frame()
Winter$CountryDummy <- ifelse(Winter$Country == 0, 0, 1)

## Model with covariates
ModelTwo_Model <-
  readRDS(here("CEoutput/ModelTwo", "Winter_MXL_ModelTwo_AllCorrelations.rds"))

ModelTwo_WTP <-
  here("CEoutput/ModelTwo", "Winter_MXL_ModelTwo_AllCorrelations_ConWTP.csv") %>% fread() %>% data.frame()

ModelTwo_Estimates <-
  here("CEoutput/ModelTwo", "Winter_MXL_ModelTwo_AllCorrelations_estimates.csv") %>% fread() %>% data.frame()



 # *************************************************************************
#### Section 2: Model Summary Function ####
 # *************************************************************************



## So this code outputs a table of estimate,  p.v stars and s.e in brackets ##
### To make it easy,  just change the model name here and the code will output the table for your model:
ModelOutputs <- function(Estimates) {
  data.frame("Variable" =  Estimates$V1,
             "Estimate" =  paste(
               ifelse(
                 Estimates$Rob.p.val.0. < 0.01,
                 paste0(round(Estimates$Estimate,  3),  "***"),
                 ifelse(
                   Estimates$Rob.p.val.0. < 0.05,
                   paste0(round(Estimates$Estimate,  3),  "**"),
                   ifelse(
                     Estimates$Rob.p.val.0. < 0.1,
                     paste0(round(Estimates$Estimate,  3),  "*"),
                     round(Estimates$Estimate,  3)
                   )
                 )
               ),
               paste0("(", round(Estimates$Rob.std.err.,  3), ")")
             ))
}


 # *************************************************************************
#### Section 3: Summarise sociodemographic variables ####
 # *************************************************************************



## Make a list of variables included in ModelTwo using their variable names
## Variables meaning socioeconomic ones
Variables <- ModelTwo_Estimates$V1 %>% ## now subset by our betas
  .[grepl("beta", .)] %>% ## thanks for suggesting grepl chatgpt
  c() %>%
  str_replace_all( ## now modify labels for the manuscript
    c(
      beta_ = "",
      Age = "DummyAge",
      Distance = "MilesDistance",
      White = "EthnicityDummyWhite",
      Income = "IncomeDummy",
      Country = "CountryDummy"
    )
  )


## Now using that list,
# summarise the mean and stddev,
# then rearrange the table
# and round nicely
Table3_LeftSide <- Winter %>%
  summarize(across(all_of(Variables), list(
    VariableMean = mean,
    Std.Dev.  = sd
  ))) %>%
  pivot_longer(
    cols      = everything(),
    names_sep = "_",
    names_to  = c("variable", "statistic")
  ) %>%
  pivot_wider(names_from = "statistic") %>%
  mutate(
    VariableMean = VariableMean %>%
      round(3) %>%
      sprintf("%.2f",. ),
    Std.Dev. = Std.Dev. %>%
      round(3) %>%
      sprintf("%.2f",. ))


 # *************************************************************************
#### Section 4: Summarise variable estimates ####
 # *************************************************************************




## Store model outputs for easy organisation
ModelTwo_Output <- ModelOutputs(ModelTwo_Estimates)

## Output the right hand side of the table
Table3_RightSide <- ModelTwo_Output %>% filter(Variable %>% grepl("beta", .))

 # *************************************************************************
#### Section 4: Output tables ####
 # *************************************************************************


## Also rearrange to have order in the manuscript
Table3 <- bind_cols(Table3_LeftSide,
                    "Estimates" = Table3_RightSide[, 2]) %>%
  slice(match(c("Gender",
                "DummyAge",
                "EthnicityDummyWhite" ,
                "IncomeDummy" ,
                "Urbanicity" ,
                "CountryDummy"), variable))

## Renaming for consistency for the manuscript
Table2 <- Table3
## Output to screen in a nice format for making tables in word
Table2 %>% write.csv(quote = FALSE, row.names = FALSE)
## Output to a discrete file if that's helpful
Table2 %>% fwrite(
  sep = ",",
  here("OtherOutput/Tables", "Table2.txt"),
  row.names = TRUE,
  quote = FALSE
)





 # *************************************************************************
#### END OF SCRIPT ####
## Next Step: 16_Winter_TableB8_ModelOutputs.R
 # *************************************************************************
