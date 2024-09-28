#### Stated preferences for the colours, smells and sounds of biodiversity  ###############
# Project: ERC Relate Project. Work Package 5
# Author: Dr Peter King (p.king1@leeds.ac.uk)
# Function: To create Table 1A and Table 1B and output the MXL highlights
# Notes: Now called Table 1A but was previously "Table2"
# Last Edited: 28/09/2024
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
library(apollo)
library(data.table)
library(here)
library(tidyverse)


 # *************************************************************************
#### Section 1: Importing Model,  Estimates,  and WTP for FULL SAMPLE models ####
 # *************************************************************************



ModelOne_Model <-
  readRDS(here("CEoutput/ModelOne", "Winter_MXL_ModelOne_AllCorrelations.rds"))
ModelOne_WTP <-
  here("CEoutput/ModelOne", "Winter_MXL_ModelOne_AllCorrelations_ConWTP.csv") %>% fread() %>% data.frame()
ModelOne_Estimates <-
  here("CEoutput/ModelOne", "Winter_MXL_ModelOne_AllCorrelations_estimates.csv") %>% fread() %>% data.frame()


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
                 paste0(Estimates$Estimate %>% round(3) %>% sprintf("%.3f", .),  "***"),
                 ifelse(
                   Estimates$Rob.p.val.0. < 0.05,
                   paste0(Estimates$Estimate %>% round(3) %>% sprintf("%.3f", .),  "**"),
                   ifelse(
                     Estimates$Rob.p.val.0. < 0.1,
                     paste0(Estimates$Estimate %>% round(3) %>% sprintf("%.3f", .),  "*"),
                     paste0(Estimates$Estimate %>% round(3) %>% sprintf("%.3f", .)))
                 )),
               paste0("(", Estimates$Rob.std.err %>% round(3) %>% sprintf("%.3f", .), ")")
             ))
}


## New function to take model and report model stats
Diagnostics <- function(Model) {
  rbind(
    "N" = Model$nIndivs,
    "AIC" = Model$AIC %>% round(3) %>% sprintf("%.3f", .),
    "Adj.R2" = Model$adjRho2_C %>% round(3) %>% sprintf("%.3f", .),
    "LogLik" = Model$LLout %>% as.numeric() %>% round(3) %>% sprintf("%.3f", .)
  )
}


 # *************************************************************************
#### Section 3A: Output Model One part of the table ####
 # *************************************************************************


## Store model outputs for easy organisation
ModelOne_Output <- ModelOutputs(ModelOne_Estimates)
## Change rownames to variable for easy reference
rownames(ModelOne_Output) <- ModelOne_Output$Variable


## Stitch rows together in the order we want
ModelOne_Table <-
  bind_cols(
    "Mean" =
      bind_rows(
        ModelOne_Output["asc_C", 1:2],
        ModelOne_Output["mu_Tax", 1:2],
        ModelOne_Output["mu_Colour2", 1:2],
        ModelOne_Output["mu_Colour", 1:2],
        ModelOne_Output["mu_Smell2", 1:2],
        ModelOne_Output["mu_Smell", 1:2],
        ModelOne_Output["mu_Sound2", 1:2],
        ModelOne_Output["mu_Sound", 1:2],
        ModelOne_Output["mu_Deadwood2", 1:2],
        ModelOne_Output["mu_Deadwood", 1:2]
      ),
    "Standard Deviation" =
      bind_rows(
        ModelOne_Output["asc_C", 1:2],
        ModelOne_Output["sig_Tax", 1:2],
        ModelOne_Output["sig_Colour2", 1:2],
        ModelOne_Output["sig_Colour", 1:2],
        ModelOne_Output["sig_Smell2", 1:2],
        ModelOne_Output["sig_Smell", 1:2],
        ModelOne_Output["sig_Sound2", 1:2],
        ModelOne_Output["sig_Sound", 1:2],
        ModelOne_Output["sig_Deadwood2", 1:2],
        ModelOne_Output["sig_Deadwood", 1:2]
      )
  )

 # *************************************************************************
#### Section 3B: Output Model Two part of the table ####
 # *************************************************************************



## Store model outputs for easy organisation
ModelTwo_Output <- ModelOutputs(ModelTwo_Estimates)
## Change rownames to variable for easy reference
rownames(ModelTwo_Output) <- ModelTwo_Output$Variable


## Stitch rows together in the order we want
ModelTwo_Table <-
  bind_cols(
    "Mean" =
      bind_rows(
        ModelTwo_Output["asc_C", 1:2],
        ModelTwo_Output["mu_Tax", 1:2],
        ModelTwo_Output["mu_Colour2", 1:2],
        ModelTwo_Output["mu_Colour", 1:2],
        ModelTwo_Output["mu_Smell2", 1:2],
        ModelTwo_Output["mu_Smell", 1:2],
        ModelTwo_Output["mu_Sound2", 1:2],
        ModelTwo_Output["mu_Sound", 1:2],
        ModelTwo_Output["mu_Deadwood2", 1:2],
        ModelTwo_Output["mu_Deadwood", 1:2]
      ),
    "Standard Deviation" =
      bind_rows(
        ModelTwo_Output["asc_C", 1:2],
        ModelTwo_Output["sig_Tax", 1:2],
        ModelTwo_Output["sig_Colour2", 1:2],
        ModelTwo_Output["sig_Colour", 1:2],
        ModelTwo_Output["sig_Smell2", 1:2],
        ModelTwo_Output["sig_Smell", 1:2],
        ModelTwo_Output["sig_Sound2", 1:2],
        ModelTwo_Output["sig_Sound", 1:2],
        ModelTwo_Output["sig_Deadwood2", 1:2],
        ModelTwo_Output["sig_Deadwood", 1:2]
      )
  )


 # *************************************************************************
#### Section 4: Create tables ####
 # *************************************************************************


## Top part with mean and SDs
Table2_Top <- bind_cols("Variable" = ModelOne_Table[, 1],
                    "MeanOne" = ModelOne_Table[, 2],
                    "SDOne" = ModelOne_Table[, 4],
                    "MeanTwo" = ModelTwo_Table[, 2],
                    "SDTwo" = ModelTwo_Table[, 4])


## Artificially fix ASC row
Table2_Top[Table2_Top$Variable == "asc_C",] <-
  c("asc_C", ModelOne_Output["asc_C", 1:2], ModelTwo_Output["asc_C", 1:2])



## New reveiwer-requested bottom row
Table2_Bottom <-  bind_cols(
  "Variable" = c("N", "AIC", "Adj.R2", "LogLike"),
  "MeanOne" = Diagnostics(ModelOne_Model)[, 1],
  "SDOne" = 0,
  "MeanTwo" = Diagnostics(ModelTwo_Model)[, 1],
  "SDTwo" = 0
)

Table2 <- rbind(Table2_Top,
                Table2_Bottom)

## Changing for naming correctness post-R2
Table1A <- Table2

# *************************************************************************
#### Section 5: Output tables ####
# *************************************************************************


## Output to screen in a nice format for making tables in word
Table1A %>% write.csv(quote = FALSE)
## Output to a discrete file if that's helpful
Table1A %>% fwrite(
  sep = ",",
  here("OtherOutput/Tables", "Table1A.txt"),
  row.names = TRUE,
  quote = FALSE
)



 # *************************************************************************
#### END OF SCRIPT ####
## Next Step: 15_Winter_Table2_ModelOutputs.R
 # *************************************************************************
