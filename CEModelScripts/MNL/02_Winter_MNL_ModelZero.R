#### Stated preferences for the colours, smells and sounds of biodiversity  ###############
# Project: ERC Relate Project. Work Package 5
# Author: Dr Peter King (p.king1@leeds.ac.uk)
# Function: To provide starting values using the multinomial logit
# Last Edited: 28/09/2024
# - double-checking that it runs without errors
# - Reported values in Table B3



# ****************************
# Replication Information: ####
# ****************************


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

# ****************************
# Setup Environment: ####
# ****************************


## Libraries that will come in handy later
library(apollo)
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggridges)
library(reshape2)
library(mded)
library(here)
library(data.table)

# ****************************
# Import Data: ####
# ****************************


database <-
  here("CEModelData", "database_Winter_Step1.csv") %>% fread() %>% data.frame()


## You can drop serial opt-outs here but we don't in-text.
# database <- database[database$SerialSQ == 1,]

## Keep only relevant columns
database <- database[, c(
  "Choice",
  "Respondent",
  "Season",
  "Task",
  "ID",
  "Tax1",
  "Tax2",
  "Sound1",
  "Sound2",
  "Smell1",
  "Smell2",
  "Colour1",
  "Colour2",
  "Deadwood1",
  "Deadwood2"
)]


## Necessary to get apollo working
apollo_initialise()


# ****************************
# Estimation Basics: ####
# ****************************


apollo_control = list(
  modelDescr = "Winter_MNL_ModelZero",
  modelName  = "Winter_MNL_ModelZero",
  ## Added dates last verified
  indivID    = "Respondent",
  ## This is the name of a column in the database indicating each unique respondent
  outputDirectory = "CEoutput/MNL"
)


## Define parameters starting values:
### Note starting mean of tax at -3 to avoid issues later when using the lognormal distribution
apollo_beta = c(
  asc_A      = 0,
  asc_B      = 0,
  asc_C = 0,
  beta_Tax    = 0,
  b_Sound   = 0,
  b_Smell = 0,
  b_Colour = 0,
  b_Deadwood = 0,
  b_Sound2   = 0,
  b_Smell2 = 0,
  b_Colour2 = 0,
  b_Deadwood2 = 0
)


## Hold Alternative-Specific Constants for non-status-quo options at zero
apollo_fixed = c("asc_A", "asc_B")


apollo_inputs = apollo_validateInputs() ## Required to check inputs are fine


# ****************************
# Estimation Specification: ####
### Note: Model in pref-space.
# ****************************

apollo_probabilities = function(apollo_beta,
                                apollo_inputs,
                                functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  P = list()

  V = list()
  V[['A']]  = asc_A +
    b_Sound  * (Sound1 == 1) + b_Sound2  * (Sound1 == 2) +
    b_Smell * (Smell1 == 1) + b_Smell2 * (Smell1 == 2) +
    b_Colour * (Colour1 == 1) + b_Colour2 * (Colour1 == 2) +
    b_Deadwood * (Deadwood1 == 7) + b_Deadwood2 * (Deadwood1 == 15) +
    beta_Tax * Tax1


  V[['B']]  =  asc_B +
    b_Sound  * (Sound2 == 1) + b_Sound2  * (Sound2 == 2)  +
    b_Smell * (Smell2 == 1) + b_Smell2 * (Smell2 == 2) +
    b_Colour * (Colour2 == 1) + b_Colour2 * (Colour2 == 2) +
    b_Deadwood * (Deadwood2 == 7) + b_Deadwood2 * (Deadwood2 == 15) +
    beta_Tax * Tax2



  V[['C']]  = asc_C

  mnl_settings = list(
    alternatives = c(A = 1, B = 2, C = 3),
    avail        = list(A = 1, B = 1, C = 1),
    choiceVar    = Choice,
    V            = V
  )

  ## Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)

  ## Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)

  ## Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


# ****************************
# Model Outputs: ####
# ****************************

#
# ## Actually estimates the model
Winter_MNL_ModelZero = apollo_estimate(apollo_beta,
                                       apollo_fixed,
                                       apollo_probabilities,
                                       apollo_inputs)

# Model output and results here alongside saving information
apollo_modelOutput(Winter_MNL_ModelZero, modelOutput_settings = list(printPVal =
                                                                       TRUE))



apollo_saveOutput(Winter_MNL_ModelZero, saveOutput_settings = list(printPVal =
                                                                     TRUE))



# *********************************************************************************************************
#### END OF SCRIPT ####
## Next Step: 03_Winter_MXL_ModelOne_PrefSpace.R
# *********************************************************************************************************
