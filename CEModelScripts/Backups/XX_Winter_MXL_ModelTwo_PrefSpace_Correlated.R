#### RELATE WP5: Winter ####
## Function: Estimates correlated WTP-space MXL for Winter data (N: 1711)
## Author: Dr Peter King (p.king1@leeds.ac.uk)
## Last change: 21/02/2024


# *******************************************************************************
# Replication Information: ####
# Selected output of 'sessionInfo()'
# *******************************************************************************


# R version 4.3.1 (2023 - 06 - 16 ucrt)
# Platform:x86_64 - w64 - mingw32 / x64 (64 - bit)
# Running under:Windows 11 x64 (build 22621)
#
# Matrix products:default
#
#
# locale:[1] LC_COLLATE = English_United Kingdom.utf8  LC_CTYPE = English_United Kingdom.utf8
# [3] LC_MONETARY = English_United Kingdom.utf8 LC_NUMERIC = C
# [5] LC_TIME = English_United Kingdom.utf8
#
# time zone:Europe / London
# tzcode source:internal
#
# attached base packages:[1] parallel  stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:[1] AMR_2.1.0          cobalt_4.5.1       WeightIt_0.14.2    MatchIt_4.5.4      reshape2_1.4.4
# [6] udunits2_0.13.2.1  PostcodesioR_0.3.1 geosphere_1.5 - 18   psych_2.3.6        readxl_1.4.3
# [11] ggdist_3.3.0       ggridges_0.5.4     matrixStats_1.0.0  Rfast_2.0.8        RcppZiggurat_0.1.6
# [16] Rcpp_1.0.11        data.table_1.14.8  here_1.0.1         mded_0.1 -
#   2         magrittr_2.0.3
# [21] lubridate_1.9.2    forcats_1.0.0      stringr_1.5.0      dplyr_1.1.2        purrr_1.0.2
# [26] readr_2.1.4        tidyr_1.3.0        tibble_3.2.1       ggplot2_3.4.3      tidyverse_2.0.0
# [31] apollo_0.3.0
#
# loaded via a namespace (and not attached):[1] tidyselect_1.2.0     farver_2.1.1         fastmap_1.1.1        digest_0.6.33
# [5] timechange_0.2.0     lifecycle_1.0.3      survival_3.5 - 7       Rsolnp_1.16
# [9] compiler_4.3.1       rlang_1.1.1          tools_4.3.1          utf8_1.2.3
# [13] yaml_2.3.7           knitr_1.43           sp_2.0 - 0             mnormt_2.1.1
# [17] plyr_1.8.8           withr_2.5.0          numDeriv_2016.8 - 1.1  grid_4.3.1
# [21] fansi_1.0.4          colorspace_2.1 - 0     scales_1.2.1         iterators_1.0.14
# [25] MASS_7.3 - 60          mcmc_0.9 - 7           cli_3.6.1            mvtnorm_1.2 -
#   2
# [29] crayon_1.5.2         rmarkdown_2.24       miscTools_0.6 - 28     generics_0.1.3
# [33] rstudioapi_0.15.0    httr_1.4.7           tzdb_0.4.0           splines_4.3.1
# [37] cellranger_1.1.0     vctrs_0.6.3          Matrix_1.6 - 1         sandwich_3.0 -
#   2
# [41] SparseM_1.81         MCMCpack_1.6 - 3       hms_1.1.3            RSGHB_1.2.2
# [45] foreach_1.5.2        glue_1.6.2           chk_0.9.0            codetools_0.2 -
#   19
# [49] rngWELL_0.10 - 9       distributional_0.3.2 stringi_1.7.12       gtable_0.3.4
# [53] randtoolbox_2.0.4    munsell_0.5.0        pillar_1.9.0         htmltools_0.5.6
# [57] quantreg_5.97        truncnorm_1.0 - 9      R6_2.5.1             maxLik_1.5 -
#   2
# [61] doParallel_1.0.17    rprojroot_2.0.3      evaluate_0.21        lattice_0.21 -
#   8
# [65] backports_1.4.1      bgw_0.1.2            MatrixModels_0.5 - 2   coda_0.19 -
#   4
# [69] nlme_3.1 - 163         xfun_0.40            zoo_1.8 - 12           pkgconfig_2.0.3

# renv::snapshot()
rm(list=ls())
library(here)

library(tidyr)
library(apollo)
library(ggridges)
library(ggplot2)
library(reshape2)
library(dplyr)
library(magrittr)
library(ggdist)
library(RColorBrewer)
library(data.table)


# *******************************************************************************
# Section 1: Import Data ####
# Selected output of 'sessionInfo()'
# *******************************************************************************


## Read in latest version of data
# database <- here("CEInputData",
#                  "database_Winter_Step1_Anonymised.csv") %>% fread() %>% data.frame()
database <- here("CEInputData","database_Winter_Step1.csv") %>% fread() %>% data.frame()


database$CountryDummy <- ifelse(database$Country == 0, 0, 1)


database$Impairment <- ifelse((database$SightIssues==1)|
                                (database$SmellIssues==1)|
                                (database$HearingIssues==1),1,0)



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
  "Deadwood2",
  "Gender",
  "DummyAge",
  "MilesDistance",
  "IncomeDummy",
  "Impairment",
  "SmellIssues",
  "HearingIssues",
  "SightIssues",
  "Country",
  "CountryDummy",
  "EthnicityDummyWhite",
  "Urbanicity",
  "MostRecentVisit",
  "WoodlandsScore"
)]



# *******************************************************************************
# Section 2: Estimation Statistics ####
# Selected output of 'sessionInfo()'
# *******************************************************************************


apollo_initialise()
apollo_control = list(
  nCores    = 10,
  ## Watch out if not using HPC!
  mixing    = TRUE,
  modelDescr = "Winter_MXL_ModelTwo_PrefSpace_Correlated",
  modelName  = "Winter_MXL_ModelTwo_PrefSpace_Correlated",
  indivID    = "Respondent",
  outputDirectory = "CEOutputData/H3"
)


## Define parameters starting values:
### Note starting mean of tax at -3 to avoid issues later when using the lognormal distribution
apollo_beta = c(
  asc_A      = 0,
  asc_B      = 0,
  asc_C = -0.15137,
  mu_Tax        = -3,
  mu_Sound = 0.25171,
  mu_Smell = 0.08875,
  mu_Colour = 0.13937,
  mu_Deadwood = 0.24873,
  mu_Sound2 = 0.26818,
  mu_Smell2 = 0.12855,
  mu_Colour2 = 0.07410,
  mu_Deadwood2 = 0.43725,
  sig_Tax = 0,
  sig_Sound         = 0,
  sig_Smell         = 0,
  sig_Colour        = 0,
  sig_Deadwood      = 0,
  sig_Sound2        = 0,
  sig_Smell2        = 0,
  sig_Colour2       = 0,
  sig_Deadwood2     = 0,

  beta_Gender = 0,
  beta_Age  = 0,
  beta_Distance = 0,
  beta_Charity  = 0,
  beta_Income  = 0,
  beta_Impairment = 0,
  beta_Country  = 0,
  beta_White = 0,
  beta_Urbanicity  = 0,
  beta_MostRecentVisit  = 0,
  beta_WoodlandsScore = 0,



  sig_Sound_Colour   = 0,
  sig_Smell_Colour = 0,
  sig_Deadwood_Colour = 0,

  sig_Sound_Smell   = 0,
  sig_Colour_Smell = 0,
  sig_Deadwood_Smell = 0,

  sig_Smell_Sound = 0,
  sig_Colour_Sound = 0,
  sig_Deadwood_Sound = 0,

  sig_Sound_Deadwood   = 0,
  sig_Smell_Deadwood = 0,
  sig_Colour_Deadwood = 0,




  sig_Sound_Colour2   = 0,
  sig_Smell_Colour2 = 0,
  sig_Deadwood_Colour2 = 0,

  sig_Sound_Smell2   = 0,
  sig_Colour_Smell2 = 0,
  sig_Deadwood_Smell2 = 0,

  sig_Smell_Sound2 = 0,
  sig_Colour_Sound2 = 0,
  sig_Deadwood_Sound2 = 0,

  sig_Sound_Deadwood2   = 0,
  sig_Smell_Deadwood2 = 0,
  sig_Colour_Deadwood2 = 0
)


## Hold Alternative-Specific Constants for non-status-quo options at zero
apollo_fixed = c("asc_A","asc_B")


## Set parameters for generating draws
### Note that draws by attribute (5) or by level (9) a big difference!
apollo_draws = list(
  interDrawsType = "pmc",## Robust to using MLHS or Sobol draws
  interNDraws    = 1000, ## Same results if you use 5000 draws
  interUnifDraws = c(),
  interNormDraws = c(
    "draws_Tax",
    "draws_Smell",
    "draws_Sound",
    "draws_Colour",
    "draws_Deadwood",
    "draws_Smell2",
    "draws_Sound2",
    "draws_Colour2",
    "draws_Deadwood2"
  ))



## Create random parameters
### Note lognormal for tax attribute to impose negative signs
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["beta_Tax"]] = -exp(mu_Tax + sig_Tax * draws_Tax )

  randcoeff[["b_Smell"]] =  (mu_Smell + sig_Smell * draws_Smell  +
                               sig_Smell_Colour * draws_Colour +
                               sig_Smell_Sound * draws_Sound +
                               sig_Smell_Deadwood * draws_Deadwood)

  randcoeff[["b_Sound"]] =  (mu_Sound + sig_Sound * draws_Sound +
                               sig_Sound_Colour * draws_Colour +
                               sig_Sound_Smell * draws_Smell +
                               sig_Sound_Deadwood * draws_Deadwood)

  randcoeff[["b_Colour"]] =  (mu_Colour + sig_Colour * draws_Colour   +
                                sig_Colour_Smell * draws_Smell +
                                sig_Colour_Sound * draws_Sound +
                                sig_Colour_Deadwood * draws_Deadwood)

  randcoeff[["b_Deadwood"]] =  (mu_Deadwood + sig_Deadwood * draws_Deadwood   +
                                  sig_Deadwood_Colour * draws_Colour +
                                  sig_Deadwood_Sound * draws_Sound +
                                  sig_Deadwood_Smell * draws_Smell )

  randcoeff[["b_Smell2"]] =  (mu_Smell2 + sig_Smell2 * draws_Smell2   +
                                sig_Smell_Colour2 * draws_Colour2 +
                                sig_Smell_Sound2 * draws_Sound2 +
                                sig_Smell_Deadwood2 * draws_Deadwood2)

  randcoeff[["b_Sound2"]] =  (mu_Sound2 + sig_Sound2 * draws_Sound2   +
                                sig_Sound_Colour2 * draws_Colour2 +
                                sig_Sound_Smell2 * draws_Smell2 +
                                sig_Sound_Deadwood2 * draws_Deadwood2)

  randcoeff[["b_Colour2"]] =  (mu_Colour2 + sig_Colour2 * draws_Colour2   +
                                 sig_Colour_Smell2 * draws_Smell2 +
                                 sig_Colour_Sound2 * draws_Sound2 +
                                 sig_Colour_Deadwood2 * draws_Deadwood2)

  randcoeff[["b_Deadwood2"]] =  (mu_Deadwood2 + sig_Deadwood2 * draws_Deadwood2   +
                                   sig_Deadwood_Colour2 * draws_Colour2 +
                                   sig_Deadwood_Sound2 * draws_Sound2 +
                                   sig_Deadwood_Smell2 * draws_Smell2)
  return(randcoeff)
}


apollo_inputs = apollo_validateInputs() ## Required to check inputs are fine


# ****************************
# Estimation Specification: ####
### Note: Model in WTP-space.
# ****************************

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){

  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))


  asc_C1 = asc_C + beta_Gender * Gender + beta_Age * DummyAge +
    beta_Distance * MilesDistance + beta_Charity * Charity +
    beta_Income * IncomeDummy + beta_Impairment * Impairment +
    beta_Country * Country + beta_White * EthnicityDummyWhite +
    beta_Urbanicity * Urbanicity +
    beta_MostRecentVisit * MostRecentVisit +
    beta_WoodlandsScore * WoodlandsScore


  P = list()

  V = list()
  V[['A']]  = asc_A  + beta_Tax*Tax1 +
    b_Sound  * (Sound1==1) + b_Sound2  * (Sound1==2) +
    b_Smell*(Smell1==1) + b_Smell2*(Smell1==2) +
    b_Colour* (Colour1==1) +b_Colour2* (Colour1==2) +
    b_Deadwood*(Deadwood1==7) + b_Deadwood2*(Deadwood1==15)
  V[['B']]  = asc_B  + beta_Tax*Tax2 +
    b_Sound  * (Sound2==1) + b_Sound2  * (Sound2==2)  +
    b_Smell*(Smell2==1) + b_Smell2*(Smell2==2) +
    b_Colour*(Colour2==1) + b_Colour2* (Colour2==2) +
    b_Deadwood*(Deadwood2==7) +b_Deadwood2*(Deadwood2==15)
  V[['C']]  = asc_C1

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

  ## Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)

  ## Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# *******************************************************************************
# Section 5: Model Estimation ####
# *******************************************************************************


# Actually estimates the model
Winter_MXL_ModelTwo_PrefSpace_Correlated = apollo_estimate(apollo_beta,
                                                apollo_fixed,
                                                apollo_probabilities,
                                                apollo_inputs)


## Model output and results here alongside saving information
apollo_modelOutput(Winter_MXL_ModelTwo_PrefSpace_Correlated,
                   modelOutput_settings = list(printPVal = TRUE))
apollo_saveOutput(Winter_MXL_ModelTwo_PrefSpace_Correlated,
                  saveOutput_settings = list(printPVal = TRUE))
# saveRDS(Winter_MXL_ModelTwo_PrefSpace_Correlated, file="Winter_MXL_ModelTwo_PrefSpace_Correlated.rds")


# ***********************************************
# Section 6: Output WTP ####
# ***********************************************

Model <-
  here("CEOutputData/H3",
       "Winter_MXL_ModelTwo_PrefSpace_Correlated_model.rds") %>% readRDS()


## UNCONDITIONAL WTP: 1000 DRAWS PER ATTRIBUTE LEVEL
apollo_unconditionals(Model,
                      apollo_probabilities,
                      apollo_inputs) %>%
  data.frame() %>%
  fwrite(sep = ",",
         here(
           "CEOutputData/H3",
           "Winter_MXL_ModelTwo_PrefSpace_Correlated_Unconditionals.csv"
         ))



## CONDITIONALS SO MUCH SMALLER
apollo_conditionals(Model,
                    apollo_probabilities,
                    apollo_inputs) %>%
  data.frame() %>%
  fwrite(sep = ",",
         here(
           "CEOutputData/H3",
           "Winter_MXL_ModelTwo_PrefSpace_Correlated_Conditionals.csv"
         ))


# End Of Script --------------------------------------------------------------
