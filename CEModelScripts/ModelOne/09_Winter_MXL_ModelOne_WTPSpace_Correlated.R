#### Stated preferences for the colours, smells and sounds of biodiversity  ###############
# Project: ERC Relate Project. Work Package 5
# Author: Dr Peter King (p.king1@leeds.ac.uk)
# Function: To estimate the WTP-space MXL specification Model One
# Function: Featured in Tables 1A, 1B, B6
# Notes: should work
# Last Edited: 28/09/2024
# - correct starting value
# - double-checking post R2
# - removing commented out old code


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

## Clear workspace:
rm(list = ls())

## This sometimes fixes encoding issues using a HPC:
Sys.setlocale("LC_ALL","C")


## Note: My working directory is the zip file and then
###I specify extensions when running scripts or importing data:
# setwd("K:/WinterAnalysis1307/WP5/WinterReplication")


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


database <- here("CEModelData", "database_Winter_Step1.csv") %>% fread() %>% data.frame()


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

apollo_initialise()


# ****************************
# Estimation Basics: ####
# ****************************


## Note 10 cores as I'm using the University of Kent 'Tesla' HPC:
apollo_control = list(
  nCores    = 10,
  analyticGrad    = TRUE, # following manual here
  mixing    = TRUE,
  modelDescr = "Winter_MXL_ModelOne_AllCorrelations",
  modelName  = "Winter_MXL_ModelOne_AllCorrelations", ## Added dates last verified
  indivID    = "Respondent", ## This is the name of a column in the database indicating each unique respondent
  outputDirectory = "CEoutput/ModelOne"
)


## Define parameters starting values:
### Note starting mean of tax at -3 to avoid issues later when using the lognormal distribution
apollo_beta = c(
  asc_A      = 0,
  asc_B      = 0,
  asc_C = -1.450,



  mu_Tax = -3.7355,
  mu_Sound = 2.8542,
  mu_Smell = 1.8859,
  mu_Colour = -5.3266,
  mu_Deadwood = -2.3712,
  mu_Sound2 = -2.0527,
  mu_Smell2 = -5.6148,
  mu_Colour2 = -10.7500,
  mu_Deadwood2 = 3.9079,
  sig_Tax = 2.9901,
  sig_Colour2 = 12.6124,
  sig_Colour = -0.5181,
  sig_Smell2 = 9.2560,
  sig_Smell = 1.3572,
  sig_Sound2 = 11.5939,
  sig_Sound = 1.4390,
  sig_Deadwood2 = 11.4997,
  sig_Deadwood = 3.2791,

  # mu_Tax        = -3.287,
  # mu_Colour2 = 0.097,
  # mu_Colour = 0.197,
  # mu_Smell2 = 0.157,
  # mu_Smell = 0.170,
  # mu_Sound2 = 0.393,
  # mu_Sound = 0.202,
  # mu_Deadwood2 = 0.732,
  # mu_Deadwood = 0.252,
  # sig_Tax	= -2.802,
  # sig_Colour2	= -0.842,
  # sig_Colour	= 0.338,
  # sig_Smell2	= 0.494,
  # sig_Smell	= -0.266,
  # sig_Sound2	= -1.131,
  # sig_Sound	= -0.067,
  # sig_Deadwood2 	= -0.681,
  # sig_Deadwood	= 0.554,

  ## Group 1
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

  ## Group 2
  sig_Sound2_Colour   = 0,
  sig_Smell2_Colour = 0,
  sig_Deadwood2_Colour = 0,
  sig_Sound2_Smell   = 0,
  sig_Colour2_Smell = 0,
  sig_Deadwood2_Smell = 0,
  sig_Smell2_Sound = 0,
  sig_Colour2_Sound = 0,
  sig_Deadwood2_Sound = 0,
  sig_Sound2_Deadwood   = 0,
  sig_Smell2_Deadwood = 0,
  sig_Colour2_Deadwood = 0,

  ## Group 3
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
  sig_Colour_Deadwood2 = 0,

  ## Group 4
  sig_Sound1_Colour2   = 0,
  sig_Smell1_Colour2 = 0,
  sig_Deadwood1_Colour2 = 0,
  sig_Sound1_Smell2   = 0,
  sig_Colour1_Smell2 = 0,
  sig_Deadwood1_Smell2 = 0,
  sig_Smell1_Sound2 = 0,
  sig_Colour1_Sound2 = 0,
  sig_Deadwood1_Sound2 = 0,
  sig_Sound1_Deadwood2   = 0,
  sig_Smell1_Deadwood2 = 0,
  sig_Colour1_Deadwood2 = 0
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
                               sig_Smell_Deadwood * draws_Deadwood +

                               sig_Smell2_Colour * draws_Colour2 +
                               sig_Smell2_Sound * draws_Sound2 +
                               sig_Smell2_Deadwood * draws_Deadwood2)

  randcoeff[["b_Sound"]] =  (mu_Sound + sig_Sound * draws_Sound +
                               sig_Sound_Colour * draws_Colour +
                               sig_Sound_Smell * draws_Smell +
                               sig_Sound_Deadwood * draws_Deadwood +

                               sig_Sound2_Colour * draws_Colour2 +
                               sig_Sound2_Smell * draws_Smell2 +
                               sig_Sound2_Deadwood * draws_Deadwood2)

  randcoeff[["b_Colour"]] =  (mu_Colour + sig_Colour * draws_Colour   +
                                sig_Colour_Smell * draws_Smell +
                                sig_Colour_Sound * draws_Sound +
                                sig_Colour_Deadwood * draws_Deadwood +

                                sig_Colour2_Smell * draws_Smell2 +
                                sig_Colour2_Sound * draws_Sound2 +
                                sig_Colour2_Deadwood * draws_Deadwood2 )

  randcoeff[["b_Deadwood"]] =  (mu_Deadwood + sig_Deadwood * draws_Deadwood   +
                                  sig_Deadwood_Colour * draws_Colour +
                                  sig_Deadwood_Sound * draws_Sound +
                                  sig_Deadwood_Smell * draws_Smell +

                                  sig_Deadwood2_Colour * draws_Colour2 +
                                  sig_Deadwood2_Sound * draws_Sound2 +
                                  sig_Deadwood2_Smell * draws_Smell2 )


  randcoeff[["b_Smell2"]] =  (mu_Smell2 + sig_Smell2 * draws_Smell2   +
                                sig_Smell_Colour2 * draws_Colour2 +
                                sig_Smell_Sound2 * draws_Sound2 +
                                sig_Smell_Deadwood2 * draws_Deadwood2 +

                                sig_Smell1_Colour2 * draws_Colour +
                                sig_Smell1_Sound2 * draws_Sound +
                                sig_Smell1_Deadwood2 * draws_Deadwood )

  randcoeff[["b_Sound2"]] =  (mu_Sound2 + sig_Sound2 * draws_Sound2   +
                                sig_Sound_Colour2 * draws_Colour2 +
                                sig_Sound_Smell2 * draws_Smell2 +
                                sig_Sound_Deadwood2 * draws_Deadwood2 +

                                sig_Sound1_Colour2 * draws_Colour +
                                sig_Sound1_Smell2 * draws_Smell +
                                sig_Sound1_Deadwood2 * draws_Deadwood)

  randcoeff[["b_Colour2"]] =  (mu_Colour2 + sig_Colour2 * draws_Colour2   +
                                 sig_Colour_Smell2 * draws_Smell2 +
                                 sig_Colour_Sound2 * draws_Sound2 +
                                 sig_Colour_Deadwood2 * draws_Deadwood2 +

                                 sig_Colour1_Smell2 * draws_Smell +
                                 sig_Colour1_Sound2 * draws_Sound +
                                 sig_Colour1_Deadwood2 * draws_Deadwood)

  randcoeff[["b_Deadwood2"]] =  (mu_Deadwood2 + sig_Deadwood2 * draws_Deadwood2   +
                                   sig_Deadwood_Colour2 * draws_Colour2 +
                                   sig_Deadwood_Sound2 * draws_Sound2 +
                                   sig_Deadwood_Smell2 * draws_Smell2 +

                                   sig_Deadwood1_Colour2 * draws_Colour +
                                   sig_Deadwood1_Sound2 * draws_Sound +
                                   sig_Deadwood1_Smell2 * draws_Smell)
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

  P = list()

  V = list()
  V[['A']]  = asc_A  + beta_Tax*(Tax1 +
                                   b_Sound  * (Sound1==1) + b_Sound2  * (Sound1==2) +
                                   b_Smell*(Smell1==1) + b_Smell2*(Smell1==2) +
                                   b_Colour* (Colour1==1) +b_Colour2* (Colour1==2) +
                                   b_Deadwood*(Deadwood1==7) + b_Deadwood2*(Deadwood1==15) )
  V[['B']]  = asc_B  + beta_Tax*(Tax2 +
                                   b_Sound  * (Sound2==1) + b_Sound2  * (Sound2==2)  +
                                   b_Smell*(Smell2==1) + b_Smell2*(Smell2==2) +
                                   b_Colour*(Colour2==1) + b_Colour2* (Colour2==2) +
                                   b_Deadwood*(Deadwood2==7) +b_Deadwood2*(Deadwood2==15))
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

  ## Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)

  ## Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


# ****************************
# Model Outputs: ####
# ****************************

#
# ## Actually estimates the model
Winter_MXL_ModelOne_AllCorrelations = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# Model output and results here alongside saving information
apollo_modelOutput(Winter_MXL_ModelOne_AllCorrelations,modelOutput_settings = list(printPVal=TRUE))
apollo_saveOutput(Winter_MXL_ModelOne_AllCorrelations,saveOutput_settings = list(printPVal=TRUE))
saveRDS(Winter_MXL_ModelOne_AllCorrelations, here("CEoutput/ModelOne","Winter_MXL_ModelOne_AllCorrelations.rds"))


# ****************************
# Summarise WTP: ####
# ****************************


## Calculate conditional WTP:
Model <- readRDS(here("CEoutput/ModelOne","Winter_MXL_ModelOne_AllCorrelations.rds")) ## Enter model of interest RDS here


## Calculate conditional WTP:
Winter_MXL_ModelOne_AllCorrelations_ConWTP <- apollo_conditionals(Model,apollo_probabilities,apollo_inputs )
fwrite(Winter_MXL_ModelOne_AllCorrelations_ConWTP %>% data.frame(),sep=",",
       here("CEoutput/ModelOne","Winter_MXL_ModelOne_AllCorrelations_ConWTP.csv"))


## Calculate unconditional WTP: (Needed for Fig.2. of the paper) [NOTE: Unconditionals make v large dataframes]
Winter_MXL_ModelOne_AllCorrelations_UnconWTP <- apollo_unconditionals(Model,apollo_probabilities,apollo_inputs )
fwrite(Winter_MXL_ModelOne_AllCorrelations_UnconWTP %>% data.frame(),sep=",",
       here("CEoutput/ModelOne","Winter_MXL_ModelOne_AllCorrelations_UnconWTP.csv"))


# *********************************************************************************************************
#### END OF SCRIPT ####
## Next Step: 10_Winter_MXL_ModelTwo_WTPSpace_Correlated.R
# *********************************************************************************************************
