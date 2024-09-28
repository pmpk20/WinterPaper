#### WP5 Winter Paper: Table 3 Model Two B ####
## Function: Estimate Mixed Logit on Attributes With (Some) Covariates
## Author: Dr Peter King (p.king1@leeds.ac.uk)
## Last change: 19/02/2024



# ****************************
# Replication Information: ####
# ****************************


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
# other attached packages:
#   [1] RColorBrewer_1.1-3 mded_0.1-2         apollo_0.3.1
# [4] Rfast_2.1.0        RcppParallel_5.1.7 RcppZiggurat_0.1.6
# [7] Rcpp_1.0.11        ggridges_0.5.4     reshape2_1.4.4
# [10] lubridate_1.9.2    forcats_1.0.0      stringr_1.5.0
# [13] purrr_1.0.2        readr_2.1.4        tidyr_1.3.0
# [16] tibble_3.2.1       ggplot2_3.4.3      tidyverse_2.0.0
# [19] data.table_1.14.8  udunits2_0.13.2.1  PostcodesioR_0.3.1
# [22] geosphere_1.5-18   dplyr_1.1.3        psych_2.3.9
# [25] magrittr_2.0.3     readxl_1.4.3       here_1.0.1


# ****************************
# Setup Environment: ####
# ****************************


## Clear workspace:
rm(list = ls())

## This sometimes fixes encoding issues using a HPC:
Sys.setlocale("LC_ALL", "C")


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
library(RColorBrewer)
library(data.table)


# ****************************
# Import Data: ####
# ****************************


## Import reshaped survey data appropriate for Apollo
database <-
  here("CEModelData", "database_Winter_Step1.csv") %>%
  fread() %>%
  data.frame()


## Define here if not already
database$Impairment <- ifelse((database$SightIssues == 1) |
                                (database$SmellIssues == 1) |
                                (database$HearingIssues == 1),
                              1,
                              0
)


## Make dummy
database$CountryDummy <- ifelse(database$Country == 0, 0, 1)


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
  "Charity",
  "Impairment",
  "Country",
  "CountryDummy",
  "EthnicityDummyWhite",
  "Urbanicity",
  "MostRecentVisit",
  "WoodlandsScore"
)]


## Necessary to get apollo working
apollo_initialise()


# ****************************
# Estimation Basics: ####
# ****************************


## Note 10 cores as I'm using the University of Kent 'Tesla' HPC:
apollo_control = list(
  nCores    = 10,
  mixing    = TRUE,
  analyticGrad    = TRUE, # following manual here
  modelDescr = "Winter_MXL_ModelThree_Correlated",
  modelName  = "Winter_MXL_ModelThree_Correlated",
  ## Added dates last verified
  indivID    = "Respondent",
  ## This is the name of a column in the database indicating each unique respondent
  outputDirectory = "CEoutput/ModelThree"
)


## Define parameters starting values:
### Note starting mean of tax at -3 to avoid issues later when using the lognormal distribution
apollo_beta = c(
  asc_A      = 0,
  asc_B      = 0,
  asc_C = -1.450,
  mu_Tax        = -3.287,
  mu_Colour2 = 0.097,
  mu_Colour = 0.197,
  mu_Smell2 = 0.157,
  mu_Smell = 0.170,
  mu_Sound2 = 0.393,
  mu_Sound = 0.202,
  mu_Deadwood2 = 0.732,
  mu_Deadwood = 0.252,
  sig_Tax	= -2.802,
  sig_Colour2	= -0.842,
  sig_Colour	= 0.338,
  sig_Smell2	= 0.494,
  sig_Smell	= -0.266,
  sig_Sound2	= -1.131,
  sig_Sound	= -0.067,
  sig_Deadwood2 	= -0.681,
  sig_Deadwood	= 0.554,

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
  sig_Colour_Deadwood2 = 0,


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
  beta_WoodlandsScore = 0
)


## Hold Alternative-Specific Constants for non-status-quo options at zero
apollo_fixed = c("asc_A","asc_B")


## Set parameters for generating draws
### Note that draws by attribute (5) or by level (9) make no difference.
apollo_draws = list(
  interDrawsType = "pmc",
  interNDraws    = 1000,
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
  ),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)




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


  ## All interaction covariates
  asc_C1 = asc_C +
    beta_Gender * Gender +
    beta_Age * DummyAge +
    beta_Distance * MilesDistance +
    beta_Charity * Charity +
    beta_Income * IncomeDummy +
    beta_Impairment * Impairment +
    beta_Country * CountryDummy +
    beta_White * EthnicityDummyWhite +
    beta_Urbanicity * Urbanicity +
    beta_MostRecentVisit * MostRecentVisit +
    beta_WoodlandsScore * WoodlandsScore


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


# ****************************
# Model Outputs: ####
# ****************************


## Actually estimates the model
Winter_MXL_ModelThree_Correlated = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# Model output and results here alongside saving information
apollo_modelOutput(Winter_MXL_ModelThree_Correlated,modelOutput_settings = list(printPVal=TRUE))
apollo_saveOutput(Winter_MXL_ModelThree_Correlated,saveOutput_settings = list(printPVal=TRUE))
saveRDS(
  Winter_MXL_ModelThree_Correlated,
  here(
    "CEoutput/ModelThree",
    "Winter_MXL_ModelThree_Correlated.rds"
  )
)


# ****************************
# Summarise WTP: ####
# ****************************


Model <- readRDS(here("CEoutput/ModelThree","Winter_MXL_ModelThree_Correlated_model.rds")) ## Enter model of interest RDS here


## Calculate conditional WTP:
Winter_MXL_ModelThree_Correlated_ConWTP <-
  apollo_conditionals(Model, apollo_probabilities, apollo_inputs)
fwrite(
  Winter_MXL_ModelThree_Correlated_ConWTP %>% data.frame(),
  sep = ",",
  here("CEoutput/ModelThree", "Winter_MXL_ModelThree_Correlated_ConWTP.csv")
)


## Calculate unconditional WTP: (Needed for Fig.2. of the paper) [NOTE: Unconditionals make v large dataframes]
Winter_MXL_ModelThree_Correlated_UnconWTP <-
  apollo_unconditionals(Model, apollo_probabilities, apollo_inputs)
fwrite(
  Winter_MXL_ModelThree_Correlated_UnconWTP %>% data.frame(),
  sep = ",",
  here("CEoutput/ModelThree", "Winter_MXL_ModelThree_Correlated_UnconWTP.csv")
)


# *********************************************************************************************************
#### END OF SCRIPT ####
## Next Step: Merge WTP and survey data
# *********************************************************************************************************

# End Of Script -------------------------------------
