


rm(list = ls())
library(here)
#library(lubridate)
library(tidyr)
library(apollo)
#library(ggridges)
library(ggplot2)
#library(reshape2)
library(dplyr)
#library(magrittr)
#library(ggdist)
#library(RColorBrewer)
library(data.table)


# ***********************************************
# Section 1: Import Data ####
# Selected output of 'sessionInfo()'
# ***********************************************


## Read in latest version of data
database<-read.csv("database_Spring_Step1_Anonymised.csv")
# database <- here("CEInputData",
#                  "database_Spring_Step1_Anonymised.csv") %>%
#   fread() %>%
#   data.frame()


# ***********************************************
# Section 2: Estimation Statistics ####
# Selected output of 'sessionInfo()'
# ***********************************************


apollo_initialise()
apollo_control = list(
  nCores    = 1, ## Watch out if not using HPC!
  #mixing    = TRUE,
  modelDescr = "H1_Spring_PreferenceSpaceModel",
  modelName  = "H1_Spring_PreferenceSpaceModel",
  indivID    = "Respondent",
  outputDirectory = "CEOutputData/H1"
)


# Define beta starting values:
apollo_beta = c(
  asc_A      = 0,
  asc_B      = 0,
  asc_C = 0,
  mu_Tax    = -3,
  mu_Sound   = 0,
  mu_Smell = 0,
  mu_Colour = 0,
  mu_Deadwood = 0,
    mu_Sound2   = 0,
  mu_Smell2 = 0,
  mu_Colour2 = 0,
  mu_Deadwood2 = 0
  
)

apollo_fixed = c("asc_A", "asc_B")


# ***********************************************
# Section 3: Define random draws ####
# ***********************************************



### Set parameters for generating draws
# apollo_draws = list(
#   interDrawsType = "pmc",
#   interNDraws    = 10,
#   interUnifDraws = c(),
#   interNormDraws = c(
#     "draws_Tax",
#     "draws_Smell",
#     "draws_Sound",
#     "draws_Colour",
#     "draws_Deadwood",
#     "draws_Smell2",
#     "draws_Sound2",
#     "draws_Colour2",
#     "draws_Deadwood2"
#   ),
#   intraDrawsType = "halton",
#   intraNDraws    = 0,
#   intraUnifDraws = c(),
#   intraNormDraws = c()
# )


### Create random parameters
# apollo_randCoeff = function(apollo_beta, apollo_inputs) {
#   randcoeff = list()
#   randcoeff[["beta_Tax"]] = -exp(mu_Tax + sig_Tax * draws_Tax)
#   randcoeff[["b_Smell"]] =  (mu_Smell + sig_Smell * draws_Smell)
#   randcoeff[["b_Sound"]] =  (mu_Sound + sig_Sound * draws_Sound)
#   randcoeff[["b_Colour"]] =  (mu_Colour + sig_Colour * draws_Colour)
#   randcoeff[["b_Deadwood"]] =  (mu_Deadwood + sig_Deadwood * draws_Deadwood)
#   randcoeff[["b_Smell2"]] =  (mu_Smell2 + sig_Smell2 * draws_Smell2)
#   randcoeff[["b_Sound2"]] =  (mu_Sound2 + sig_Sound2 * draws_Sound2)
#   randcoeff[["b_Colour2"]] =  (mu_Colour2 + sig_Colour2 * draws_Colour2)
#   randcoeff[["b_Deadwood2"]] =  (mu_Deadwood2 + sig_Deadwood2 * draws_Deadwood2)
#   return(randcoeff)
# }

apollo_inputs = apollo_validateInputs()


# ***********************************************
# Section 4: Define model ####
# ***********************************************




apollo_probabilities = function(apollo_beta,
                                apollo_inputs,
                                functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  asc_C1 = asc_C 

  P = list()

  V = list()
  V[['A']]  = beta_Tax * Tax1 +
    b_Sound  * (Sound1 == 1) + b_Sound2  * (Sound1 == 2) +
    b_Smell * (Smell1 == 1) + b_Smell2 * (Smell1 == 2) +
    b_Colour * (Colour1 == 1) + b_Colour2 * (Colour1 == 2) +
    b_Deadwood * (Deadwood1 == 7) + b_Deadwood2 * (Deadwood1 == 15)


  V[['B']]  =  beta_Tax * Tax2 +
    b_Sound  * (Sound2 == 1) + b_Sound2  * (Sound2 == 2)  +
    b_Smell * (Smell2 == 1) + b_Smell2 * (Smell2 == 2) +
    b_Colour * (Colour2 == 1) + b_Colour2 * (Colour2 == 2) +
    b_Deadwood * (Deadwood2 == 7) + b_Deadwood2 * (Deadwood2 == 15)


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


# ***********************************************
# Section 5: Model Estimation ####
# ***********************************************



## Actually estimates the model
H1_Spring_PreferenceSpaceModel = apollo_estimate(apollo_beta,
                                                 apollo_fixed,
                                                 apollo_probabilities,
                                                 apollo_inputs)


# ## Model output and results here alongside saving information
apollo_modelOutput(H1_Spring_PreferenceSpaceModel,
                   modelOutput_settings = list(printPVal = TRUE))
apollo_saveOutput(H1_Spring_PreferenceSpaceModel,
                  saveOutput_settings = list(printPVal = TRUE))

# saveRDS(H1_Spring_PreferenceSpaceModel, file="H1_Spring_PreferenceSpaceModel.rds")


# ***********************************************
# Section 6: Output WTP ####
# ***********************************************
apollo_modelOutput(model)

# Model <- here("CEOutputData/H1",
#               "H1_Spring_PreferenceSpaceModel_model.rds") %>% readRDS()
# 
# 
# ## UNCONDITIONAL WTP: 1000 DRAWS PER ATTRIBUTE LEVEL
# apollo_unconditionals(Model,
#                       apollo_probabilities,apollo_inputs) %>% fwrite(
#   sep = ",",
#   here(
#     "CEOutputData/H1",
#     "H1_Spring_PreferenceSpaceModel_Unconditionals.csv"
#   )
# )
# 
# 
# 
# ## CONDITIONALS SO MUCH SMALLER
# apollo_conditionals(Model,
#                     apollo_probabilities,apollo_inputs) %>% fwrite(
#   sep = ",",
#   here(
#     "CEOutputData/H1",
#     "H1_Spring_PreferenceSpaceModel_Conditionals.csv"
#   )
# )
# 
# 


# End Of Script --------------------------------------------------------------
