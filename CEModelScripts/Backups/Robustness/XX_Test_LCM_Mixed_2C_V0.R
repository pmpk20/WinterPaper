#### RELATE WP5: Latent class ####

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
  "Impairment",

  "SightIssues" ,
  "SmellIssues" ,
  "HearingIssues",

  "Country",
  "EthnicityDummyWhite",
  "Urbanicity",
  "MostRecentVisit",
  "WoodlandsScore"
)]

# database <- here("CEModelData", "database_Winter_Step1.csv") %>% fread() %>% data.frame()
apollo_initialise()


# *******************************************************************************
# Section 2: Estimation Statistics ####
# Selected output of 'sessionInfo()'
# *******************************************************************************

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "LC_MMNL",
  modelDescr      = "Latent class with continuous random parameters on Swiss route choice data",
  indivID         = "Respondent",
  nCores          = 4,
  outputDirectory = "CEoutput/Robustness"
)


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #



# Define beta starting values:
apollo_beta = c(
  asc_A      = 0,
  asc_B      = 0,
  # beta_Tax = -0.02209,
  asc_C = -0.15137,

  mu_Tax_Class1    = -3,
  sig_Tax_Class1 = -0.01,
  mu_Colour2_Class1 = 0.07410,
  mu_Colour_Class1 = 0.13937,
  mu_Smell2_Class1 = 0.12855,
  mu_Smell_Class1 = 0.08875,
  mu_Sound2_Class1 = 0.26818,
  mu_Sound_Class1 = 0.25171,
  mu_Deadwood2_Class1 = 0.43725,
  mu_Deadwood_Class1 = 0.24873,

  sig_Colour2_Class1 = -0.01,
  sig_Colour_Class1 = -0.01,
  sig_Smell2_Class1 = -0.01,
  sig_Smell_Class1 = -0.01,
  sig_Sound2_Class1   = -0.01,
  sig_Sound_Class1   = -0.01,
  sig_Deadwood2_Class1 = -0.01,
  sig_Deadwood_Class1 = -0.01,

  mu_Tax_Class2    = -3,
  sig_Tax_Class2 = -0.01,
  mu_Colour2_Class2 = 0.07410,
  mu_Colour_Class2 = 0.13937,
  mu_Smell2_Class2 = 0.12855,
  mu_Smell_Class2 = 0.08875,
  mu_Sound2_Class2 = 0.26818,
  mu_Sound_Class2 = 0.25171,
  mu_Deadwood2_Class2 = 0.43725,
  mu_Deadwood_Class2 = 0.24873,

  sig_Colour2_Class2 = -0.01,
  sig_Colour_Class2 = -0.01,
  sig_Smell2_Class2 = -0.01,
  sig_Smell_Class2 = -0.01,
  sig_Sound2_Class2   = -0.01,
  sig_Sound_Class2   = -0.01,
  sig_Deadwood2_Class2 = -0.01,
  sig_Deadwood_Class2 = -0.01,

  delta_Class1 = 0,
  delta_Class2 = 0
)

apollo_fixed = c("asc_A", "asc_B", "delta_Class2")



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
# apollo_fixed = c("asc2", "delta_b", "gamma_commute_b","gamma_car_av_b")

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #



### Set parameters for generating draws
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


### Create ran
### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs) {
  randcoeff = list()
  randcoeff[["beta_Tax_Class1"]] = -exp(mu_Tax_Class1 + sig_Tax_Class1 * draws_Tax)
  randcoeff[["b_Smell_Class1"]] =  (mu_Smell_Class1 + sig_Smell_Class1 * draws_Smell)
  randcoeff[["b_Sound_Class1"]] =  (mu_Sound_Class1 + sig_Sound_Class1 * draws_Sound)
  randcoeff[["b_Colour_Class1"]] =  (mu_Colour_Class1 + sig_Colour_Class1 * draws_Colour)
  randcoeff[["b_Deadwood_Class1"]] =  (mu_Deadwood_Class1 + sig_Deadwood_Class1 * draws_Deadwood)
  randcoeff[["b_Smell2_Class1"]] =  (mu_Smell2_Class1 + sig_Smell2_Class1 * draws_Smell2)
  randcoeff[["b_Sound2_Class1"]] =  (mu_Sound2_Class1 + sig_Sound2_Class1 * draws_Sound2)
  randcoeff[["b_Colour2_Class1"]] =  (mu_Colour2_Class1 + sig_Colour2_Class1 * draws_Colour2)
  randcoeff[["b_Deadwood2_Class1"]] =  (mu_Deadwood2_Class1 + sig_Deadwood2_Class1 * draws_Deadwood2)

  randcoeff[["beta_Tax_Class2"]] = -exp(mu_Tax_Class2 + sig_Tax_Class2 * draws_Tax)
  randcoeff[["b_Smell_Class2"]] =  (mu_Smell_Class2 + sig_Smell_Class2 * draws_Smell)
  randcoeff[["b_Sound_Class2"]] =  (mu_Sound_Class2 + sig_Sound_Class2 * draws_Sound)
  randcoeff[["b_Colour_Class2"]] =  (mu_Colour_Class2 + sig_Colour_Class2 * draws_Colour)
  randcoeff[["b_Deadwood_Class2"]] =  (mu_Deadwood_Class2 + sig_Deadwood_Class2 * draws_Deadwood)
  randcoeff[["b_Smell2_Class2"]] =  (mu_Smell2_Class2 + sig_Smell2_Class2 * draws_Smell2)
  randcoeff[["b_Sound2_Class2"]] =  (mu_Sound2_Class2 + sig_Sound2_Class2 * draws_Sound2)
  randcoeff[["b_Colour2_Class2"]] =  (mu_Colour2_Class2 + sig_Colour2_Class2 * draws_Colour2)
  randcoeff[["b_Deadwood2_Class2"]] =  (mu_Deadwood2_Class2 + sig_Deadwood2_Class2 * draws_Deadwood2)

  return(randcoeff)
}


# ################################################################# #
#### DEFINE LATENT CLASS COMPONENTS                              ####
# ################################################################# #

apollo_lcPars = function(apollo_beta, apollo_inputs){
  lcpars = list()

  lcpars[["beta_Tax"]] = list(beta_Tax_Class1, beta_Tax_Class2)
  lcpars[["b_Smell"]] = list(b_Smell_Class1, b_Smell_Class2)
  lcpars[["b_Sound"]] = list(b_Sound_Class1, b_Sound_Class2)
  lcpars[["b_Colour"]] = list(b_Colour_Class1, b_Colour_Class2)
  lcpars[["b_Deadwood"]] = list(b_Deadwood_Class1, b_Deadwood_Class2)
  lcpars[["b_Smell2"]] = list(b_Smell2_Class1, b_Smell2_Class2)
  lcpars[["b_Sound2"]] = list(b_Sound2_Class1, b_Sound2_Class2)
  lcpars[["b_Colour2"]] = list(b_Colour2_Class1, b_Colour2_Class2)
  lcpars[["b_Deadwood2"]] = list(b_Deadwood2_Class1, b_Deadwood2_Class2)

  V = list()
  V[["Class1"]] = delta_Class1
  V[["Class2"]] = delta_Class2

  classAlloc_settings = list(
    classes      = c(Class1 = 1, Class2 = 2),
    utilities    = V
  )

  lcpars[["pi_values"]] = apollo_classAlloc(classAlloc_settings)

  return(lcpars)
}

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){

  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  ### Create list of probabilities P
  P = list()

  ### Define settings for MNL model component that are generic across classes
  mnl_settings = list(
    alternatives = c(A = 1, B = 2, C = 3),
    avail        = list(A = 1, B = 1, C = 1),
    choiceVar    = Choice
  )

  ### Loop over classes
  for (s in 1:2) {

    ### Compute class-specific utilities
    V=list()
    V[['A']]  = asc_A - beta_Tax[[s]] * (
      b_Sound[[s]]  * (Sound1 == 1) +
      b_Sound2[[s]]  * (Sound1 == 2) +
      b_Smell[[s]] * (Smell1 == 1) +
      b_Smell2[[s]] * (Smell1 == 2) +
      b_Colour[[s]] * (Colour1 == 1) +
      b_Colour2[[s]] * (Colour1 == 2) +
      b_Deadwood[[s]] * (Deadwood1 == 7) +
      b_Deadwood2[[s]] * (Deadwood1 == 15) -
        Tax1)


    V[['B']]  = asc_B - beta_Tax[[s]] * (
      b_Sound[[s]]  * (Sound2 == 1) +
      b_Sound2[[s]]  * (Sound2 == 2) +
      b_Smell[[s]] * (Smell2 == 1) +
      b_Smell2[[s]] * (Smell2 == 2) +
      b_Colour[[s]] * (Colour2 == 1) +
      b_Colour2[[s]] * (Colour2 == 2) +
      b_Deadwood[[s]] * (Deadwood2 == 7) +
      b_Deadwood2[[s]] * (Deadwood2 == 15) -
        Tax2)

    V[['C']]  = asc_C

    mnl_settings$utilities = V
    mnl_settings$componentName = paste0("Class_", s)

    ### Compute within-class choice probabilities using MNL model
    P[[paste0("Class_", s)]] = apollo_mnl(mnl_settings, functionality)

    ### Take product across observation for same individual
    P[[paste0("Class_", s)]] = apollo_panelProd(P[[paste0("Class_", s)]], apollo_inputs , functionality)

    ### Average across inter-individual draws within classes
    P[[paste0("Class_", s)]] = apollo_avgInterDraws(P[[paste0("Class_", s)]], apollo_inputs, functionality)
  }

  ### Compute latent class model probabilities
  lc_settings  = list(inClassProb = P, classProb = pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)

  ### Average across inter-individual draws in class allocation probabilities
  P[["model"]] = apollo_avgInterDraws(P[["model"]], apollo_inputs, functionality)

  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


# ################################################################# #
#### MODEL ESTIMATION AND OUTPUT                                 ####
# ################################################################# #

### Estimate model
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

### Show output in screen
apollo_modelOutput(model)

### Save output to file(s)
apollo_saveOutput(model)
