
# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "MNL_SP",
  modelDescr      = "Simple MNL model on mode choice SP data",
  indivID         = "ID",
  outputDirectory = "output"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

### Loading data from package
### if data is to be loaded from a file (e.g. called data.csv), 
### the code would be: database = read.csv("data.csv",header=TRUE)
database = apollo_modeChoiceData
### for data dictionary, use ?apollo_modeChoiceData

### Use only SP data
database = subset(database,database$SP==1)

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c( # asc_car      = 0,
              asc_bus      = 0,
              asc_air      = 0,
              asc_rail     = 0,
              b_tt_car     = 0,
              b_tt_bus     = 0,
              b_tt_air     = 0,
              b_tt_rail    = 0,
              b_access     = 0,
              b_cost       = 0)
              # b_no_frills  = 0,
              # b_wifi       = 0,
              # b_food       = 0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c() # c("asc_car","b_no_frills")

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
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[["car"]]  =            b_tt_car  * time_car                           + b_cost * cost_car
  V[["bus"]]  = asc_bus  + b_tt_bus  * time_bus  + b_access * access_bus  + b_cost * cost_bus 
  V[["air"]]  = asc_air  + b_tt_air  * time_air  + b_access * access_air  + b_cost * cost_air   # + b_no_frills * ( service_air == 1 )  + b_wifi * ( service_air == 2 )  + b_food * ( service_air == 3 )
  V[["rail"]] = asc_rail + b_tt_rail * time_rail + b_access * access_rail + b_cost * cost_rail  # + b_no_frills * ( service_rail == 1 ) + b_wifi * ( service_rail == 2 ) + b_food * ( service_rail == 3 )
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(car=1, bus=2, air=3, rail=4), 
    avail         = list(car=av_car, bus=av_bus, air=av_air, rail=av_rail), 
    choiceVar     = choice,
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model, modelOutput_settings = list(printPVal = TRUE))








### Simulation #################################################################

library(MASS)

nsim <- 1000
betas <- model$estimate
vcmat <- model$robvarcov


# 1. Take draws from a multivariate normal distribution described by the beta vector and the (robust) variance-covariance matrix to simulate the model nsim times

draws <- mvrnorm(nsim, mu=betas, Sigma=vcmat)


# 2. Do whatever you wish with each of the nsim draws, i.e. the simulated models. Here: Compute and store marg. WTP for some of the attributes

sim.wtp <- matrix(0, nrow=nsim, ncol=5)    # use this matrix to store simulated WTP distributions

for(i in 1:nsim){
  sim.wtp[i,1] <- -draws[i,4] / draws[i,9] 
  sim.wtp[i,2] <- -draws[i,5] / draws[i,9] 
  sim.wtp[i,3] <- -draws[i,6] / draws[i,9] 
  sim.wtp[i,4] <- -draws[i,7] / draws[i,9] 
  sim.wtp[i,5] <- -draws[i,8] / draws[i,9] 
}


# 3. Extract 95% confidence intervals from the empirical WTP distributions in sim.wtp and display

wtp <- matrix(0, nrow=5, ncol=3) # use this matrix to store WTP means and CIs

for(i in 1:nrow(wtp)){
  wtp[i,1] <- -betas[i+3] / betas[9]
  wtp[i,2:3] <- quantile(sim.wtp[,i], probs=c(0.025,0.975))
}
colnames(wtp) <- c("mean","lb","ub")
rownames(wtp) <- c("tt_car","tt_bus","tt_air","tt_rail","access")
round(wtp, digits=2)





