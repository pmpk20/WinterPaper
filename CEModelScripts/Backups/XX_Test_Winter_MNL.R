#### WP5 Winter Paper: Table 3 Model One ####
## Function: Estimate Mixed Logit on Attributes Only
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 30/01/2023
## TODO: Make sure it always outputs exactly to table



# ****************************
# Replication Information: ####
# ****************************


# R version 4.1.3 (2022-03-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19043)
# Matrix products: default
#   [1] gridExtra_2.3  ggdist_3.2.0   here_1.0.1     mded_0.1-2     reshape2_1.4.4
# [6] ggridges_0.5.3 ggplot2_3.3.6  magrittr_2.0.3 dplyr_1.0.9    apollo_0.2.7


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
# install.packages(
#   c("magrittr",
#     "ggplot2",
#     "ggridges",
#     "mded",
#     "here",
#     "data.table"),
#   repos = "http://cran.us.r-project.org")

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
  "Tax3",
  "Sound1",
  "Sound2",
  "Sound3",
  "Smell1",
  "Smell2",
  "Smell3",
  "Colour1",
  "Colour2",
  "Colour3",
  "Deadwood1",
  "Deadwood2",
  "Deadwood3",
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


# database$Sound1_1 <- ifelse(database$Sound1 == 1, 1, 0)
# database$Sound1_2 <- ifelse(database$Sound1 == 2, 1, 0)
# database$Smell1_1 <- ifelse(database$Smell1 == 1, 1, 0)
# database$Smell1_2 <- ifelse(database$Smell1 == 2, 1, 0)
# database$Colour1_1 <- ifelse(database$Colour1 == 1, 1, 0)
# database$Colour1_2 <- ifelse(database$Colour1 == 2, 1, 0)
# database$Deadwood1_1 <- ifelse(database$Deadwood1 == 7, 1, 0)
# database$Deadwood1_2 <- ifelse(database$Deadwood1 == 15, 1, 0)
#
# database$Sound2_1 <- ifelse(database$Sound2 == 1, 1, 0)
# database$Sound2_2 <- ifelse(database$Sound2 == 2, 1, 0)
# database$Smell2_1 <- ifelse(database$Smell2 == 1, 1, 0)
# database$Smell2_2 <- ifelse(database$Smell2 == 2, 1, 0)
# database$Colour2_1 <- ifelse(database$Colour2 == 1, 1, 0)
# database$Colour2_2 <- ifelse(database$Colour2 == 2, 1, 0)
# database$Deadwood2_1 <- ifelse(database$Deadwood2 == 7, 1, 0)
# database$Deadwood2_2 <- ifelse(database$Deadwood2 == 15, 1, 0)



## Necessary to get apollo working
apollo_initialise()


# ****************************
# Estimation Basics: ####
# ****************************


## Note 10 cores as I'm using the University of Kent 'Tesla' HPC:
apollo_control = list(
  modelDescr = "Winter_MNL_Test_PriceFixed",
  modelName  = "Winter_MNL_Test_PriceFixed", ## Added dates last verified
  indivID    = "Respondent"
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
apollo_fixed = c("asc_A","asc_B")


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
  V[['A']]  = beta_Tax * Tax1 +
    b_Sound  * (Sound1 == 1) + b_Sound2  * (Sound1 == 2) +
    b_Smell * (Smell1 == 1) + b_Smell2 * (Smell1 == 2) +
    b_Colour * (Colour1 == 1) + b_Colour2 * (Colour1 == 2) +
    b_Deadwood * (Deadwood1 == 7) + b_Deadwood2 * (Deadwood1 == 15)


  V[['B']]  =  + beta_Tax * Tax2 +
    b_Sound  * (Sound2 == 1) + b_Sound2  * (Sound2 == 2)  +
    b_Smell * (Smell2 == 1) + b_Smell2 * (Smell2 == 2) +
    b_Colour * (Colour2 == 1) + b_Colour2 * (Colour2 == 2) +
    b_Deadwood * (Deadwood2 == 7) + b_Deadwood2 * (Deadwood2 == 15)

  V[['C']]  = asc_C

  mnl_settings = list(
    alternatives = c(A=1, B=2,C=3),
    avail        = list(A=1, B=1,C=1),
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
Winter_MNL_Test_PriceFixed = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# Model output and results here alongside saving information
apollo_modelOutput(Winter_MNL_Test_PriceFixed,modelOutput_settings = list(printPVal=TRUE))



apollo_saveOutput(Winter_MNL_Test_PriceFixed,saveOutput_settings = list(printPVal=TRUE))
# saveRDS(Winter_MNL_Test_PriceFixed, here("CEoutput/ModelOne","Winter_MNL_Test_PriceFixed.rds"))


# ****************************
# Summarise WTP: ####
# ****************************



nsim <- 1000
betas <- Winter_MNL_Test_PriceFixed$estimate[3:12]
vcmat <- Winter_MNL_Test_PriceFixed$robvarcov


# 1. Take draws from a multivariate normal distribution described by the beta vector and the (robust) variance-covariance matrix to simulate the model nsim times

draws <- mvrnorm(nsim, mu=betas, Sigma=vcmat)


# 2. Do whatever you wish with each of the nsim draws, i.e. the simulated models. Here: Compute and store marg. WTP for some of the attributes

sim.wtp <- matrix(0, nrow = nsim, ncol = 8)    # use this matrix to store simulated WTP distributions

for(i in 1:nsim){
  sim.wtp[i,1] <- -draws[i,3] / draws[i,2]
  sim.wtp[i,2] <- -draws[i,4] / draws[i,2]
  sim.wtp[i,3] <- -draws[i,5] / draws[i,2]
  sim.wtp[i,4] <- -draws[i,6] / draws[i,2]
  sim.wtp[i,5] <- -draws[i,7] / draws[i,2]
  sim.wtp[i,6] <- -draws[i,8] / draws[i,2]
  sim.wtp[i,7] <- -draws[i,9] / draws[i,2]
  sim.wtp[i,8] <- -draws[i,10] / draws[i,2]

}


# 3. Extract 95% confidence intervals from the empirical WTP distributions in sim.wtp and display

wtp <-
  matrix(0, nrow = 8, ncol = 3) # use this matrix to store WTP means and CIs

for(i in 1:nrow(wtp)){
  wtp[i,1] <- -betas[i+2] / betas[2]
  wtp[i, 2:3] <- quantile(sim.wtp[, i], probs = c(0.025, 0.975))
}
round(wtp, digits=2)
WTP <- wtp %>% data.frame()
colnames(WTP) <- c("mean","lb","ub")
WTP$variable <-
  c(
    "b_Sound",
    "b_Smell",
    "b_Colour",
    "b_Deadwood",
    "b_Sound2",
    "b_Smell2",
    "b_Colour2",
    "b_Deadwood2"
  )



PlotData <- WTP %>%
  slice(match(c(
    "b_Colour2",
    "b_Colour",
    "b_Smell2",
    "b_Smell",
    "b_Sound2",
    "b_Sound",
    "b_Deadwood2",
    "b_Deadwood"
  ), variable)) %>%
  mutate(variable = factor(variable,
                           levels = unique(variable)))


TextSize <- 12

PlotData %>% ggplot(aes(y = rev(variable), x = mean)) +
  geom_boxplot() +
  geom_errorbar(aes(xmin = lb, xmax = ub),
                width = 0.25,
                position = position_dodge(width = 0.75))+
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  geom_vline(xintercept = 0, alpha = 0.5) +
  scale_x_continuous(name = "",
                     breaks = seq(-50, 50, 5)) +
  scale_y_discrete(name = "Attribute and level",
                   labels = rev(PlotData$variable)) +
  scale_fill_brewer(
    name = "",
    type = "seq",
    guide = guide_legend(reverse = TRUE)
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10,
                               colour = "black",
                               family = "serif"),
    legend.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.title = element_text(size = TextSize,
                                colour = "black",
                                family = "serif"),
    axis.text.x = element_text(size = TextSize,
                               colour = "black",
                               family = "serif"), ## Change text to be clearer for reader
    axis.text.y = element_text(size = TextSize,
                               colour = "black",
                               family = "serif"),
    axis.title.y = element_text(size = TextSize,
                                colour = "black",
                                family = "serif"),
    axis.title.x = element_text(size = TextSize,
                                colour = "black",
                                family = "serif")
  )







# *********************************************************************************************************
#### END OF SCRIPT ####
## Next Step: Run More Mixed Logit Models
## Below: OLD CODE DO NOT USE
# *********************************************************************************************************

# End  -------------------------------------
