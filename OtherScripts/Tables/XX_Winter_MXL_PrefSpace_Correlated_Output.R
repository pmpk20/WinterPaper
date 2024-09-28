#### RELATE WP5: Table 2 MXL Model Outputs  ###############
# Script author: Peter King (p.king1@leeds.ac.uk)
# Last Edited: 16/02/2024
# Changes:
## - Format for mean and SD columns
## - Adding model diagnostics in as per reviewers
## - reordered variables


# *************************************************************************
#### Section 0: Setup and estimate models ####
## NOTE: Data is stored in PK R WORK/Strictly Replication Files Only/Winter Replication/CEModelScripts/
## NOTE: Results are stored in PK R WORK/Strictly Replication Files Only/Winter Replication/CEModelData/
# *************************************************************************


library(apollo)
library(data.table)
library(here)
library(tidyverse)


# *************************************************************************
#### Section 1: Importing Model,  Estimates,  and WTP for FULL SAMPLE models ####
# *************************************************************************

## **************
## Models
## **************
Winter_MXL_ModelOne_PrefSpace_Correlated_model <- here("CEoutput/ModelOne", "Winter_MXL_ModelOne_PrefSpace_Correlated_model.rds") %>% readRDS()
Winter_MXL_ModelTwo_PrefSpace_Correlated_model <- here("CEoutput/ModelTwo", "Winter_MXL_ModelTwo_PrefSpace_Correlated_model.rds") %>% readRDS()
Winter_MXL_ModelThree_PrefSpace_Correlated_model <- here("CEoutput/ModelTwo", "Winter_MXL_ModelThree_PrefSpace_Correlated_model.rds") %>% readRDS()


## **************
## Estimates
## **************
Winter_MXL_ModelOne_PrefSpace_Correlated_estimates <- here("CEoutput/ModelOne", "Winter_MXL_ModelOne_PrefSpace_Correlated_estimates.csv") %>% fread() %>% data.frame()
Winter_MXL_ModelTwo_PrefSpace_Correlated_estimates <- here("CEoutput/ModelTwo", "Winter_MXL_ModelTwo_PrefSpace_Correlated_estimates.csv") %>% fread() %>% data.frame()
Winter_MXL_ModelThree_PrefSpace_Correlated_estimates <- here("CEoutput/ModelTwo", "Winter_MXL_ModelThree_PrefSpace_Correlated_estimates.csv") %>% fread() %>% data.frame()





# *************************************************************************
#### Section 2: Model Summary Function ####
# *************************************************************************


OutputOrder <- c("asc_A","asc_B","asc_C",
                 "mu_Tax",
                 "mu_Colour2","mu_Colour",
                 "mu_Smell2","mu_Smell",
                 "mu_Sound2","mu_Sound",
                 "mu_Deadwood2","mu_Deadwood",
                 "sig_Tax",
                 "sig_Colour2","sig_Colour",
                 "sig_Smell2","sig_Smell",
                 "sig_Sound2","sig_Sound",
                 "sig_Deadwood2","sig_Deadwood",
                 "beta_Gender","beta_Age","beta_Distance",
                 "beta_Income","beta_Impairment","beta_Country",
                 "beta_White","beta_Urbanicity",
                 "beta_MostRecentVisit", "beta_WoodlandsScore",
                 "sig_Sound_Colour",
                 "sig_Smell_Colour",
                 "sig_Deadwood_Colour",
                 "sig_Sound_Smell",
                 "sig_Colour_Smell",
                 "sig_Deadwood_Smell",
                 "sig_Smell_Sound",
                 "sig_Colour_Sound",
                 "sig_Deadwood_Sound",
                 "sig_Sound_Deadwood",
                 "sig_Smell_Deadwood",
                 "sig_Colour_Deadwood",
                 "sig_Sound_Colour2",
                 "sig_Smell_Colour2",
                 "sig_Deadwood_Colour2",
                 "sig_Sound_Smell2",
                 "sig_Colour_Smell2",
                 "sig_Deadwood_Smell2",
                 "sig_Smell_Sound2",
                 "sig_Colour_Sound2",
                 "sig_Deadwood_Sound2",
                 "sig_Sound_Deadwood2",
                 "sig_Smell_Deadwood2",
                 "sig_Colour_Deadwood2")

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

  # %>%
  #   slice(match(OutputOrder, Variable))
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
#### Section 3A: Diagnostics ####
# *************************************************************************


BottomRow <-
  cbind(
    "Variable" = Diagnostics(Winter_MXL_ModelOne_PrefSpace_Correlated_model) %>% rownames(),
    "Estimate" = Diagnostics(Winter_MXL_ModelOne_PrefSpace_Correlated_model),

    "Estimate" = Diagnostics(Winter_MXL_ModelTwo_PrefSpace_Correlated_model),
    "Estimate" = Diagnostics(Winter_MXL_ModelTwo_PrefSpace_Correlated_model),

      "Estimate" = Diagnostics(Winter_MXL_ModelThree_PrefSpace_Correlated_model),
    "Estimate" = Diagnostics(Winter_MXL_ModelThree_PrefSpace_Correlated_model)
  ) %>% data.frame()


# *************************************************************************
#### Section 3B: Main covariates ####
# *************************************************************************



## Name all model outputs here
Winter_MXL_ModelOne_PrefSpace_Correlated_output <- ModelOutputs(Winter_MXL_ModelOne_PrefSpace_Correlated_estimates)
Winter_MXL_ModelTwo_PrefSpace_Correlated_output <- ModelOutputs(Winter_MXL_ModelTwo_PrefSpace_Correlated_estimates)
Winter_MXL_ModelThree_PrefSpace_Correlated_output <- ModelOutputs(Winter_MXL_ModelThree_PrefSpace_Correlated_estimates)



## Combine in one table
MainOutputs <-
  cbind(

    cbind(
      rbind("Variable" = Winter_MXL_ModelOne_PrefSpace_Correlated_output[1], data.frame("Variable" = c(0,0,0,0,0,0,0,0,0,0,0))),
      "Estimate" = rbind(Winter_MXL_ModelOne_PrefSpace_Correlated_output[2], data.frame("Estimate" = c(0,0,0,0,0,0,0,0,0,0,0)))),

    cbind(
      rbind("Variable" = Winter_MXL_ModelThree_PrefSpace_Correlated_output[1], data.frame("Variable" =  c(0,0,0,0,0))),
      "Estimate" = rbind(Winter_MXL_ModelThree_PrefSpace_Correlated_output[2], data.frame("Estimate" =  c(0,0,0,0,0)))),

    Winter_MXL_ModelTwo_PrefSpace_Correlated_output

  )


TableHeadings <- c(
  "M1: Variable",
  "M1: Estimate",
  "M2: Variable",
  "M2: Estimate",
  "M3: Variable",
  "M3: Estimate")



## Rename for ease of reference
colnames(MainOutputs) <- TableHeadings
colnames(BottomRow) <- TableHeadings


MainOutputs %>% write.csv(quote = FALSE)


# *************************************************************************
#### Section 5: Output tables ####
# *************************************************************************


Output <- rbind(MainOutputs, BottomRow)

## Output to screen in a nice format for making tables in word
Output %>% write.csv(quote = FALSE)
## Output to a discrete file if that's helpful
Output %>% fwrite(
  sep = ",",
  here("OtherOutput/Tables", "TableX_PrefSpace_Correlated.txt"),
  row.names = TRUE,
  quote = FALSE
)



# *************************************************************************
#### END OF SCRIPT ####
# *************************************************************************
