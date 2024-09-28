#### RELATE WP5: Table 2 MXL Model Outputs  ###############
# Script author: Peter King (p.king1@leeds.ac.uk)
# Last Edited: 16/02/2024
# Changes:
## - Format for mean and SD columns
## - Adding model diagnostics in as per reviewers
## - reordered variables


# *************************************************************************
#### Section 0: Setup and estimate models ####
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
# Winter_MXL_ModelOne_Interactions_model <- here("CEoutput/ModelOne", "Winter_MXL_ModelOne_Interactions.rds") %>% readRDS()
# Winter_MXL_ModelOne_PrefSpace_Interactions_model <- here("CEoutput/ModelOne", "Winter_MXL_ModelOne_PrefSpace_Interactions.rds") %>% readRDS()#
Winter_MXL_ModelOne_Correlated_InteractionsInUtility_model <- here("CEoutput/ModelOne", "Winter_MXL_ModelOne_Correlated_InteractionsInUtility.rds") %>% readRDS()

## **************
## Estimates
## **************
# Winter_MXL_ModelOne_Interactions_estimates <- here("CEoutput/ModelOne", "Winter_MXL_ModelOne_Interactions_estimates.csv") %>% fread() %>% data.frame()
# Winter_MXL_ModelOne_PrefSpace_Interactions_estimates <- here("CEoutput/ModelOne", "Winter_MXL_ModelOne_PrefSpace_Interactions_estimates.csv") %>% fread() %>% data.frame()
Winter_MXL_ModelOne_Correlated_InteractionsInUtility_estimates <-
  here(
    "CEoutput/ModelOne",
    "Winter_MXL_ModelOne_Correlated_InteractionsInUtility_estimates.csv"
  ) %>% fread() %>% data.frame()



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
#### Section 3A: Diagnostics ####
# *************************************************************************


BottomRow <-
  cbind(
    "Variable" = Diagnostics(Winter_MXL_ModelOne_Correlated_InteractionsInUtility_model) %>% rownames(),
    "Estimate" = data.frame("Estimate" = Diagnostics(Winter_MXL_ModelOne_Correlated_InteractionsInUtility_model)))


# *************************************************************************
#### Section 3B: Main covariates ####
# *************************************************************************


## Name all model outputs here
Winter_MXL_ModelOne_Correlated_InteractionsInUtility_output <- ModelOutputs(Winter_MXL_ModelOne_Correlated_InteractionsInUtility_estimates)


## Combine in one table
MainOutputs <- cbind(
  Winter_MXL_ModelOne_Correlated_InteractionsInUtility_output$Variable,
  Winter_MXL_ModelOne_Correlated_InteractionsInUtility_output$Estimate
)

TableHeadings <- c(
  "M1: Variable",
  "M1: Estimate")

colnames(MainOutputs) <- TableHeadings
colnames(BottomRow) <- TableHeadings


# *************************************************************************
#### Section 5: Output tables ####
# *************************************************************************


Output <- rbind(MainOutputs, BottomRow)

## Output to screen in a nice format for making tables in word
Output %>% write.csv(quote = FALSE)
## Output to a discrete file if that's helpful
Output %>% fwrite(
  sep = ",",
  here("OtherOutput/Tables", "TableX_MXL_Interactions.txt"),
  row.names = TRUE,
  quote = FALSE
)



# *************************************************************************
#### END OF SCRIPT ####
# *************************************************************************


