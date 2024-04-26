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
Winter_MNL_ModelZero_model <- here("CEoutput/ModelOne", "Winter_MNL_ModelZero_model.rds") %>% readRDS()

## **************
## Estimates
## **************
Winter_MNL_ModelZero_estimates <- here("CEoutput/ModelOne", "Winter_MNL_ModelZero_estimates.csv") %>% fread() %>% data.frame()





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
    "Variable" = Diagnostics(Winter_MNL_ModelZero_model) %>% rownames(),
    "Estimate" = data.frame("Estimate" = Diagnostics(Winter_MNL_ModelZero_model)  ))


# *************************************************************************
#### Section 3B: Main covariates ####
# *************************************************************************


## Name all model outputs here
Winter_MNL_ModelZero_output <- ModelOutputs(Winter_MNL_ModelZero_estimates)


## Combine in one table
MainOutputs <- Winter_MNL_ModelZero_output


# *************************************************************************
#### Section 5: Output tables ####
# *************************************************************************


Output <- rbind(MainOutputs, BottomRow)

## Output to screen in a nice format for making tables in word
Output %>% write.csv(quote=F)
## Output to a discrete file if that's helpful
Output %>% fwrite(
  sep = ",",
  here("OtherOutput/Tables", "TableX_MNL.txt"),
  row.names = TRUE,
  quote = FALSE
)



# *************************************************************************
#### END OF SCRIPT ####
# *************************************************************************


