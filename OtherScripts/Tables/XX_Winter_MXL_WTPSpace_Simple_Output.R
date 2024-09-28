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
Winter_MXL_ModelOne_model <- here("CEoutput/ModelOne", "Winter_MXL_ModelOne_model.rds") %>% readRDS()
Winter_MXL_ModelTwo_model <- here("CEoutput/ModelTwo", "Winter_MXL_ModelTwo_model.rds") %>% readRDS()
Winter_MXL_ModelThree_model <- here("CEoutput/ModelThree", "Winter_MXL_ModelThree_model.rds") %>% readRDS()


## **************
## Estimates
## **************
Winter_MXL_ModelOne_estimates <- here("CEoutput/ModelOne", "Winter_MXL_ModelOne_estimates.csv") %>% fread() %>% data.frame()
Winter_MXL_ModelTwo_estimates <- here("CEoutput/ModelTwo", "Winter_MXL_ModelTwo_estimates.csv") %>% fread() %>% data.frame()
Winter_MXL_ModelThree_estimates <- here("CEoutput/ModelThree", "Winter_MXL_ModelThree_estimates.csv") %>% fread() %>% data.frame()





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
    "Variable" = Diagnostics(Winter_MXL_ModelOne_model) %>% rownames(),
    "Estimate" = Diagnostics(Winter_MXL_ModelOne_model),

    "Estimate" = Diagnostics(Winter_MXL_ModelThree_model),
    "Estimate" = Diagnostics(Winter_MXL_ModelThree_model),

    "Estimate" = Diagnostics(Winter_MXL_ModelTwo_model),
    "Estimate" = Diagnostics(Winter_MXL_ModelTwo_model)

  ) %>% data.frame()


# *************************************************************************
#### Section 3B: Main covariates ####
# *************************************************************************


## Name all model outputs here
Winter_MXL_ModelOne_output <- ModelOutputs(Winter_MXL_ModelOne_estimates)
Winter_MXL_ModelTwo_output <- ModelOutputs(Winter_MXL_ModelTwo_estimates)
Winter_MXL_ModelThree_output <- ModelOutputs(Winter_MXL_ModelThree_estimates)


## Combine in one table
MainOutputs <-
  cbind(

    cbind(
      rbind("Variable" = Winter_MXL_ModelOne_output[1], data.frame("Variable" = c(0,0,0,0,0,0,0,0,0,0,0,0,0))),
      "Estimate" = rbind(Winter_MXL_ModelOne_output[2], data.frame("Estimate" = c(0,0,0,0,0,0,0,0,0,0,0,0,0)))),

    cbind(
      rbind("Variable" = Winter_MXL_ModelThree_output[1], data.frame("Variable" =  c(0,0,0,0,0,0,0))),
      "Estimate" = rbind(Winter_MXL_ModelThree_output[2], data.frame("Estimate" =  c(0,0,0,0,0,0,0)))),

    Winter_MXL_ModelTwo_output

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
Output %>% write.csv(quote=F)
## Output to a discrete file if that's helpful
Output %>% fwrite(
  sep = ",",
  here("OtherOutput/Tables", "TableX_Simple.txt"),
  row.names = TRUE,
  quote = FALSE
)



# *************************************************************************
#### END OF SCRIPT ####
# *************************************************************************








