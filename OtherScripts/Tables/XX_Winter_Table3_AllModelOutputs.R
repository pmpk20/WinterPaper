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


## Run these only if not already estimated
## The difference between each model is in the filenames
## For reference: Levels = =  Sound Medium and Sound High vs Attributes = =  Sound
## Full data (N =  1711) vs Truncated (N =  ~1500)
# source("CEModelScripts/Replication_WP5_Winter_MXL_LevelsOnly_FullSample.R")
# source("CEModelScripts/Replication_WP5_Winter_MXL_LevelsOnly_Truncated.R")
# source("CEModelScripts/Replication_WP5_Winter_MXL_AttributesOnly_FullSample.R")
# source("CEModelScripts/Replication_WP5_Winter_MXL_AttributesOnly_Truncated.R")
# source("CEModelScripts/Replication_WP5_Winter_MXL_Levels_SomeCovariates_FullSample.R")
# source("CEModelScripts/Replication_WP5_Winter_MXL_Levels_SomeCovariates_Truncated.R")
# source("CEModelScripts/Replication_WP5_Winter_MXL_Levels_AllCovariates_FullSample.R")
# source("CEModelScripts/Replication_WP5_Winter_MXL_Levels_AllCovariates_Truncated.R")


# *************************************************************************
#### Section 1: Importing Model,  Estimates,  and WTP for FULL SAMPLE models ####
# *************************************************************************

## **************
## Models
## **************
Winter_MXL_ModelOne_model <- here("CEoutput/ModelOne", "Winter_MXL_ModelOne_SVS.rds") %>% readRDS()
Winter_MXL_ModelTwo_model <- here("CEoutput/ModelTwo", "Winter_MXL_ModelTwo.rds") %>% readRDS()
Winter_MXL_ModelThree_model <- here("CEoutput/ModelTwo", "Winter_MXL_ModelThree_model.rds") %>% readRDS()
Winter_MXL_ModelOne_Correlated_model <- here("CEoutput/ModelOne", "Winter_MXL_ModelOne_Correlated_model.rds") %>% readRDS()
Winter_MXL_ModelTwo_Correlated_model <- here("CEoutput/ModelTwo", "Winter_MXL_ModelTwo_Correlated_model.rds") %>% readRDS()
Winter_MXL_ModelThree_Correlated_model <- here("CEoutput/ModelTwo", "Winter_MXL_ModelThree_Correlated_model.rds") %>% readRDS()
Winter_MXL_ModelOne_PrefSpace_model <- here("CEoutput/ModelOne", "Winter_MXL_ModelOne_PrefSpace.rds") %>% readRDS()
Winter_MXL_ModelTwo_PrefSpace_model <- here("CEoutput/ModelTwo", "Winter_MXL_ModelTwo_PrefSpace_model.rds") %>% readRDS()
Winter_MXL_ModelThree_PrefSpace_model <- here("CEoutput/ModelTwo", "Winter_MXL_ModelThree_PrefSpace_model.rds") %>% readRDS()
Winter_MXL_ModelOne_PrefSpace_Correlated <- here("CEoutput/ModelOne", "Winter_MXL_ModelOne_PrefSpace_Correlated_model.rds") %>% readRDS()
Winter_MXL_ModelTwo_PrefSpace_Correlated_model <- here("CEoutput/ModelTwo", "Winter_MXL_ModelTwo_PrefSpace_Correlated_model.rds") %>% readRDS()
Winter_MXL_ModelThree_PrefSpace_Correlated_model <- here("CEoutput/ModelTwo", "Winter_MXL_ModelThree_PrefSpace_Correlated_model.rds") %>% readRDS()
Winter_MXL_ModelOne_PrefSpace_Interactions_model <- here("CEoutput/ModelOne", "Winter_MXL_ModelOne_PrefSpace_Interactions_model.rds") %>% readRDS()
Winter_MXL_ModelOne_Interactions_model <- here("CEoutput/ModelOne", "Winter_MXL_ModelOne_Interactions_model.rds") %>% readRDS()


## **************
## Estimates
## **************
Winter_MXL_ModelOne_estimates <- here("CEoutput/ModelOne", "Winter_MXL_ModelOne_SVS_estimates.csv") %>% fread() %>% data.frame()
Winter_MXL_ModelTwo_estimates <- here("CEoutput/ModelTwo", "Winter_MXL_ModelTwo_estimates.csv") %>% fread() %>% data.frame()
Winter_MXL_ModelThree_estimates <- here("CEoutput/ModelTwo", "Winter_MXL_ModelThree_estimates.csv") %>% fread() %>% data.frame()
Winter_MXL_ModelOne_Correlated_estimates <- here("CEoutput/ModelOne", "Winter_MXL_ModelOne_Correlated_estimates.csv") %>% fread() %>% data.frame()
Winter_MXL_ModelTwo_Correlated_estimates <- here("CEoutput/ModelTwo", "Winter_MXL_ModelTwo_Correlated_estimates.csv") %>% fread() %>% data.frame()
Winter_MXL_ModelThree_Correlated_estimates <- here("CEoutput/ModelTwo", "Winter_MXL_ModelThree_Correlated_estimates.csv") %>% fread() %>% data.frame()
Winter_MXL_ModelOne_PrefSpace_estimates <- here("CEoutput/ModelOne", "Winter_MXL_ModelOne_PrefSpace_estimates.csv") %>% fread() %>% data.frame()
Winter_MXL_ModelTwo_PrefSpace_estimates <- here("CEoutput/ModelTwo", "Winter_MXL_ModelTwo_PrefSpace_estimates.csv") %>% fread() %>% data.frame()
Winter_MXL_ModelThree_PrefSpace_estimates <- here("CEoutput/ModelTwo", "Winter_MXL_ModelThree_PrefSpace_estimates.csv") %>% fread() %>% data.frame()
Winter_MXL_ModelOne_PrefSpace_Correlated_estimates <- here("CEoutput/ModelOne", "Winter_MXL_ModelOne_PrefSpace_Correlated_estimates.csv") %>% fread() %>% data.frame()
Winter_MXL_ModelTwo_PrefSpace_Correlated_estimates <- here("CEoutput/ModelTwo", "Winter_MXL_ModelTwo_PrefSpace_Correlated_estimates.csv") %>% fread() %>% data.frame()
Winter_MXL_ModelThree_PrefSpace_Correlated_estimates <- here("CEoutput/ModelTwo", "Winter_MXL_ModelThree_PrefSpace_Correlated_estimates.csv") %>% fread() %>% data.frame()
Winter_MXL_ModelOne_PrefSpace_Interactions_estimates <- here("CEoutput/ModelOne", "Winter_MXL_ModelOne_PrefSpace_Interactions_estimates.csv") %>% fread() %>% data.frame()
Winter_MXL_ModelOne_Interactions_estimates <- here("CEoutput/ModelOne", "Winter_MXL_ModelOne_Interactions_estimates.csv") %>% fread() %>% data.frame()





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
  Diagnostics(Winter_MXL_ModelOne_model) %>% rownames(),
Diagnostics(Winter_MXL_ModelOne_model),
Diagnostics(Winter_MXL_ModelTwo_model),
Diagnostics(Winter_MXL_ModelThree_model),

Diagnostics(Winter_MXL_ModelOne_Correlated_model),
Diagnostics(Winter_MXL_ModelTwo_Correlated_model),
Diagnostics(Winter_MXL_ModelThree_Correlated_model),

Diagnostics(Winter_MXL_ModelOne_PrefSpace_model),
Diagnostics(Winter_MXL_ModelTwo_PrefSpace_model),
Diagnostics(Winter_MXL_ModelThree_PrefSpace_model),

Diagnostics(Winter_MXL_ModelOne_PrefSpace_Correlated),
Diagnostics(Winter_MXL_ModelTwo_PrefSpace_Correlated_model),
Diagnostics(Winter_MXL_ModelThree_PrefSpace_Correlated_model),


Diagnostics(Winter_MXL_ModelOne_PrefSpace_Interactions_model),
Diagnostics(Winter_MXL_ModelOne_Interactions_model)
)



# *************************************************************************
#### Section 3B: Main covariates ####
# *************************************************************************


## Necessary for basic filtering
BasicOrder <- ModelOutputs(Winter_MXL_ModelOne_estimates)["Variable"]
CovariateOrder_Start <- ModelOutputs(Winter_MXL_ModelTwo_PrefSpace_estimates)["Variable"]
CovariateOrder_Final <- CovariateOrder_Start[!CovariateOrder_Start$Variable %in% (BasicOrder %>% unlist()), ]

SigOrder_Start <- ModelOutputs(Winter_MXL_ModelThree_Correlated_estimates)["Variable"]
SigOrder_Final <- SigOrder_Start %>% filter(!Variable %in% (BasicOrder %>% unlist()) &
                                              !Variable %in% (CovariateOrder_Final %>% unlist()))

Interactions <- Winter_MXL_ModelOne_PrefSpace_Interactions_output$Variable[22:29]


## Name all model outputs here
Winter_MXL_ModelOne_output <- ModelOutputs(Winter_MXL_ModelOne_estimates)
Winter_MXL_ModelTwo_output <- ModelOutputs(Winter_MXL_ModelTwo_estimates)
Winter_MXL_ModelThree_output <- ModelOutputs(Winter_MXL_ModelThree_estimates)
Winter_MXL_ModelOne_PrefSpace_output <- ModelOutputs(Winter_MXL_ModelOne_PrefSpace_estimates)
Winter_MXL_ModelTwo_PrefSpace_output <- ModelOutputs(Winter_MXL_ModelTwo_PrefSpace_estimates)
Winter_MXL_ModelThree_PrefSpace_output <- ModelOutputs(Winter_MXL_ModelThree_PrefSpace_estimates)
Winter_MXL_ModelOne_Correlated_output <- ModelOutputs(Winter_MXL_ModelOne_Correlated_estimates)
Winter_MXL_ModelTwo_Correlated_output <- ModelOutputs(Winter_MXL_ModelTwo_Correlated_estimates)
Winter_MXL_ModelThree_Correlated_output <- ModelOutputs(Winter_MXL_ModelThree_Correlated_estimates)
Winter_MXL_ModelOne_PrefSpace_Correlated_output <- ModelOutputs(Winter_MXL_ModelOne_PrefSpace_Correlated_estimates)
Winter_MXL_ModelTwo_PrefSpace_Correlated_output <- ModelOutputs(Winter_MXL_ModelTwo_PrefSpace_Correlated_estimates)
Winter_MXL_ModelThree_PrefSpace_Correlated_output <- ModelOutputs(Winter_MXL_ModelThree_PrefSpace_Correlated_estimates)
Winter_MXL_ModelOne_PrefSpace_Interactions_output <- ModelOutputs(Winter_MXL_ModelOne_PrefSpace_Interactions_estimates)
Winter_MXL_ModelOne_Interactions_output <- ModelOutputs(Winter_MXL_ModelOne_Interactions_estimates)


## Combine in one table
MainOutputs <-
cbind(
  "ModelOne" = Winter_MXL_ModelOne_output[Winter_MXL_ModelOne_output$Variable %in% c(BasicOrder %>% unlist()), ],
  "ModelTwo" = Winter_MXL_ModelTwo_output[Winter_MXL_ModelTwo_output$Variable %in% c(BasicOrder %>% unlist()), ][2],
  "ModelThree" = Winter_MXL_ModelThree_output[Winter_MXL_ModelThree_output$Variable %in% c(BasicOrder %>% unlist()), ][2],
  "ModelOne_PrefSpace" = Winter_MXL_ModelOne_PrefSpace_output[Winter_MXL_ModelOne_PrefSpace_output$Variable %in% c(BasicOrder %>% unlist()), ][2],
  "ModelTwo_PrefSpace" = Winter_MXL_ModelTwo_PrefSpace_output[Winter_MXL_ModelTwo_PrefSpace_output$Variable %in% c(BasicOrder %>% unlist()), ][2],
  "ModelThree_PrefSpace" = Winter_MXL_ModelThree_PrefSpace_output[Winter_MXL_ModelThree_PrefSpace_output$Variable %in% c(BasicOrder %>% unlist()), ][2],
  "ModelOne_Correlated" = Winter_MXL_ModelOne_Correlated_output[Winter_MXL_ModelOne_Correlated_output$Variable %in% c(BasicOrder %>% unlist()), ][2],
  "ModelTwo_Correlated" = Winter_MXL_ModelTwo_Correlated_output[Winter_MXL_ModelTwo_Correlated_output$Variable %in% c(BasicOrder %>% unlist()), ][2],
  "ModelThree_Correlated" = Winter_MXL_ModelThree_Correlated_output[Winter_MXL_ModelThree_Correlated_output$Variable %in% c(BasicOrder %>% unlist()), ][2],
  "ModelOne_PrefSpace_Correlated" = Winter_MXL_ModelOne_PrefSpace_Correlated_output[Winter_MXL_ModelOne_PrefSpace_Correlated_output$Variable %in% c(BasicOrder %>% unlist()), ][2],
  "ModelTwo_PrefSpace_Correlated" = Winter_MXL_ModelTwo_PrefSpace_Correlated_output[Winter_MXL_ModelTwo_PrefSpace_Correlated_output$Variable %in% c(BasicOrder %>% unlist()), ][2],
  "ModelThree_PrefSpace_Correlated" = Winter_MXL_ModelThree_PrefSpace_Correlated_output[Winter_MXL_ModelThree_PrefSpace_Correlated_output$Variable %in% c(BasicOrder %>% unlist()), ][2],
  "ModelOne_PrefSpace_Interactions" = Winter_MXL_ModelOne_PrefSpace_Interactions_output[Winter_MXL_ModelOne_PrefSpace_Interactions_output$Variable %in% c(BasicOrder %>% unlist()), ][2],
  "ModelOne_Interactions" = Winter_MXL_ModelOne_Interactions_output[Winter_MXL_ModelOne_Interactions_output$Variable %in% c(BasicOrder %>% unlist()), ][2]
)


TableHeadings <- c(
  "Variable",
  "ModelOne",
  "ModelTwo",
  "ModelThree",
  "ModelOne_PrefSpace",
  "ModelTwo_PrefSpace",
  "ModelThree_PrefSpace",
  "ModelOne_Correlated",
  "ModelTwo_Correlated",
  "ModelThree_Correlated",
  "ModelOne_PrefSpace_Correlated",
  "ModelTwo_PrefSpace_Correlated",
  "ModelThree_PrefSpace_Correlated",
  "ModelOne_PrefSpace_Interactions",
  "ModelOne_Interactions"
)


## Rename for ease of reference
colnames(MainOutputs) <- TableHeadings

MainOutputs %>% write.csv(quote = FALSE)



## **********************************
#### Covariates part
# ***********************************

Buffer_Long <- data.frame("Estimate" = c(0,0 ,0,0,0,0,0 ,0,0,0,0))
Buffer_Short <- data.frame("Estimate" = c(0,0,0 ,0,0))



CovOutputs <-
  cbind(
    CovariateOrder_Final,
    rbind(Winter_MXL_ModelOne_output[Winter_MXL_ModelOne_output$Variable %in% c(CovariateOrder_Final %>% unlist()), ][2], Buffer_Long %>% data.frame()),
    rbind(Winter_MXL_ModelTwo_output[Winter_MXL_ModelTwo_output$Variable %in% c(CovariateOrder_Final %>% unlist()), ][2]),
    rbind(Winter_MXL_ModelThree_output[Winter_MXL_ModelThree_output$Variable %in% c(CovariateOrder_Final %>% unlist()), ][2], Buffer_Short %>% data.frame()),
    rbind(Winter_MXL_ModelOne_PrefSpace_output[Winter_MXL_ModelOne_PrefSpace_output$Variable %in% c(CovariateOrder_Final %>% unlist()), ][2], Buffer_Long %>% data.frame()),
    rbind(Winter_MXL_ModelTwo_PrefSpace_output[Winter_MXL_ModelTwo_PrefSpace_output$Variable %in% c(CovariateOrder_Final %>% unlist()), ][2]),
    rbind(Winter_MXL_ModelThree_PrefSpace_output[Winter_MXL_ModelThree_PrefSpace_output$Variable %in% c(CovariateOrder_Final %>% unlist()), ][2], Buffer_Short %>% data.frame()),
    rbind(Winter_MXL_ModelOne_Correlated_output[Winter_MXL_ModelOne_Correlated_output$Variable %in% c(CovariateOrder_Final %>% unlist()), ][2], Buffer_Long %>% data.frame()),
    rbind(Winter_MXL_ModelTwo_Correlated_output[Winter_MXL_ModelTwo_Correlated_output$Variable %in% c(CovariateOrder_Final %>% unlist()), ][2], Buffer_Short %>% data.frame()),
    rbind(Winter_MXL_ModelThree_Correlated_output[Winter_MXL_ModelThree_Correlated_output$Variable %in% c(CovariateOrder_Final %>% unlist()), ][2], Buffer_Short %>% data.frame()),
    rbind(Winter_MXL_ModelOne_PrefSpace_Correlated_output[Winter_MXL_ModelOne_PrefSpace_Correlated_output$Variable %in% c(CovariateOrder_Final %>% unlist()), ][2], Buffer_Long %>% data.frame()),
    rbind(Winter_MXL_ModelTwo_PrefSpace_Correlated_output[Winter_MXL_ModelTwo_PrefSpace_Correlated_output$Variable %in% c(CovariateOrder_Final %>% unlist()), ][2]),
    rbind(Winter_MXL_ModelThree_PrefSpace_Correlated_output[Winter_MXL_ModelThree_PrefSpace_Correlated_output$Variable %in% c(CovariateOrder_Final %>% unlist()), ][2], Buffer_Short %>% data.frame()),

    rbind(Winter_MXL_ModelOne_PrefSpace_Interactions_output[Winter_MXL_ModelOne_PrefSpace_Interactions_output$Variable %in% c(CovariateOrder_Final %>% unlist()), ][2], Buffer_Long %>% data.frame()),
    rbind(Winter_MXL_ModelOne_Interactions_output[Winter_MXL_ModelOne_Interactions_output$Variable %in% c(CovariateOrder_Final %>% unlist()), ][2], Buffer_Long %>% data.frame())
  )


colnames(CovOutputs) <- TableHeadings

## **********************************
#### Sig part
# ***********************************


Buffer_Longest <- data.frame("Estimate" = c(0,0,0,0,0,0,
                                            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))

SigOutputs <-
  cbind(
    SigOrder_Final,
    rbind(Winter_MXL_ModelOne_output[Winter_MXL_ModelOne_output$Variable %in% c(SigOrder_Final %>% unlist()), ][2], Buffer_Longest %>% data.frame()),
    rbind(Winter_MXL_ModelTwo_output[Winter_MXL_ModelTwo_output$Variable %in% c(SigOrder_Final %>% unlist()), ][2], Buffer_Longest %>% data.frame()),
    rbind(Winter_MXL_ModelThree_output[Winter_MXL_ModelThree_output$Variable %in% c(SigOrder_Final %>% unlist()), ][2], Buffer_Longest %>% data.frame()),
    rbind(Winter_MXL_ModelOne_PrefSpace_output[Winter_MXL_ModelOne_PrefSpace_output$Variable %in% c(SigOrder_Final %>% unlist()), ][2], Buffer_Longest %>% data.frame()),
    rbind(Winter_MXL_ModelTwo_PrefSpace_output[Winter_MXL_ModelTwo_PrefSpace_output$Variable %in% c(SigOrder_Final %>% unlist()), ][2], Buffer_Longest %>% data.frame()),
    rbind(Winter_MXL_ModelThree_PrefSpace_output[Winter_MXL_ModelThree_PrefSpace_output$Variable %in% c(SigOrder_Final %>% unlist()), ][2], Buffer_Longest %>% data.frame()),

    Winter_MXL_ModelOne_Correlated_output[Winter_MXL_ModelOne_Correlated_output$Variable %in% c(SigOrder_Final %>% unlist()), ][2],
    Winter_MXL_ModelTwo_Correlated_output[Winter_MXL_ModelTwo_Correlated_output$Variable %in% c(SigOrder_Final %>% unlist()), ][2],
    Winter_MXL_ModelThree_Correlated_output[Winter_MXL_ModelThree_Correlated_output$Variable %in% c(SigOrder_Final %>% unlist()), ][2],

    Winter_MXL_ModelOne_PrefSpace_Correlated_output[Winter_MXL_ModelOne_PrefSpace_Correlated_output$Variable %in% c(SigOrder_Final %>% unlist()), ][2],
    Winter_MXL_ModelTwo_PrefSpace_Correlated_output[Winter_MXL_ModelTwo_PrefSpace_Correlated_output$Variable %in% c(SigOrder_Final %>% unlist()), ][2],
    Winter_MXL_ModelThree_PrefSpace_Correlated_output[Winter_MXL_ModelThree_PrefSpace_Correlated_output$Variable %in% c(SigOrder_Final %>% unlist()), ][2],
    rbind(Winter_MXL_ModelOne_PrefSpace_Interactions_output[Winter_MXL_ModelOne_PrefSpace_Interactions_output$Variable %in% c(SigOrder_Final %>% unlist()), ][2], Buffer_Longest %>% data.frame()),
    rbind(Winter_MXL_ModelOne_Interactions_output[Winter_MXL_ModelOne_Interactions_output$Variable %in% c(SigOrder_Final %>% unlist()), ][2], Buffer_Longest %>% data.frame())
  )


colnames(SigOutputs) <- TableHeadings



## **********************************
#### Interactions part
# ***********************************


Buffer_Ints <- data.frame("Estimate" = c(0,0,0,0,0,0,0,0))

Int_Outputs <- cbind(
  "Variable" = Interactions,
  "ModelOne" = Buffer_Ints,
  "ModelTwo" = Buffer_Ints,
  "ModelThree" = Buffer_Ints,
  "ModelOne_PrefSpace" = Buffer_Ints,
  "ModelTwo_PrefSpace" = Buffer_Ints,
  "ModelThree_PrefSpace" = Buffer_Ints,
  "ModelOne_Correlated" = Buffer_Ints,
  "ModelTwo_Correlated" = Buffer_Ints,
  "ModelThree_Correlated" = Buffer_Ints,
  "ModelOne_PrefSpace_Correlated" = Buffer_Ints,
  "ModelTwo_PrefSpace_Correlated" = Buffer_Ints,
  "ModelThree_PrefSpace_Correlated" = Buffer_Ints,
  "ModelOne_PrefSpace_Interactions" = rbind(Winter_MXL_ModelOne_PrefSpace_Interactions_output[Winter_MXL_ModelOne_PrefSpace_Interactions_output$Variable %in% c(Interactions %>% unlist()), ][2]),
  "ModelOne_Interactions" = rbind(Winter_MXL_ModelOne_Interactions_output[Winter_MXL_ModelOne_Interactions_output$Variable %in% c(Interactions %>% unlist()), ][2])
)


colnames(Int_Outputs) <- TableHeadings




# *************************************************************************
#### Section 3B: Main covariates ####
# *************************************************************************


colnames(BottomRow) <- TableHeadings

AllTogether <-
rbind(
  MainOutputs,
  CovOutputs,
  SigOutputs,
  Int_Outputs,
  BottomRow
)

AllTogether %>% write.csv(quote = FALSE)


# *************************************************************************
#### Section 5: Output tables ####
# *************************************************************************


## Output to screen in a nice format for making tables in word
AllTogether %>% write.csv(quote=F)
## Output to a discrete file if that's helpful
AllTogether %>% fwrite(
  sep = ",",
  here("OtherOutput/Tables", "Table3_NEW.txt"),
  row.names = TRUE,
  quote = FALSE
)



# *************************************************************************
#### END OF SCRIPT ####
# *************************************************************************
