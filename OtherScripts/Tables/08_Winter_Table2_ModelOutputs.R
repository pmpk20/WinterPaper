#### RELATE WP5: Table 2 MXL Model Outputs  ###############
# Script author: Peter King (p.king1@leeds.ac.uk)
# Last Edited: 25/02/2024
# Changes:
## - Format for mean and SD columns
## - Adding model diagnostics in as per reviewers
## - reordered variables
## - Changing to correlated models


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



ModelOne_Model <-
  readRDS(here("CEoutput/ModelOne", "Winter_MXL_ModelOne_AllCorrelations.rds"))
ModelOne_WTP <-
  here("CEoutput/ModelOne", "Winter_MXL_ModelOne_AllCorrelations_ConWTP.csv") %>% fread() %>% data.frame()
ModelOne_Estimates <-
  here("CEoutput/ModelOne", "Winter_MXL_ModelOne_AllCorrelations_estimates.csv") %>% fread() %>% data.frame()


## Model with covariates
ModelTwo_Model <-
  readRDS(here("CEoutput/ModelTwo", "Winter_MXL_ModelTwo_AllCorrelations.rds"))
ModelTwo_WTP <-
  here("CEoutput/ModelTwo", "Winter_MXL_ModelTwo_AllCorrelations_ConWTP.csv") %>% fread() %>% data.frame()
ModelTwo_Estimates <-
  here("CEoutput/ModelTwo", "Winter_MXL_ModelTwo_AllCorrelations_estimates.csv") %>% fread() %>% data.frame()




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
#### Section 3A: Output Model One part of the table ####
 # *************************************************************************


## Store model outputs for easy organisation
ModelOne_Output <- ModelOutputs(ModelOne_Estimates)
## Change rownames to variable for easy reference
rownames(ModelOne_Output) <- ModelOne_Output$Variable


## Stitch rows together in the order we want
ModelOne_Table <-
  bind_cols(
    "Mean" =
      bind_rows(
        ModelOne_Output["asc_C", 1:2],
        ModelOne_Output["mu_Tax", 1:2],
        ModelOne_Output["mu_Colour2", 1:2],
        ModelOne_Output["mu_Colour", 1:2],
        ModelOne_Output["mu_Smell2", 1:2],
        ModelOne_Output["mu_Smell", 1:2],
        ModelOne_Output["mu_Sound2", 1:2],
        ModelOne_Output["mu_Sound", 1:2],
        ModelOne_Output["mu_Deadwood2", 1:2],
        ModelOne_Output["mu_Deadwood", 1:2]
      ),
    "Standard Deviation" =
      bind_rows(
        ModelOne_Output["asc_C", 1:2],
        ModelOne_Output["sig_Tax", 1:2],
        ModelOne_Output["sig_Colour2", 1:2],
        ModelOne_Output["sig_Colour", 1:2],
        ModelOne_Output["sig_Smell2", 1:2],
        ModelOne_Output["sig_Smell", 1:2],
        ModelOne_Output["sig_Sound2", 1:2],
        ModelOne_Output["sig_Sound", 1:2],
        ModelOne_Output["sig_Deadwood2", 1:2],
        ModelOne_Output["sig_Deadwood", 1:2]
      )
  )

 # *************************************************************************
#### Section 3B: Output Model Two part of the table ####
 # *************************************************************************



## Store model outputs for easy organisation
ModelTwo_Output <- ModelOutputs(ModelTwo_Estimates)
## Change rownames to variable for easy reference
rownames(ModelTwo_Output) <- ModelTwo_Output$Variable


## Stitch rows together in the order we want
ModelTwo_Table <-
  bind_cols(
    "Mean" =
      bind_rows(
        ModelTwo_Output["asc_C", 1:2],
        ModelTwo_Output["mu_Tax", 1:2],
        ModelTwo_Output["mu_Colour2", 1:2],
        ModelTwo_Output["mu_Colour", 1:2],
        ModelTwo_Output["mu_Smell2", 1:2],
        ModelTwo_Output["mu_Smell", 1:2],
        ModelTwo_Output["mu_Sound2", 1:2],
        ModelTwo_Output["mu_Sound", 1:2],
        ModelTwo_Output["mu_Deadwood2", 1:2],
        ModelTwo_Output["mu_Deadwood", 1:2]
      ),
    "Standard Deviation" =
      bind_rows(
        ModelTwo_Output["asc_C", 1:2],
        ModelTwo_Output["sig_Tax", 1:2],
        ModelTwo_Output["sig_Colour2", 1:2],
        ModelTwo_Output["sig_Colour", 1:2],
        ModelTwo_Output["sig_Smell2", 1:2],
        ModelTwo_Output["sig_Smell", 1:2],
        ModelTwo_Output["sig_Sound2", 1:2],
        ModelTwo_Output["sig_Sound", 1:2],
        ModelTwo_Output["sig_Deadwood2", 1:2],
        ModelTwo_Output["sig_Deadwood", 1:2]
      )
  )


 # *************************************************************************
#### Section 4: Create tables ####
 # *************************************************************************


## Top part with mean and SDs
Table2_Top <- bind_cols("Variable" = ModelOne_Table[, 1],
                    "MeanOne" = ModelOne_Table[, 2],
                    "SDOne" = ModelOne_Table[, 4],
                    "MeanTwo" = ModelTwo_Table[, 2],
                    "SDTwo" = ModelTwo_Table[, 4])


## Artificially fix ASC row
Table2_Top[Table2_Top$Variable == "asc_C",] <-
  c("asc_C", ModelOne_Output["asc_C", 1:2], ModelTwo_Output["asc_C", 1:2])



## New reveiwer-requested bottom row
Table2_Bottom <-  bind_cols(
  "Variable" = c("N", "AIC", "Adj.R2", "LogLike"),
  "MeanOne" = Diagnostics(ModelOne_Model)[, 1],
  "SDOne" = 0,
  "MeanTwo" = Diagnostics(ModelTwo_Model)[, 1],
  "SDTwo" = 0
)

Table2 <- rbind(Table2_Top,
                Table2_Bottom)


# *************************************************************************
#### Section 5: Output tables ####
# *************************************************************************


## Output to screen in a nice format for making tables in word
Table2 %>% write.csv(quote = FALSE)
## Output to a discrete file if that's helpful
Table2 %>% fwrite(
  sep = ",",
  here("OtherOutput/Tables", "Table2.txt"),
  row.names = TRUE,
  quote = FALSE
)



 # *************************************************************************
#### END OF SCRIPT ####
 # *************************************************************************
