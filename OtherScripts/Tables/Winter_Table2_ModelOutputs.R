#### RELATE WP5: Table 2 MXL Model Outputs  ###############
# Script author: Peter King (p.m.king@kent.ac.uk)
# Last Edited: 01/02/2023
# Trying to output the models here


#----------------------------------------------------------------------------------------------------------
#### Section 0: Setup and estimate models ####
## NOTE: Data is stored in PK R WORK/Strictly Replication Files Only/Winter Replication/CEModelScripts/
## NOTE: Results are stored in PK R WORK/Strictly Replication Files Only/Winter Replication/CEModelData/
#----------------------------------------------------------------------------------------------------------


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


#----------------------------------------------------------------------------------------------------------
#### Section 1: Importing Model,  Estimates,  and WTP for FULL SAMPLE models ####
#----------------------------------------------------------------------------------------------------------


ModelOne_Model <- readRDS(here("CEoutput/ModelOne","Winter_MXL_ModelOne.rds"))
ModelOne_WTP <- here("CEoutput/ModelOne","Winter_MXL_ModelOne_ConWTP.csv") %>% fread() %>% data.frame()
ModelOne_Estimates <- here("CEoutput/ModelOne","Winter_MXL_ModelOne_estimates.csv") %>% fread() %>% data.frame()


## Model with covariates
ModelTwo_Model <- readRDS(here("CEoutput/ModelTwo","Winter_MXL_ModelTwo.rds"))
ModelTwo_WTP <- here("CEoutput/ModelTwo","Winter_MXL_ModelTwo_ConWTP.csv") %>% fread() %>% data.frame()
ModelTwo_Estimates <- here("CEoutput/ModelTwo","Winter_MXL_ModelTwo_estimates.csv") %>% fread() %>% data.frame()




#----------------------------------------------------------------------------------------------------------
#### Section 2: Model Summary Function ####
#----------------------------------------------------------------------------------------------------------


## So this code outputs a table of estimate,  p.v stars and s.e in brackets ##
### To make it easy,  just change the model name here and the code will output the table for your model:
ModelOutputs <- function(Estimates) {
  data.frame("Variable" =  Estimates$V1,
                    "Estimate" =  paste(
    ifelse(
      Estimates$Rob.p.val.0. < 0.01,
      paste0(round(Estimates$Estimate,  3),  "***"),
      ifelse(
        Estimates$Rob.p.val.0. < 0.05,
        paste0(round(Estimates$Estimate,  3),  "**"),
        ifelse(
          Estimates$Rob.p.val.0. < 0.1,
          paste0(round(Estimates$Estimate,  3),  "*"),
          round(Estimates$Estimate,  3)
        )
      )
    ),
    paste0("(", round(Estimates$Rob.std.err.,  3), ")")
  ))
}

#----------------------------------------------------------------------------------------------------------
#### Section 3A: Output Model One part of the table ####
#----------------------------------------------------------------------------------------------------------


## Store model outputs for easy organisation
ModelOne_Output <- ModelOutputs(ModelOne_Estimates)
## Change rownames to variable for easy reference
rownames(ModelOne_Output) <- ModelOne_Output$Variable


## Stitch rows together in the order we want
ModelOne_Table <- bind_rows(
  ModelOne_Output["asc_C",1:2],
  ModelOne_Output["mu_Tax",1:2],
  ModelOne_Output["mu_Colour",1:2],
  ModelOne_Output["mu_Colour2",1:2],
  ModelOne_Output["mu_Smell",1:2],
  ModelOne_Output["mu_Smell2",1:2],
  ModelOne_Output["mu_Sound",1:2],
  ModelOne_Output["mu_Sound2",1:2],
  ModelOne_Output["mu_Deadwood",1:2],
  ModelOne_Output["mu_Deadwood2",1:2],
  ModelOne_Output["sig_Tax",1:2],
  ModelOne_Output["sig_Colour",1:2],
  ModelOne_Output["sig_Colour2",1:2],
  ModelOne_Output["sig_Smell",1:2],
  ModelOne_Output["sig_Smell2",1:2],
  ModelOne_Output["sig_Sound",1:2],
  ModelOne_Output["sig_Sound2",1:2],
  ModelOne_Output["sig_Deadwood",1:2],
  ModelOne_Output["sig_Deadwood2",1:2]
)


#----------------------------------------------------------------------------------------------------------
#### Section 3B: Output Model Two part of the table ####
#----------------------------------------------------------------------------------------------------------


## Store model outputs for easy organisation
ModelTwo_Output <- ModelOutputs(ModelTwo_Estimates)
## Change rownames to variable for easy reference
rownames(ModelTwo_Output) <- ModelTwo_Output$Variable


## Stitch rows together in the order we want
ModelTwo_Table <- bind_rows(
  ModelTwo_Output["asc_C",1:2],
  ModelTwo_Output["mu_Tax",1:2],
  ModelTwo_Output["mu_Colour",1:2],
  ModelTwo_Output["mu_Colour2",1:2],
  ModelTwo_Output["mu_Smell",1:2],
  ModelTwo_Output["mu_Smell2",1:2],
  ModelTwo_Output["mu_Sound",1:2],
  ModelTwo_Output["mu_Sound2",1:2],
  ModelTwo_Output["mu_Deadwood",1:2],
  ModelTwo_Output["mu_Deadwood2",1:2],
  ModelTwo_Output["sig_Tax",1:2],
  ModelTwo_Output["sig_Colour",1:2],
  ModelTwo_Output["sig_Colour2",1:2],
  ModelTwo_Output["sig_Smell",1:2],
  ModelTwo_Output["sig_Smell2",1:2],
  ModelTwo_Output["sig_Sound",1:2],
  ModelTwo_Output["sig_Sound2",1:2],
  ModelTwo_Output["sig_Deadwood",1:2],
  ModelTwo_Output["sig_Deadwood2",1:2]
)


#----------------------------------------------------------------------------------------------------------
#### Section 4: Output tables ####
#----------------------------------------------------------------------------------------------------------


Table2 <- bind_cols("Variable"=ModelOne_Table[,1],
                    "ModelOne"=ModelOne_Table[,2],
                    "ModelTwo"=ModelTwo_Table[,2])



## Output to screen in a nice format for making tables in word
Table2 %>% write.csv(quote=F)
## Output to a discrete file if that's helpful
write.table(Table2,
            here("CEoutput","Table2.txt"),
            sep=",",quote=F)




#----------------------------------------------------------------------------------------------------------
#### END OF SCRIPT ####
## Next: SpatialPlots.R,  Spatial
#----------------------------------------------------------------------------------------------------------
