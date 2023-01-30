#### RELATE WP5: Replication code to summarise all models  ###############
# Script author: Peter King (p.m.king@kent.ac.uk)
# Last Edited: 30/01/2023
# This is the easiest way to see all the models run


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


## So this code outputs a LaTeX table of estimate,  p.v stars and s.e in brackets ##
### To make it easy,  just change the model name here and the code will output the table for your model:
Estimates <- ModelOne_Estimates

write.csv(data.frame("Variable" =  Estimates$X,
                  "Estimate" =  paste(
  ifelse(
    Estimates$p.val.0. < 0.01,
    paste0(round(Estimates$Estimate,  3),  "***"),
    ifelse(
      Estimates$p.val.0. < 0.05,
      paste0(round(Estimates$Estimate,  3),  "**"),
      ifelse(
        Estimates$p.val.0. < 0.1,
        paste0(round(Estimates$Estimate,  3),  "*"),
        round(Estimates$Estimate,  3)
      )
    )
  ),
  paste0("(", round(Estimates$Std.err.,  3), ")")
)))



#----------------------------------------------------------------------------------------------------------
#### Section 4: Outputting WTP: Model One ####
#----------------------------------------------------------------------------------------------------------


# ## Model One: Attributes Only,  Full Sample WTP:
# ModelOne_WTP_Means <-data.frame(cbind("TaxWTP"= ModelOne_WTP$beta_Tax.post.mean,
#                                      "SmellWTP"= ModelOne_WTP$b_Smell.post.mean,
#                                      "SoundWTP"= ModelOne_WTP$b_Sound.post.mean,
#                                      "ColourWTP"= ModelOne_WTP$b_Colour.post.mean,
#                                      "DeadwoodWTP"= ModelOne_WTP$b_Deadwood.post.mean))
#
# ## Outputting to LaTeX table:
# xtable(t(round(
#   ModelOne_WTP_Means %>% summarise(across(everything(),  list(mean)))
#   ,  3
# )))


## Outputting WTP table:
## Yes,  this looks horrible,  but the table it makes doesn't.
write.csv(
  rbind(
    cbind("Median" =  round(median(ModelTwo_WTP$beta_Tax.post.mean),  3), "SD" =  round(median(ModelTwo_WTP$beta_Tax.post.sd),  3), "95% C-I" =  paste0("(", round(quantile(ModelTwo_WTP$beta_Tax.post.mean,  c(0.45)),  3),  ") - (",  round(quantile(ModelTwo_WTP$beta_Tax.post.mean,  c(0.55)), 3), ")")),
    cbind("Median" =  round(median(ModelOne_WTP$b_Smell.post.mean),  3), "SD" =  round(median(ModelOne_WTP$b_Smell.post.sd),  3), "95% C-I" =  paste0("(", round(quantile(ModelOne_WTP$b_Smell.post.mean,  c(0.45)),  3),  ") - (",  round(quantile(ModelOne_WTP$b_Smell.post.mean,  c(0.55)), 3), ")")),
    cbind("Median" =  round(median(ModelTwo_WTP$b_Smell.post.mean),  3), "SD" =  round(median(ModelTwo_WTP$b_Smell.post.sd),  3), "95% C-I" =  paste0("(", round(quantile(ModelTwo_WTP$b_Smell.post.mean,  c(0.45)),  3),  ") - (",  round(quantile(ModelTwo_WTP$b_Smell.post.mean,  c(0.55)), 3), ")")),
    cbind("Median" =  round(median(ModelTwo_WTP$b_Smell2.post.mean),  3), "SD" =  round(median(ModelTwo_WTP$b_Smell2.post.sd),  3), "95% C-I" =  paste0("(", round(quantile(ModelTwo_WTP$b_Smell2.post.mean,  c(0.45)),  3),  ") - (",  round(quantile(ModelTwo_WTP$b_Smell2.post.mean,  c(0.55)), 3), ")")),
    cbind("Median" =  round(median(ModelOne_WTP$b_Sound.post.mean),  3), "SD" =  round(median(ModelOne_WTP$b_Sound.post.sd),  3), "95% C-I" =  paste0("(", round(quantile(ModelOne_WTP$b_Sound.post.mean,  c(0.45)),  3),  ") - (",  round(quantile(ModelOne_WTP$b_Sound.post.mean,  c(0.55)), 3), ")")),
    cbind("Median" =  round(median(ModelTwo_WTP$b_Sound.post.mean),  3), "SD" =  round(median(ModelTwo_WTP$b_Sound.post.sd),  3), "95% C-I" =  paste0("(", round(quantile(ModelTwo_WTP$b_Sound.post.mean,  c(0.45)),  3),  ") - (",  round(quantile(ModelTwo_WTP$b_Sound.post.mean,  c(0.55)), 3), ")")),
    cbind("Median" =  round(median(ModelTwo_WTP$b_Sound2.post.mean),  3), "SD" =  round(median(ModelTwo_WTP$b_Sound2.post.sd),  3), "95% C-I" =  paste0("(", round(quantile(ModelTwo_WTP$b_Sound2.post.mean,  c(0.45)),  3),  ") - (",  round(quantile(ModelTwo_WTP$b_Sound2.post.mean,  c(0.55)), 3), ")")),
    cbind("Median" =  round(median(ModelOne_WTP$b_Colour.post.mean),  3), "SD" =  round(median(ModelOne_WTP$b_Colour.post.sd),  3), "95% C-I" =  paste0("(", round(quantile(ModelOne_WTP$b_Colour.post.mean,  c(0.45)),  3),  ") - (",  round(quantile(ModelOne_WTP$b_Colour.post.mean,  c(0.55)), 3), ")")),
    cbind("Median" =  round(median(ModelTwo_WTP$b_Colour.post.mean),  3), "SD" =  round(median(ModelTwo_WTP$b_Colour.post.sd),  3), "95% C-I" =  paste0("(", round(quantile(ModelTwo_WTP$b_Colour.post.mean,  c(0.45)),  3),  ") - (",  round(quantile(ModelTwo_WTP$b_Colour.post.mean,  c(0.55)), 3), ")")),
    cbind("Median" =  round(median(ModelTwo_WTP$b_Colour2.post.mean),  3), "SD" =  round(median(ModelTwo_WTP$b_Colour2.post.sd),  3), "95% C-I" =  paste0("(", round(quantile(ModelTwo_WTP$b_Colour2.post.mean,  c(0.45)),  3),  ") - (",  round(quantile(ModelTwo_WTP$b_Colour2.post.mean,  c(0.55)), 3), ")")),
    cbind("Median" =  round(median(ModelOne_WTP$b_Deadwood.post.mean),  3), "SD" =  round(median(ModelOne_WTP$b_Deadwood.post.sd),  3), "95% C-I" =  paste0("(", round(quantile(ModelOne_WTP$b_Deadwood.post.mean,  c(0.45)),  3),  ") - (",  round(quantile(ModelOne_WTP$b_Deadwood.post.mean,  c(0.55)), 3), ")")),
    cbind("Median" =  round(median(ModelTwo_WTP$b_Deadwood.post.mean),  3), "SD" =  round(median(ModelTwo_WTP$b_Deadwood.post.sd),  3), "95% C-I" =  paste0("(", round(quantile(ModelTwo_WTP$b_Deadwood.post.mean,  c(0.45)),  3),  ") - (",  round(quantile(ModelTwo_WTP$b_Deadwood.post.mean,  c(0.55)), 3), ")")),
    cbind("Median" =  round(median(ModelTwo_WTP$b_Deadwood2.post.mean),  3), "SD" =  round(median(ModelTwo_WTP$b_Deadwood2.post.sd),  3), "95% C-I" =  paste0("(", round(quantile(ModelTwo_WTP$b_Deadwood2.post.mean,  c(0.45)),  3),  ") - (",  round(quantile(ModelTwo_WTP$b_Deadwood2.post.mean,  c(0.55)), 3), ")"))))

#----------------------------------------------------------------------------------------------------------
#### Section 4B: Outputting WTP: Model Two ####
#----------------------------------------------------------------------------------------------------------

## Model Two: Levels Only,  Full Sample WTP:
ModelTwo_WTP_Means <-data.frame(cbind("TaxWTP"= ModelTwo_WTP$beta_Tax.post.mean,
                                     "SmellWTP"= ModelTwo_WTP$b_Smell.post.mean,
                                     "SmellWTP2"= ModelTwo_WTP$b_Smell2.post.mean,
                                     "SoundWTP"= ModelTwo_WTP$b_Sound.post.mean,
                                     "SoundWTP2"= ModelTwo_WTP$b_Sound2.post.mean,
                                     "ColourWTP"= ModelTwo_WTP$b_Colour.post.mean,
                                     "ColourWTP2"= ModelTwo_WTP$b_Colour2.post.mean,
                                     "DeadwoodWTP"= ModelTwo_WTP$b_Deadwood.post.mean,
                                     "DeadwoodWTP2"= ModelTwo_WTP$b_Deadwood2.post.mean))


## Outputting to LaTeX table:
write.csv(t(round(
  ModelTwo_WTP_Means %>% summarise(across(everything(),  list(mean)))
  ,  3
)))



#----------------------------------------------------------------------------------------------------------
#### Section 5: Model Diagnostics ####
#----------------------------------------------------------------------------------------------------------


apollo_modelOutput(ModelOne_Model, modelOutput_settings =  list(printPVal= TRUE))
apollo_modelOutput(ModelTwo_Model, modelOutput_settings =  list(printPVal= TRUE))


#----------------------------------------------------------------------------------------------------------
#### Section 6: Model Diagnostics ####
#----------------------------------------------------------------------------------------------------------


## The latest version with WTP appended is 2022_01_07
Winter <- data.frame(fread(here("WinterReplication/OtherData","Winter_dataframe_Step3.csv")))
WTP <- data.frame(fread(here("CEoutput/ModelTwo","Winter_MXL_ModelTwo_ConWTP.csv")))
Winter <- cbind(Winter,WTP)
fwrite(Winter,here("OtherData","Winter_dataframe_Step4.csv"))




#----------------------------------------------------------------------------------------------------------
#### END OF SCRIPT ####
## Next: SpatialPlots.R,  Spatial
#----------------------------------------------------------------------------------------------------------
