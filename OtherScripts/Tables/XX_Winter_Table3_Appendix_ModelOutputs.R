#### RELATE WP5: Replication code for Table3 full version  ###############
# Script author: Peter King (p.king1@leeds.ac.uk)
# Last Edited: 25/02/2024
# Change:
# - Changed to correlated models



# *************************************************************************
#### Section 0: Setup and estimate models ####
## NOTE: Data is stored in PK R WORK/Strictly Replication Files Only/Winter Replication/CEModelScripts/
## NOTE: Results are stored in PK R WORK/Strictly Replication Files Only/Winter Replication/CEModelData/
# *************************************************************************


library(stats)
library(data.table)
library(here)
library(tidyverse)
library(magrittr)

# *************************************************************************
#### Section 1: Importing Model,  Estimates,  and WTP for FULL SAMPLE models ####
# *************************************************************************


## Read in participant data here:
Winter <- here("OtherData", "Winter_dataframe_Step4.csv") %>% fread() %>% data.frame()


## Model with covariates
ModelThree_Model <-
  readRDS(here("CEoutput/ModelThree", "Winter_MXL_ModelThree_Correlated.rds"))

ModelThree_WTP <-
  here("CEoutput/ModelThree", "Winter_MXL_ModelThree_Correlated_ConWTP.csv") %>% fread() %>% data.frame()

ModelThree_Estimates <-
  here("CEoutput/ModelThree", "Winter_MXL_ModelThree_Correlated_estimates.csv") %>% fread() %>% data.frame()



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


# *************************************************************************
#### Section 3: Summarise sociodemographic variables ####
# *************************************************************************



## Make a list of variables included in ModelThree using their variable names
Variables <- ModelThree_Estimates$V1[46:56] %>%
  c() %>%
  str_replace_all(
    c(
      beta_ = "",
      Age = "DummyAge",
      Distance = "MilesDistance",
      White = "EthnicityDummyWhite",
      Income = "IncomeDummy"
    )
  )


## Now using that list,
# summarise the mean and stddev,
# then rearrange the table
# and round nicely
Table3_LeftSide <- Winter %>%
  summarize(across(all_of(Variables), list(
    VariableMean = mean,
    Std.Dev.  = sd
  ))) %>%
  pivot_longer(
    cols      = everything(),
    names_sep = "_",
    names_to  = c("variable", "statistic")
  ) %>%
  pivot_wider(names_from = "statistic") %>%
  mutate(
    VariableMean = VariableMean %>%
      round(3) %>%
      sprintf("%.2f",. ),
    Std.Dev. = Std.Dev. %>%
      round(3) %>%
      sprintf("%.2f",. ))


# *************************************************************************
#### Section 4: Summarise variable estimates ####
# *************************************************************************




## Store model outputs for easy organisation
ModelThree_Output <- ModelOutputs(ModelThree_Estimates)

## Output the right hand side of the table
Table3_RightSide <- ModelThree_Output[46:56, 1:2]


# *************************************************************************
#### Section 4: Output tables ####
# *************************************************************************



Table3 <- bind_cols(Table3_LeftSide,
                    "Estimates" = Table3_RightSide[, 2])



## Output to screen in a nice format for making tables in word
Table3 %>% write.csv(quote = F)
## Output to a discrete file if that's helpful
Table3 %>% fwrite(
  sep = ",",
  here("OtherOutput/Tables", "Table3_Appendix.txt"),
  row.names = TRUE,
  quote = FALSE
)





# *************************************************************************
#### END OF SCRIPT ####
# *************************************************************************
