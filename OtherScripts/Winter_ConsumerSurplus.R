#### RELATE WP5: Replication code to summarise all models  ###############
# Script author: Peter King (p.m.king@kent.ac.uk)
# Last Edited: 30/01/2023
# Bit inelegant but it works


#----------------------------------------------------------------------------------------------------------
#### Section 0: Setup and estimate models ####
## NOTE: Data is stored in PK R WORK/Strictly Replication Files Only/Winter Replication/CEModelScripts/
## NOTE: Results are stored in PK R WORK/Strictly Replication Files Only/Winter Replication/CEModelData/
#----------------------------------------------------------------------------------------------------------

library(data.table)
library(tidyverse)

#----------------------------------------------------------------------------------------------------------
#### Section 1: Importing Model,  Estimates,  and WTP for FULL SAMPLE models ####
#----------------------------------------------------------------------------------------------------------


## Estimates model in Preference-Space to allow calculation of consumer surplus:
Model <- readRDS(here("CEoutput/ModelOne","Winter_MXL_ModelOne_PrefSpace.rds"))


#----------------------------------------------------------------------------------------------------------
#### Section 2: Consumer Surplus ####
#----------------------------------------------------------------------------------------------------------


## Imports WTP:
WTP <-data.frame(fread(here("CEoutput/ModelOne","Winter_MXL_ModelOne_PrefSpace_ConWTP.csv")))
WTP %<>% select(ends_with("post.mean"))


## Parameters to calculate CS:
MedianTax <- -exp(Model$estimate["mu_Tax"])

## Utility for changing from SQ to medium levels:
V_SQToMedium <- -Model$estimate["asc_C"]+  Model$estimate["mu_Sound"]+Model$estimate["mu_Smell"]+Model$estimate["mu_Colour"]+Model$estimate["mu_Deadwood"]

## Utility for changing from SQ to high levels:
V_SQToHigh <- -Model$estimate["asc_C"] +  Model$estimate["mu_Sound2"]+Model$estimate["mu_Smell2"]+Model$estimate["mu_Colour2"]+Model$estimate["mu_Deadwood2"]

## Utility for SQ
V_SQ <- 0


## Using logsum approach from: https://link.springer.com/content/pdf/10.1007%2F978-3-030-62669-3.pdf
CS_SQToMedium <- -log(V_SQToMedium - V_SQ)/MedianTax
CS_SQToHigh <- -log(V_SQToHigh - V_SQ)/MedianTax

## Values but using individual-WTP:
CS_SQToMedium_Individual <- (-log(abs(-Model$estimate["asc_C"]+  WTP$b_Smell.post.mean+  WTP$b_Sound.post.mean+  WTP$b_Colour.post.mean+  WTP$b_Deadwood.post.mean))/MedianTax)

CS_SQToHigh_Individual <- (-log(abs(-Model$estimate["asc_C"]+  WTP$b_Smell2.post.mean+  WTP$b_Sound2.post.mean+  WTP$b_Colour2.post.mean+  WTP$b_Deadwood2.post.mean))/MedianTax)


#----------------------------------------------------------------------------------------------------------
#### END OF SCRIPT ####
## Next: SpatialPlots.R,  Spatial
#----------------------------------------------------------------------------------------------------------
