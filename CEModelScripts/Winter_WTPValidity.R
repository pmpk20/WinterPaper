#### RELATE WP5: Replication code to perform validity checks on WTP  ###############
# Script author: Peter King (p.m.king@kent.ac.uk)
# Last Edited: 30/01/2023
# Based on Sarrias (2020) https://doi.org/10.1016/j.jocm.2020.100224



#----------------------------------------------------------------------------------------------------------
#### Section 0: Importing Model,  Estimates,  and WTP for FULL SAMPLE models ####
#----------------------------------------------------------------------------------------------------------



ModelOne_Model <- readRDS(here("CEoutput/ModelOne","Winter_MXL_ModelOne.rds"))
ModelOne_WTP <- here("CEoutput/ModelOne","Winter_MXL_ModelOne_ConWTP.csv") %>% fread() %>% data.frame()
ModelOne_Estimates <- here("CEoutput/ModelOne","Winter_MXL_ModelOne_estimates.csv") %>% fread() %>% data.frame()


## Model with covariates
ModelTwo_Model <- readRDS(here("CEoutput/ModelTwo","Winter_MXL_ModelTwo.rds"))
ModelTwo_WTP <- here("CEoutput/ModelTwo","Winter_MXL_ModelTwo_ConWTP.csv") %>% fread() %>% data.frame()
ModelTwo_Estimates <- here("CEoutput/ModelTwo","Winter_MXL_ModelTwo_estimates.csv") %>% fread() %>% data.frame()




#----------------------------------------------------------------------------------------------------------
#### Section 1: Check Conditional Means ####
## Conditional Mean should be within 90% of Model mean
#----------------------------------------------------------------------------------------------------------



# Medium Level Estimates: --------------------------------------------------------

MediumMeans <- cbind(
  ## Check Tax Point:
  ifelse(
    mean(ModelOne_WTP$beta_Tax.post.mean) >=
      ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="mu_Tax"]/100*90,
    "Pass",
    "Fail"
  ),

  ## Check Colour Point:
  ifelse(
    mean(ModelOne_WTP$b_Colour.post.mean) >=
      ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="mu_Colour"]/100*90,
    "Pass",
    "Fail"
  ),


  ## Check Sound Point:
  ifelse(
    mean(ModelOne_WTP$b_Sound.post.mean) >=
      ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="mu_Sound"]/100*90,
    "Pass",
    "Fail"
  ),


  ## Check Smell Point:
  ifelse(
    mean(ModelOne_WTP$b_Smell.post.mean) >=
      ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="mu_Smell"]/100*90,
    "Pass",
    "Fail"
  ),

  ## Check Deadwood Point:
  ifelse(
    mean(ModelOne_WTP$b_Deadwood.post.mean) >=
      ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="mu_Deadwood"]/100*90,
    "Pass",
    "Fail"
  ))



# High Level Estimates: --------------------------------------------------------

HighMeans <- cbind(
  ## Check Colour Point:
  ifelse(
    mean(ModelOne_WTP$b_Colour2.post.mean) >=
      ModelOne_Estimates$Estimate[ModelOne_Estimates$X == "mu_Colour"] /
      100 * 90,
    "Pass",
    "Fail"
  ),


  ## Check Sound Point:
  ifelse(
    mean(ModelOne_WTP$b_Sound2.post.mean) >=
      ModelOne_Estimates$Estimate[ModelOne_Estimates$X == "mu_Sound"] /
      100 * 90,
    "Pass",
    "Fail"
  ),


  ## Check Smell Point:
  ifelse(
    mean(ModelOne_WTP$b_Smell2.post.mean) >=
      ModelOne_Estimates$Estimate[ModelOne_Estimates$X == "mu_Smell"] /
      100 * 90,
    "Pass",
    "Fail"
  ),

  ## Check Deadwood Point:
  ifelse(
    mean(ModelOne_WTP$b_Deadwood2.post.mean) >=
      ModelOne_Estimates$Estimate[ModelOne_Estimates$X == "mu_Deadwood"] /
      100 * 90,
    "Pass",
    "Fail"
  )
)


# Summary: ----------------------------------------------------------------

Test_Means <- cbind(MediumMeans,
      HighMeans)



#----------------------------------------------------------------------------------------------------------
#### Section 1: Check Conditional Variance ####
## Conditional Variance should be within 60% of Model variance
#----------------------------------------------------------------------------------------------------------


# Medium Estimates: --------------------------------------------------------


MediumVariance <- cbind(
  ## Check Tax Point:
  ifelse(
    mean(ModelOne_WTP$beta_Tax.post.sd) >=
      abs(ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="sig_Tax"])/100*60,
    "Pass",
    "Fail"
  ),

  ## Check Colour Point:
  ifelse(
    mean(ModelOne_WTP$b_Colour.post.sd) >=
      abs(ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="sig_Colour"])/100*60,
    "Pass",
    "Fail"
  ),


  ## Check Sound Point:
  ifelse(
    mean(ModelOne_WTP$b_Sound.post.sd) >=
      abs(ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="sig_Sound"])/100*60,
    "Pass",
    "Fail"
  ),


  ## Check Smell Point:
  ifelse(
    mean(ModelOne_WTP$b_Smell.post.sd) >=
      abs(ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="sig_Smell"])/100*60,
    "Pass",
    "Fail"
  ),

  ## Check Deadwood Point:
  ifelse(
    mean(ModelOne_WTP$b_Deadwood.post.sd) >=
      abs(ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="sig_Deadwood"])/100*60,
    "Pass",
    "Fail"
  ))


# UHigh Estimates: --------------------------------------------------------

HighVariance <- cbind(
  ## Check Colour Point:
  ifelse(
    mean(ModelOne_WTP$b_Colour2.post.sd) >=
      abs(ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="sig_Colour"])/100*60,
    "Pass",
    "Fail"
  ),


  ## Check Sound Point:
  ifelse(
    mean(ModelOne_WTP$b_Sound2.post.sd) >=
      abs(ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="sig_Sound"])/100*60,
    "Pass",
    "Fail"
  ),


  ## Check Smell Point:
  ifelse(
    mean(ModelOne_WTP$b_Smell2.post.sd) >=
      abs(ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="sig_Smell"])/100*60,
    "Pass",
    "Fail"
  ),

  ## Check Deadwood Point:
  ifelse(
    mean(ModelOne_WTP$b_Deadwood2.post.sd) >=
      abs(ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="sig_Deadwood"])/100*60,
    "Pass",
    "Fail"
  ))


# Summary: ----------------------------------------------------------------

## Summarise above tests:
Test_Variance <- cbind(MediumVariance,
      HighVariance)
## Outcome: All Pass


#----------------------------------------------------------------------------------------------------------
#### Section 1: Check Conditional Distribution ####
## Should fail to reject Kolmogorov-Smirnov Test
#----------------------------------------------------------------------------------------------------------


## I make a function here to simplify reporting:
## Simply states reject or not based on PValue.
Report_PValue <- function(Test) {
  return(ifelse(
    Test$p.value < 0.05,
  "Reject",
  "Don't Reject"))
}


# Medium Model: --------------------------------------------------


## Check TaxWTP
TaxWTP_kstest <- ks.test(ModelOne_WTP$beta_Tax.post.mean,"pnorm",
                         ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="mu_Tax"],
                         abs(ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="sig_Tax"]))
Report_PValue(TaxWTP_kstest)


# Check ColourWTPNew
ColourWTP_kstest <- ks.test(ModelOne_WTP$b_Colour.post.mean,"pnorm",
                            ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="mu_Colour"],
                            abs(ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="sig_Colour"]))
Report_PValue(ColourWTP_kstest)



# Check SmellWTPNew
SmellWTP_kstest <- ks.test(ModelOne_WTP$b_Smell.post.mean,"pnorm",
                           ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="mu_Smell"],
                           abs(ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="sig_Smell"]))
Report_PValue(SmellWTP_kstest)


# Check SoundWTPNew
SoundWTP_kstest <- ks.test(ModelOne_WTP$b_Sound.post.mean,"pnorm",
                           ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="mu_Sound"],
                           abs(ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="sig_Sound"]))
Report_PValue(SoundWTP_kstest)


# Check DeadwoodWTPNew
DeadwoodWTP_kstest <- ks.test(ModelOne_WTP$b_Deadwood.post.mean,"pnorm",
                              ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="mu_Deadwood"],
                              abs(ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="sig_Deadwood"]))
Report_PValue(DeadwoodWTP_kstest)



# Summary: ----------------------------------------------------------------


Test_KS_1 <- cbind(
  Report_PValue(TaxWTP_kstest),
  Report_PValue(ColourWTP_kstest),
  Report_PValue(SmellWTP_kstest),
  Report_PValue(SoundWTP_kstest),
  Report_PValue(DeadwoodWTP_kstest)
)



# Check ColourWTP2
ColourWTP2_kstest <- ks.test(ModelOne_WTP$b_Colour2.post.mean,"pnorm",
                             ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="mu_Colour"],
                             abs(ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="sig_Colour"]))
Report_PValue(ColourWTP2_kstest)



# Check SmellWTP2
SmellWTP2_kstest <- ks.test(ModelOne_WTP$b_Smell2.post.mean,"pnorm",
                            ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="mu_Smell"],
                            abs(ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="sig_Smell"]))
Report_PValue(SmellWTP2_kstest)


# Check SoundWTP2
SoundWTP2_kstest <- ks.test(ModelOne_WTP$b_Sound2.post.mean,"pnorm",
                            ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="mu_Sound"],
                            abs(ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="sig_Sound"]))
Report_PValue(SoundWTP2_kstest)


# Check DeadwoodWTP2
DeadwoodWTP2_kstest <- ks.test(ModelOne_WTP$b_Deadwood2.post.mean,"pnorm",
                               ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="mu_Deadwood"],
                               abs(ModelOne_Estimates$Estimate[ModelOne_Estimates$X=="sig_Deadwood"]))
Report_PValue(DeadwoodWTP2_kstest)


# Summary: ----------------------------------------------------------------


Test_KS_2 <- cbind(
  Report_PValue(ColourWTP2_kstest),
  Report_PValue(SmellWTP2_kstest),
  Report_PValue(SoundWTP2_kstest),
  Report_PValue(DeadwoodWTP2_kstest)
)


#----------------------------------------------------------------
# All Outputs:
#----------------------------------------------------------------



rbind(Test_KS_1,Test_KS_1)
Test_Means
Test_Variance

#----------------------------------------------------------------
# Old Code
#----------------------------------------------------------------

#
# # Plotting distribution of WTP: ----------------------------------------------------------------
# plot(ecdf(rnorm(n = length(ModelTwo_WTP$b_Colour.post.mean),mean =  ModelTwo_Estimates$Estimate[ModelTwo_Estimates$X=="mu_Colour"],sd =  abs(ModelTwo_Estimates$Estimate[ModelTwo_Estimates$X=="sig_Colour"]))))
#
# plot(ecdf(
#   rnorm(
#     n = length(Winter$ColourWTPNew),
#     mean =  mean(Winter$ColourWTPNew),
#     sd =  sd(Winter$ColourWTPNew)
#   )
# ),add=TRUE)
#
#
# plot(ecdf(ModelTwo_WTP$b_Colour.post.mean),add=TRUE)
#
#
#
#
# # Test For Collinearity ---------------------------------------------------
#
#
# ## Step One: Read in data
# Winter <- data.frame(read.csv("Z:/WinterAnalysis1307/WP5/WinterReplication/OtherData/Winter_dataframe_2022-01-07.csv"))
# database <- data.frame(read.csv("Z:/WinterAnalysis1307/WP5/WinterReplication/OtherData/database_Winter_2021-10-27.csv"))
#
# ## Step Two: Estimate linear model with same specification as MXL
# Winter$Impairment <- ifelse((Winter$SightIssues==1)|
#                               (Winter$SmellIssues==1)|
#                               (Winter$HearingIssues==1),1,0)
#
# ### Specify covariates:
# Covariates_Colour <- formula(ColourWTP~Gender + DummyAge +
# MilesDistance + Charity +
# IncomeDummy + Impairment +
# Country + EthnicityDummyWhite +
# Urbanicity + MostRecentVisit +
# TeenVisitWoodlands + WoodlandsScore)
#
# Covariates2_Colour <- formula(ColourWTP~Gender + DummyAge +
#                                MilesDistance + Charity +
#                                IncomeDummy + Impairment +
#                                Country + EthnicityDummyWhite +
#                                Urbanicity + MostRecentVisit +
#                                WoodlandsScore)
#
# Variables <- cbind(
#   "Gender"=Winter$Gender ,
#   "DummyAge"=Winter$DummyAge ,
#   "MilesDistance"=Winter$MilesDistance ,
#   "Charity"=Winter$Charity ,
#   "IncomeDummy"=Winter$IncomeDummy ,
#   "Impairment"=Winter$Impairment ,
#   "Country"=Winter$Country ,
#   "EthnicityDummyWhite"=Winter$EthnicityDummyWhite ,
#   "Urbanicity"=Winter$Urbanicity ,
#   "MostRecentVisit"=Winter$MostRecentVisit ,
#   "TeenVisitWoodlands"=Winter$TeenVisitWoodlands ,
#   "WoodlandsScore"=Winter$WoodlandsScore
# )
#
# ### Estimate Model:
# LMModel_Colour <- lm(formula = Covariates_Colour,data = Winter)
# LMModel2_Colour <- lm(formula = Covariates2_Colour,data = Winter)
# ols_step_best_subset(LMModel_Colour)



# End Of Script ------------------------------------
