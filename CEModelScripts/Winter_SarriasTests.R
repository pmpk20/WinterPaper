#### RELATE WP5: Replication code to perform validity checks on WTP  ###############
# Script author: Peter King (p.m.king@kent.ac.uk)
# Last Edited: 30/01/2023
# Based on Sarrias (2020) https://doi.org/10.1016/j.jocm.2020.100224


library(apollo)
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggridges)
library(reshape2)
library(mded)
library(here)
library(data.table)
library(stringr)


#----------------------------------------------------------------------------------------------------------
#### Section 0: Importing Model,  Estimates,  and WTP for FULL SAMPLE models ####
#----------------------------------------------------------------------------------------------------------



ModelOne_WTP <- here("CEoutput/ModelOne","Winter_MXL_ModelOne_ConWTP.csv") %>% fread() %>% data.frame()
ModelOne_Estimates <- here("CEoutput/ModelOne","Winter_MXL_ModelOne_estimates.csv") %>% fread() %>% data.frame()


## Model with covariates
ModelTwo_WTP <- here("CEoutput/ModelTwo","Winter_MXL_ModelTwo_ConWTP.csv") %>% fread() %>% data.frame()
ModelTwo_Estimates <- here("CEoutput/ModelTwo","Winter_MXL_ModelTwo_estimates.csv") %>% fread() %>% data.frame()



#----------------------------------------------------------------------------------------------------------
#### Section 1: Define summary functions ####
#----------------------------------------------------------------------------------------------------------


## For the means we test whether the mean conditional WTP is more than 90% of the
### parameter estimate from the model
SarriasTestMeans <- function(WTP, Estimates,Variable) {

  ifelse(
    WTP %>%
      select(paste0("b_",Variable,".post.mean")) %>%
      summarise(across(everything(),list(mean))) %>%
      as.numeric() >= Estimates %>%
      filter(V1==paste0("b_",Variable)) %>%
      select("Estimate") %>%
      abs() %>%
      divide_by(100) %>%
      multiply_by(90) %>%
      as.numeric(),"Pass","Fail") %>% c()

}



## For the variances  we test whether the variance of the
### conditional WTP is more than 60% of the
### parameter estimate from the model
SarriasTestVariances <- function(WTP, Estimates,Variable) {

  ifelse(
    WTP %>% select(paste0("b_",Variable,".post.sd"))  %>%
      summarise(across(everything(),list(mean))) %>%
      as.numeric() >= Estimates %>%
      filter(V1==paste0("sig_",Variable)) %>%
      select("Estimate") %>%
      divide_by(100) %>%
      multiply_by(60) %>%
      as.numeric(),"Pass","Fail") %>% c()

}


## For the distribution we use the Kolmogorov-Smirnov test:
SarriasTestDistributions <- function(WTP, Estimates,Variable) {

  ifelse(ks.test(
    WTP %>% select(paste0("b_",Variable,".post.mean")),
    "pnorm",

    Estimates %>%
      filter(V1==paste0("b_",Variable)) %>%
      select("Estimate") %>% as.numeric(),

    Estimates %>%
      filter(V1==paste0("sig_",Variable)) %>%
      select("Estimate")  %>% as.numeric() %>% abs()
    )$p.value < 0.05,
    "Pass",
    "Fail") %>% c()

}


#----------------------------------------------------------------------------------------------------------
#### Section 1B: Initialising variables ####
#----------------------------------------------------------------------------------------------------------


## Changing variable names to make the function easier to write
ModelOne_Estimates$V1 <- ModelOne_Estimates$V1 %>%
  c() %>%
  str_replace_all(c(mu_="b_",
                    b_Tax="beta_Tax"))

## Changing variable names to make the function easier to write
ModelTwo_Estimates$V1 <- ModelTwo_Estimates$V1 %>%
  c() %>%
  str_replace_all(c(mu_="b_",
                    b_Tax="beta_Tax"))


## Variables to loop through:
Index <- c("Colour","Colour2",
           "Smell","Smell2",
           "Sound","Sound2",
           "Deadwood","Deadwood2")


## Data that each loop fills in
MeanTests <- matrix(0,8,1) %>% data.frame()
VarTests <- matrix(0,8,1) %>% data.frame()
DistTests <- matrix(0,8,1) %>% data.frame()


#----------------------------------------------------------------------------------------------------------
#### Section 2: Use a loop to do all the tests ####
## Conditional Mean should be within 90% of Model mean
## Conditional variance should be within 60% of Model variance
## Distribution test must not fail KS test
#----------------------------------------------------------------------------------------------------------


## Loop through each attribute and output both test
### If you can work out how to put custom functions into foreach() this would be even faster
for (i in Index){
  MeanTests[match(i,Index),] <- SarriasTestMeans(ModelOne_WTP, ModelOne_Estimates,Index[match(i,Index)])
  VarTests[match(i,Index),] <- SarriasTestVariances(ModelOne_WTP, ModelOne_Estimates,Index[match(i,Index)])
  DistTests[match(i,Index),] <- SarriasTestDistributions(ModelOne_WTP, ModelOne_Estimates,Index[match(i,Index)])
}


#----------------------------------------------------------------------------------------------------------
#### Section 4: Output tables ####
#----------------------------------------------------------------------------------------------------------



## Compile all results here:
SarriasTests <- bind_cols("Variables"=Index,
                         "Means"=MeanTests$.,
                         "Variances"=VarTests$.,
                         "Distributions"=DistTests$.)



## Output to screen in a nice format for making tables in word
SarriasTests %>% write.csv(quote=F)
## Output to a discrete file if that's helpful
write.table(SarriasTests,
            here("CEoutput","SarriasTests.txt"),
            sep=",",quote=F)




#----------------------------------------------------------------------------------------------------------
#### END OF SCRIPT ####
#----------------------------------------------------------------------------------------------------------
