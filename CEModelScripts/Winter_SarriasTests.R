#### RELATE WP5: Replication code to perform validity checks on WTP  ###############
# Script author: Peter King (p.m.king@kent.ac.uk)
# Last Edited: 03/02/2023
# Based on Sarrias (2020) https://doi.org/10.1016/j.jocm.2020.100224


## sessionInfo() ---------------------------------------------------------------
# > sessionInfo()
# R version 4.2.0 (2022-04-22 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19044)
#
# Matrix products: default
#
# locale:
#   [1] LC_COLLATE=English_United Kingdom.utf8  LC_CTYPE=English_United Kingdom.utf8    LC_MONETARY=English_United Kingdom.utf8
# [4] LC_NUMERIC=C                            LC_TIME=English_United Kingdom.utf8
#
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:
#   [1] stringr_1.5.0     data.table_1.14.6 mded_0.1-2        reshape2_1.4.4    ggridges_0.5.4    ggplot2_3.4.0     magrittr_2.0.3
# [8] dplyr_1.0.10      apollo_0.2.8      here_1.0.1
#
# loaded via a namespace (and not attached):
#   [1] tidyselect_1.2.0    zoo_1.8-11          xfun_0.36           splines_4.2.0       lattice_0.20-45     colorspace_2.0-3    generics_0.1.3
# [8] vctrs_0.5.1         htmltools_0.5.4     yaml_2.3.6          MCMCpack_1.6-3      utf8_1.2.2          survival_3.3-1      rlang_1.0.6
# [15] pillar_1.8.1        withr_2.5.0         DBI_1.1.3           glue_1.6.2          plyr_1.8.8          matrixStats_0.63.0  lifecycle_1.0.3
# [22] MatrixModels_0.5-1  munsell_0.5.0       gtable_0.3.1        mvtnorm_1.1-3       coda_0.19-4         evaluate_0.20       knitr_1.41
# [29] miscTools_0.6-26    fastmap_1.1.0       SparseM_1.81        RSGHB_1.2.2         quantreg_5.94       parallel_4.2.0      fansi_1.0.3
# [36] Rcpp_1.0.9          scales_1.2.1        mcmc_0.9-7          maxLik_1.5-2        mnormt_2.1.1        digest_0.6.31       stringi_1.7.12
# [43] numDeriv_2016.8-1.1 grid_4.2.0          rprojroot_2.0.3     cli_3.6.0           tools_4.2.0         sandwich_3.0-2      tibble_3.1.8
# [50] pkgconfig_2.0.3     MASS_7.3-56         Matrix_1.5-3        randtoolbox_2.0.3   assertthat_0.2.1    rmarkdown_2.20      rstudioapi_0.14
# [57] R6_2.5.1            rngWELL_0.10-9      compiler_4.2.0



## Libraries: ---------------------------------------------------------------
library(apollo)
library(dplyr)
library(magrittr)
library(reshape2)
library(mded)
library(here)
library(data.table)
library(stringr)
library(Rfast)


# *********************************************************************************************************
#### Section 0: Importing Model,  Estimates,  and WTP for FULL SAMPLE models ####
# *********************************************************************************************************


## Basic model, no covariates, in WTP-space.
# ModelOne_WTP <- here("CEoutput/ModelOne","Winter_MXL_ModelOne_ConWTP.csv") %>% fread() %>% data.frame()
# ModelOne_Estimates <- here("CEoutput/ModelOne","Winter_MXL_ModelOne_estimates.csv") %>% fread() %>% data.frame()
# ModelOne_UC <- here("CEoutput/ModelOne","Winter_MXL_ModelOne_UnconWTP.csv") %>% fread() %>% data.frame()


## Model with covariates
ModelTwo_WTP <- here("CEoutput/ModelTwo","Winter_MXL_ModelTwo_ConWTP.csv") %>% fread() %>% data.frame()
ModelTwo_Estimates <- here("CEoutput/ModelTwo","Winter_MXL_ModelTwo_estimates.csv") %>% fread() %>% data.frame()
ModelTwo_UC <- here("CEoutput/ModelTwo","Winter_MXL_ModelTwo_UnconWTP.csv") %>% fread() %>% data.frame()



# *********************************************************************************************************
#### Section 1: Define summary functions ####
# *********************************************************************************************************


## For the means we test whether the mean conditional WTP is more than 90% of the
### parameter estimate from the model.
## Example: SarriasTestMeans(ModelOne_WTP, ModelOne_Estimates,"beta_Tax")
SarriasTestMeans <- function(WTP, Estimates,Variable) {

  A <- WTP %>%
    select(paste0("b_",Variable,".post.mean")) %>%
    summarise(across(everything(),list(mean))) %>%
    as.numeric()
  B <- Estimates %>%
    filter(V1==paste0("b_",Variable)) %>%
    select("Estimate") %>%
    abs() %>%
    divide_by(100) %>%
    multiply_by(90) %>%
    as.numeric()

  ifelse(
    A >= B ,
    paste0("Pass: Mean of conditional WTP (",
           A %>% round(3), ") >= 90% of mu parameter (", B %>% round(3), ")"),
    paste0("Fail: Mean of conditional WTP (",
           A %>% round(3), ") < 90% of mu parameter (", B %>% round(3), ")")
  ) %>%
    c()

}


## For the variances  we test whether the variance of the
### conditional WTP is more than 60% of the
### parameter estimate from the model.
## Example: SarriasTestVariances(ModelOne_WTP, ModelOne_Estimates,"beta_Tax")
SarriasTestVariances <- function(WTP, Estimates,Variable) {

  A <- WTP %>% select(paste0("b_",Variable,".post.sd"))  %>%
    summarise(across(everything(),list(mean))) %>%
    as.numeric()


  B <- Estimates %>%
    filter(V1==paste0("sig_",Variable)) %>%
    select("Estimate") %>%
    divide_by(100) %>%
    multiply_by(60) %>%
    as.numeric()

  ifelse(
    A >= B ,
    paste0("Pass: Variance of conditional WTP (",
           A %>% round(3), ") >= 60% of sigma parameter (", B %>% round(3), ")"),
    paste0("Fail: Variance of conditional WTP (",
           A %>% round(3), ") < 60% of sigma parameter (", B %>% round(3), ")")
  ) %>%
    c()

}



## For the distribution we use the Kolmogorov-Smirnov test:
## Example:  SarriasTestDistributions(ModelOne_WTP, ModelOne_Estimates,"b_Colour")
## Using ks.test() with four arguments: the distribution of WTP by attribute,
# "Pnorm", Variable specific estimate and variable specific variance
SarriasTestDistributions <- function(Conditionals, Unconditionals, Variable) {

  Q <- Conditionals %>% nrow()

  ## SO firstly simulate a distribution of WTP with the mean and SD of the conditionals:
  X_Dist <- pnorm(
    q = Q,
    mean = Conditionals %>%
      select(paste0("b_", Variable, ".post.mean")) %>% unlist() %>% as.numeric(),
    sd = Conditionals %>%
      select(paste0("b_", Variable, ".post.sd")) %>% unlist() %>% as.numeric()
  )

  ## Then simulate a distribution of WTP using moments from the UNconditionals
  Y_Mean <-
    Unconditionals %>% select(starts_with(paste0("b_", Variable, "."))) %>%
    summarise_all(mean) %>% as.matrix() %>% rowmeans()

  Y_SD <-
    Unconditionals %>% select(starts_with(paste0("b_", Variable, "."))) %>%
    summarise_all(sd) %>% as.matrix() %>% rowmeans()

  Y_Dist <- pnorm(q = Q, mean = Y_Mean, sd = Y_SD)


  ## NOW use the KS test to
  # " test whether the distribution of the conditional means equals the estimated unconditional distribution"
  Test2 <- ks.test(
    X_Dist, Y_Dist)


  ## Output Result with some text:
  # Sarris (2020) notes that:
  # "Failure to reject the null hypothesis is a conservative indicator
  # that the distribution selected for the parameter is correct"
  ifelse(
    Test2$p.value > 0.05,
    paste0("Pass (KS test stat = ", Test2$statistic %>% round(3), ", P = ", Test2$p.value %>% round(3),")"),
    paste0("Fail (KS test stat = ", Test2$statistic %>% round(3), ", P = ", Test2$p.value %>% round(3),")")) %>% c()


}


# *********************************************************************************************************
#### Section 1B: Initialising variables ####
# *********************************************************************************************************


## Changing variable names to make the function easier to write
## Also using ModelTwo as it's the WTP-space with covariates one
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


# *********************************************************************************************************
#### Section 2: Use a loop to do all the tests ####
## Conditional Mean should be within 90% of Model mean
## Conditional variance should be within 60% of Model variance
## Distribution test must not fail KS test
# *********************************************************************************************************


## Loop through each attribute and output both test
### If you can work out how to put custom functions into foreach() this would be even faster
for (i in Index){
  MeanTests[match(i,Index),] <- SarriasTestMeans(ModelTwo_WTP, ModelTwo_Estimates,Index[match(i,Index)])
  VarTests[match(i,Index),] <- SarriasTestVariances(ModelTwo_WTP, ModelTwo_Estimates,Index[match(i,Index)])
  DistTests[match(i,Index),] <- SarriasTestDistributions(ModelTwo_WTP, ModelTwo_UC,Index[match(i,Index)])
}


# *********************************************************************************************************
#### Section 4: Output tables ####
# *********************************************************************************************************



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




# *********************************************************************************************************
#### END OF SCRIPT / OLD CODE ####
# *********************************************************************************************************


#
# SarriasTestDistributions <- function(WTP, Estimates,Variable) {
#
#   ifelse(ks.test(
#     WTP %>% select(paste0("b_",Variable,".post.mean")),
#     "pnorm",
#
#     Estimates %>%
#       filter(V1==paste0("b_",Variable)) %>%
#       select("Estimate") %>% as.numeric(),
#
#     Estimates %>%
#       filter(V1==paste0("sig_",Variable)) %>%
#       select("Estimate")  %>% as.numeric() %>% abs()
#     )$p.value < 0.05,
#     "Pass",
#     "Fail") %>% c()
#
## OLD CODE FOR REFERENCE

# Y_Mean <- Estimates %>%
#   filter(V1==paste0("b_",Variable)) %>%
#   select("Estimate") %>% as.numeric()
# #
# Y_SD <- Estimates %>%
#   filter(V1==paste0("sig_",Variable)) %>%
#   select("Estimate")  %>% as.numeric() %>% abs()

# Test1 <- ks.test(
#   X,
#   "pnorm",
#   Mu, Sigma)
#
# ifelse(
#   Test1$p.value > 0.05,
#   paste0("Pass: ", Test1$statistic %>% round(3), " (P = ", Test1$p.value %>% round(3),")"),
#   paste0("Fail: ", Test1$statistic %>% round(3), " (P = ", Test1$p.value %>% round(3),")")) %>% c()
#

## Second approach

# Test2 <- ks.test(
#   X,"pnorm", Y_Mean, Y_SD)
# }



#
# SarriasTestDistributions <- function(WTP, Estimates,Variable) {
#
#   ifelse(ks.test(
#     WTP %>% select(paste0("b_",Variable,".post.mean")),
#     "pnorm",
#
#     Estimates %>%
#       filter(V1==paste0("b_",Variable)) %>%
#       select("Estimate") %>% as.numeric(),
#
#     Estimates %>%
#       filter(V1==paste0("sig_",Variable)) %>%
#       select("Estimate")  %>% as.numeric() %>% abs()
#     )$p.value < 0.05,
#     "Pass",
#     "Fail") %>% c()
#
## OLD CODE FOR REFERENCE

# Y_Mean <- Estimates %>%
#   filter(V1==paste0("b_",Variable)) %>%
#   select("Estimate") %>% as.numeric()
# #
# Y_SD <- Estimates %>%
#   filter(V1==paste0("sig_",Variable)) %>%
#   select("Estimate")  %>% as.numeric() %>% abs()

# Test1 <- ks.test(
#   X,
#   "pnorm",
#   Mu, Sigma)
#
# ifelse(
#   Test1$p.value > 0.05,
#   paste0("Pass: ", Test1$statistic %>% round(3), " (P = ", Test1$p.value %>% round(3),")"),
#   paste0("Fail: ", Test1$statistic %>% round(3), " (P = ", Test1$p.value %>% round(3),")")) %>% c()
#

## Second approach

# Test2 <- ks.test(
#   X,"pnorm", Y_Mean, Y_SD)
# }


# SarriasTestMeans <- function(WTP, Estimates,Variable) {
#
#   ifelse(
#     WTP %>%
#       select(paste0("b_",Variable,".post.mean")) %>%
#       summarise(across(everything(),list(mean))) %>%
#       as.numeric() >= Estimates %>%
#       filter(V1==paste0("b_",Variable)) %>%
#       select("Estimate") %>%
#       abs() %>%
#       divide_by(100) %>%
#       multiply_by(90) %>%
#       as.numeric(),"Pass","Fail") %>% c()
#
# }


# SarriasTestVariances <- function(WTP, Estimates,Variable) {
#
#   ifelse(
#     WTP %>% select(paste0("b_",Variable,".post.sd"))  %>%
#       summarise(across(everything(),list(mean))) %>%
#       as.numeric() >=
#       Estimates %>%
#       filter(V1==paste0("sig_",Variable)) %>%
#       select("Estimate") %>%
#       divide_by(100) %>%
#       multiply_by(60) %>%
#       as.numeric(),"Pass","Fail") %>% c()
#
# }


