#### RELATE WP5: Replication code to perform validity checks on WTP  ###############
# Script author: Peter King (p.king1@leeds.ac.uk)
# Last Edited: 31/05/2024
# Based on Sarrias (2020) https://doi.org/10.1016/j.jocm.2020.100224
# Changes:
# - changing to correlated conditional WTP
# - round(3) to format(digits = 4, nsmall = 3)
# - Changing threshold to defined numbers



## sessionInfo() ---------------------------------------------------------------
# R version 4.4.1 (2024-06-14 ucrt)
# Platform: x86_64-w64-mingw32/x64
# Running under: Windows 11 x64 (build 22631)
# Matrix products: default
# locale:
#   [1] C
# system code page: 65001
# time zone: Europe/London
# tzcode source: internal
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base
# other attached packages:
#   [1] ggdist_3.3.2       stringi_1.8.4      sf_1.0-16
# [4] udunits2_0.13.2.1  PostcodesioR_0.3.1 geosphere_1.5-18
# [7] psych_2.4.3        readxl_1.4.3       coin_1.4-3
# [10] survival_3.7-0     Rfast_2.1.0        RcppParallel_5.1.7
# [13] RcppZiggurat_0.1.6 Rcpp_1.0.12        lubridate_1.9.3
# [16] forcats_1.0.0      stringr_1.5.1      purrr_1.0.2
# [19] readr_2.1.5        tidyr_1.3.1        tibble_3.2.1
# [22] tidyverse_2.0.0    RColorBrewer_1.1-3 data.table_1.15.4
# [25] here_1.0.1         mded_0.1-2         reshape2_1.4.4
# [28] ggridges_0.5.6     ggplot2_3.5.1      magrittr_2.0.3
# [31] dplyr_1.1.4        apollo_0.3.3
# loaded via a namespace (and not attached):
#   [1] DBI_1.2.3            mnormt_2.1.1         sandwich_3.1-0
# [4] rlang_1.1.4          multcomp_1.4-25      e1071_1.7-14
# [7] matrixStats_1.3.0    compiler_4.4.1       systemfonts_1.1.0
# [10] vctrs_0.6.5          quantreg_5.98        pkgconfig_2.0.3
# [13] backports_1.5.0      mcmc_0.9-8           utf8_1.2.4
# [16] tzdb_0.4.0           miscTools_0.6-28     ragg_1.3.2
# [19] MatrixModels_0.5-3   modeltools_0.2-23    Deriv_4.1.3
# [22] broom_1.0.6          parallel_4.4.1       R6_2.5.1
# [25] Rsolnp_1.16          car_3.1-2            cellranger_1.1.0
# [28] numDeriv_2016.8-1.1  zoo_1.8-12           rngWELL_0.10-9
# [31] Matrix_1.7-0         splines_4.4.1        timechange_0.3.0
# [34] tidyselect_1.2.1     rstudioapi_0.16.0    abind_1.4-5
# [37] maxLik_1.5-2.1       codetools_0.2-20     lattice_0.22-6
# [40] plyr_1.8.9           withr_3.0.0          coda_0.19-4.1
# [43] RSGHB_1.2.2          units_0.8-5          proxy_0.4-27
# [46] pillar_1.9.0         carData_3.0-5        KernSmooth_2.23-24
# [49] stats4_4.4.1         distributional_0.4.0 generics_0.1.3
# [52] rprojroot_2.0.4      sp_2.1-4             truncnorm_1.0-9
# [55] hms_1.1.3            munsell_0.5.1        scales_1.3.0
# [58] randtoolbox_2.0.4    class_7.3-22         glue_1.7.0
# [61] tools_4.4.1          SparseM_1.83         mvtnorm_1.2-5
# [64] grid_4.4.1           MCMCpack_1.7-0       libcoin_1.0-10
# [67] colorspace_2.1-0     nlme_3.1-165         cli_3.6.3
# [70] textshaping_0.4.0    fansi_1.0.6          gtable_0.3.5
# [73] rstatix_0.7.2        digest_0.6.35        classInt_0.4-10
# [76] bgw_0.1.3            TH.data_1.1-2        farver_2.1.2
# [79] lifecycle_1.0.4      httr_1.4.7           MASS_7.3-61


## Libraries: ---------------------------------------------------------------
library(apollo)
library(dplyr)
library(magrittr)
library(stringr)
library(here)
library(data.table)
library(Rfast)


# *********************************************************************************************************
#### Section 0: Importing Model,  Estimates,  and WTP for FULL SAMPLE models ####
# *********************************************************************************************************


## Model with correlations
ModelTwo_WTP <-
  here("CEoutput/ModelTwo",
       "Winter_MXL_ModelTwo_InText_WithASCRandom_ConWTP.csv") %>% fread() %>% data.frame()
ModelTwo_Estimates <-
  here("CEoutput/ModelTwo",
       "Winter_MXL_ModelTwo_InText_WithASCRandom_estimates.csv") %>% fread() %>% data.frame()
ModelTwo_UC <-
  here("CEoutput/ModelTwo",
       "Winter_MXL_ModelTwo_InText_WithASCRandom_UnconWTP.csv") %>% fread() %>% data.frame()


## Basic model, no covariates, in WTP-space.
# ModelTwo_WTP <- here("CEoutput/ModelOne","Winter_MXL_ModelOne_ConWTP.csv") %>% fread() %>% data.frame()
# ModelTwo_Estimates <- here("CEoutput/ModelOne","Winter_MXL_ModelOne_estimates.csv") %>% fread() %>% data.frame()
# ModelTwo_UC <- here("CEoutput/ModelOne","Winter_MXL_ModelOne_UnconWTP.csv") %>% fread() %>% data.frame()


## Model with covariates no correlations
# ModelTwo_WTP <- here("CEoutput/ModelTwo","Winter_MXL_ModelTwo_ConWTP.csv") %>% fread() %>% data.frame()
# ModelTwo_Estimates <- here("CEoutput/ModelTwo","Winter_MXL_ModelTwo_estimates.csv") %>% fread() %>% data.frame()
# ModelTwo_UC <- here("CEoutput/ModelTwo","Winter_MXL_ModelTwo_UnconWTP.csv") %>% fread() %>% data.frame()



# *********************************************************************************************************
#### Section 1: Define summary functions ####
# *********************************************************************************************************



# *********************************************************************************************************
#### Means:

## Define threshold here
Threshold_MeanTest <- 0.9

## For the means we test whether the mean conditional WTP is more than 90% of the
### parameter estimate from the model.
## Example: SarriasTestMeans(ModelOne_WTP, ModelOne_Estimates,"beta_Tax")
SarriasTestMeans <- function(WTP, Estimates,Variable) {


  ## Recover mean WTP
  A <- paste0("b_", Variable, ".post.mean") %>% ## Specify which variable
    WTP[[.]] %>% ## select columns
    mean()


  ## Recover MU value
  B_1 <- Estimates[Estimates$V1 ==
                     paste0("b_", Variable),
              "Estimate"]


  ## Scale for testing
  B_2 <- B_1 %>%
    multiply_by(Threshold_MeanTest)


## Test criteria and output
  ifelse(
    ## Test
    abs(A) >= abs(B_2) ,


    ## if TRUE
    paste0(
      "Pass: Mean of conditional WTP (",
      A %>% format(digits = 4, nsmall = 3),
      ") >= 90% of μ parameter (",
      B_1 %>% format(digits = 4, nsmall = 3),
      ")"
    ),


    ## if FALSE
    paste0(
      "Fail: Mean of conditional WTP (",
      A %>% format(digits = 4, nsmall = 3),
      ") < 90% of μ parameter (",
      B_1 %>% format(digits = 4, nsmall = 3),
      ")"
    )
  )

}


# *********************************************************************************************************
#### Variances:


## Define threshold here
Threshold_VarianceTest <- 0.6


## For the variances  we test whether the variance of the
### conditional WTP is more than 60% of the
### parameter estimate from the model.
## Example: SarriasTestVariances(ModelOne_WTP, ModelOne_Estimates,"beta_Tax")
SarriasTestVariances <- function(WTP, Estimates,Variable) {

  ## Recover WTP std dev
  A <- paste0("b_", Variable, ".post.sd") %>% ## Specify which variable
    WTP[[.]] %>% ## select columns
    mean()


  ## Recover sigma
  B_1 <- Estimates[Estimates$V1 ==
                     paste0("sig_", Variable),
                   "Estimate"]

  ## Scale to 60%
  B_2 <- B_1 %>%
    multiply_by(Threshold_VarianceTest)


  ## Test criteria and output
  ifelse(
    ## Test
    abs(A) >= abs(B_2) ,


    ## if TRUE
    paste0(
      "Pass: Variance of conditional WTP (",
      A %>% format(digits = 4, nsmall = 3),
      ") >= 60% of σ parameter (",
      B_1 %>% format(digits = 4, nsmall = 3),
      ")"
    ),


    ## if FALSE
    paste0(
      "Fail: Variance of conditional WTP (",
      A %>% format(digits = 4, nsmall = 3),
      ") < 60% of σ parameter (",
      B_1 %>% format(digits = 4, nsmall = 3),
      ")"
    )
  )

}


# *********************************************************************************************************
#### Distributions:


## For the distribution we use the Kolmogorov-Smirnov test:
## Example:  SarriasTestDistributions(ModelOne_WTP, ModelOne_Estimates,"b_Colour")
SarriasTestDistributions <- function(Conditionals, Unconditionals, Variable) {


  ## Conditionals
  X_Dist <- paste0("b_", Variable, ".post.mean") %>% ## Specify which variable
    Conditionals[[.]] %>% ## select columns
    mean()



  ## Unconditionals
  Y_Dist <- Unconditionals %>%
    dplyr::select(starts_with(paste0("b_",
                                              Variable,
                                              "."))) %>%
                    as.matrix() %>%
                    Rfast::rowmeans()

  ## KS.test here. Same with stats:: or dgof::
  Test2 <- ks.test(
    X_Dist,
    Y_Dist)


  ## Output Result with some text:
  # Sarris (2020) notes that:
  # "Failure to reject the null hypothesis is a conservative indicator
  # that the distribution selected for the parameter is correct"
  ifelse(
    ## Test (NOTE: 1% significance level)
    Test2$p.value > 0.01,

    ## if TRUE
    paste0("Pass: (KS test stat = ",
           Test2$statistic %>% format(digits = 3, nsmall = 3),
           ", P = ",
           Test2$p.value %>% format(digits = 3, nsmall = 3),")"),


    ## if FALSE
    paste0("Fail: (KS test stat = ",
           Test2$statistic %>% format(digits = 3, nsmall = 3),
           ", P = ",
           Test2$p.value %>% format(digits = 3, nsmall = 3),
           ")"))


}


# *********************************************************************************************************
#### Section 1B: Initialising variables ####
# *********************************************************************************************************


## Changing variable names to make the function easier to write
## Also using ModelTwo as it's the WTP-space with covariates one
ModelTwo_Estimates$V1 <- ModelTwo_Estimates$V1 %>%
  c() %>%
  str_replace_all(c(mu_ = "b_",
                    b_Tax = "beta_Tax"))


## Variables to loop through:
Index <- c("Colour2",
           "Colour",
           "Smell2",
           "Smell",
           "Sound2",
           "Sound",
           "Deadwood2",
           "Deadwood")


## Data that each loop fills in
MeanTests <- matrix(0, 8, 1) %>% data.frame()
VarTests <- matrix(0, 8, 1) %>% data.frame()
DistTests <- matrix(0, 8, 1) %>% data.frame()


# *********************************************************************************************************
#### Section 2: Use a loop to do all the tests ####
## Conditional Mean. PASS IFF: within 90% of Model mean
## Conditional variance. PASS IFF: within 60% of Model variance
## Distribution test. PASS IFF: fail to reject KS test
# *********************************************************************************************************



## Loop through each attribute and output both test
## We could foreach() this but let's be reasonable
for (i in Index) {

  MeanTests[match(i, Index), ] <-
    SarriasTestMeans(ModelTwo_WTP, ModelTwo_Estimates, Index[match(i, Index)])

  VarTests[match(i, Index), ] <-
    SarriasTestVariances(ModelTwo_WTP, ModelTwo_Estimates, Index[match(i, Index)])

  DistTests[match(i, Index), ] <-
    SarriasTestDistributions(ModelTwo_WTP, ModelTwo_UC, Index[match(i, Index)])

  }


# *********************************************************************************************************
#### Section 4: Output tables ####
# *********************************************************************************************************



## Compile all results here:
SarriasTests <- bind_cols(
  "Variables" = Index,
  "Means" = MeanTests$.,
  "Variances" = VarTests$.,
  "Distributions" = DistTests$.)



SarriasTests %>% View()
TableB10 <- SarriasTests
## Output to screen in a nice format for making tables in word
TableB10 %>% fwrite(quote = FALSE)
## Output to a discrete file if that's helpful
TableB10 %>% fwrite(
  sep = "#",
  here("OtherOutput/Tables",
       "TableB10_SarriasTests.txt"),
  row.names = TRUE,
  quote = FALSE
)


# **********************************************************************************
#### END OF SCRIPT ####
## Next step: 19_Winter_ConsumerSurplus.R
# **********************************************************************************
