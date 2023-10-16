#### RELATE WP5: Winter Spatial Lag Models  ###############
# Function: Calculate and output spatial lag models for each attribute level
# Script author: Peter King (p.m.king@kent.ac.uk)
# Last Edited: 10/05/2023
# Changes: Tidying up
# Note: There's probably a way to use loops to make the call to SpatialLagModel()
## run faster and make the inputs less repetitive.


# **********************************************************************
#### Section 0: Setup ####
# **********************************************************************


## sessionInfo() for my office PC not the HPC ---------------------------------------------------------------
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
#   [1] parallel  stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:
#   [1] foreach_1.5.2        xtable_1.8-4         spatialreg_1.2-6     Matrix_1.5-3         spdep_1.2-7          spData_2.2.1
# [7] sp_1.6-0             sf_1.0-9             spacetime_1.2-8      microbenchmark_1.4.9 forcats_0.5.2        purrr_1.0.1
# [13] readr_2.1.3          tidyr_1.2.1          tibble_3.1.8         tidyverse_1.3.2      ggdist_3.2.1         matrixStats_0.63.0
# [19] Rfast_2.0.6          RcppZiggurat_0.1.6   Rcpp_1.0.9           lubridate_1.9.0      timechange_0.2.0     PostcodesioR_0.3.1
# [25] stringi_1.7.12       stringr_1.5.0        data.table_1.14.6    mded_0.1-2           reshape2_1.4.4       ggridges_0.5.4
# [31] ggplot2_3.4.0        magrittr_2.0.3       dplyr_1.0.10         apollo_0.2.8         here_1.0.1
#
# loaded via a namespace (and not attached):
#   [1] googledrive_2.0.0    colorspace_2.0-3     deldir_1.0-6         class_7.3-20         ellipsis_0.3.2       rprojroot_2.0.3
# [7] fs_1.6.0             proxy_0.4-27         rstudioapi_0.14      farver_2.1.1         MatrixModels_0.5-1   fansi_1.0.3
# [13] mvtnorm_1.1-3        RSGHB_1.2.2          xml2_1.3.3           codetools_0.2-18     splines_4.2.0        mnormt_2.1.1
# [19] doParallel_1.0.17    knitr_1.41           jsonlite_1.8.4       mcmc_0.9-7           broom_1.0.2          dbplyr_2.3.0
# [25] compiler_4.2.0       httr_1.4.4           backports_1.4.1      assertthat_0.2.1     fastmap_1.1.0        gargle_1.2.1
# [31] cli_3.6.0            s2_1.1.2             htmltools_0.5.4      quantreg_5.94        tools_4.2.0          coda_0.19-4
# [37] gtable_0.3.1         glue_1.6.2           wk_0.7.1             cellranger_1.1.0     vctrs_0.5.1          nlme_3.1-157
# [43] iterators_1.0.14     randtoolbox_2.0.3    xfun_0.36            rvest_1.0.3          lifecycle_1.0.3      rngWELL_0.10-9
# [49] googlesheets4_1.0.1  LearnBayes_2.15.1    MASS_7.3-56          zoo_1.8-11           scales_1.2.1         miscTools_0.6-26
# [55] hms_1.1.2            sandwich_3.0-2       expm_0.999-7         SparseM_1.81         RColorBrewer_1.1-3   yaml_2.3.6
# [61] e1071_1.7-12         boot_1.3-28          intervals_0.15.2     rlang_1.0.6          pkgconfig_2.0.3      distributional_0.3.1
# [67] evaluate_0.20        lattice_0.20-45      tidyselect_1.2.0     plyr_1.8.8           R6_2.5.1             generics_0.1.3
# [73] DBI_1.1.3            pillar_1.8.1         haven_2.5.1          withr_2.5.0          units_0.8-1          xts_0.12.2
# [79] survival_3.3-1       modelr_0.1.10        crayon_1.5.2         KernSmooth_2.23-20   utf8_1.2.2           tzdb_0.3.0
# [85] rmarkdown_2.20       maxLik_1.5-2         grid_4.2.0           readxl_1.4.1         classInt_0.4-8       reprex_2.0.2
# [91] digest_0.6.31        numDeriv_2016.8-1.1  MCMCpack_1.6-3       munsell_0.5.0
# install.packages(c("doSNOW","doParallel","doMPI","foreach"),repos="http://cran.us.r-project.org")




# install.packages(
#   c("rgdal"),
#   repos = "http://cran.us.r-project.org",
#   lib = "/shared/home/pk372/anaconda3/envs/WinterEnv/lib/R/library"
# )

## Libraries: ---------------------------------------------------------------
library(tidyr)
library(spacetime)
library(dplyr)
library(sf)
library(spdep)
library(spatialreg)
library(here)
library(data.table)
rm(list=ls())





# **********************************************************************
#### Section 1: Import ####
# **********************************************************************



## Import spatial data which has WTP per place
GB_Winter <- st_read(here("OtherData", "GB_Winter_Final.gpkg"))
Winter <-
  here("OtherData", "Winter_dataframe_Step4.csv") %>% fread() %>% data.frame()


## Drop rows that have missing data in any of the following we use in the models:
GB_Winter <- GB_Winter %>% drop_na(Colour_WTP_Medium, WoodlandsScore,
                                   MilesDistance, MostRecentVisit,
                                   DummyAge, Gender,
                                   IncomeDummy, Impairment,
                                   GDHI,Density, Area_ha_median)


## Leftover from old versions is that I mix these two up
## Anyway specify the data you want to use here
Data <- GB_Winter
Data_Winter <- Data



  # **********************************************************************
# Section 2: Nearest Neighbours ####
## I actually redefine these later but this section is a warning -
## if this doesn't work then the function in Section3 won't either.
# **********************************************************************


## Determine number of neighbours:
K = sqrt(nrow(Data))


## Trim data to avoid missing or duplicated coordinates:
Data <-  Data[!is.na(Data$LonH),]
Data <-  Data[!is.na(Data$LatH),]
Data <-  Data[which(!duplicated(Data$LatH)),]
Data <-  Data[which(!duplicated(Data$LonH)),]


## Specify coordinates:
Woodlands <- data.frame(cbind(Data$LatH, abs(Data$LonH)))
CoordsMatrix <- data.matrix(Woodlands) #converting into matrix class


## Calculate neighbours and weights using inverse distance:
KNearNeigh <- knearneigh(CoordsMatrix, k = K)
KNN_ToNBs <- knn2nb(KNearNeigh)
Distances <- nbdists(KNN_ToNBs, CoordsMatrix)
Distances_Inverse <- lapply(Distances, function(x)
  1 / (x ^ 2))
KNN_Weights <- nb2listw(KNN_ToNBs, glist = Distances_Inverse)


# **********************************************************************
# Section 3: Summary function ####
# **********************************************************************


## This function takes Model (a formula() object),
## the name of the WTP variable (eg: "Colour_WTP_Medium"),
## the Data (Data_Winter should be used and specified in Section 1)
## and the number of neighbours K
SpatialLagModel <- function(Model, WTP,Data, K) {


  ## Trim data to avoid missing or duplicated coordinates:
  ## Yes this is above but doing it twice to avoid any errors
  Data <-  Data[!is.na(Data$LonH),]
  Data <-  Data[!is.na(Data$LatH),]
  Data <-  Data[which(!duplicated(Data$LatH)),]
  Data <-  Data[which(!duplicated(Data$LonH)),]


  ## Use given K or calculate as square root of sample size
  if(missing(K)) {
    K = sqrt(nrow(Data))
  } else {
    K
  }


  ## Specify coordinates:
  Woodlands <- data.frame(cbind(Data$LatH, abs(Data$LonH)))
  CoordsMatrix <- data.matrix(Woodlands) #converting into matrix class


  ## Calculate neighbours and weights using inverse distance:
  KNearNeighs <- knearneigh(CoordsMatrix, k = K)
  KNN_ToNB <- knn2nb(KNearNeighs)
  Distance <- nbdists(KNN_ToNB, CoordsMatrix)
  Distance_Inverse <- lapply(Distance, function(x)
    1 / (x ^ 2))
  NearestNeighbour <- nb2listw(KNN_ToNB, glist = Distance_Inverse)


  ## Using paste0 to make custom variable names
  SLM_dummy_Attribute <- paste0("SLM_Dummy_", WTP)


  # Using lagsarlm() and lm() to calculate and compare models
  SLM_dummy_Attribute <- lagsarlm(Model, data=Data, NearestNeighbour)
  SLM_dummy_Attribute_S <- summary(SLM_dummy_Attribute)
  OLS_dummy_Attribute <- lm(Model, data=Data) # Dummy regression
  logLik(OLS_dummy_Attribute) ## Does this need to be printed?
  OLS_dummy_Attribute_S <- summary(OLS_dummy_Attribute)


  ## Calculate misc tests:
  Test_Attribute <- lm.LMtests(OLS_dummy_Attribute,
                               NearestNeighbour,
                               test = "LMlag" )


  Stat_Attribute <- round(t(cbind("Stat"=Test_Attribute$LMlag$statistic,
                                  "P.V"=Test_Attribute$LMlag$p.value)),3)
  An_Attribute <- anova(SLM_dummy_Attribute,OLS_dummy_Attribute)


  ## For easy inference just output the results like this:
  Result <- ifelse(
    Test_Attribute$LMlag$p.value < 0.05,
    paste0("Missing Spatial Lag with P: ", round(Test_Attribute$LMlag$p.value,3)),
    paste0("No Spatial Lag with P: ", round(Test_Attribute$LMlag$p.value,3))
  )



  ## Save outputs in correct place with correct names:
  saveRDS(SLM_dummy_Attribute, paste0(here(),"/OtherOutput/Spatial/","SLM_Dummy_",WTP,"_K", round(K),"_Model",".rds"))
  saveRDS(Test_Attribute, paste0(here(),"/OtherOutput/Spatial/","SLM_Dummy_",WTP,"_K", round(K),"_Result",".rds"))

  return(list(Result,SLM_dummy_Attribute_S))
}




# **********************************************************************
# Section 4A: Define loop ####
# **********************************************************************


## Specify the loop of WTP characters to loop through
Dependent <- c(
  "Colour_WTP_Medium",
  "Colour_WTP_High",

  "Smell_WTP_Medium",
  "Smell_WTP_High",

  "Sound_WTP_Medium",
  "Sound_WTP_High",

  "Deadwood_WTP_Medium",
  "Deadwood_WTP_High"
)


## Independent variables to control for
Independent <- c("Gender +
                DummyAge +
                EthnicityDummyWhite +
                IncomeDummy +
                Urbanicity +
                Charity +
                Country +
                MilesDistance +
                MostRecentVisit +
                WoodlandsScore +
                Impairment +
                Area_ha_median +
                GDHI + Density"
)


# **********************************************************************
# Section 4B: Run loop ####
# **********************************************************************


## Reformulate updates the model object
## we update the LHS for different attributes WTP
## then pipe that into SpatialLagModel()
for (i in 1:length(Dependent)) {

  reformulate(termlabels = Independent,
              response = Dependent[i]) %>% SpatialLagModel(Dependent[i] %>% as.character(),
                                                           Data_Winter,
                                                           K = 5)


}


# **********************************************************************
#### Section 5: Loop in outputs ####
# **********************************************************************


### Models themselves: ----------------------------------------------------------
for (i in 1:length(Dependent)) {

assign(x = paste0(Dependent[i], "_Model"),
       value = here("OtherOutput/Spatial",
                    paste0("SLM_Dummy_", Dependent[i], "_K5_Model.rds")) %>% readRDS())

  }

### Diagnostics themselves: ----------------------------------------------------------
for (i in 1:length(Dependent)) {

  assign(x = paste0(Dependent[i], "_Result"),
         value = here("OtherOutput/Spatial",
                      paste0("SLM_Dummy_", Dependent[i], "_K5_Result.rds")) %>% readRDS())

}


# **********************************************************************
#### Section 6A: Define Summary Functions ####
# **********************************************************************


## So this code outputs a table of estimate, p.v stars and s.e in brackets ##
ModelOutput <- function(Model,Result) {
  Estimates <- summary(Model)
  rbind(
    data.frame(
      "Value" = rownames(Estimates$Coef),
      "Data" = paste0(ifelse(
        Estimates$Coef[, 4] < 0.01,
        paste0(Estimates$Coef[, 1] %>% round(3) %>% sprintf("%.3f", .), "***"),
        ifelse(
          Estimates$Coef[, 4] < 0.05,
          paste0(Estimates$Coef[, 1] %>% round(3) %>% sprintf("%.3f", .), "**"),
          ifelse(
            Estimates$Coef[, 4] < 0.1,
            paste0(Estimates$Coef[, 1] %>% round(3) %>% sprintf("%.3f", .), "*"),
            Estimates$Coef[, 4] %>% round(3) %>% sprintf("%.3f", .)))),
        " (",Estimates$Coef[, 3] %>% round(3) %>% sprintf("%.3f", .),")")),

    data.frame("Value" = c("Stat"),
               "Data" = Result$LMlag$statistic %>% round(3) %>% sprintf("%.3f", .)),

    data.frame("Value" = c("LR"),
               "Data" = paste(ifelse(
                 Estimates$LR1$p.value < 0.01,
                 paste0(Estimates$rho %>% round(3) %>% sprintf("%.3f", .), "***"),
                 ifelse(
                   Estimates$LR1$p.value < 0.05,
                   paste0(Estimates$rho %>% round(3) %>% sprintf("%.3f", .), "**"),
                   ifelse(
                     Estimates$LR1$p.value < 0.1,
                     paste0(Estimates$rho %>% round(3) %>% sprintf("%.3f", .), "*"),
                     Estimates$LR1$p.value %>% round(3) %>% sprintf("%.3f", .)
                   )
                 )
               ),
               paste0("(",
                      Estimates$rho.se %>% round(3) %>% sprintf("%.3f", .),
                      ")")))[1,],

    data.frame("Value" = c("logLik"), "Data" = logLik(Estimates) %>% round(3) %>% sprintf("%.3f", .)),
    data.frame("Value" = c("AIC"), "Data" = AIC(Estimates) %>% round(3) %>% sprintf("%.3f", .)))
}


# **********************************************************************
#### Section 6B: Use Summary Functions For Output Tables ####
# **********************************************************************


## Maybe a little ugly but outputs table in one go
Table7 <- cbind(
  "Attribute" = ModelOutput(Colour_WTP_Medium_Model, Colour_WTP_Medium_Result)[, 1],
  "Colour: Medium" = ModelOutput(Colour_WTP_Medium_Model, Colour_WTP_Medium_Result)[, 2],
  "Colour: High" = ModelOutput(Colour_WTP_High_Model, Colour_WTP_High_Result)[, 2],

  "Smell: Medium" = ModelOutput(Smell_WTP_Medium_Model, Smell_WTP_Medium_Result)[, 2],
  "Smell: High" = ModelOutput(Smell_WTP_High_Model, Smell_WTP_High_Result)[, 2],

  "Sound: Medium" = ModelOutput(Sound_WTP_Medium_Model, Sound_WTP_Medium_Result)[, 2],
  "Sound: High" = ModelOutput(Sound_WTP_High_Model, Sound_WTP_High_Result)[, 2],

  "Decomposition: Medium" = ModelOutput(Deadwood_WTP_Medium_Model, Deadwood_WTP_Medium_Result)[, 2],
  "Decomposition: High" = ModelOutput(Deadwood_WTP_High_Model, Deadwood_WTP_High_Result)[, 2]
)

# **********************************************************************
#### Section 7: Export ####
# **********************************************************************


## Output to screen:
Table7 %>% write.csv(quote = FALSE, row.names = FALSE)



## Output to a discrete file if that's helpful
write.table(
  Table7,
  here("OtherOutput/Spatial", "Table7_SpatialLagModels.txt"),
  sep = ",",
  quote = FALSE
)




# **********************************************************************
#### END OF SCRIPT ####
# **********************************************************************
