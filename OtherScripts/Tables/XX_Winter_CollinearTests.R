#### RELATE WP5: Winter Collinearity  ###############
# Function: Calculate and output collinearity tests
# Script author: Peter King (p.king1@leeds.ac.uk)
# Last Edited: 16/10/2023
# Changes:


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
library(car)
library(dplyr)
library(here)
library(data.table)
rm(list=ls())





# **********************************************************************
#### Section 1: Import ####
# **********************************************************************


Winter <-
  here("CEModelData", "database_Winter_Step1.csv") %>% fread() %>% data.frame()




# **********************************************************************
#### Section 2: Estimate Linear Model ####
# **********************************************************************


Model1 <- lm(data = Winter,
             formula = Choice ~
               Tax1 +
               Sound1 + Sound2 + Smell1 + Smell2 + Colour1 + Colour2 + Deadwood1 + Deadwood2 +
               Gender +
               DummyAge +
               MilesDistance + Charity +
               IncomeDummy  +
               Country + EthnicityDummyWhite +
               Urbanicity +
               WoodlandsScore +
               MostRecentVisit +
               ChildVisitWoodlands +
               TeenVisitWoodlands +
               AdultVisitWoodlands)


# **********************************************************************
#### Section 3: Calculate VIF ####
# **********************************************************************


## The largest scores are for non-attributes
## particularly measures of visit frequency
vif1 <- vif(Model1)



# **********************************************************************
#### Section 3B: Correlation tests ####
# **********************************************************************


## Most recent is significantly and positively correlated with all three
cor.test(Winter$MostRecentVisit, Winter$ChildVisitWoodlands)
cor.test(Winter$MostRecentVisit, Winter$TeenVisitWoodlands)
cor.test(Winter$MostRecentVisit, Winter$AdultVisitWoodlands)



