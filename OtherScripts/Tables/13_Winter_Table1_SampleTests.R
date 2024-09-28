#### Stated preferences for the colours, smells and sounds of biodiversity  ###############
# Project: ERC Relate Project. Work Package 5
# Author: Dr Peter King (p.king1@leeds.ac.uk)
# Function: To output G tests of the sample for Table B2
# Notes: you can do this more elegantly
# Last Edited: 28/09/2024
# - double-checking post R2
# - updating sessioninfo()

# ****************************************************************************
#### Section 0: Setup ####
# ****************************************************************************


## sessionInfo() for my office PC not the HPC *********************************
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



## Libraries: **************************************************************
library(tidyr)
library(spacetime)
library(dplyr)
library(sf)
library(spdep)
library(spatialreg)
library(here)
library(data.table)
library(AMR)
rm(list=ls())





## ****************************************************************************
#### Step One: Read in data frame with all respondents ####
## ****************************************************************************



Winter <-
  here("OtherData", "Winter_dataframe_Step4.csv") %>% fread() %>% data.frame()



## ****************************************************************************
#### Step Two: Construct table variable by variable ####
## ****************************************************************************


## Total number of respondents. Defined here for repeated later use
Total <- Winter %>% nrow()



## ***************************************************************************
#### Gender ####


## G test of frequencies of male/female sample vs population
Test_Gender <- g.test(
  Winter$Gender %>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total),
  p = c(49, 51),
  rescale.p = TRUE
)


Row1_Gender <- cbind(
  "Variable" = "Gender",
  "Category" = "Male",
  "N" = data.frame(Winter$Gender %>% table() %>% as.numeric())[1, ] %>% round(3),
  "Sample (%)" = data.frame(
    Winter$Gender %>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total)
  )[, 1] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 49,
  "PV" = Test_Gender$statistic %>% as.numeric() %>% round(3)
)


Row2_Gender <- cbind(
  "Variable" = "Gender",
  "Category" = "Female",
  "N" = data.frame(Winter$Gender %>% table() %>% as.numeric())[2, ] %>% round(3),
  "Sample (%)" = data.frame(
    Winter$Gender %>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total)
  )[, 2] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 51,
  "PV" = paste0("PV: ", Test_Gender$p.value %>% as.numeric() %>% round(3))
)


## Add all rows together here
Rows_Gender <- rbind(Row1_Gender, Row2_Gender)


## ***************************************************************************
#### ExactAge ####



## Code as categories here for ease
Winter$AgeCategory <- ifelse(Winter$ExactAge <= 29,
                             0,
                             ifelse(
                               Winter$ExactAge <= 39,
                               1,
                               ifelse(
                                 Winter$ExactAge <= 49,
                                 2,
                                 ifelse(Winter$ExactAge <= 59, 3,
                                        ifelse(Winter$ExactAge <= 69, 4, 5))
                               )
                             ))



## G test here
## P are given frequency
Test_ExactAge <- g.test(
  Winter$AgeCategory %>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total),
  p = c(16.2, 13.3, 14.6, 12.1, 10.80, 11.7),
  rescale.p = TRUE
)


Row1_ExactAge <- cbind(
  "Variable" = "AgeCategory",
  "Category" = "18 - 29",
  "N" = data.frame(Winter$AgeCategory %>% table() %>% as.numeric())[1, ] %>% round(3),
  "Sample (%)" = data.frame(
    Winter$AgeCategory %>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total)
  )[, 1] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 16.2,
  "PV" = Test_ExactAge$statistic %>% as.numeric() %>% round(3)
)


Row2_ExactAge <- cbind(
  "Variable" = "AgeCategory",
  "Category" = "30 - 39",
  "N" = data.frame(Winter$AgeCategory %>% table() %>% as.numeric())[2, ] %>% round(3),
  "Sample (%)" = data.frame(
    Winter$AgeCategory %>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total)
  )[, 2] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 13.3,
  "PV" = Test_ExactAge$statistic %>% as.numeric() %>% round(3)
)

Row3_ExactAge <- cbind(
  "Variable" = "AgeCategory",
  "Category" = "40 - 49",
  "N" = data.frame(Winter$AgeCategory %>% table() %>% as.numeric())[3, ] %>% round(3),
  "Sample (%)" = data.frame(
    Winter$AgeCategory %>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total)
  )[, 3] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 14.6,
  "PV" = Test_ExactAge$statistic %>% as.numeric() %>% round(3)
)

Row4_ExactAge <- cbind(
  "Variable" = "AgeCategory",
  "Category" = "50 - 59",
  "N" = data.frame(Winter$AgeCategory %>% table() %>% as.numeric())[4, ] %>% round(3),
  "Sample (%)" = data.frame(
    Winter$AgeCategory %>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total)
  )[, 4] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 12.1,
  "PV" = Test_ExactAge$statistic %>% as.numeric() %>% round(3)
)


Row5_ExactAge <- cbind(
  "Variable" = "AgeCategory",
  "Category" = "60 - 69",
  "N" = data.frame(Winter$AgeCategory %>% table() %>% as.numeric())[5, ] %>% round(3),
  "Sample (%)" = data.frame(
    Winter$AgeCategory %>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total)
  )[, 5] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 10.80,
  "PV" = Test_ExactAge$statistic %>% as.numeric() %>% round(3)
)

Row6_ExactAge <- cbind(
  "Variable" = "AgeCategory",
  "Category" = "70+",
  "N" = data.frame(Winter$AgeCategory %>% table() %>% as.numeric())[6, ] %>% round(3),
  "Sample (%)" = data.frame(
    Winter$AgeCategory %>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total)
  )[, 6] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 11.7,
  "PV" = paste0("PV: ", Test_ExactAge$p.value %>% as.numeric() %>% round(3))
)


## Combine all rows here
Rows_ExactAge <- rbind(
  Row1_ExactAge,
  Row2_ExactAge,
  Row3_ExactAge,
  Row4_ExactAge,
  Row5_ExactAge,
  Row6_ExactAge
)






## ***************************************************************************
#### EthnicityDummyWhite ####



Test_EthnicityDummyWhite <- g.test(
  Winter$EthnicityDummyWhite %>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total),
  p = c(87, 13),
  rescale.p = TRUE
)


Row1_EthnicityDummyWhite <- cbind(
  "Variable" = "EthnicityDummyWhite",
  "Category" = "Non-white",
  "N" = data.frame(Winter$EthnicityDummyWhite %>% table() %>% as.numeric())[1, ] %>% round(3),
  "Sample (%)" = data.frame(
    Winter$EthnicityDummyWhite %>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total)
  )[, 1] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 13,
  "PV" = Test_EthnicityDummyWhite$statistic %>% as.numeric() %>% round(3)
)


Row2_EthnicityDummyWhite <- cbind(
  "Variable" = "EthnicityDummyWhite",
  "Category" = "White",
  "N" = data.frame(Winter$EthnicityDummyWhite %>% table() %>% as.numeric())[2, ] %>% round(3),
  "Sample (%)" = data.frame(
    Winter$EthnicityDummyWhite %>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total)
  )[, 2] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 87,
  "PV" = paste0(
    "PV: ",
    Test_EthnicityDummyWhite$p.value %>% as.numeric() %>% round(3)
  )
)


Rows_EthnicityDummyWhite <-
  rbind(Row1_EthnicityDummyWhite, Row2_EthnicityDummyWhite)



## ***************************************************************************
#### Urbanicity ####


Test_Urbanicity <- g.test(
  Winter$Urbanicity %>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total),
  p = c(18.5, 81.5),
  rescale.p = TRUE
)


Row1_Urbanicity <- cbind(
  "Variable" = "Urbanicity",
  "Category" = "Rural",
  "N" = data.frame(Winter$Urbanicity %>% table() %>% as.numeric())[1, ] %>% round(3),
  "Sample (%)" = data.frame(
    Winter$Urbanicity %>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total)
  )[, 1] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 18.5,
  "PV" = Test_Urbanicity$statistic %>% as.numeric() %>% round(3)
)


Row2_Urbanicity <- cbind(
  "Variable" = "Urbanicity",
  "Category" = "Urban",
  "N" = data.frame(Winter$Urbanicity %>% table() %>% as.numeric())[2, ] %>% round(3),
  "Sample (%)" = data.frame(
    Winter$Urbanicity %>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total)
  )[, 2] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 81.5,
  "PV" = paste0("PV: ", Test_Urbanicity$p.value %>% as.numeric() %>% round(3))
)


Rows_Urbanicity <- rbind(Row1_Urbanicity, Row2_Urbanicity)



## ***************************************************************************
#### Occupation ####


Winter$Occupation <-
  Winter$Now..thinking.of.the.chief..income.earner.in.your.household..which.might.be.you.or.somebody.else.in.the..household..which.of.these.best.describes.the.current.status.of.the.chief.income..earner.


Winter$Occupation <-
  ifelse(
    Winter$Occupation == "Student/Full time education",
    1,
    ifelse(
      Winter$Occupation == "Unemployed/on benefit",
      2,
      ifelse(Winter$Occupation == "Retired", 3, 4)
    )
  )


Test_Occupation <- g.test(
  Winter$Occupation %>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total),
  p = c(8.2, 4,  24, 76),
  rescale.p = TRUE
)


Row1_Occupation <- cbind(
  "Variable" = "Occupation",
  "Category" = "Student",
  "N" = data.frame(Winter$Occupation %>% table() %>% as.numeric())[1, ] %>% round(3),
  "Sample (%)" = data.frame(
    Winter$Occupation %>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total)
  )[, 1] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 8.2,
  "PV" = Test_Occupation$statistic %>% as.numeric() %>% round(3)
)


Row2_Occupation <- cbind(
  "Variable" = "Occupation",
  "Category" = "Unemployed",
  "N" = data.frame(Winter$Occupation %>% table() %>% as.numeric())[1, ] %>% round(3),
  "Sample (%)" = data.frame(
    Winter$Occupation %>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total)
  )[, 1] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 4,
  "PV" = Test_Occupation$statistic %>% as.numeric() %>% round(3)
)


Row3_Occupation <- cbind(
  "Variable" = "Occupation",
  "Category" = "Retired",
  "N" = data.frame(Winter$Occupation %>% table() %>% as.numeric())[1, ] %>% round(3),
  "Sample (%)" = data.frame(
    Winter$Occupation %>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total)
  )[, 1] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 24,
  "PV" = Test_Occupation$statistic %>% as.numeric() %>% round(3)
)


Row4_Occupation <- cbind(
  "Variable" = "Occupation",
  "Category" = "Employed",
  "N" = data.frame(Winter$Occupation %>% table() %>% as.numeric())[2, ] %>% round(3),
  "Sample (%)" = data.frame(
    Winter$Occupation %>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total)
  )[, 2] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 76,
  "PV" = paste0("PV: ", Test_Occupation$p.value %>% as.numeric() %>% round(3))
)


Rows_Occupation <- rbind(Row1_Occupation,
                         Row2_Occupation,
                         Row3_Occupation,
                         Row4_Occupation)



## ***************************************************************************
#### IncomeCategories ####
## Taken from here: https://www.ethnicity-facts-figures.service.gov.uk/work-pay-and-benefits/pay-and-income/household-income/latest




Weekly <-  c(200,
             399,
             599,
             799,
             999,
             1199,
             1399,
             1599,
             1799,
             1999,
             2000)


Freq <-  c(6,
           20,
           19,
           14,
           11,
           8,
           6,
           4,
           3,
           2,
           7)



Income <- data.frame(Weekly, Freq)

Income$Weekly <- Income$Weekly * 4 * 12


Groups <-
  c(Income[2, 1],
    Income[3, 1],
    Income[5, 1],
    Income[6, 1])

Freqs <- c(Income[1, 2] + Income[2, 2],
           Income[3, 2],
           Income[4, 2] + Income[5, 2],
           Income[6:11, 2] %>% sum())

Population <- data.frame(Groups, Freqs)


Winter$IncomeCategories <-
  ifelse(
    Winter$IncomeLevels < Population$Groups[1],
    0,
    ifelse(
      Winter$IncomeLevels < Population$Groups[2],
      1,
      ifelse(Winter$IncomeLevels < Population$Groups[3], 2, 3)
    )
  )


Test_IncomeCategories <- g.test(
  Winter$IncomeCategories %>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total),
  p = c(26, 19, 25, 30),
  rescale.p = TRUE
)


Row1_IncomeCategories <- cbind(
  "Variable" = "IncomeCategories",
  "Category" = "£0 – £20,500",
  "N" = data.frame(Winter$IncomeCategories %>% table() %>% as.numeric())[1, ] %>% round(3),
  "Sample (%)" = data.frame(
    Winter$IncomeCategories %>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total)
  )[, 1] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 26,
  "PV" = Test_IncomeCategories$statistic %>% as.numeric() %>% round(3)
)


Row2_IncomeCategories <- cbind(
  "Variable" = "IncomeCategories",
  "Category" = "£20,501 – £26,800",
  "N" = data.frame(Winter$IncomeCategories %>% table() %>% as.numeric())[2, ] %>% round(3),
  "Sample (%)" = data.frame(
    Winter$IncomeCategories %>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total)
  )[, 2] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 19,
  "PV" = paste0(
    "PV: ",
    Test_IncomeCategories$p.value %>% as.numeric() %>% round(3)
  )
)


Row3_IncomeCategories <- cbind(
  "Variable" = "IncomeCategories",
  "Category" = "£26,801 – £54,000",
  "N" = data.frame(Winter$IncomeCategories %>% table() %>% as.numeric())[3, ] %>% round(3),
  "Sample (%)" = data.frame(
    Winter$IncomeCategories %>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total)
  )[, 3] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 25,
  "PV" = paste0(
    "PV: ",
    Test_IncomeCategories$p.value %>% as.numeric() %>% round(3)
  )
)


Row4_IncomeCategories <- cbind(
  "Variable" = "IncomeCategories",
  "Category" = "£54,000+",
  "N" = data.frame(Winter$IncomeCategories %>% table() %>% as.numeric())[4, ] %>% round(3),
  "Sample (%)" = data.frame(
    Winter$IncomeCategories %>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total)
  )[, 4] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 30,
  "PV" = paste0(
    "PV: ",
    Test_IncomeCategories$p.value %>% as.numeric() %>% round(3)
  )
)


Rows_IncomeCategories <-
  rbind(
    Row1_IncomeCategories,
    Row2_IncomeCategories,
    Row3_IncomeCategories,
    Row4_IncomeCategories
  )


# ****************************************************************************
#### Section D: Combination ####
# ****************************************************************************


Table1 <- rbind(
  Rows_Gender,
  Rows_ExactAge,
  Rows_EthnicityDummyWhite,
  Rows_IncomeCategories,
  Rows_Urbanicity,
  Rows_Occupation
) %>% data.frame()


## Export
Table1 %>%
  fwrite(
    sep = ",",
    here("OtherOutput/Tables", "Table1.txt"),
    row.names = TRUE,
    quote = FALSE
  )


# *********************************************************************************************************
#### END OF SCRIPT ####
## Next Step: 14_Winter_Table1A_ModelOutputs.R
# *********************************************************************************************************
