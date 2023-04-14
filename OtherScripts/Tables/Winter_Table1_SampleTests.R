#### RELATE WP5: Winter  ###############
# Function: Calculate and output G tests for the sample
# Script author: Peter King (p.m.king@kent.ac.uk)
# Last Edited: 14/04/2023
# Changes: Created


# ****************************************************************************
#### Section 0: Setup ####
# ****************************************************************************


## sessionInfo() for my office PC not the HPC *********************************
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



Winter <- data.frame(fread(here("OtherData","Winter_dataframe_Step4.csv")))



## ****************************************************************************
#### Step Two ####
## ****************************************************************************


Total <- Winter %>% nrow()



## ***************************************************************************
#### Gender ####


Test_Gender <- g.test(
  Winter$Gender%>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total),
  p = c(49,51),
  rescale.p = TRUE
)


Row1_Gender <- cbind(
  "Variable" = "Gender",
  "Category" = "Male",
  "N" = data.frame(Winter$Gender%>% table() %>% as.numeric())[1,] %>% round(3),
  "Sample (%)" = data.frame(Winter$Gender%>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total))[,1] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 49,
  "PV" = Test_Gender$statistic %>% as.numeric() %>% round(3)
)


Row2_Gender <- cbind(
  "Variable" = "Gender",
  "Category" = "Female",
  "N" = data.frame(Winter$Gender%>% table() %>% as.numeric())[2,] %>% round(3),
  "Sample (%)" = data.frame(Winter$Gender%>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total))[,2] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 51,
  "PV" = paste0("PV: ", Test_Gender$p.value %>% as.numeric() %>% round(3))
)


Rows_Gender <- rbind(Row1_Gender, Row2_Gender)


## ***************************************************************************
#### ExactAge ####


## Code as categories here for ease
Winter$AgeCategory <- ifelse(Winter$ExactAge <= 29, 0,
                             ifelse(Winter$ExactAge <= 39,1,
                                    ifelse(Winter$ExactAge <= 49, 2,
                                           ifelse(Winter$ExactAge <= 59, 3,
                                                  ifelse(Winter$ExactAge <= 69, 4, 5)))))



Test_ExactAge <- g.test(
  Winter$AgeCategory%>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total),
  p = c(16.2, 13.3, 14.6, 12.1, 10.80, 11.7),
  rescale.p = TRUE
)


Row1_ExactAge <- cbind(
  "Variable" = "AgeCategory",
  "Category" = "18 - 29",
  "N" = data.frame(Winter$AgeCategory%>% table() %>% as.numeric())[1,] %>% round(3),
  "Sample (%)" = data.frame(Winter$AgeCategory%>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total))[,1] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 16.2,
  "PV" = Test_ExactAge$statistic %>% as.numeric() %>% round(3)
)


Row2_ExactAge <- cbind(
  "Variable" = "AgeCategory",
  "Category" = "30 - 39",
  "N" = data.frame(Winter$AgeCategory%>% table() %>% as.numeric())[2,] %>% round(3),
  "Sample (%)" = data.frame(Winter$AgeCategory%>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total))[,2] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 13.3,
  "PV" = Test_ExactAge$statistic %>% as.numeric() %>% round(3)
)

Row3_ExactAge <- cbind(
  "Variable" = "AgeCategory",
  "Category" = "40 - 49",
  "N" = data.frame(Winter$AgeCategory%>% table() %>% as.numeric())[3,] %>% round(3),
  "Sample (%)" = data.frame(Winter$AgeCategory%>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total))[,3] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 14.6,
  "PV" = Test_ExactAge$statistic %>% as.numeric() %>% round(3)
)

Row4_ExactAge <- cbind(
  "Variable" = "AgeCategory",
  "Category" = "50 - 59",
  "N" = data.frame(Winter$AgeCategory%>% table() %>% as.numeric())[4,] %>% round(3),
  "Sample (%)" = data.frame(Winter$AgeCategory%>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total))[,4] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 12.1,
  "PV" = Test_ExactAge$statistic %>% as.numeric() %>% round(3)
)


Row5_ExactAge <- cbind(
  "Variable" = "AgeCategory",
  "Category" = "60 - 69",
  "N" = data.frame(Winter$AgeCategory%>% table() %>% as.numeric())[5,] %>% round(3),
  "Sample (%)" = data.frame(Winter$AgeCategory%>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total))[,5] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 10.80,
  "PV" = Test_ExactAge$statistic %>% as.numeric() %>% round(3)
)

Row6_ExactAge <- cbind(
  "Variable" = "AgeCategory",
  "Category" = "70+",
  "N" = data.frame(Winter$AgeCategory%>% table() %>% as.numeric())[6,] %>% round(3),
  "Sample (%)" = data.frame(Winter$AgeCategory%>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total))[,6] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 11.7,
  "PV" = paste0("PV: ", Test_ExactAge$p.value %>% as.numeric() %>% round(3))
)


Rows_ExactAge <- rbind(Row1_ExactAge, Row2_ExactAge,
                       Row3_ExactAge, Row4_ExactAge,
                       Row5_ExactAge, Row6_ExactAge)






## ***************************************************************************
#### EthnicityDummyWhite ####


Test_EthnicityDummyWhite <- g.test(
  Winter$EthnicityDummyWhite%>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total),
  p = c(87, 13),
  rescale.p = TRUE
)


Row1_EthnicityDummyWhite <- cbind(
  "Variable" = "EthnicityDummyWhite",
  "Category" = "Non-white",
  "N" = data.frame(Winter$EthnicityDummyWhite%>% table() %>% as.numeric())[1,] %>% round(3),
  "Sample (%)" = data.frame(Winter$EthnicityDummyWhite%>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total))[,1] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 13,
  "PV" = Test_EthnicityDummyWhite$statistic %>% as.numeric() %>% round(3)
)


Row2_EthnicityDummyWhite <- cbind(
  "Variable" = "EthnicityDummyWhite",
  "Category" = "White",
  "N" = data.frame(Winter$EthnicityDummyWhite%>% table() %>% as.numeric())[2,] %>% round(3),
  "Sample (%)" = data.frame(Winter$EthnicityDummyWhite%>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total))[,2] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 87,
  "PV" = paste0("PV: ", Test_EthnicityDummyWhite$p.value %>% as.numeric() %>% round(3))
)


Rows_EthnicityDummyWhite <- rbind(Row1_EthnicityDummyWhite, Row2_EthnicityDummyWhite)



## ***************************************************************************
#### Urbanicity ####


Test_Urbanicity <- g.test(
  Winter$Urbanicity%>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total),
  p = c(18.5, 81.5),
  rescale.p = TRUE
)


Row1_Urbanicity <- cbind(
  "Variable" = "Urbanicity",
  "Category" = "Rural",
  "N" = data.frame(Winter$Urbanicity%>% table() %>% as.numeric())[1,] %>% round(3),
  "Sample (%)" = data.frame(Winter$Urbanicity%>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total))[,1] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 18.5,
  "PV" = Test_Urbanicity$statistic %>% as.numeric() %>% round(3)
)


Row2_Urbanicity <- cbind(
  "Variable" = "Urbanicity",
  "Category" = "Urban",
  "N" = data.frame(Winter$Urbanicity%>% table() %>% as.numeric())[2,] %>% round(3),
  "Sample (%)" = data.frame(Winter$Urbanicity%>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total))[,2] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 81.5,
  "PV" = paste0("PV: ", Test_Urbanicity$p.value %>% as.numeric() %>% round(3))
)


Rows_Urbanicity <- rbind(Row1_Urbanicity, Row2_Urbanicity)



## ***************************************************************************
#### Occupation ####


Winter$Occupation <- Winter$Now..thinking.of.the.chief..income.earner.in.your.household..which.might.be.you.or.somebody.else.in.the..household..which.of.these.best.describes.the.current.status.of.the.chief.income..earner.


Winter$Occupation <- ifelse(Winter$Occupation == "Student/Full time education", 1,
                            ifelse(Winter$Occupation == "Unemployed/on benefit", 2,
                                   ifelse(Winter$Occupation == "Retired", 3, 4)))


Test_Occupation <- g.test(
  Winter$Occupation%>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total),
  p = c(8.2, 4,  24, 76),
  rescale.p = TRUE
)


Row1_Occupation <- cbind(
  "Variable" = "Occupation",
  "Category" = "Student",
  "N" = data.frame(Winter$Occupation%>% table() %>% as.numeric())[1,] %>% round(3),
  "Sample (%)" = data.frame(Winter$Occupation%>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total))[,1] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 8.2,
  "PV" = Test_Occupation$statistic %>% as.numeric() %>% round(3)
)


Row2_Occupation <- cbind(
  "Variable" = "Occupation",
  "Category" = "Unemployed",
  "N" = data.frame(Winter$Occupation%>% table() %>% as.numeric())[1,] %>% round(3),
  "Sample (%)" = data.frame(Winter$Occupation%>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total))[,1] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 4,
  "PV" = Test_Occupation$statistic %>% as.numeric() %>% round(3)
)


Row3_Occupation <- cbind(
  "Variable" = "Occupation",
  "Category" = "Retired",
  "N" = data.frame(Winter$Occupation%>% table() %>% as.numeric())[1,] %>% round(3),
  "Sample (%)" = data.frame(Winter$Occupation%>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total))[,1] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 24,
  "PV" = Test_Occupation$statistic %>% as.numeric() %>% round(3)
)


Row4_Occupation <- cbind(
  "Variable" = "Occupation",
  "Category" = "Employed",
  "N" = data.frame(Winter$Occupation%>% table() %>% as.numeric())[2,] %>% round(3),
  "Sample (%)" = data.frame(Winter$Occupation%>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total))[,2] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 76,
  "PV" = paste0("PV: ", Test_Occupation$p.value %>% as.numeric() %>% round(3))
)


Rows_Occupation <- rbind(Row1_Occupation, Row2_Occupation,
                         Row3_Occupation, Row4_Occupation)



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



Income <- data.frame(Weekly,Freq)

Income$Weekly <- Income$Weekly*4*12


Groups <-
  c(Income[2,1],
    Income[3,1],
    Income[5,1],
    Income[6,1])

Freqs <- c(
  Income[1,2]+Income[2,2],
  Income[3,2],
  Income[4,2]+Income[5,2],
  Income[6:11,2] %>% sum())

Population <- data.frame(Groups, Freqs)


Winter$IncomeCategories <- ifelse(Winter$IncomeLevels < Population$Groups[1], 0,
                                  ifelse(Winter$IncomeLevels < Population$Groups[2], 1,
                                         ifelse(Winter$IncomeLevels < Population$Groups[3], 2, 3)))


Test_IncomeCategories <- g.test(
  Winter$IncomeCategories %>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total),
  p = c(26, 19, 25, 30),
  rescale.p = TRUE
)


Row1_IncomeCategories <- cbind(
  "Variable" = "IncomeCategories",
  "Category" = "£0 – £20,500",
  "N" = data.frame(Winter$IncomeCategories%>% table() %>% as.numeric())[1,] %>% round(3),
  "Sample (%)" = data.frame(Winter$IncomeCategories%>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total))[,1] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 26,
  "PV" = Test_IncomeCategories$statistic %>% as.numeric() %>% round(3)
)


Row2_IncomeCategories <- cbind(
  "Variable" = "IncomeCategories",
  "Category" = "£20,501 – £26,800",
  "N" = data.frame(Winter$IncomeCategories%>% table() %>% as.numeric())[2,] %>% round(3),
  "Sample (%)" = data.frame(Winter$IncomeCategories%>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total))[,2] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 19,
  "PV" = paste0("PV: ", Test_IncomeCategories$p.value %>% as.numeric() %>% round(3))
)


Row3_IncomeCategories <- cbind(
  "Variable" = "IncomeCategories",
  "Category" = "£26,801 – £54,000",
  "N" = data.frame(Winter$IncomeCategories%>% table() %>% as.numeric())[3,] %>% round(3),
  "Sample (%)" = data.frame(Winter$IncomeCategories%>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total))[,3] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 25,
  "PV" = paste0("PV: ", Test_IncomeCategories$p.value %>% as.numeric() %>% round(3))
)


Row4_IncomeCategories <- cbind(
  "Variable" = "IncomeCategories",
  "Category" = "£54,000+",
  "N" = data.frame(Winter$IncomeCategories%>% table() %>% as.numeric())[4,] %>% round(3),
  "Sample (%)" = data.frame(Winter$IncomeCategories%>% table() %>% as.numeric() %>% data.frame() %>% t() %>% divide_by(Total))[,4] %>% round(3) %>% multiply_by(100),
  "Population (%)" = 30,
  "PV" = paste0("PV: ", Test_IncomeCategories$p.value %>% as.numeric() %>% round(3))
)


Rows_IncomeCategories <- rbind(Row1_IncomeCategories, Row2_IncomeCategories,
                               Row3_IncomeCategories, Row4_IncomeCategories)


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
  fwrite(sep=",",
         here("OtherOutput/Tables","Table1.txt"),
         row.names = TRUE,
         quote = FALSE)


# End Of Script **************************************************************
