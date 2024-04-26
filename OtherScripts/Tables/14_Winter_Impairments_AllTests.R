#### RELATE Winter Paper ####
## Function: Calculate Poe tests between impairments
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 20/03/2024
# Changes:
# - Using conditional WTP from correlated model
# - Using Mann-Whitney means test
# - Adding table



# ******************************
# Replication Information: ####
# ******************************


# R version 4.2.0 (2022-04-22 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19044)
# Matrix products: default
# locale:
#   [1] LC_COLLATE=English_United Kingdom.utf8  LC_CTYPE=English_United Kingdom.utf8
# [3] LC_MONETARY=English_United Kingdom.utf8 LC_NUMERIC=C
# [5] LC_TIME=English_United Kingdom.utf8
#
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:
#   [1] Rfast_2.0.6          RcppZiggurat_0.1.6   Rcpp_1.0.9           distributional_0.3.1 ggdist_3.2.1
# [6] data.table_1.14.6    here_1.0.1           ggridges_0.5.4       reshape2_1.4.4       apollo_0.2.8
# [11] magrittr_2.0.3       forcats_0.5.2        stringr_1.5.0        dplyr_1.0.10         purrr_1.0.1
# [16] readr_2.1.3          tidyr_1.2.1          tibble_3.1.8         ggplot2_3.4.0        tidyverse_1.3.2
#
# loaded via a namespace (and not attached):
#   [1] mcmc_0.9-7          matrixStats_0.63.0  fs_1.6.0            lubridate_1.9.0     RColorBrewer_1.1-3
# [6] httr_1.4.4          rprojroot_2.0.3     numDeriv_2016.8-1.1 tools_4.2.0         backports_1.4.1
# [11] utf8_1.2.2          R6_2.5.1            DBI_1.1.3           colorspace_2.0-3    withr_2.5.0
# [16] tidyselect_1.2.0    mnormt_2.1.1        compiler_4.2.0      cli_3.6.0           rvest_1.0.3
# [21] quantreg_5.94       SparseM_1.81        xml2_1.3.3          sandwich_3.0-2      labeling_0.4.2
# [26] scales_1.2.1        mvtnorm_1.1-3       digest_0.6.31       RSGHB_1.2.2         MCMCpack_1.6-3
# [31] pkgconfig_2.0.3     dbplyr_2.3.0        rlang_1.0.6         readxl_1.4.1        rstudioapi_0.14
# [36] generics_0.1.3      farver_2.1.1        zoo_1.8-11          jsonlite_1.8.4      googlesheets4_1.0.1
# [41] Matrix_1.5-3        munsell_0.5.0       fansi_1.0.3         lifecycle_1.0.3     stringi_1.7.12
# [46] MASS_7.3-56         plyr_1.8.8          grid_4.2.0          parallel_4.2.0      crayon_1.5.2
# [51] lattice_0.20-45     haven_2.5.1         splines_4.2.0       hms_1.1.2           pillar_1.8.1
# [56] randtoolbox_2.0.3   reprex_2.0.2        glue_1.6.2          modelr_0.1.10       vctrs_0.5.1
# [61] tzdb_0.3.0          miscTools_0.6-26    MatrixModels_0.5-1  cellranger_1.1.0    gtable_0.3.1
# [66] assertthat_0.2.1    broom_1.0.2         rngWELL_0.10-9      coda_0.19-4         survival_3.3-1
# [71] googledrive_2.0.0   gargle_1.2.1        maxLik_1.5-2        timechange_0.2.0    ellipsis_0.3.2



# **********************************************************
# Setup Environment: ####
# **********************************************************


library(tidyverse)
library(magrittr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggridges)
library(here)
library(data.table)
library(Rfast)


# **********************************************************
# Section 1: Import Data ####
# **********************************************************


## Import Respondents Data
Winter <- here("OtherData","Winter_dataframe_Step4.csv") %>% fread() %>% data.frame()


## Import new WTP
WTP <-
  here("CEoutput/ModelTwo",
       "Winter_MXL_ModelTwo_AllCorrelations_ConWTP.csv") %>% fread() %>% data.frame()


# **********************************************************
# Section 2: Truncate Data ####
# **********************************************************


## Drop missing rows
Winter <-  Winter[!is.na(Winter$MilesDistance), ] ## Drop missing distances
Winter <-  Winter[!is.na(Winter$Overall), ] ## Drop respondents not completing BIOWELL


## If the conditionals are imported then run this to recover only useful variables
WTP <- WTP[WTP %>% select(-ends_with(c(".ID", ".post.sd"))) %>% colnames()] %>% data.frame()



WTP <-
  bind_cols(
    "Colour_WTP_High" = WTP$b_Colour2.post.mean,
    "Colour_WTP_Medium" = WTP$b_Colour.post.mean,
    "Smell_WTP_High" = WTP$b_Smell2.post.mean,
    "Smell_WTP_Medium" = WTP$b_Smell.post.mean,
    "Sound_WTP_High" = WTP$b_Sound2.post.mean,
    "Sound_WTP_Medium" = WTP$b_Sound.post.mean,
    "Deadwood_WTP_High" = WTP$b_Deadwood2.post.mean,
    "Deadwood_WTP_Medium" = WTP$b_Deadwood.post.mean
  )



Data <- cbind(
  Winter[1:217],
  WTP
)


# **********************************************************
# Section 3A: In-text results ####
## Here we test whether WTP without impairment (==0) is greater
## than WTP with an impairment (==1)
# **********************************************************


## Colours:
OutputColours <- paste0("of colours (p = ", c(wilcox.test(Data[, "Colour_WTP_Medium"][Data$SightIssues == 0],
                                                          Data[, "Colour_WTP_Medium"][Data$SightIssues == 1], alternative = "greater")$p.value) %>% round(3),
                        " and ", c(wilcox.test(Data[, "Colour_WTP_High"][Data$SightIssues == 0],
                                               Data[, "Colour_WTP_High"][Data$SightIssues == 1], alternative = "greater")$p.value) %>% round(3),
                        ")")



## Smell:
OutputSmells <- paste0(", smells (p = ", c(wilcox.test(Data[,"Smell_WTP_Medium"][Data$SmellIssues==0],
                                                       Data[,"Smell_WTP_Medium"][Data$SmellIssues==1], alternative = "greater")$p.value) %>% round(3),
                       " and ", c(wilcox.test(Data[,"Smell_WTP_High"][Data$SmellIssues==0],
                                              Data[,"Smell_WTP_High"][Data$SmellIssues==1], alternative = "greater")$p.value) %>% round(3),
                       ")")


## Sounds:
OutputSounds <- paste0(", sounds (p = ", c(wilcox.test(Data[,"Sound_WTP_Medium"][Data$HearingIssues==0],
                                                          Data[,"Sound_WTP_Medium"][Data$HearingIssues==1], alternative = "greater")$p.value) %>% round(3),
                       " and ", c(wilcox.test(Data[,"Sound_WTP_High"][Data$HearingIssues==0],
                                              Data[,"Sound_WTP_High"][Data$HearingIssues==1], alternative = "greater")$p.value) %>% round(3),
                       ")")



## Deadwood:
OutputDeadwood <- paste0(", or deadwood for decomposition (p = ", c(wilcox.test(Data[, "Deadwood_WTP_Medium"][Data$Impairment == 0],
                                                            Data[, "Deadwood_WTP_Medium"][Data$Impairment == 1], alternative = "greater")$p.value) %>% round(3),
                         " and ", c(wilcox.test(Data[, "Deadwood_WTP_High"][Data$Impairment == 0],
                                                Data[, "Deadwood_WTP_High"][Data$Impairment == 1], alternative = "greater")$p.value) %>% round(3),
                         ")")


# **********************************************************
# Section 3B: Export results ####
# **********************************************************


## Output here
paste0("Mann-Whitney non-parametric one-sided tests of whether conditional mean WTP was higher without a relevant impairment found some statistical differences in WTP for increases to the variety ",
       OutputColours,
       OutputSmells,
       OutputSounds,
       OutputDeadwood,
       " for the ‘medium’ and ‘high’ levels respectively. This finding that having an impairment reduced WTP is illustrated in Figure BX of the appendix."
) %>% c() %>% list() %>% fwrite(
  sep = ",",
  here("OtherOutput/Tables", "TableX_ImpairmentsPoeTests.txt"),
  row.names = TRUE,
  quote = FALSE
)


# **********************************************************
# Section 4A: Setup table ####
# **********************************************************


## Classifies P values
AddStars <- function(Input) {
  ifelse(Input < 0.01,
         paste0(Input %>% round(3) %>% sprintf("%.3f", .), "***"),
         ifelse(Input < 0.05,
                paste0(Input %>% round(3) %>% sprintf("%.3f", .), "**"),
                ifelse(Input < 0.1,
                       paste0(Input %>% round(3) %>% sprintf("%.3f", .), "*"),
                       paste0(Input %>% round(3) %>% sprintf("%.3f", .), " "))))
}


Left <-  c(
  "Colour_WTP_High",
  "Colour_WTP_Medium",
  "Smell_WTP_High",
  "Smell_WTP_Medium",
  "Sound_WTP_High",
  "Sound_WTP_Medium",
  "Deadwood_WTP_High",
  "Deadwood_WTP_Medium"
)

Right <- c(
  "SightIssues",
  "SightIssues",
  "SmellIssues",
  "SmellIssues",

  "HearingIssues",
  "HearingIssues",

  "Impairment",
  "Impairment"
)


## Initialise here
results <- list()


## loop row by row to estimate
## (a) test and (b) effect size
for (i in seq_along(Left)) {
  Test <- wilcox.test(formula = as.formula(paste0(Left[i], " ~ ", Right[i])),
                      data = Data,
                      alternative = "greater")
  ES <- rstatix::wilcox_effsize(formula = as.formula(paste0(Left[i], " ~ ", Right[i])), data = Data, alternative = "greater")

  ## Stitch results together here
  results[[i]] <-     rbind(
    Test$data.name,
    ES$n1 %>% round(3),
    ES$n2 %>% round(3),
    Test$statistic %>% round(3),
    Test$p.value %>% AddStars(),
    ES$effsize %>% round(3)
  )
}

Output <- do.call(bind_cols, results) %>% t() %>% data.frame()


colnames(Output) <- c("Name",
                      "Number without",
                      "Number with",
                      "Statistic",
                      "P. value",
                      "Effect Size")



# **********************************************************
# Section 4B: Export results ####
# **********************************************************


## Output here
Output %>% fwrite(
  sep = "#",
  here("OtherOutput/Tables", "TableX_ImpairmentTests.txt"),
  row.names = TRUE,
  quote = FALSE
)



# End Of Script ****************************************************
