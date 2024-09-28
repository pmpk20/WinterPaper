#### RELATE Winter Paper ####
## Function: Calculates Table 5 WTP estimates
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 25/02/2024
# Changes:
# - newer WTP


# *****************************
# Replication Information: ####
# *****************************


# R version 4.2.0 (2022-04-22 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19044)
#   [1] LC_COLLATE=English_United Kingdom.utf8  LC_CTYPE=English_United Kingdom.utf8
#   [1] here_1.0.1     reshape2_1.4.4 ggridges_0.5.3 ggplot2_3.3.6  magrittr_2.0.3 dplyr_1.0.9    apollo_0.2.7
#   [1] zoo_1.8-10          tidyselect_1.1.2    purrr_0.3.4         splines_4.2.0       lattice_0.20-45
# [6] colorspace_2.0-3    generics_0.1.2      vctrs_0.4.1         MCMCpack_1.6-3      utf8_1.2.2
# [11] survival_3.3-1      rlang_1.0.2         pillar_1.7.0        withr_2.5.0         glue_1.6.2
# [16] plyr_1.8.7          matrixStats_0.62.0  lifecycle_1.0.1     stringr_1.4.0       MatrixModels_0.5-0
# [21] munsell_0.5.0       gtable_0.3.0        mvtnorm_1.1-3       coda_0.19-4         miscTools_0.6-26
# [26] SparseM_1.81        RSGHB_1.2.2         quantreg_5.93       parallel_4.2.0      fansi_1.0.3
# [31] Rcpp_1.0.8.3        scales_1.2.0        tmvnsim_1.0-2       farver_2.1.0        mcmc_0.9-7
# [36] maxLik_1.5-2        mnormt_2.0.2        digest_0.6.29       stringi_1.7.6       rprojroot_2.0.3
# [41] numDeriv_2016.8-1.1 grid_4.2.0          cli_3.3.0           tools_4.2.0         sandwich_3.0-1
# [46] tibble_3.1.7        crayon_1.5.1        pkgconfig_2.0.3     MASS_7.3-56         ellipsis_0.3.2
# [51] Matrix_1.4-1        randtoolbox_1.31.1  R6_2.5.1            rngWELL_0.10-7      compiler_4.2.0


# *****************************
# Setup Environment: ####
# *****************************


library(magrittr)
library(dplyr)
library(apollo)
library(reshape2)
library(here)
library(data.table)

# *****************************
# Section 1: Import Data ####
# *****************************


WTP <-
  here("CEoutput/ModelTwo",
       "Winter_MXL_ModelTwo_AllCorrelations_ConWTP.csv") %>% fread() %>% data.frame()


# *****************************
# Section 2: Arrange Data ####
# *****************************


## Define a function that summarises median, SD, and 95% by variable
calculate_summary_table <- function(data, columns) {
  Upper <- WTP[, paste0(i, "post.mean")] %>% quantile(c(0.975)) %>% round(3) %>% sprintf("%.3f", .)
  Lower <- WTP[, paste0(i, "post.mean")] %>% quantile(c(0.025)) %>% round(3) %>% sprintf("%.3f", .)


  cbind(
    "Variable" = columns,
    "Median" = WTP[, paste0(i, "post.mean")] %>% median() %>% round(3) %>% sprintf("%.3f", .),
    "SD" = WTP[, paste0(i, "post.sd")] %>% median() %>% round(3) %>% sprintf("%.3f", .),
    "95% C-I" = paste0("(",Lower, ") - (", Upper, ")")
) %>% data.frame() %>% return()
}


## List of attributes to loop through
Variables <- c(
  "beta_Tax.",
  "b_Colour2.",
  "b_Colour.",
  "b_Smell2.",
  "b_Smell.",
  "b_Sound2.",
  "b_Sound.",
  "b_Deadwood2.",
  "b_Deadwood."
)



# Initialize an empty list to store the results
summary_results <- list()

# The actual loop
for (i in Variables){
  summary_results[[i]] <- calculate_summary_table(WTP, i)
}


## Rearrange here
Table3 <- do.call(rbind, summary_results)


Table3$Variable <-
  c(
    "Tax",
    "Colour: high",
    "Colour: medium",

    "Smell: high",
    "Smell: medium",

    "Sound: high",
    "Sound: medium",

    "Deadwood: high",
    "Deadwood: medium"
    )




# *****************************
# Section 3: Export Data ####
# *****************************


## Output to console for easy ctrl C+V
write.csv(Table3, quote = FALSE)


# Output to correct location
Table3 %>% fwrite(
  sep = ",",
  here("OtherOutput/Tables", "Table3_WTPSummary.txt"),
  row.names = TRUE,
  quote = FALSE
)




# End Of Script ------------------------------
