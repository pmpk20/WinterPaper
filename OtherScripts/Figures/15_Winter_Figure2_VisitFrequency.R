#### RELATE WP5: Recovering unconditionals  ###############
# Script author: Peter King (p.m.king@kent.ac.uk)
# Last Edited: 03/02/2023
# Change: Removing figure minor y lines and adding pound sign.
# Description: So here we take all the unconditionals and use them to specify the
## mean, max, min, and quantiles for the distribution of each attribute in each
## VisitFrequency. So unconditionals > moments > summary > create plot.


# ***************************************************************************
#### Section 0: Setup and estimate models ####
# ***************************************************************************



## sessionInfo() for Replicability ------------------------------------------------------------------------------
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
#   [1] forcats_0.5.2      purrr_1.0.1        readr_2.1.3        tidyr_1.2.1        tibble_3.1.8       tidyverse_1.3.2    ggdist_3.2.1
# [8] matrixStats_0.63.0 Rfast_2.0.6        RcppZiggurat_0.1.6 Rcpp_1.0.9         lubridate_1.9.0    timechange_0.2.0   PostcodesioR_0.3.1
# [15] stringi_1.7.12     stringr_1.5.0      data.table_1.14.6  mded_0.1-2         reshape2_1.4.4     ggridges_0.5.4     ggplot2_3.4.0
# [22] magrittr_2.0.3     dplyr_1.0.10       apollo_0.2.8       here_1.0.1
#
# loaded via a namespace (and not attached):
#   [1] mcmc_0.9-7           fs_1.6.0             doParallel_1.0.17    RColorBrewer_1.1-3   httr_1.4.4           rprojroot_2.0.3
# [7] numDeriv_2016.8-1.1  tools_4.2.0          backports_1.4.1      utf8_1.2.2           R6_2.5.1             DBI_1.1.3
# [13] colorspace_2.0-3     withr_2.5.0          tidyselect_1.2.0     mnormt_2.1.1         compiler_4.2.0       rvest_1.0.3
# [19] cli_3.6.0            quantreg_5.94        SparseM_1.81         xml2_1.3.3           sandwich_3.0-2       scales_1.2.1
# [25] mvtnorm_1.1-3        digest_0.6.31        rmarkdown_2.20       RSGHB_1.2.2          MCMCpack_1.6-3       pkgconfig_2.0.3
# [31] htmltools_0.5.4      dbplyr_2.3.0         fastmap_1.1.0        readxl_1.4.1         rlang_1.0.6          rstudioapi_0.14
# [37] farver_2.1.1         generics_0.1.3       jsonlite_1.8.4       zoo_1.8-11           googlesheets4_1.0.1  distributional_0.3.1
# [43] Matrix_1.5-3         munsell_0.5.0        fansi_1.0.3          lifecycle_1.0.3      yaml_2.3.6           MASS_7.3-56
# [49] plyr_1.8.8           grid_4.2.0           crayon_1.5.2         lattice_0.20-45      haven_2.5.1          splines_4.2.0
# [55] hms_1.1.2            knitr_1.41           pillar_1.8.1         randtoolbox_2.0.3    codetools_0.2-18     reprex_2.0.2
# [61] glue_1.6.2           evaluate_0.20        modelr_0.1.10        tzdb_0.3.0           vctrs_0.5.1          foreach_1.5.2
# [67] miscTools_0.6-26     MatrixModels_0.5-1   cellranger_1.1.0     gtable_0.3.1         assertthat_0.2.1     xfun_0.36
# [73] broom_1.0.2          rngWELL_0.10-9       coda_0.19-4          survival_3.3-1       googledrive_2.0.0    gargle_1.2.1
# [79] iterators_1.0.14     maxLik_1.5-2         ellipsis_0.3.2



## Libraries ------------------------------------------------------------------------------

# install.packages("data.table",repos="http://cran.us.r-project.org")
# options(scipen=90)
library(apollo)
library(tidyverse)
library(dplyr)
library(magrittr)
library(mded)
library(here)
library(data.table)
library(Rfast)


# ***************************************************************************
#### Section 1: Setup parallelisation and benchmarking ####
# ***************************************************************************



## Step4 has the right number of rows
Winter <- here("OtherData","Winter_dataframe_Step4.csv") %>% fread() %>% data.frame()


## For various reasons I'm dropping the existing WTP to re-add it here
## This makes fixing the rest of the code easier
Winter <- Winter[,1:217]

## Truncate to have same number of rows as in the models
Winter <- Winter[!is.na(Winter$MilesDistance), ] ## Drop missing distances
Winter <- Winter[!is.na(Winter$Overall), ] ## Drop respondents not completing BIOWELL




## This is the WTP from the model itself:
### Note: 1711 rows (one per respondent), 9001 variables (1000 per attribute level)
### Note: added all four datasets here so you can choose
# WTP <- data.frame(fread(here("WinterReplication/CEModelData","WP5_Winter_MXL_ModelOne_2022_07_29_WTP.csv")))
# WTP <- data.frame(fread(here("WinterReplication/CEModelData","WP5_Winter_MXL_ModelOne_2022_07_29_UCWTP.csv")))
# WTP <- data.frame(fread(here("WinterReplication/CEModelData","WP5_Winter_MXL_ModelTwo_2022_07_29_WTP.csv")))
WTP <- here("CEoutput/ModelTwo","Winter_MXL_ModelTwo_UnconWTP.csv") %>% fread() %>% data.frame()

## If the conditionals are imported then run this to recover only useful variables
WTP <- WTP[WTP %>% select(-ends_with(c(".ID",".post.sd"))) %>% colnames()] %>% data.frame()


## Combine all for ease:
WinterWTPCombined <- cbind(Winter, WTP)


# ***************************************************************************
#### Section 2: Define useful variables ####
# ***************************************************************************


## So I'll use these later to name rows
Names <- c("Tax",
           "ColourMedium",
           "ColourHigh",
           "SoundMedium",
           "SoundHigh",
           "SmellMedium",
           "SmellHigh",
           "DeadwoodMedium",
           "DeadwoodHigh")

# ## Label X axis of ggplot boxplots
# Labels <- c("Tax",
#             "Colour\n Medium","Colour\n High",
#             "Sound\n Medium","Sound\n High",
#             "Smell\n Medium", "Smell\n High",
#             "Deadwood\n Medium","Deadwood\n High")


# ## New version
Labels <- c("Tax",
            "Colours:\n medium","Colours:\n high",
            "Sounds:\n medium","Sounds:\n high",
            "Smells:\n medium", "Smells:\n high",
            "Deadwood:\ndecomposition\n medium","Deadwood:\ndecomposition\n high")


## Label grouping variable
VisitFrequency <- c(0,1,2,3,4,5)


LegendLabels <- c(
  paste0("I do not visit\n(N = ",Winter %>% filter(MostRecentVisit==0) %>% nrow(),")"),
  paste0("Once or twice a season\n(N = " ,Winter %>% filter(MostRecentVisit==1) %>% nrow(),")"),
  paste0("Once or twice a month\n(N = ",Winter %>% filter(MostRecentVisit==2) %>% nrow(),")"),
  paste0("Once a week\n(N = " ,Winter %>% filter(MostRecentVisit==3) %>% nrow(),")"),
  paste0("Several times a week\n(N = " ,Winter %>% filter(MostRecentVisit==4) %>% nrow(),")"),
  paste0("Every day\n(N = " ,Winter %>% filter(MostRecentVisit==5) %>% nrow(),")"))


# ***************************************************************************
#### Section 2B or most likely not 2B: Trying to make the tails longer by using the entire distribution ####
# ***************************************************************************


## So this function calculates one stat per attribute per VisitFrequency
### So: rowmeans() not rowMeans() is actually insanely fast if you can be bothered to transform to and from matrices.
Summarizer <- function(Attribute) {

  bind_cols(
    "y0"=  WinterWTPCombined %>% select(starts_with(Attribute),"MostRecentVisit")  %>% group_by(MostRecentVisit) %>% summarise_all(quantile,c(0.05)) %>% as.matrix() %>% rowmeans(),
    "y25"= WinterWTPCombined %>% select(starts_with(Attribute),"MostRecentVisit")  %>% group_by(MostRecentVisit) %>% summarise_all(quantile,c(0.25)) %>% as.matrix() %>% rowmeans(),
    "y50"= WinterWTPCombined %>% select(starts_with(Attribute),"MostRecentVisit")  %>% group_by(MostRecentVisit) %>% summarise_all(median) %>% as.matrix() %>% rowmeans(),
    "y75"= WinterWTPCombined %>% select(starts_with(Attribute),"MostRecentVisit")  %>% group_by(MostRecentVisit) %>% summarise_all(quantile,c(0.75)) %>% as.matrix() %>% rowmeans() ,
    "y100"=WinterWTPCombined %>% select(starts_with(Attribute),"MostRecentVisit")  %>% group_by(MostRecentVisit) %>% summarise_all(quantile,c(0.95)) %>% as.matrix() %>% rowmeans()) %>% data.frame()

}


## Binds the results for each attribute; maybe we can loop through this.
Summaries <- bind_rows(
  Summarizer("beta_Tax"),
  Summarizer("b_Colour."),
  Summarizer("b_Colour2."),
  Summarizer("b_Sound."),
  Summarizer("b_Sound2."),
  Summarizer("b_Smell."),
  Summarizer("b_Smell2."),
  Summarizer("b_Deadwood."),
  Summarizer("b_Deadwood2."))


## Bind the results with variable and MostRecentVisit ID for ease later
NewerData <- bind_cols(
  "variable"=rep(Names,each=6),
  "MostRecentVisit"=rep(0:5,times=9),
  Summaries)


## Without explicitly telling GGPLOT the factor levels it will plot in a horrible order!
NewerData$variable <- factor(NewerData$variable,levels=unique(NewerData$variable))



NewerData %>%
  fwrite(sep=",",
         here("OtherOutput",
              "FigureS1_PlotData.csv"))

# ***************************************************************************
#### Section 3: Create Plot ####
# ***************************************************************************


## Import direct plot data here:
NewerData <- here("OtherOutput", "FigureS1_PlotData.csv") %>% fread() %>% data.frame()
#
# # Old version
# Figure2_VisitFrequency <-
#   ggplot(NewerData, aes(x = rev(variable), fill = as.factor(MostRecentVisit))) +
#
#   geom_boxplot(
#     varwidth = 0.5,
#     outlier.shape = NA,
#     aes(
#       ymin = y0,
#       lower = y25,
#       middle = y50,
#       upper = y75,
#       ymax = y100,
#     ),
#     stat = "identity"
#   )+
#   scale_x_discrete(name = "Attribute",
#                    label = rev(Labels),
#                    limits = Names) +
#   theme_bw() +
#   geom_hline(yintercept = 0) +
#   ylab("WTP (GBP) via local council tax, per household, per annum") +
#   scale_y_continuous(limits = c(-10, 25)
#                      , breaks = seq(-10, 25, 1)) +
#   scale_fill_brewer(
#     name = "Visit Frequency",
#     type = "seq",
#     label = LegendLabels,
#     guide = guide_legend(reverse = FALSE)
#   ) +
#   theme(
#     legend.position = "bottom",
#     legend.background = element_blank(),
#     legend.box.background = element_rect(colour = "black"),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor.y = element_blank()
#   ) +
#   coord_flip()



## Reshape here so that I can add the errorbars
NewerData_Pivoted <- NewerData %>% pivot_longer(cols = y0:y100)


## THis is an important step for the correct plot order!
NewerData_Pivoted$variable <-
  factor(NewerData_Pivoted$variable, levels = unique(NewerData_Pivoted$variable))


## Better way with working error bars!
Figure2_VisitFrequency <- ggplot(NewerData_Pivoted, aes(
  x = rev(variable),
  y = value,
  fill = as.factor(MostRecentVisit)
)) +
  stat_boxplot(geom = "errorbar", width = 0.25, position = position_dodge(width = 0.75)) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(name = "Attribute",
                   label = rev(Labels),
                   limits = Names) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  ylab("WTP (GBP) via local council tax, per household, per annum") +
  scale_y_continuous(limits = c(-10, 20)
                     , breaks = seq(-10, 20, 5)) +
  scale_fill_brewer(
    name = "Visit Frequency",
    type = "seq",
    label = LegendLabels,
    guide = guide_legend(reverse = FALSE)
  ) +
  theme(
    legend.position = "bottom",
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(size = 10,
                               colour = "black"), ## Change text to be clearer for reader
    axis.text.y = element_text(size = 10,
                               colour = "black")
  ) +
  coord_flip()


# ***************************************************************************
#### Section 3: Export Plot ####
# ***************************************************************************


## Save with high DPI in the right place. I think png is the right format.
ggsave(
  Figure2_VisitFrequency,
  device = "png",
  filename = paste0(
    here(),
    "/OtherOutput/Figures/",
    "Figure2_VisitFrequency.png"
  ),
  width = 20,
  height = 15,
  units = "cm",
  dpi = 500
)

# End Of Script -----------------------------------------------------------
