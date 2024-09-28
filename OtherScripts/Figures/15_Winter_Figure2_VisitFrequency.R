#### RELATE WP5: Plot WTP by visit frequency  ###############
# Script author: Peter King (p.king1@leeds.ac.uk)
# Last Edited: 25/02/2024
# Change: major change to use conditionals instead of unconditionals
## - using correlated model

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





## **************************
## Import Survey Data
## **************************
## Use step three and add unconditionals rather than use Step4.csv with conditionals
here()
Winter <- here("OtherData","Winter_dataframe_Step4.csv") %>% fread() %>% data.frame()
## For various reasons I'm dropping the existing WTP to re-add it here
## This makes fixing the rest of the code easier
Winter <- Winter[,1:217]

## Truncate to have same number of rows as in the models
Winter <- Winter[!is.na(Winter$MilesDistance), ] ## Drop missing distances
Winter <- Winter[!is.na(Winter$Overall), ] ## Drop respondents not completing BIOWELL



## **************************
## Import WTP
## **************************

## This is the WTP from the model itself:
WTP <- here("CEoutput/ModelTwo", "Winter_MXL_ModelTwo_ConWTP.csv") %>% fread() %>% data.frame()
WTP <- here("CEoutput/ModelTwo", "Winter_MXL_ModelTwo_AllCorrelations_ConWTP.csv") %>% fread() %>% data.frame()

## If the conditionals are imported then run this to recover only useful variables
WTP <- WTP[WTP %>% select(-ends_with(c(".ID", ".post.sd"))) %>% colnames()] %>% data.frame()



WTP <-
  bind_cols(
    "ColourHigh" = WTP$b_Colour2.post.mean,
    "ColourMedium" = WTP$b_Colour.post.mean,
    "SmellHigh" = WTP$b_Smell2.post.mean,
    "SmellMedium" = WTP$b_Smell.post.mean,
    "SoundHigh" = WTP$b_Sound2.post.mean,
    "SoundMedium" = WTP$b_Sound.post.mean,
    "DeadwoodHigh" = WTP$b_Deadwood2.post.mean,
    "DeadwoodMedium" = WTP$b_Deadwood.post.mean,

    "MostRecentVisit" = Winter$MostRecentVisit
  )

# ***************************************************************************
#### Section 2: Define useful variables ####
# ***************************************************************************


## So I'll use these later to name rows
Names <- c(
  "Tax",
  "ColourHigh",
  "ColourMedium",
  "SmellHigh",
  "SmellMedium",
  "SoundHigh",
  "SoundMedium",
  "DeadwoodHigh",
  "DeadwoodMedium"
)

# ## Label X axis of ggplot boxplots
# Labels <- c("Tax",
#             "Colour\n Medium","Colour\n High",
#             "Sound\n Medium","Sound\n High",
#             "Smell\n Medium", "Smell\n High",
#             "Deadwood\n Medium","Deadwood\n High")


# ## New version
Labels <- c(
  "Tax",
  "Colours:\n high",
  "Colours:\n medium",
  "Smells:\n high",
  "Smells:\n medium",
  "Sounds:\n high",
  "Sounds:\n medium",
  "Deadwood:\ndecomposition\n high",
  "Deadwood:\ndecomposition\n medium"
)

## Label grouping variable
VisitFrequency <- c(0, 1, 2, 3, 4, 5)


LegendLabels <- c(
  paste0("I do not visit\n(N = ",Winter %>% filter(MostRecentVisit==0) %>% nrow(),")"),
  paste0("Once or twice a season\n(N = " ,Winter %>% filter(MostRecentVisit==1) %>% nrow(),")"),
  paste0("Once or twice a month\n(N = ",Winter %>% filter(MostRecentVisit==2) %>% nrow(),")"),
  paste0("Once a week\n(N = " ,Winter %>% filter(MostRecentVisit==3) %>% nrow(),")"),
  paste0("Several times a week\n(N = " ,Winter %>% filter(MostRecentVisit==4) %>% nrow(),")"),
  paste0("Every day\n(N = " ,Winter %>% filter(MostRecentVisit==5) %>% nrow(),")"))


## I got these from RColorBrewer but adding specifically here to
### (a) see the colours in the console with new RStudio,
### (b) make scale_fill_manual() easier
Colours <- c(
  "#C6DBEF",
  "#C6DBEF",
  "#08306B",
  "#08306B",
  "#6BAED6",
  "#6BAED6",
  "#2171B5",
  "#2171B5",
  "#F7FBFF"
)

## Update all text sizes here for consistency
TextSize <- 12
# ***************************************************************************
#### Section 3: Create Data ####
# ***************************************************************************


PlotData <- WTP %>%
  pivot_longer(1:8) %>%
  mutate("YMIN" = quantile(value, c(0.025)),
         "YMAX" = quantile(value, c(0.975)))

## THis is an important step for the correct plot order!
PlotData$name <-
  factor(PlotData$name,
         levels = unique(PlotData$name))


# ***************************************************************************
#### Section 3: Create Plot ####
# ***************************************************************************



## Better way with working error bars!
Figure2_VisitFrequency <- ggplot(PlotData,
                                 aes(
                                   x = value,
                                   y = rev(name),
                                   fill = MostRecentVisit %>% as.factor()
                                 )) +
  stat_boxplot(geom = "errorbar",
               width = 0.5,
               position = position_dodge(width = 0.75)) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  geom_vline(xintercept = 0, alpha = 0.5) +
  scale_x_continuous(name = "Marginal WTP (GBP) in local council tax, per household, per annum",
                     breaks = seq(-50, 50, 5)) +
  scale_y_discrete(name = "Attribute and level", labels = rev(Labels)) +
  scale_fill_brewer(
    name = "Visit\nFrequency",
    type = "seq",
    label = LegendLabels,
    guide = guide_legend(reverse = FALSE)
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10,
                               colour = "black",
                               family = "serif"),
    legend.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.title = element_text(size = TextSize,
                                colour = "black",
                                family = "serif"),
    axis.text.x = element_text(size = TextSize,
                               colour = "black",
                               family = "serif"), ## Change text to be clearer for reader
    axis.text.y = element_text(size = TextSize,
                               colour = "black",
                               family = "serif"),
    axis.title.y = element_text(size = TextSize,
                                colour = "black",
                                family = "serif"),
    axis.title.x = element_text(size = TextSize,
                                colour = "black",
                                family = "serif")
  ) +
  coord_cartesian(xlim = c(-15, 30))


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
  width = 25,
  height = 20,
  units = "cm",
  dpi = 500
)

# End Of Script -----------------------------------------------------------
