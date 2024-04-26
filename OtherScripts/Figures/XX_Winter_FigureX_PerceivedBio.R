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

## Truncate to have same number of rows as in the models
Winter <- Winter[!is.na(Winter$MilesDistance), ] ## Drop missing distances
Winter <- Winter[!is.na(Winter$Overall), ] ## Drop respondents not completing BIOWELL



# ***************************************************************************
#### Section 2: Define useful variables ####
# ***************************************************************************


## So I'll use these later to name rows
Names <- c(
  "WoodlandsColours",
  "WoodlandsSmells",
  "WoodlandsSounds",
  "WoodlandsTree",
  "WoodlandsScore"
)

# ## Label X axis of ggplot boxplots
# Labels <- c("Tax",
#             "Colour\n Medium","Colour\n High",
#             "Sound\n Medium","Sound\n High",
#             "Smell\n Medium", "Smell\n High",
#             "Deadwood\n Medium","Deadwood\n High")


# ## New version
Labels <- c(
  "Colours",
  "Smells",
  "Sounds",
  "Deadwood\ndecomposition",
  "Overall\nmeasure"
)


## I got these from RColorBrewer but adding specifically here to
### (a) see the colours in the console with new RStudio,
### (b) make scale_fill_manual() easier
Colours <- c(
  "#C6DBEF",
  "#08306B",
  "#6BAED6",
  "#2171B5",
  "#F7FBFF"
)

## Update all text sizes here for consistency
TextSize <- 12


# ***************************************************************************
#### Section 3: Create Data ####
# ***************************************************************************


PlotData <- Winter %>%
  select(all_of(Names)) %>%
  pivot_longer(cols = 1:5)

## THis is an important step for the correct plot order!
PlotData$name <-
  factor(PlotData$name,
         levels = unique(PlotData$name))


# ***************************************************************************
#### Section 4: Create Plot ####
# ***************************************************************************


FigureX_PerceivedBio <- ggplot(PlotData,
                               aes(
                                 x = value,
                                 y = name,
                                 fill = name,
                                 group = name
                               )) +
  ggdist::stat_histinterval() +
  scale_y_discrete(name = "Perceived biodiversity measure",
                   label = Labels,
                   limits = Names) +
  theme_bw() +
  geom_vline(xintercept = 50, alpha = 0.5) +
  scale_x_continuous(name = "",
                     breaks = seq(0, 100, 10)) +
  scale_fill_brewer(
    name = "Perceived biodiversity",
    type = "seq",
    label = Labels,
    guide = guide_legend(reverse = TRUE)
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
  )


# ***************************************************************************
#### Section 3: Export Plot ####
# ***************************************************************************


## Save with high DPI in the right place. I think png is the right format.
ggsave(
  FigureX_PerceivedBio,
  device = "png",
  filename = paste0(
    here(),
    "/OtherOutput/Figures/",
    "FigureX_PerceivedBio.png"
  ),
  width = 25,
  height = 20,
  units = "cm",
  dpi = 500
)

# End Of Script -----------------------------------------------------------
