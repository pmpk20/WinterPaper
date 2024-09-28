#### RELATE Winter Paper ####
## Function: Literally plots one figure: distributions of each attributes' WTP
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 20/10/2023
## Changes:
## - Added option to import data in. Fixed label formatting for Zoe.
## - Changed y axis label
## - rewrote Summarizer() to select() once not five times


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


## Use step three and add unconditionals rather than use Step4.csv with conditionals
here()
Winter <- here("OtherData","Winter_dataframe_Step3.csv") %>% fread() %>% data.frame()


## This is the WTP from the model itself:
### Note: 1711 rows (one per respondent), 9001 variables (1000 per attribute level)
### Note: added all four datasets here so you can choose
# WTP <- data.frame(fread(here("WinterReplication/CEModelData","WP5_Winter_MXL_ModelOne_2022_07_29_WTP.csv")))
# WTP <- data.frame(fread(here("WinterReplication/CEModelData","WP5_Winter_MXL_ModelOne_2022_07_29_UCWTP.csv")))
# WTP <- data.frame(fread(here("WinterReplication/CEModelData","WP5_Winter_MXL_ModelTwo_2022_07_29_WTP.csv")))
WTP <- here("CEoutput/ModelTwo", "Winter_MXL_ModelTwo_UnconWTP.csv") %>% fread() %>% data.frame()


## If the conditionals are imported then run this to recover only useful variables
WTP <- WTP[WTP %>% select(-ends_with(c(".ID", ".post.sd"))) %>% colnames()] %>% data.frame()


# **********************************************************
# Section 2: Summarise Unconditionals by attribute ####
# **********************************************************

## So this function calculates one stat per attribute per season
### So: rowmeans() not rowMeans() is actually insanely fast if you can be bothered to transform to and from matrices.
## This function selects the variable from WTP and calculates summary stats from it.
Summarizer <- function(i) {

  Test <- WTP %>% select(starts_with(i))

  bind_cols(
    "y0" = Test %>% summarise_all(quantile, c(0.05)) %>% as.matrix() %>% rowmeans(),
    "y25" = Test %>% summarise_all(quantile, c(0.25)) %>% as.matrix() %>% rowmeans(),
    "y50" = Test %>% summarise_all(median) %>% as.matrix() %>% rowmeans(),
    "y75" = Test %>% summarise_all(quantile, c(0.75)) %>% as.matrix() %>% rowmeans() ,
    "y100" = Test %>% summarise_all(quantile, c(0.95)) %>% as.matrix() %>% rowmeans()) %>% data.frame()

}


## List of variable names
Attribute <- c(
  "beta_Tax",
  "b_Colour2.",
  "b_Colour.",
  "b_Smell2.",
  "b_Smell.",
  "b_Sound2.",
  "b_Sound.",
  "b_Deadwood2.",
  "b_Deadwood."
)


## Initialising data here to make it faster
Summaries <- matrix(0,length(Attribute),5) %>% data.frame()


## Loop through each variable and produce summaries.
## If you have time rewrite this with foreach() using .export=c("Summarizer")
for (i in 1:length(Attribute)) {
  Summaries[i,] <- Summarizer(Attribute[i])

}




# **********************************************************
#### Section 2B: Additional wrangling and setup of labels ####
# **********************************************************


## I want this order on the plot:
Names <- c(
  "Tax",
  "ColourHigh",
  "ColourMedium",
  "SmellHigh",
  "SmellMedium",
  "SoundHigh",
  "SoundMedium",
  "Deadwood\nDecomposition High",
  "Deadwood\nDecomposition Medium"
)



## Label X axis of ggplot boxplots
## Using gsub() to add spaces to the list
# Names <-
  # gsub(pattern = "Deadwood",
  #      replacement = "Deadwood\nDecomposition ",
  #      x = Names)


Labels <-
  Names %>% gsub(pattern = "Decomposition",
       replacement = "decomposition") %>%
  gsub(pattern = "Medium", replacement = "\nmedium") %>%
  gsub(pattern = "High", replacement = "\nhigh") %>%
  gsub(pattern = "Colour", replacement = "Colours:") %>%
  gsub(pattern = "Smell", replacement = "Smells:") %>%
  gsub(pattern = "Sound", replacement = "Sounds:") %>%
  gsub(pattern = "Deadwood", replacement = "Deadwood:") %>%
  c()


## Bind the results with variable and season ID for ease later
NewerData <- bind_cols(Summaries,
                       "variable" = Names)


## Clearly label which column represents which stat
colnames(NewerData) <- c("y0",
                         "y25",
                         "y50",
                         "y75",
                         "y100", "variable")


NewerData %>%
  fwrite(sep=",",
         here("OtherOutput",
              "Figure2_PlotData.csv"))


# **********************************************************
#### Section 3: Create Plot ####
# **********************************************************


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

## Direct import here
# NewerData <- here("OtherOutput", "Figure2_PlotData.csv") %>% fread() %>% data.frame()


## here is the final plot I use.
## I put Newerdata in then use the columns to specify the points of the boxplot
Figure2 <-
  ggplot(NewerData, aes(x = rev(variable), fill = as.factor(variable)))+
  geom_errorbar(aes(
    ymin = y0,
    ymax = y100,
  ),width = 0.2)+ ## errorbar means you can have the nicer whiskers
  geom_boxplot(
    varwidth = 0.5,
    outlier.shape = NA,
    aes(
      ymin = y0,
      lower = y25,
      middle = y50,
      upper = y75,
      ymax = y100,
    ),
    stat = "identity" ## means you can specify moments as in AES()
  ) +
  scale_x_discrete(name = "Attribute and level",
                   label = rev(Labels),
                   limits = Names) + ## Using rev() to make the order I want
  theme_bw() + ## Just looks nicer imo
  geom_hline(yintercept = 0) + ## I like the zero line for ease of comparison
  ylab("Marginal WTP (GBP) in local council tax, per household, per annum") +
  scale_y_continuous(breaks = seq(-10, 20, 5)) +
  scale_fill_manual(name = "Attributes",
                    label = Labels,
                    values = Colours) +
  theme(
    legend.position = "none",
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
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
  coord_flip(ylim = c(-10, 20))


# **********************************************************
#### Section 3: Export Plot ####
## Removed sys.date() and changed save location
# **********************************************************



## Save output in highest DPI
ggsave(
  Figure2,
  device = "png",
  filename = paste0(
    here(),
    "/OtherOutput/Figures/",
    "Winter_Figure2_BW.png"
  ),
  width = 20,
  height = 15,
  units = "cm",
  dpi = 500
)


# End Of Script ****************************************************
