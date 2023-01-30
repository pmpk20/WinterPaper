#### RELATE Winter Paper ####
## Function: Literally plots one figure: distributions of each attributes' WTP
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 30/01/2023
## TODO: Trying to use foreach() to speed up the loop


#------------------------------
# Replication Information: ####
#------------------------------


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



#----------------------------------------------------------------------------------------------------------
# Setup Environment: ####
#----------------------------------------------------------------------------------------------------------


library(tidyverse)
library(magrittr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggridges)
library(here)
library(data.table)
library(Rfast)


#----------------------------------------------------------------------------------------------------------
# Section 1: Import Data ####
#----------------------------------------------------------------------------------------------------------


## The latest version with WTP appended is 2022_01_07
here()
Winter <- data.frame(fread(here("WinterReplication/OtherData","Winter_dataframe_Step3.csv")))


## This is the WTP from the model itself:
### Note: 1711 rows (one per respondent), 9001 variables (1000 per attribute level)
### Note: added all four datasets here so you can choose
# WTP <- data.frame(fread(here("WinterReplication/CEModelData","WP5_Winter_MXL_ModelOne_2022_07_29_WTP.csv")))
# WTP <- data.frame(fread(here("WinterReplication/CEModelData","WP5_Winter_MXL_ModelOne_2022_07_29_UCWTP.csv")))
# WTP <- data.frame(fread(here("WinterReplication/CEModelData","WP5_Winter_MXL_ModelTwo_2022_07_29_WTP.csv")))
WTP <- data.frame(fread(here("CEoutput/ModelTwo","Winter_MXL_ModelTwo_UnconWTP.csv")))


## If the conditionals are imported then run this to recover only useful variables
WTP <- WTP[WTP %>% select(-ends_with(c(".ID",".post.sd"))) %>% colnames()] %>% data.frame()

#----------------------------------------------------------------------------------------------------------
# Section 2: Summarise Unconditionals by attribute ####
#----------------------------------------------------------------------------------------------------------

## So this function calculates one stat per attribute per season
### So: rowmeans() not rowMeans() is actually insanely fast if you can be bothered to transform to and from matrices.
Summarizer <- function(i) {

  bind_cols(
    "y0"=  WTP %>% select(starts_with(i)) %>% summarise_all(quantile,c(0.05)) %>% as.matrix() %>% rowmeans(),
    "y25"= WTP %>% select(starts_with(i)) %>% summarise_all(quantile,c(0.25)) %>% as.matrix() %>% rowmeans(),
    "y50"= WTP %>% select(starts_with(i)) %>% summarise_all(mean) %>% as.matrix() %>% rowmeans(),
    "y75"= WTP %>% select(starts_with(i)) %>% summarise_all(quantile,c(0.75)) %>% as.matrix() %>% rowmeans() ,
    "y100"=WTP %>% select(starts_with(i)) %>% summarise_all(quantile,c(0.95)) %>% as.matrix() %>% rowmeans()) %>% data.frame()

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
### There must be a way to use foreach() but I can't figure out how to include my custom function
for (i in 1:length(Attribute)){
  Summaries[i,] <- Summarizer(Attribute[i])

}



#----------------------------------------------------------------------------------------------------------
#### Section 2B: Additional wrangling and setup of labels ####
#----------------------------------------------------------------------------------------------------------


## I want this order on the plot:
Names <- c("Tax",
           "ColourHigh",
           "ColourMedium",
           "SmellHigh",
           "SmellMedium",
           "SoundHigh",
           "SoundMedium",
           "DeadwoodHigh",
           "DeadwoodMedium")



## Label X axis of ggplot boxplots
## Using gsub() to add spaces to the list
Names <- gsub(pattern = "Deadwood",replacement = "Decomposing Trees ",x = Names)


Labels <- gsub(pattern = "Medium",replacement = "\n Medium",x = Names) %>%
  gsub(pattern = "High",replacement = "\n High") %>%
  c()

## Bind the results with variable and season ID for ease later
NewerData <- bind_cols(
  Summaries,
  "variable"=Names)

colnames(NewerData) <- c("y0",
             "y25",
             "y50",
             "y75",
             "y100","variable")


#----------------------------------------------------------------------------------------------------------
#### Section 3: Create Plot ####
#----------------------------------------------------------------------------------------------------------

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

Figure2 <-
  ggplot(NewerData,aes(x=rev(variable), fill=as.factor(variable))) +
  geom_boxplot(varwidth = 0.5,outlier.shape = NA,
               aes(
                 ymin=y0,
                 lower=y25,
                 middle=y50,
                 upper=y75,
                 ymax=y100,
               ),stat="identity")+
  scale_x_discrete(name="Attribute",label=rev(Labels),limits= Names)+
  theme_bw()+geom_hline(yintercept=0)+
  ylab("Marginal WTP in \U00a3 GBP per Household per annum in local tax.")+
  scale_y_continuous(limits=c(-10,25)
                     ,breaks = seq(-10,25,1))+
  scale_fill_manual(name="Attributes",
                    label=Labels,
                    values=Colours)+
  theme(legend.position = "none",
        legend.background=element_blank(),
        legend.box.background = element_rect(colour="black"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank())+coord_flip()


#----------------------------------------------------------------------------------------------------------
#### Section 3: Export Plot ####
#----------------------------------------------------------------------------------------------------------


## Okay this is silly but I like filenames with "_" not "-"
### Anyway the point is to add a date to the plot to know which version we're using
Date <- gsub(pattern = "-",replacement = "_",Sys.Date())


## Save output in highest DPI
ggsave(Figure2, device = "png",
       filename = paste0(here(),"/WinterReplication/","Winter_Figure2_BW_",Date,".png"),
       width=20,height=15,units = "cm",dpi=500)


# End Of Script ----------------------------------------------------------------
