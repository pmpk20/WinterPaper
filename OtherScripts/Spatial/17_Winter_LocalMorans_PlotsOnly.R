#### RELATE Winter Paper ####
## Function: Plots Local Moran Hotspots
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 20/10/2023
## TODO: My god this is a mess but it works
## Note: This may no longer work as I've anonymised all the data


# *****************************
# Replication Information: ####
# *****************************


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



# *****************************
# Setup Environment: ####
# *****************************


library(plyr) # Watch for this causing problems against dplyr
library(dplyr)
library(tidyverse)
library(magrittr)
library(reshape2)
library(ggplot2)
library(ggridges)
library(here)
library(data.table)
library(Rfast)
library(spdep)
library(gplots)
library(RColorBrewer)
library(tmap)


# *****************************
# Section 1: Import Plot Data ####
# *****************************

Data <- here("OtherOutput", "FigureS2_PlotData.csv") %>% fread() %>% data.frame()


# *****************************
# Section 2: Import Spatial Data ####
# *****************************


GB <-
  st_read(
    here::here(
      "OtherData",
      "Counties_and_Unitary_Authorities_(December_2019)_Boundaries_UK_BUC.shp"
    )
  )
GB <- st_transform(GB, crs = 4326) ## Comes in BNG so convert to LatLon


## Join county name to county name
GB_New <- left_join(x = GB, Data, by = c("ctyua19nm" = "County"))


GB_New <-
  GB_New[!is.na(GB_New$LonH), ]
GB_New <-
  GB_New[!is.na(GB_New$LatH), ]
GB_New <-
  GB_New[which(!duplicated(GB_New$LatH)), ]
GB_New <-
  GB_New[which(!duplicated(GB_New$LonH)), ]
GB_New <- GB_New[!GB_New$ctyua19nm == "Belfast", ]

## One map only
GB_New2 <- GB_New[!GB_New$ctyua19nm == "Shetland Islands", ]


# ************************************************************
# Define variables ####
# ************************************************************

## Labels for the plots
breaks <- seq(1, 5, 1)
labels <-  c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")

## Now with line breaking "\n" to format easier
labels_2 <-
  c("High-High",
    "Low-Low",
    "High-Low",
    "Low-High",
    "Not\nSignif.")



colors <- c(RColorBrewer::brewer.pal(9, "Blues")[c(9, 7, 5, 3, 1)])


# *****************************
## Plotting setup: ####
# *****************************



## Tax:
Plot_Tax <-
  tmap::tm_shape(st_transform(GB_New, "+proj=laea +x_0=0 +y_0=0 +lon_0=0 +lat_0=0")) +
  tm_polygons(col = "Colors_Tax") + tm_layout(
    legend.position = c("RIGHT", "top"),
    legend.title.size = 0.5,
    legend.height = 0.5,
    legend.just = "right",
    legend.text.size = 0.5,
    panel.label.size = 0.5,
    panel.label.height = 5
  )
# tm_add_legend(title = "Cluster Type",
#               labels = labels,
#               col = colors)


## Colour: Medium
Plot_Colour_Medium <-
  tm_shape(GB_New) +
  tm_polygons(col = "Colors_Colour") + tm_layout(
    legend.position = c("RIGHT", "top"),
    legend.title.size = 0.5,
    legend.height = 0.5,
    legend.just = "right",
    legend.text.size = 0.5,
    panel.label.size = 0.5,
    panel.label.height = 5
  ) +
  tm_credits("Colours:\nmedium\n(i)",size = 0.75,
             fontfamily = "serif",
             position = c("LEFT", "TOP"))

## Colour: High
Plot_Colour_High <-
  tm_shape(GB_New) +
  tm_polygons(col = "Colors_ColourHigh") + tm_layout(
    legend.position = c("RIGHT", "top"),
    legend.title.size = 0.5,
    legend.height = 0.5,
    legend.just = "right",
    legend.text.size = 0.5,
    panel.label.size = 0.5,
    panel.label.height = 5
  ) +
  tm_credits("Colours:\nhigh\n(v)",size = 0.75,
             fontfamily = "serif",
             position = c("LEFT", "TOP"))



#---------------
## Colour: Medium
Plot_Smell_Medium <-
  tm_shape(GB_New) +
  tm_polygons(col = "Colors_Smell") + tm_layout(
    legend.position = c("RIGHT", "top"),
    legend.title.size = 0.5,
    legend.height = 0.5,
    legend.just = "right",
    legend.text.size = 0.5,
    panel.label.size = 0.5,
    panel.label.height = 5
  ) +
  tm_credits("Smells:\nmedium\n(ii)",size = 0.75,
             fontfamily = "serif",
             position = c("LEFT", "TOP"))


## Colour: High
Plot_Smell_High <-
  tm_shape(GB_New) +
  tm_polygons(col = "Colors_SmellHigh") + tm_layout(
    legend.position = c("RIGHT", "top"),
    legend.title.size = 0.5,
    legend.height = 0.5,
    legend.just = "right",
    legend.text.size = 0.5,
    panel.label.size = 0.5,
    panel.label.height = 5
  ) +
  tm_credits("Smells:\nhigh\n(vi)",size = 0.75,
             fontfamily = "serif",
             position = c("LEFT", "TOP"))


#--------------------
## Colour: Medium
Plot_Sound_Medium <-
  tm_shape(GB_New) +
  tm_polygons(col = "Colors_Sound") + tm_layout(
    legend.position = c("RIGHT", "top"),
    legend.title.size = 0.5,
    legend.height = 0.5,
    legend.just = "right",
    legend.text.size = 0.5,
    panel.label.size = 0.5,
    panel.label.height = 5
  ) +
  tm_credits("Sounds:\nmedium\n(iii)",size = 0.75,
             fontfamily = "serif",
             position = c("LEFT", "TOP"))


## Colour: High
Plot_Sound_High <-
  tm_shape(GB_New) +
  tm_polygons(col = "Colors_SoundHigh") + tm_layout(
    legend.position = c("RIGHT", "top"),
    legend.title.size = 0.5,
    legend.height = 0.5,
    legend.just = "right",
    legend.text.size = 0.5,
    panel.label.size = 0.5,
    panel.label.height = 5
  ) +
  tm_credits("Sounds:\nhigh\n(vii)",size = 0.75,
             fontfamily = "serif",
             position = c("LEFT", "TOP"))



## This is the key one as it has the legend!
Plot_Decomposition_Medium <-
  tm_shape(GB_New) +
  tm_polygons(col = "Colors_Decomposition") +
  tm_layout(legend.position = c(0.59, 0.56),
            legend.width = 0.8,
            legend.title.size = 0.85,
            legend.title.fontfamily = "serif",
            legend.text.size = 0.65,
            legend.text.fontfamily = "serif"

  ) +
  tm_credits(
    "Deadwood\ndecomposition:\nmedium\n(iv)",
    size = 0.75,
    fontfamily = "serif",
    position = c("LEFT", "TOP")
  ) +
  tm_add_legend(
    title = "Cluster Type",
    labels = labels,
    col = colors,
    legend.format = list(fontfamily = "serif")
  ) +
  tm_compass(north = 0,
           type = "arrow", size = 0.4,
           position = c(0.85, 0.02)) +
  tm_scale_bar(position = c(0.41, 0.003))

## Decomposition: Medium
# Plot_Decomposition_Medium <-
#   tm_shape(GB_New) +
#   tm_polygons(col = "Colors_Decomposition") + tm_layout(
#     legend.position = c("RIGHT", "top"),
#     legend.title.size = 0.5,
#     legend.height = 0.5,
#     legend.just = "right",
#     legend.text.size = 0.5,
#     panel.label.size = 0.5,
#     panel.label.height = 5,
#     title = "Deadwood\ndecomposition:\nmedium\n(iv)",
#     title.size = 0.85,
#     title.fontfamily = "serif"
#   ) +
#   tm_add_legend(title = "Cluster Type",
#                 labels = labels,
#                 col = colors)
#
#
#
# Plot_Decomposition_Medium <- tm_shape(GB_New) +
#   tm_polygons(col = "Colors_Decomposition") + tm_layout(legend.position = c(0.6, 0.6)) +
#   tm_add_legend(title = "Cluster Type",
#                 labels = labels,
#                 col = colors) +
#   tm_credits(
#     "Deadwood\ndecomposition:\nmedium\n(iv)",
#     size = 0.75,
#     fontfamily = "serif",
#     position = c("LEFT", "TOP")
#   )



## Decomposition: High
Plot_Decomposition_High <-
  tm_shape(GB_New) +
  tm_polygons(col = "Colors_DecompositionHigh") + tm_layout(
    legend.position = c("RIGHT", "top"),
    legend.title.size = 0.5,
    legend.height = 0.5,
    legend.just = "right",
    legend.text.size = 0.5,
    panel.label.size = 0.5,
    panel.label.height = 5
  ) +
  tm_credits("Deadwood\ndecomposition:\nhigh\n(viii)",size = 0.75,
             fontfamily = "serif",
             position = c("LEFT", "TOP"))


# *****************************
## Exporting: ####
# *****************************


## Arrange all:
Winter_Figure4B <- tmap_arrange(
  Plot_Colour_Medium,
  Plot_Smell_Medium,
  Plot_Sound_Medium,
  Plot_Decomposition_Medium,

  Plot_Colour_High,
  Plot_Smell_High,
  Plot_Sound_High,
  Plot_Decomposition_High,
  nrow=2,ncol=4
)

tmap_save(Winter_Figure4B,
          here::here("OtherOutput/Figures", "Winter_Figure4B.png"),
          dpi = 500)



# End of script -----------------------------------------------------
