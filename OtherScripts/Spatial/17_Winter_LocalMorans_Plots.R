#### RELATE Winter Paper ####
## Function: Plots Local Moran Hotspots
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 01/02/2023
## TODO: My god this is a mess but it works


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
# Section 1: Import Data ####
# *****************************


## Import step4 which has WTP already appended
Winter <- here("OtherData", "Winter_dataframe_Step4.csv") %>% fread() %>% data.frame()


## Remove potentially tricky observations
Winter <-
  Winter[!is.na(Winter$MilesDistance), ] ## Drop missing distances
Winter <-
  Winter[!is.na(Winter$Overall), ] ## Drop respondents not completing BIOWELL


# *****************************
# Section 1B: Clean Data ####
# *****************************


## Other renaming:
colnames(Winter)[which(names(Winter) == "Deadwood_WTP_Medium")] <-
  "Decomposition_WTP_Medium"

colnames(Winter)[which(names(Winter) == "Deadwood_WTP_High")] <-
  "Decomposition_WTP_High"




##Trim crazy WTPs
Data <- Winter[Winter$Tax_WTP_Medium > -50, ]
Data <- Winter


# *****************************
# Section 2: Define Nearest Neighbours ####
# *****************************


K = sqrt(nrow(Data))
Data <-
  Data[!is.na(Data$LonH),]
Data <-
  Data[!is.na(Data$LatH),]
Data <-
  Data[which(!duplicated(Data$LatH)),]
Data <-
  Data[which(!duplicated(Data$LonH)),]



K = sqrt(nrow(Data))
## Produces the K-Nearest Neighbour spatial weights
Woodlands <- data.frame(cbind(Data$LatH, abs(Data$LonH)))
CoordsMatrix <- data.matrix(Woodlands) #converting into matrix class
KNearNeigh <- knearneigh(CoordsMatrix, k = K)
KNN_ToNBs <- knn2nb(KNearNeigh)
Distances <- nbdists(KNN_ToNBs, CoordsMatrix)
Distances_Inverse <- lapply(Distances, function(x)
  1 / (x ^ 2))
KNN_Weights <- nb2listw(KNN_ToNBs, glist = Distances_Inverse)



# *****************************
## Tax:
# *****************************



LocalMoran_Tax <-
  localmoran(as.data.frame(Data[, c("Tax_WTP_Medium")])[, 1], KNN_Weights)
Data$ScaleWTP_Tax <-
  as.numeric(scale(as.data.frame(Data[, c("Tax_WTP_Medium")])[, 1]))
Data$lag_WTP_Tax <-
  as.numeric(lag.listw(KNN_Weights, Data$ScaleWTP_Tax))



## Classify categories:
### Scale>0, Lag>0, and Local stat sig: High-High
Data[(Data$ScaleWTP_Tax >= 0 &
        Data$lag_WTP_Tax >= 0) &
       (LocalMoran_Tax[, 5] <= 0.05), "quad_sig_WTP_Tax"] <- 1


### Scale<0, Lag<0, and Local stat sig: Low-Low
Data[(Data$ScaleWTP_Tax <= 0 &
        Data$lag_WTP_Tax <= 0) &
       (LocalMoran_Tax[, 5] <= 0.05), "quad_sig_WTP_Tax"] <- 2

### Scale<0, Lag<0, and Local stat sig: Low-Low
Data[(Data$ScaleWTP_Tax >= 0 &
        Data$lag_WTP_Tax <= 0) &
       (LocalMoran_Tax[, 5] <= 0.05), "quad_sig_WTP_Tax"] <- 3


### Scale<0, Lag<0, and Local stat sig: Low-Low
Data[(Data$ScaleWTP_Tax >= 0 &
        Data$lag_WTP_Tax <= 0) &
       (LocalMoran_Tax[, 5] <= 0.05), "quad_sig_WTP_Tax"] <- 4


### Scale<0, Lag>0, and Local stat sig: NS
Data[(Data$ScaleWTP_Tax <= 0 &
        Data$lag_WTP_Tax >= 0) &
       (LocalMoran_Tax[, 5] <= 0.05), "quad_sig_WTP_Tax"] <- 5


## Impute anything missing
Data$quad_sig_WTP_Tax <-
  ifelse(is.na(Data$quad_sig_WTP_Tax) == TRUE, 5, Data$quad_sig_WTP_Tax)




## Labels for the plots
breaks <- seq(1, 5, 1)
labels <-
  c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(Data$quad_sig_WTP_Tax, breaks)
colors <- c(RColorBrewer::brewer.pal(9, "Blues")[c(9, 7, 5, 3, 1)])
Data$Colors_Tax <-
  as.character(col2hex(c(RColorBrewer::brewer.pal(9, "Blues")[c(9, 7, 5, 3, 1)])[np]))

Data$quad_sig_WTP_Tax <- recode(
  Data$quad_sig_WTP_Tax,
  '5' = "Not Signif.",
  '4' = "Low-High",
  '3' = "High-Low",
  '2' = "Low-Low",
  '1' = "High-High"
)





# *****************************
## Colour:
# *****************************



LocalMoran_Colour <-
  localmoran(as.data.frame(Data[, c("Colour_WTP_Medium")])[, 1], KNN_Weights)
Data$ScaleWTP_Colour <-
  as.numeric(scale(as.data.frame(Data[, c("Colour_WTP_Medium")])[, 1]))
Data$lag_WTP_Colour <-
  as.numeric(lag.listw(KNN_Weights, Data$ScaleWTP_Colour))


## Classify categories:
### Scale>0, Lag>0, and Local stat sig: High-High
Data[(Data$ScaleWTP_Colour >= 0 &
        Data$lag_WTP_Colour >= 0) &
       (LocalMoran_Colour[, 5] <= 0.05), "quad_sig_WTP_Colour"] <- 1


### Scale<0, Lag<0, and Local stat sig: Low-Low
Data[(Data$ScaleWTP_Colour <= 0 &
        Data$lag_WTP_Colour <= 0) &
       (LocalMoran_Colour[, 5] <= 0.05), "quad_sig_WTP_Colour"] <- 2

### Scale<0, Lag<0, and Local stat sig: Low-Low
Data[(Data$ScaleWTP_Colour >= 0 &
        Data$lag_WTP_Colour <= 0) &
       (LocalMoran_Colour[, 5] <= 0.05), "quad_sig_WTP_Colour"] <- 3


### Scale<0, Lag<0, and Local stat sig: Low-Low
Data[(Data$ScaleWTP_Colour >= 0 &
        Data$lag_WTP_Colour <= 0) &
       (LocalMoran_Colour[, 5] <= 0.05), "quad_sig_WTP_Colour"] <- 4


### Scale<0, Lag>0, and Local stat sig: NS
Data[(Data$ScaleWTP_Colour <= 0 &
        Data$lag_WTP_Colour >= 0) &
       (LocalMoran_Colour[, 5] <= 0.05), "quad_sig_WTP_Colour"] <- 5


Data$quad_sig_WTP_Colour <-
  ifelse(is.na(Data$quad_sig_WTP_Colour) == TRUE,
         5,
         Data$quad_sig_WTP_Colour)

## Labels for the plots
breaks <- seq(1, 5, 1)
labels <-
  c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(Data$quad_sig_WTP_Colour, breaks)
colors <- c(RColorBrewer::brewer.pal(9, "Blues")[c(9, 7, 5, 3, 1)])
Data$Colors_Colour <-
  as.character(col2hex(c(RColorBrewer::brewer.pal(9, "Blues")[c(9, 7, 5, 3, 1)])[np]))

Data$quad_sig_WTP_Colour <- recode(
  Data$quad_sig_WTP_Colour,
  '5' = "Not Signif.",
  '4' = "Low-High",
  '3' = "High-Low",
  '2' = "Low-Low",
  '1' = "High-High"
)


# *****************************
## Smell:
# *****************************



LocalMoran_Smell <-
  localmoran(as.data.frame(Data[, c("Smell_WTP_Medium")])[, 1], KNN_Weights)
Data$ScaleWTP_Smell <-
  as.numeric(scale(as.data.frame(Data[, c("Smell_WTP_Medium")])[, 1]))
Data$lag_WTP_Smell <-
  as.numeric(lag.listw(KNN_Weights, Data$ScaleWTP_Smell))


## Classify categories:
Data[(Data$ScaleWTP_Smell >= 0 &
        Data$lag_WTP_Smell <= 0) &
       (LocalMoran_Smell[, 5] <= 0.05), "quad_sig_WTP_Smell"] <- 3
Data[(Data$ScaleWTP_Smell >= 0 &
        Data$lag_WTP_Smell <= 0) &
       (LocalMoran_Smell[, 5] <= 0.05), "quad_sig_WTP_Smell"] <- 4
Data[(Data$ScaleWTP_Smell <= 0 &
        Data$lag_WTP_Smell >= 0) &
       (LocalMoran_Smell[, 5] <= 0.05), "quad_sig_WTP_Smell"] <- 5
Data[(Data$ScaleWTP_Smell >= 0 &
        Data$lag_WTP_Smell >= 0) &
       (LocalMoran_Smell[, 5] <= 0.05), "quad_sig_WTP_Smell"] <- 1
Data[(Data$ScaleWTP_Smell <= 0 &
        Data$lag_WTP_Smell <= 0) &
       (LocalMoran_Smell[, 5] <= 0.05), "quad_sig_WTP_Smell"] <- 2
Data$quad_sig_WTP_Smell <-
  ifelse(is.na(Data$quad_sig_WTP_Smell) == TRUE,
         5,
         Data$quad_sig_WTP_Smell)

## Labels for the plots
breaks <- seq(1, 5, 1)
labels <-
  c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(Data$quad_sig_WTP_Smell, breaks)
colors <- c(RColorBrewer::brewer.pal(9, "Blues")[c(9, 7, 5, 3, 1)])
Data$Colors_Smell <-
  as.character(col2hex(c(RColorBrewer::brewer.pal(9, "Blues")[c(9, 7, 5, 3, 1)])[np]))

Data$quad_sig_WTP_Smell <- recode(
  Data$quad_sig_WTP_Smell,
  '5' = "Not Signif.",
  '4' = "Low-High",
  '3' = "High-Low",
  '2' = "Low-Low",
  '1' = "High-High"
)


# *****************************
## Sound:
# *****************************


LocalMoran_Sound <-
  localmoran(as.data.frame(Data[, c("Sound_WTP_Medium")])[, 1], KNN_Weights)
Data$ScaleWTP_Sound <-
  as.numeric(scale(as.data.frame(Data[, c("Sound_WTP_Medium")])[, 1]))
Data$lag_WTP_Sound <-
  as.numeric(lag.listw(KNN_Weights, Data$ScaleWTP_Sound))


## Classify categories:
Data[(Data$ScaleWTP_Sound >= 0 &
        Data$lag_WTP_Sound <= 0) &
       (LocalMoran_Sound[, 5] <= 0.05), "quad_sig_WTP_Sound"] <- 3
Data[(Data$ScaleWTP_Sound >= 0 &
        Data$lag_WTP_Sound <= 0) &
       (LocalMoran_Sound[, 5] <= 0.05), "quad_sig_WTP_Sound"] <- 4
Data[(Data$ScaleWTP_Sound <= 0 &
        Data$lag_WTP_Sound >= 0) &
       (LocalMoran_Sound[, 5] <= 0.05), "quad_sig_WTP_Sound"] <- 5
Data[(Data$ScaleWTP_Sound >= 0 &
        Data$lag_WTP_Sound >= 0) &
       (LocalMoran_Sound[, 5] <= 0.05), "quad_sig_WTP_Sound"] <- 1
Data[(Data$ScaleWTP_Sound <= 0 &
        Data$lag_WTP_Sound <= 0) &
       (LocalMoran_Sound[, 5] <= 0.05), "quad_sig_WTP_Sound"] <- 2
Data$quad_sig_WTP_Sound <-
  ifelse(is.na(Data$quad_sig_WTP_Sound) == TRUE,
         5,
         Data$quad_sig_WTP_Sound)

## Labels for the plots
breaks <- seq(1, 5, 1)
labels <-
  c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(Data$quad_sig_WTP_Sound, breaks)
colors <- c(RColorBrewer::brewer.pal(9, "Blues")[c(9, 7, 5, 3, 1)])
Data$Colors_Sound <-
  as.character(col2hex(c(RColorBrewer::brewer.pal(9, "Blues")[c(9, 7, 5, 3, 1)])[np]))

Data$quad_sig_WTP_Sound <- recode(
  Data$quad_sig_WTP_Sound,
  '5' = "Not Signif.",
  '4' = "Low-High",
  '3' = "High-Low",
  '2' = "Low-Low",
  '1' = "High-High"
)



# *****************************
## Decomposition:
# *****************************


LocalMoran_Decomposition <-
  localmoran(as.data.frame(Data[, c("Decomposition_WTP_Medium")])[, 1], KNN_Weights)
Data$ScaleWTP_Decomposition <-
  as.numeric(scale(as.data.frame(Data[, c("Decomposition_WTP_Medium")])[, 1]))
Data$lag_WTP_Decomposition <-
  as.numeric(lag.listw(KNN_Weights, Data$ScaleWTP_Decomposition))


## Classify categories:
Data[(Data$ScaleWTP_Decomposition >= 0 &
        Data$lag_WTP_Decomposition <= 0) &
       (LocalMoran_Decomposition[, 5] <= 0.05), "quad_sig_WTP_Decomposition"] <-
  3
Data[(Data$ScaleWTP_Decomposition >= 0 &
        Data$lag_WTP_Decomposition <= 0) &
       (LocalMoran_Decomposition[, 5] <= 0.05), "quad_sig_WTP_Decomposition"] <-
  4
Data[(Data$ScaleWTP_Decomposition <= 0 &
        Data$lag_WTP_Decomposition >= 0) &
       (LocalMoran_Decomposition[, 5] <= 0.05), "quad_sig_WTP_Decomposition"] <-
  5
Data[(Data$ScaleWTP_Decomposition >= 0 &
        Data$lag_WTP_Decomposition >= 0) &
       (LocalMoran_Decomposition[, 5] <= 0.05), "quad_sig_WTP_Decomposition"] <-
  1
Data[(Data$ScaleWTP_Decomposition <= 0 &
        Data$lag_WTP_Decomposition <= 0) &
       (LocalMoran_Decomposition[, 5] <= 0.05), "quad_sig_WTP_Decomposition"] <-
  2
Data$quad_sig_WTP_Decomposition <-
  ifelse(is.na(Data$quad_sig_WTP_Decomposition) == TRUE,
         5,
         Data$quad_sig_WTP_Decomposition)

## Labels for the plots
breaks <- seq(1, 5, 1)
labels <-
  c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(Data$quad_sig_WTP_Decomposition, breaks)
colors <- c(RColorBrewer::brewer.pal(9, "Blues")[c(9, 7, 5, 3, 1)])
Data$Colors_Decomposition <-
  as.character(col2hex(c(RColorBrewer::brewer.pal(9, "Blues")[c(9, 7, 5, 3, 1)])[np]))

Data$quad_sig_WTP_Decomposition <-
  recode(
    Data$quad_sig_WTP_Decomposition,
    '5' = "Not Signif.",
    '4' = "Low-High",
    '3' = "High-Low",
    '2' = "Low-Low",
    '1' = "High-High"
  )


# *****************************
## Colour High:
# *****************************


LocalMoran_ColourHigh <-
  localmoran(as.data.frame(Data[, c("Colour_WTP_High")])[, 1], KNN_Weights)
Data$ScaleWTP_ColourHigh <-
  as.numeric(scale(as.data.frame(Data[, c("Colour_WTP_High")])[, 1]))
Data$lag_WTP_ColourHigh <-
  as.numeric(lag.listw(KNN_Weights, Data$ScaleWTP_ColourHigh))


## Classify categories:
Data[(Data$ScaleWTP_ColourHigh >= 0 &
        Data$lag_WTP_ColourHigh <= 0) &
       (LocalMoran_ColourHigh[, 5] <= 0.05), "quad_sig_WTP_ColourHigh"] <-
  3
Data[(Data$ScaleWTP_ColourHigh >= 0 &
        Data$lag_WTP_ColourHigh <= 0) &
       (LocalMoran_ColourHigh[, 5] <= 0.05), "quad_sig_WTP_ColourHigh"] <-
  4
Data[(Data$ScaleWTP_ColourHigh <= 0 &
        Data$lag_WTP_ColourHigh >= 0) &
       (LocalMoran_ColourHigh[, 5] <= 0.05), "quad_sig_WTP_ColourHigh"] <-
  5
Data[(Data$ScaleWTP_ColourHigh >= 0 &
        Data$lag_WTP_ColourHigh >= 0) &
       (LocalMoran_ColourHigh[, 5] <= 0.05), "quad_sig_WTP_ColourHigh"] <-
  1
Data[(Data$ScaleWTP_ColourHigh <= 0 &
        Data$lag_WTP_ColourHigh <= 0) &
       (LocalMoran_ColourHigh[, 5] <= 0.05), "quad_sig_WTP_ColourHigh"] <-
  2
Data$quad_sig_WTP_ColourHigh <-
  ifelse(is.na(Data$quad_sig_WTP_ColourHigh) == TRUE,
         5,
         Data$quad_sig_WTP_ColourHigh)

## Labels for the plots
breaks <- seq(1, 5, 1)
labels <-
  c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(Data$quad_sig_WTP_ColourHigh, breaks)
colors <- c(RColorBrewer::brewer.pal(9, "Blues")[c(9, 7, 5, 3, 1)])
Data$Colors_ColourHigh <-
  as.character(col2hex(c(RColorBrewer::brewer.pal(9, "Blues")[c(9, 7, 5, 3, 1)])[np]))

Data$quad_sig_WTP_ColourHigh <- recode(
  Data$quad_sig_WTP_ColourHigh,
  '5' = "Not Signif.",
  '4' = "Low-High",
  '3' = "High-Low",
  '2' = "Low-Low",
  '1' = "High-High"
)



# *****************************
## Smell High:
# *****************************


LocalMoran_SmellHigh <-
  localmoran(as.data.frame(Data[, c("Smell_WTP_High")])[, 1], KNN_Weights)
Data$ScaleWTP_SmellHigh <-
  as.numeric(scale(as.data.frame(Data[, c("Smell_WTP_High")])[, 1]))
Data$lag_WTP_SmellHigh <-
  as.numeric(lag.listw(KNN_Weights, Data$ScaleWTP_SmellHigh))


## Classify categories:
Data[(Data$ScaleWTP_SmellHigh >= 0 &
        Data$lag_WTP_SmellHigh <= 0) &
       (LocalMoran_SmellHigh[, 5] <= 0.05), "quad_sig_WTP_SmellHigh"] <-
  3
Data[(Data$ScaleWTP_SmellHigh >= 0 &
        Data$lag_WTP_SmellHigh <= 0) &
       (LocalMoran_SmellHigh[, 5] <= 0.05), "quad_sig_WTP_SmellHigh"] <-
  4
Data[(Data$ScaleWTP_SmellHigh <= 0 &
        Data$lag_WTP_SmellHigh >= 0) &
       (LocalMoran_SmellHigh[, 5] <= 0.05), "quad_sig_WTP_SmellHigh"] <-
  5
Data[(Data$ScaleWTP_SmellHigh >= 0 &
        Data$lag_WTP_SmellHigh >= 0) &
       (LocalMoran_SmellHigh[, 5] <= 0.05), "quad_sig_WTP_SmellHigh"] <-
  1
Data[(Data$ScaleWTP_SmellHigh <= 0 &
        Data$lag_WTP_SmellHigh <= 0) &
       (LocalMoran_SmellHigh[, 5] <= 0.05), "quad_sig_WTP_SmellHigh"] <-
  2
Data$quad_sig_WTP_SmellHigh <-
  ifelse(is.na(Data$quad_sig_WTP_SmellHigh) == TRUE,
         5,
         Data$quad_sig_WTP_SmellHigh)

## Labels for the plots
breaks <- seq(1, 5, 1)
labels <-
  c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(Data$quad_sig_WTP_SmellHigh, breaks)
colors <- c(RColorBrewer::brewer.pal(9, "Blues")[c(9, 7, 5, 3, 1)])
Data$Colors_SmellHigh <-
  as.character(col2hex(c(RColorBrewer::brewer.pal(9, "Blues")[c(9, 7, 5, 3, 1)])[np]))

Data$quad_sig_WTP_SmellHigh <- recode(
  Data$quad_sig_WTP_SmellHigh,
  '5' = "Not Signif.",
  '4' = "Low-High",
  '3' = "High-Low",
  '2' = "Low-Low",
  '1' = "High-High"
)


# *****************************
## Sound High:
# *****************************


LocalMoran_SoundHigh <-
  localmoran(as.data.frame(Data[, c("Sound_WTP_High")])[, 1], KNN_Weights)
Data$ScaleWTP_SoundHigh <-
  as.numeric(scale(as.data.frame(Data[, c("Sound_WTP_High")])[, 1]))
Data$lag_WTP_SoundHigh <-
  as.numeric(lag.listw(KNN_Weights, Data$ScaleWTP_SoundHigh))


## Classify categories:
Data[(Data$ScaleWTP_SoundHigh >= 0 &
        Data$lag_WTP_SoundHigh <= 0) &
       (LocalMoran_SoundHigh[, 5] <= 0.05), "quad_sig_WTP_SoundHigh"] <-
  3
Data[(Data$ScaleWTP_SoundHigh >= 0 &
        Data$lag_WTP_SoundHigh <= 0) &
       (LocalMoran_SoundHigh[, 5] <= 0.05), "quad_sig_WTP_SoundHigh"] <-
  4
Data[(Data$ScaleWTP_SoundHigh <= 0 &
        Data$lag_WTP_SoundHigh >= 0) &
       (LocalMoran_SoundHigh[, 5] <= 0.05), "quad_sig_WTP_SoundHigh"] <-
  5
Data[(Data$ScaleWTP_SoundHigh >= 0 &
        Data$lag_WTP_SoundHigh >= 0) &
       (LocalMoran_SoundHigh[, 5] <= 0.05), "quad_sig_WTP_SoundHigh"] <-
  1
Data[(Data$ScaleWTP_SoundHigh <= 0 &
        Data$lag_WTP_SoundHigh <= 0) &
       (LocalMoran_SoundHigh[, 5] <= 0.05), "quad_sig_WTP_SoundHigh"] <-
  2
Data$quad_sig_WTP_SoundHigh <-
  ifelse(is.na(Data$quad_sig_WTP_SoundHigh) == TRUE,
         5,
         Data$quad_sig_WTP_SoundHigh)

## Labels for the plots
breaks <- seq(1, 5, 1)
labels <-
  c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(Data$quad_sig_WTP_SoundHigh, breaks)
colors <- c(RColorBrewer::brewer.pal(9, "Blues")[c(9, 7, 5, 3, 1)])
Data$Colors_SoundHigh <-
  as.character(col2hex(c(RColorBrewer::brewer.pal(9, "Blues")[c(9, 7, 5, 3, 1)])[np]))

Data$quad_sig_WTP_SoundHigh <- recode(
  Data$quad_sig_WTP_SoundHigh,
  '5' = "Not Signif.",
  '4' = "Low-High",
  '3' = "High-Low",
  '2' = "Low-Low",
  '1' = "High-High"
)

# *****************************
## Decomposition High:
# *****************************


LocalMoran_DecompositionHigh <-
  localmoran(as.data.frame(Data[, c("Decomposition_WTP_High")])[, 1], KNN_Weights)
Data$ScaleWTP_DecompositionHigh <-
  as.numeric(scale(as.data.frame(Data[, c("Decomposition_WTP_High")])[, 1]))
Data$lag_WTP_DecompositionHigh <-
  as.numeric(lag.listw(KNN_Weights, Data$ScaleWTP_DecompositionHigh))


## Classify categories:
Data[(Data$ScaleWTP_DecompositionHigh >= 0 &
        Data$lag_WTP_DecompositionHigh <= 0) &
       (LocalMoran_DecompositionHigh[, 5] <= 0.05), "quad_sig_WTP_DecompositionHigh"] <-
  3
Data[(Data$ScaleWTP_DecompositionHigh >= 0 &
        Data$lag_WTP_DecompositionHigh <= 0) &
       (LocalMoran_DecompositionHigh[, 5] <= 0.05), "quad_sig_WTP_DecompositionHigh"] <-
  4
Data[(Data$ScaleWTP_DecompositionHigh <= 0 &
        Data$lag_WTP_DecompositionHigh >= 0) &
       (LocalMoran_DecompositionHigh[, 5] <= 0.05), "quad_sig_WTP_DecompositionHigh"] <-
  5
Data[(Data$ScaleWTP_DecompositionHigh >= 0 &
        Data$lag_WTP_DecompositionHigh >= 0) &
       (LocalMoran_DecompositionHigh[, 5] <= 0.05), "quad_sig_WTP_DecompositionHigh"] <-
  1
Data[(Data$ScaleWTP_DecompositionHigh <= 0 &
        Data$lag_WTP_DecompositionHigh <= 0) &
       (LocalMoran_DecompositionHigh[, 5] <= 0.05), "quad_sig_WTP_DecompositionHigh"] <-
  2
Data$quad_sig_WTP_DecompositionHigh <-
  ifelse(
    is.na(Data$quad_sig_WTP_DecompositionHigh) == TRUE,
    5,
    Data$quad_sig_WTP_DecompositionHigh
  )

## Labels for the plots
breaks <- seq(1, 5, 1)
labels <-
  c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(Data$quad_sig_WTP_DecompositionHigh, breaks)
colors <- c(RColorBrewer::brewer.pal(9, "Blues")[c(9, 7, 5, 3, 1)])
Data$Colors_DecompositionHigh <-
  as.character(col2hex(c(RColorBrewer::brewer.pal(9, "Blues")[c(9, 7, 5, 3, 1)])[np]))

Data$quad_sig_WTP_DecompositionHigh <-
  recode(
    Data$quad_sig_WTP_DecompositionHigh,
    '5' = "Not Signif.",
    '4' = "Low-High",
    '3' = "High-Low",
    '2' = "Low-Low",
    '1' = "High-High"
  )


# *****************************
## Plotting setup:
# *****************************

GB <-
  st_read(
    here(
      "OtherData",
      "Counties_and_Unitary_Authorities_(December_2019)_Boundaries_UK_BUC.shp"
    )
  )
GB <- st_transform(GB, crs = 4326) ## Comes in BNG so convert to LatLon


Data$County <- ifelse(Data$County == "West Northamptonshire",
                      "Northamptonshire",
                      Data$County)

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


# *****************************
## Plotting making each plot individually then stitch together:
# *****************************





## Tax:
Plot_Tax <-
  tm_shape(st_transform(GB_New, "+proj=laea +x_0=0 +y_0=0 +lon_0=0 +lat_0=0")) +
  tm_polygons(col = "Colors_Tax") + tm_layout(
    legend.position = c("RIGHT", "top"),
    legend.title.size = 0.5,
    legend.height = 0.5,
    legend.just = "right",
    legend.text.size = 0.5,
    panel.label.size = 0.5,
    panel.label.height = 5
  ) +
  tm_add_legend(title = "Cluster Type",
                labels = labels,
                col = colors)


## Colour: Medium
Plot_Colour_Medium <-
  tm_shape(st_transform(GB_New, "+proj=laea +x_0=0 +y_0=0 +lon_0=0 +lat_0=0")) +
  tm_polygons(col = "Colors_Colour") + tm_layout(
    legend.position = c("RIGHT", "top"),
    legend.title.size = 0.5,
    legend.height = 0.5,
    legend.just = "right",
    legend.text.size = 0.5,
    panel.label.size = 0.5,
    panel.label.height = 5
  ) +
  tm_add_legend(title = "Cluster Type",
                labels = labels,
                col = colors)

## Colour: High
Plot_Colour_High <-
  tm_shape(st_transform(GB_New, "+proj=laea +x_0=0 +y_0=0 +lon_0=0 +lat_0=0")) +
  tm_polygons(col = "Colors_ColourHigh") + tm_layout(
    legend.position = c("RIGHT", "top"),
    legend.title.size = 0.5,
    legend.height = 0.5,
    legend.just = "right",
    legend.text.size = 0.5,
    panel.label.size = 0.5,
    panel.label.height = 5
  ) +
  tm_add_legend(title = "Cluster Type",
                labels = labels,
                col = colors)


#---------------
## Colour: Medium
Plot_Smell_Medium <-
  tm_shape(st_transform(GB_New, "+proj=laea +x_0=0 +y_0=0 +lon_0=0 +lat_0=0")) +
  tm_polygons(col = "Colors_Smell") + tm_layout(
    legend.position = c("RIGHT", "top"),
    legend.title.size = 0.5,
    legend.height = 0.5,
    legend.just = "right",
    legend.text.size = 0.5,
    panel.label.size = 0.5,
    panel.label.height = 5
  ) +
  tm_add_legend(title = "Cluster Type",
                labels = labels,
                col = colors)

## Colour: High
Plot_Smell_High <-
  tm_shape(st_transform(GB_New, "+proj=laea +x_0=0 +y_0=0 +lon_0=0 +lat_0=0")) +
  tm_polygons(col = "Colors_SmellHigh") + tm_layout(
    legend.position = c("RIGHT", "top"),
    legend.title.size = 0.5,
    legend.height = 0.5,
    legend.just = "right",
    legend.text.size = 0.5,
    panel.label.size = 0.5,
    panel.label.height = 5
  ) +
  tm_add_legend(title = "Cluster Type",
                labels = labels,
                col = colors)

#--------------------
## Colour: Medium
Plot_Sound_Medium <-
  tm_shape(st_transform(GB_New, "+proj=laea +x_0=0 +y_0=0 +lon_0=0 +lat_0=0")) +
  tm_polygons(col = "Colors_Sound") + tm_layout(
    legend.position = c("RIGHT", "top"),
    legend.title.size = 0.5,
    legend.height = 0.5,
    legend.just = "right",
    legend.text.size = 0.5,
    panel.label.size = 0.5,
    panel.label.height = 5
  ) +
  tm_add_legend(title = "Cluster Type",
                labels = labels,
                col = colors)

## Colour: High
Plot_Sound_High <-
  tm_shape(st_transform(GB_New, "+proj=laea +x_0=0 +y_0=0 +lon_0=0 +lat_0=0")) +
  tm_polygons(col = "Colors_SoundHigh") + tm_layout(
    legend.position = c("RIGHT", "top"),
    legend.title.size = 0.5,
    legend.height = 0.5,
    legend.just = "right",
    legend.text.size = 0.5,
    panel.label.size = 0.5,
    panel.label.height = 5
  ) +
  tm_add_legend(title = "Cluster Type",
                labels = labels,
                col = colors)


## Decomposition: Medium
Plot_Decomposition_Medium <-
  tm_shape(st_transform(GB_New, "+proj=laea +x_0=0 +y_0=0 +lon_0=0 +lat_0=0")) +
  tm_polygons(col = "Colors_Decomposition") + tm_layout(
    legend.position = c("RIGHT", "top"),
    legend.title.size = 0.5,
    legend.height = 0.5,
    legend.just = "right",
    legend.text.size = 0.5,
    panel.label.size = 0.5,
    panel.label.height = 5
  ) +
  tm_add_legend(title = "Cluster Type",
                labels = labels,
                col = colors)

## Decomposition: High
Plot_Decomposition_High <-
  tm_shape(st_transform(GB_New, "+proj=laea +x_0=0 +y_0=0 +lon_0=0 +lat_0=0")) +
  tm_polygons(col = "Colors_DecompositionHigh") + tm_layout(
    legend.position = c("RIGHT", "top"),
    legend.title.size = 0.5,
    legend.height = 0.5,
    legend.just = "right",
    legend.text.size = 0.5,
    panel.label.size = 0.5,
    panel.label.height = 5
  ) +
  tm_add_legend(title = "Cluster Type",
                labels = labels,
                col = colors)

# *****************************
## Adding all plots to one:
# *****************************



## Arrange all:
Winter_Figure4A <- tmap_arrange(
  Plot_Colour_Medium,
  Plot_Smell_Medium,
  Plot_Sound_Medium,
  Plot_Decomposition_Medium,nrow=2,ncol=2
)

tmap_save(Winter_Figure4A,"Winter_Figure4A.png",dpi = 1000)



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
          here("OtherOutput/Figures", "Winter_Figure4B.png"),
          dpi = 1000)



# End of script -----------------------------------------------------
