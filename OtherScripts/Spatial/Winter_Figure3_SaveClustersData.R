#### RELATE All-Seasons Paper ####
## Function: Outputs the clusters but no plots
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 21/07/2022
## TODO: Export to usable format


## Structure:
### So for each attribute we do the same thing; 
### calculate local Morans I per place using locm_{attribute}
### Then categorise clusters using 5 ways
### This is cumbersome but basically there are 8 sections, as there are 
### four attributes with two levels each

#------------------------------
# Replication Information: ####
#------------------------------


## Selected output from "sessionInfo()"
# R version 4.2.0 (2022-04-22 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19044)
#   [1] LC_COLLATE=English_United Kingdom.utf8  LC_CTYPE=English_United Kingdom.utf8   
#   [1] tmap_3.3-3     sf_1.0-7       here_1.0.1     reshape2_1.4.4 ggridges_0.5.3 ggplot2_3.3.6  magrittr_2.0.3
# [8] dplyr_1.0.9    apollo_0.2.7  
#   [1] viridisLite_0.4.0   splines_4.2.0       tmvnsim_1.0-2       rngWELL_0.10-7      sp_1.4-7           
# [6] randtoolbox_1.31.1  numDeriv_2016.8-1.1 pillar_1.7.0        lattice_0.20-45     quantreg_5.93      
# [11] glue_1.6.2          digest_0.6.29       RColorBrewer_1.1-3  colorspace_2.0-3    sandwich_3.0-1     
# [16] htmltools_0.5.2     Matrix_1.4-1        plyr_1.8.7          XML_3.99-0.9        pkgconfig_2.0.3    
# [21] raster_3.5-15       SparseM_1.81        stars_0.5-5         purrr_0.3.4         mvtnorm_1.1-3      
# [26] scales_1.2.0        terra_1.5-21        MatrixModels_0.5-0  tibble_3.1.7        proxy_0.4-26       
# [31] generics_0.1.2      farver_2.1.0        ellipsis_0.3.2      withr_2.5.0         leafsync_0.1.0     
# [36] maxLik_1.5-2        cli_3.3.0           mnormt_2.0.2        survival_3.3-1      crayon_1.5.1       
# [41] mcmc_0.9-7          fansi_1.0.3         MASS_7.3-56         lwgeom_0.2-8        class_7.3-20       
# [46] tools_4.2.0         lifecycle_1.0.1     matrixStats_0.62.0  RSGHB_1.2.2         stringr_1.4.0      
# [51] MCMCpack_1.6-3      munsell_0.5.0       compiler_4.2.0      e1071_1.7-9         rlang_1.0.2        
# [56] tmaptools_3.1-1     classInt_0.4-3      units_0.8-0         grid_4.2.0          dichromat_2.0-0.1  
# [61] miscTools_0.6-26    htmlwidgets_1.5.4   crosstalk_1.2.0     base64enc_0.1-3     leafem_0.2.0       
# [66] codetools_0.2-18    gtable_0.3.0        abind_1.4-5         DBI_1.1.2           R6_2.5.1           
# [71] zoo_1.8-10          fastmap_1.1.0       utf8_1.2.2          rprojroot_2.0.3     KernSmooth_2.23-20 
# [76] stringi_1.7.6       parallel_4.2.0      Rcpp_1.0.8.3        png_0.1-7           vctrs_0.4.1        
# [81] leaflet_2.1.1       tidyselect_1.1.2    coda_0.19-4


#------------------------------
# Setup Environment: ####
#------------------------------

install.packages("ggpubr",repos="http://cran.us.r-project.org")
library(sf)
library(magrittr)
library(dplyr)
library(apollo)
library(reshape2)
library(tmap)
library(spdep)
library(here)
library(gplots)
library(RColorBrewer)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(gplots)


#------------------------------
# Section 1: Import and trim Data ####
#------------------------------


## Import Spatial Data
Data_Winter <- st_read(here("OtherData","GB_Winter_2021-11-26.gpkg"))
Data_Winter <- st_transform(Data_Winter, crs = 4326)


## Truncate map for missing coordinates:
Data_Winter <- Data_Winter[!is.na(Data_Winter$ColourWTP),]
Data_Winter <-
  Data_Winter[!is.na(Data_Winter$LonH),]
Data_Winter <-
  Data_Winter[!is.na(Data_Winter$LatH),]
Data_Winter <-
  Data_Winter[which(!duplicated(Data_Winter$LatH)),]


## Drop missing data:
Data_Winter <-
  Data_Winter[!is.na(Data_Winter$MilesDistance),]
Data_Winter <-
  Data_Winter[!is.na(Data_Winter$WoodlandsScore),]

## Renaming "deadwood" to "decomposition" 
### On 02/07 for consistency with manuscript
colnames(Data_Winter)[which(names(Data_Winter)=="DeadwoodWTP")] <- "DecompositionWTP"
colnames(Data_Winter)[which(names(Data_Winter)=="DeadwoodWTP2")] <- "DecompositionWTP2"


Data <- Data_Winter

#-----------------------------------------
# Section 2: Define Nearest Neighbours ####
#-----------------------------------------



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



#----------------------------------------------------------------------
## Colour:
#----------------------------------------------------------------------


locm_Colour <- localmoran(as.data.frame(Data[, c("ColourWTP")])[, 1], KNN_Weights)
Data$ScaleWTP_Colour <-
  as.numeric(scale(as.data.frame(Data[, c("ColourWTP")])[, 1]))
Data$lag_WTP_Colour <- as.numeric(lag.listw(KNN_Weights, Data$ScaleWTP_Colour))


## Classify categories:
Data[(Data$ScaleWTP_Colour >= 0 &
        Data$lag_WTP_Colour <= 0) &
       (locm_Colour[, 5] <= 0.05), "quad_sig_WTP_Colour"] <- 3
Data[(Data$ScaleWTP_Colour >= 0 &
        Data$lag_WTP_Colour <= 0) &
       (locm_Colour[, 5] <= 0.05), "quad_sig_WTP_Colour"] <- 4
Data[(Data$ScaleWTP_Colour <= 0 &
        Data$lag_WTP_Colour >= 0) &
       (locm_Colour[, 5] <= 0.05), "quad_sig_WTP_Colour"] <- 5
Data[(Data$ScaleWTP_Colour >= 0 &
        Data$lag_WTP_Colour >= 0) &
       (locm_Colour[, 5] <= 0.05), "quad_sig_WTP_Colour"] <- 1
Data[(Data$ScaleWTP_Colour <= 0 &
        Data$lag_WTP_Colour <= 0) &
       (locm_Colour[, 5] <= 0.05), "quad_sig_WTP_Colour"] <- 2
Data$quad_sig_WTP <-
  ifelse(is.na(Data$quad_sig_WTP_Colour) == TRUE, 5, Data$quad_sig_WTP_Colour)

## Labels for the plots
breaks <- seq(1, 5, 1)
labels <-
  c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(Data$quad_sig_WTP_Colour, breaks)
colors <- c(RColorBrewer::brewer.pal(9,"Blues")[c(9,7,5,3,1)])
Data$Colors_Colour <- as.character(col2hex(c(RColorBrewer::brewer.pal(9,"Blues")[c(9,7,5,3,1)])[np]))

Data$quad_sig_WTP_Colour <- recode(Data$quad_sig_WTP_Colour,
                                   '5'="Not Signif.",
                                   '4'="Low-High",
                                   '3'="High-Low",
                                   '2'="Low-Low",
                                   '1'="High-High")


#----------------------------------------------------------------------
## Smell:
#----------------------------------------------------------------------


locm_Smell <- localmoran(as.data.frame(Data[, c("SmellWTP")])[, 1], KNN_Weights)
Data$ScaleWTP_Smell <-
  as.numeric(scale(as.data.frame(Data[, c("SmellWTP")])[, 1]))
Data$lag_WTP_Smell <- as.numeric(lag.listw(KNN_Weights, Data$ScaleWTP_Smell))


## Classify categories:
Data[(Data$ScaleWTP_Smell >= 0 &
        Data$lag_WTP_Smell <= 0) &
       (locm_Smell[, 5] <= 0.05), "quad_sig_WTP_Smell"] <- 3
Data[(Data$ScaleWTP_Smell >= 0 &
        Data$lag_WTP_Smell <= 0) &
       (locm_Smell[, 5] <= 0.05), "quad_sig_WTP_Smell"] <- 4
Data[(Data$ScaleWTP_Smell <= 0 &
        Data$lag_WTP_Smell >= 0) &
       (locm_Smell[, 5] <= 0.05), "quad_sig_WTP_Smell"] <- 5
Data[(Data$ScaleWTP_Smell >= 0 &
        Data$lag_WTP_Smell >= 0) &
       (locm_Smell[, 5] <= 0.05), "quad_sig_WTP_Smell"] <- 1
Data[(Data$ScaleWTP_Smell <= 0 &
        Data$lag_WTP_Smell <= 0) &
       (locm_Smell[, 5] <= 0.05), "quad_sig_WTP_Smell"] <- 2
Data$quad_sig_WTP <-
  ifelse(is.na(Data$quad_sig_WTP_Smell) == TRUE, 5, Data$quad_sig_WTP_Smell)

## Labels for the plots
breaks <- seq(1, 5, 1)
labels <-
  c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(Data$quad_sig_WTP_Smell, breaks)
colors <- c(RColorBrewer::brewer.pal(9,"Blues")[c(9,7,5,3,1)])
Data$Colors_Smell <- as.character(col2hex(c(RColorBrewer::brewer.pal(9,"Blues")[c(9,7,5,3,1)])[np]))

Data$quad_sig_WTP_Smell <- recode(Data$quad_sig_WTP_Smell,
                                  '5'="Not Signif.",
                                  '4'="Low-High",
                                  '3'="High-Low",
                                  '2'="Low-Low",
                                  '1'="High-High")

#----------------------------------------------------------------------
## Sound:
#----------------------------------------------------------------------


locm_Sound <- localmoran(as.data.frame(Data[, c("SoundWTP")])[, 1], KNN_Weights)
Data$ScaleWTP_Sound <-
  as.numeric(scale(as.data.frame(Data[, c("SoundWTP")])[, 1]))
Data$lag_WTP_Sound <- as.numeric(lag.listw(KNN_Weights, Data$ScaleWTP_Sound))


## Classify categories:
Data[(Data$ScaleWTP_Sound >= 0 &
        Data$lag_WTP_Sound <= 0) &
       (locm_Sound[, 5] <= 0.05), "quad_sig_WTP_Sound"] <- 3
Data[(Data$ScaleWTP_Sound >= 0 &
        Data$lag_WTP_Sound <= 0) &
       (locm_Sound[, 5] <= 0.05), "quad_sig_WTP_Sound"] <- 4
Data[(Data$ScaleWTP_Sound <= 0 &
        Data$lag_WTP_Sound >= 0) &
       (locm_Sound[, 5] <= 0.05), "quad_sig_WTP_Sound"] <- 5
Data[(Data$ScaleWTP_Sound >= 0 &
        Data$lag_WTP_Sound >= 0) &
       (locm_Sound[, 5] <= 0.05), "quad_sig_WTP_Sound"] <- 1
Data[(Data$ScaleWTP_Sound <= 0 &
        Data$lag_WTP_Sound <= 0) &
       (locm_Sound[, 5] <= 0.05), "quad_sig_WTP_Sound"] <- 2
Data$quad_sig_WTP <-
  ifelse(is.na(Data$quad_sig_WTP_Sound) == TRUE, 5, Data$quad_sig_WTP_Sound)

## Labels for the plots
breaks <- seq(1, 5, 1)
labels <-
  c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(Data$quad_sig_WTP_Sound, breaks)
colors <- c(RColorBrewer::brewer.pal(9,"Blues")[c(9,7,5,3,1)])
Data$Colors_Sound <- as.character(col2hex(c(RColorBrewer::brewer.pal(9,"Blues")[c(9,7,5,3,1)])[np]))

Data$quad_sig_WTP_Sound <- recode(Data$quad_sig_WTP_Sound,
                                  '5'="Not Signif.",
                                  '4'="Low-High",
                                  '3'="High-Low",
                                  '2'="Low-Low",
                                  '1'="High-High")

#----------------------------------------------------------------------
## Decomposition:
#----------------------------------------------------------------------


locm_Decomposition <- localmoran(as.data.frame(Data[, c("DecompositionWTP")])[, 1], KNN_Weights)
Data$ScaleWTP_Decomposition <-
  as.numeric(scale(as.data.frame(Data[, c("DecompositionWTP")])[, 1]))
Data$lag_WTP_Decomposition <- as.numeric(lag.listw(KNN_Weights, Data$ScaleWTP_Decomposition))


## Classify categories:
Data[(Data$ScaleWTP_Decomposition >= 0 &
        Data$lag_WTP_Decomposition <= 0) &
       (locm_Decomposition[, 5] <= 0.05), "quad_sig_WTP_Decomposition"] <- 3
Data[(Data$ScaleWTP_Decomposition >= 0 &
        Data$lag_WTP_Decomposition <= 0) &
       (locm_Decomposition[, 5] <= 0.05), "quad_sig_WTP_Decomposition"] <- 4
Data[(Data$ScaleWTP_Decomposition <= 0 &
        Data$lag_WTP_Decomposition >= 0) &
       (locm_Decomposition[, 5] <= 0.05), "quad_sig_WTP_Decomposition"] <- 5
Data[(Data$ScaleWTP_Decomposition >= 0 &
        Data$lag_WTP_Decomposition >= 0) &
       (locm_Decomposition[, 5] <= 0.05), "quad_sig_WTP_Decomposition"] <- 1
Data[(Data$ScaleWTP_Decomposition <= 0 &
        Data$lag_WTP_Decomposition <= 0) &
       (locm_Decomposition[, 5] <= 0.05), "quad_sig_WTP_Decomposition"] <- 2
Data$quad_sig_WTP <-
  ifelse(is.na(Data$quad_sig_WTP_Decomposition) == TRUE, 5, Data$quad_sig_WTP_Decomposition)

## Labels for the plots
breaks <- seq(1, 5, 1)
labels <-
  c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(Data$quad_sig_WTP_Decomposition, breaks)
colors <- c(RColorBrewer::brewer.pal(9,"Blues")[c(9,7,5,3,1)])
Data$Colors_Decomposition <- as.character(col2hex(c(RColorBrewer::brewer.pal(9,"Blues")[c(9,7,5,3,1)])[np]))

Data$quad_sig_WTP_Decomposition <- recode(Data$quad_sig_WTP_Decomposition,
                                          '5'="Not Signif.",
                                          '4'="Low-High",
                                          '3'="High-Low",
                                          '2'="Low-Low",
                                          '1'="High-High")


#----------------------------------------------------------------------
## Colour High:
#----------------------------------------------------------------------


locm_ColourHigh <- localmoran(as.data.frame(Data[, c("ColourWTP2")])[, 1], KNN_Weights)
Data$ScaleWTP_ColourHigh <-
  as.numeric(scale(as.data.frame(Data[, c("ColourWTP2")])[, 1]))
Data$lag_WTP_ColourHigh <- as.numeric(lag.listw(KNN_Weights, Data$ScaleWTP_ColourHigh))


## Classify categories:
Data[(Data$ScaleWTP_ColourHigh >= 0 &
        Data$lag_WTP_ColourHigh <= 0) &
       (locm_ColourHigh[, 5] <= 0.05), "quad_sig_WTP_ColourHigh"] <- 3
Data[(Data$ScaleWTP_ColourHigh >= 0 &
        Data$lag_WTP_ColourHigh <= 0) &
       (locm_ColourHigh[, 5] <= 0.05), "quad_sig_WTP_ColourHigh"] <- 4
Data[(Data$ScaleWTP_ColourHigh <= 0 &
        Data$lag_WTP_ColourHigh >= 0) &
       (locm_ColourHigh[, 5] <= 0.05), "quad_sig_WTP_ColourHigh"] <- 5
Data[(Data$ScaleWTP_ColourHigh >= 0 &
        Data$lag_WTP_ColourHigh >= 0) &
       (locm_ColourHigh[, 5] <= 0.05), "quad_sig_WTP_ColourHigh"] <- 1
Data[(Data$ScaleWTP_ColourHigh <= 0 &
        Data$lag_WTP_ColourHigh <= 0) &
       (locm_ColourHigh[, 5] <= 0.05), "quad_sig_WTP_ColourHigh"] <- 2
Data$quad_sig_WTP <-
  ifelse(is.na(Data$quad_sig_WTP_ColourHigh) == TRUE, 5, Data$quad_sig_WTP_ColourHigh)

## Labels for the plots
breaks <- seq(1, 5, 1)
labels <-
  c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(Data$quad_sig_WTP_ColourHigh, breaks)
colors <- c(RColorBrewer::brewer.pal(9,"Blues")[c(9,7,5,3,1)])
Data$Colors_ColourHigh <- as.character(col2hex(c(RColorBrewer::brewer.pal(9,"Blues")[c(9,7,5,3,1)])[np]))

Data$quad_sig_WTP_ColourHigh <- recode(Data$quad_sig_WTP_ColourHigh,
                                   '5'="Not Signif.",
                                   '4'="Low-High",
                                   '3'="High-Low",
                                   '2'="Low-Low",
                                   '1'="High-High")


#----------------------------------------------------------------------
## Smell High:
#----------------------------------------------------------------------


locm_SmellHigh <- localmoran(as.data.frame(Data[, c("SmellWTP2")])[, 1], KNN_Weights)
Data$ScaleWTP_SmellHigh <-
  as.numeric(scale(as.data.frame(Data[, c("SmellWTP2")])[, 1]))
Data$lag_WTP_SmellHigh <- as.numeric(lag.listw(KNN_Weights, Data$ScaleWTP_SmellHigh))


## Classify categories:
Data[(Data$ScaleWTP_SmellHigh >= 0 &
        Data$lag_WTP_SmellHigh <= 0) &
       (locm_SmellHigh[, 5] <= 0.05), "quad_sig_WTP_SmellHigh"] <- 3
Data[(Data$ScaleWTP_SmellHigh >= 0 &
        Data$lag_WTP_SmellHigh <= 0) &
       (locm_SmellHigh[, 5] <= 0.05), "quad_sig_WTP_SmellHigh"] <- 4
Data[(Data$ScaleWTP_SmellHigh <= 0 &
        Data$lag_WTP_SmellHigh >= 0) &
       (locm_SmellHigh[, 5] <= 0.05), "quad_sig_WTP_SmellHigh"] <- 5
Data[(Data$ScaleWTP_SmellHigh >= 0 &
        Data$lag_WTP_SmellHigh >= 0) &
       (locm_SmellHigh[, 5] <= 0.05), "quad_sig_WTP_SmellHigh"] <- 1
Data[(Data$ScaleWTP_SmellHigh <= 0 &
        Data$lag_WTP_SmellHigh <= 0) &
       (locm_SmellHigh[, 5] <= 0.05), "quad_sig_WTP_SmellHigh"] <- 2
Data$quad_sig_WTP <-
  ifelse(is.na(Data$quad_sig_WTP_SmellHigh) == TRUE, 5, Data$quad_sig_WTP_SmellHigh)

## Labels for the plots
breaks <- seq(1, 5, 1)
labels <-
  c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(Data$quad_sig_WTP_SmellHigh, breaks)
colors <- c(RColorBrewer::brewer.pal(9,"Blues")[c(9,7,5,3,1)])
Data$Colors_SmellHigh <- as.character(col2hex(c(RColorBrewer::brewer.pal(9,"Blues")[c(9,7,5,3,1)])[np]))

Data$quad_sig_WTP_SmellHigh <- recode(Data$quad_sig_WTP_SmellHigh,
                                  '5'="Not Signif.",
                                  '4'="Low-High",
                                  '3'="High-Low",
                                  '2'="Low-Low",
                                  '1'="High-High")

#----------------------------------------------------------------------
## Sound High:
#----------------------------------------------------------------------


locm_SoundHigh <- localmoran(as.data.frame(Data[, c("SoundWTP2")])[, 1], KNN_Weights)
Data$ScaleWTP_SoundHigh <-
  as.numeric(scale(as.data.frame(Data[, c("SoundWTP2")])[, 1]))
Data$lag_WTP_SoundHigh <- as.numeric(lag.listw(KNN_Weights, Data$ScaleWTP_SoundHigh))


## Classify categories:
Data[(Data$ScaleWTP_SoundHigh >= 0 &
        Data$lag_WTP_SoundHigh <= 0) &
       (locm_SoundHigh[, 5] <= 0.05), "quad_sig_WTP_SoundHigh"] <- 3
Data[(Data$ScaleWTP_SoundHigh >= 0 &
        Data$lag_WTP_SoundHigh <= 0) &
       (locm_SoundHigh[, 5] <= 0.05), "quad_sig_WTP_SoundHigh"] <- 4
Data[(Data$ScaleWTP_SoundHigh <= 0 &
        Data$lag_WTP_SoundHigh >= 0) &
       (locm_SoundHigh[, 5] <= 0.05), "quad_sig_WTP_SoundHigh"] <- 5
Data[(Data$ScaleWTP_SoundHigh >= 0 &
        Data$lag_WTP_SoundHigh >= 0) &
       (locm_SoundHigh[, 5] <= 0.05), "quad_sig_WTP_SoundHigh"] <- 1
Data[(Data$ScaleWTP_SoundHigh <= 0 &
        Data$lag_WTP_SoundHigh <= 0) &
       (locm_SoundHigh[, 5] <= 0.05), "quad_sig_WTP_SoundHigh"] <- 2
Data$quad_sig_WTP <-
  ifelse(is.na(Data$quad_sig_WTP_SoundHigh) == TRUE, 5, Data$quad_sig_WTP_SoundHigh)

## Labels for the plots
breaks <- seq(1, 5, 1)
labels <-
  c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(Data$quad_sig_WTP_SoundHigh, breaks)
colors <- c(RColorBrewer::brewer.pal(9,"Blues")[c(9,7,5,3,1)])
Data$Colors_SoundHigh <- as.character(col2hex(c(RColorBrewer::brewer.pal(9,"Blues")[c(9,7,5,3,1)])[np]))

Data$quad_sig_WTP_SoundHigh <- recode(Data$quad_sig_WTP_SoundHigh,
                                  '5'="Not Signif.",
                                  '4'="Low-High",
                                  '3'="High-Low",
                                  '2'="Low-Low",
                                  '1'="High-High")

#----------------------------------------------------------------------
## Decomposition High:
#----------------------------------------------------------------------


locm_DecompositionHigh <- localmoran(as.data.frame(Data[, c("DecompositionWTP2")])[, 1], KNN_Weights)
Data$ScaleWTP_DecompositionHigh <-
  as.numeric(scale(as.data.frame(Data[, c("DecompositionWTP2")])[, 1]))
Data$lag_WTP_DecompositionHigh <- as.numeric(lag.listw(KNN_Weights, Data$ScaleWTP_DecompositionHigh))


## Classify categories:
Data[(Data$ScaleWTP_DecompositionHigh >= 0 &
        Data$lag_WTP_DecompositionHigh <= 0) &
       (locm_DecompositionHigh[, 5] <= 0.05), "quad_sig_WTP_DecompositionHigh"] <- 3
Data[(Data$ScaleWTP_DecompositionHigh >= 0 &
        Data$lag_WTP_DecompositionHigh <= 0) &
       (locm_DecompositionHigh[, 5] <= 0.05), "quad_sig_WTP_DecompositionHigh"] <- 4
Data[(Data$ScaleWTP_DecompositionHigh <= 0 &
        Data$lag_WTP_DecompositionHigh >= 0) &
       (locm_DecompositionHigh[, 5] <= 0.05), "quad_sig_WTP_DecompositionHigh"] <- 5
Data[(Data$ScaleWTP_DecompositionHigh >= 0 &
        Data$lag_WTP_DecompositionHigh >= 0) &
       (locm_DecompositionHigh[, 5] <= 0.05), "quad_sig_WTP_DecompositionHigh"] <- 1
Data[(Data$ScaleWTP_DecompositionHigh <= 0 &
        Data$lag_WTP_DecompositionHigh <= 0) &
       (locm_DecompositionHigh[, 5] <= 0.05), "quad_sig_WTP_DecompositionHigh"] <- 2
Data$quad_sig_WTP <-
  ifelse(is.na(Data$quad_sig_WTP_DecompositionHigh) == TRUE, 5, Data$quad_sig_WTP_DecompositionHigh)

## Labels for the plots
breaks <- seq(1, 5, 1)
labels <-
  c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(Data$quad_sig_WTP_DecompositionHigh, breaks)
colors <- c(RColorBrewer::brewer.pal(9,"Blues")[c(9,7,5,3,1)])
Data$Colors_DecompositionHigh <- as.character(col2hex(c(RColorBrewer::brewer.pal(9,"Blues")[c(9,7,5,3,1)])[np]))

Data$quad_sig_WTP_DecompositionHigh <- recode(Data$quad_sig_WTP_DecompositionHigh,
                                          '5'="Not Signif.",
                                          '4'="Low-High",
                                          '3'="High-Low",
                                          '2'="Low-Low",
                                          '1'="High-High")



#----------------------------------------------------------------------
## Output Data:
#----------------------------------------------------------------------


## Exporting shapefile with clster information to GPKG format
st_write(Data,
         "WP5_Winter_DataPlusClusters_2022_07_21.gpkg",
         append=FALSE)


# End Of Script -----------------------------