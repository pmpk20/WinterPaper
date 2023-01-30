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
Winter <- data.frame(fread(here("OtherData","Winter_dataframe_Step4.csv")))
colnames(ModelOne_WTP) <- stri_replace_all_regex(colnames(ModelOne_WTP),
                                                 pattern = c("b_",".post.mean","beta_"),
                                                 replacement = c("","WTP",""),
                                                 vectorise_all = FALSE)

Winter <- cbind(Winter[,c(1:211)],
                Winter[,c(226:230)],
                ModelOne_WTP)


##Trim crazy WTPs
Winter <- Winter[Winter$TaxWTP >-50,]

colnames(Winter) <- stri_replace_all_regex(colnames(Winter),
                                                 pattern = c("2WTP"),
                                                 replacement = c("WTP2"),
                                                 vectorise_all = FALSE)


Data <- Winter



K = sqrt(nrow(Data))
Data <-
  Data[!is.na(Data$LonH),]
Data <-
  Data[!is.na(Data$LatH),]
Data <-
  Data[which(!duplicated(Data$LatH)),]
Data <-
  Data[which(!duplicated(Data$LonH)),]

Woodlands <- data.frame(cbind(Data$LatH,abs(Data$LonH)))
coords <-data.matrix(Woodlands) #converting into matrix class
# coords <- st_centroid(st_geometry(Data), of_largest_polygon=TRUE)

## This creates the spatial weights:
kw10 <- knearneigh(coords, k=K)
kw10kmnb <- knn2nb(kw10)
dist <- nbdists(kw10kmnb, coords)
dist2 <- lapply(dist, function(x)
  1 / (x ^ 2))
NearestNeighbours <-
  nb2listw(kw10kmnb, glist = dist2)



## Estimate Global Moran values using monte-carlo simulation
Colour2Moran_Plotname <-
  moran.mc(Data$ColourWTP2, NearestNeighbours, nsim = 500)
Smell2Moran_Plotname <-
  moran.mc(Data$SmellWTP2, NearestNeighbours, nsim = 500)
Sound2Moran_Plotname <-
  moran.mc(Data$SoundWTP2, NearestNeighbours, nsim = 500)
Deadwood2Moran_Plotname <-
  moran.mc(Data$DeadwoodWTP2, NearestNeighbours, nsim = 500)

ColourMoran_Plotname <-
  moran.mc(Data$ColourWTP, NearestNeighbours, nsim = 500)
SmellMoran_Plotname <-
  moran.mc(Data$SmellWTP, NearestNeighbours, nsim = 500)
SoundMoran_Plotname <-
  moran.mc(Data$SoundWTP, NearestNeighbours, nsim = 500)
DeadwoodMoran_Plotname <-
  moran.mc(Data$DeadwoodWTP, NearestNeighbours, nsim = 500)

TaxMoran_Plotname <-
  moran.mc(Data$TaxWTP, NearestNeighbours, nsim = 500)


# Results ---------------------------------



GlobalMoran_Statistics <- c(
  TaxMoran_Plotname$statistic,
  ColourMoran_Plotname$statistic,
  Colour2Moran_Plotname$statistic ,
  SmellMoran_Plotname$statistic ,
  Smell2Moran_Plotname$statistic ,
  SoundMoran_Plotname$statistic ,
  Sound2Moran_Plotname$statistic ,
  DeadwoodMoran_Plotname$statistic,
  Deadwood2Moran_Plotname$statistic)



GlobalMoran_Pvalues <- c(
  TaxMoran_Plotname$p.value,
  ColourMoran_Plotname$p.value,
  Colour2Moran_Plotname$p.value ,
  SmellMoran_Plotname$p.value ,
  Smell2Moran_Plotname$p.value ,
  SoundMoran_Plotname$p.value ,
  Sound2Moran_Plotname$p.value ,
  DeadwoodMoran_Plotname$p.value,
  Deadwood2Moran_Plotname$p.value)


rbind(GlobalMoran_Statistics,GlobalMoran_Pvalues) %>%
  round(3) %>%
  write.csv(quote=F,row.names=F)


