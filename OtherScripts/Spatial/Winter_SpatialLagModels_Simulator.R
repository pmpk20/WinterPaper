#### RELATE WP5: Winter Spatial Lag Models  ###############
# Script author: Peter King (p.m.king@kent.ac.uk)
# Last Edited: 03/02/2023
# Here I estimate the spatial lag model for each attribute for 1000 draws.


#----------------------------------------------------------------------------------------------------------
#### Section 0: Setup ####
#----------------------------------------------------------------------------------------------------------



## sessionInfo() for my office PC not the HPC ---------------------------------------------------------------
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
#   [1] foreach_1.5.2        xtable_1.8-4         spatialreg_1.2-6     Matrix_1.5-3         spdep_1.2-7          spData_2.2.1
# [7] sp_1.6-0             sf_1.0-9             spacetime_1.2-8      microbenchmark_1.4.9 forcats_0.5.2        purrr_1.0.1
# [13] readr_2.1.3          tidyr_1.2.1          tibble_3.1.8         tidyverse_1.3.2      ggdist_3.2.1         matrixStats_0.63.0
# [19] Rfast_2.0.6          RcppZiggurat_0.1.6   Rcpp_1.0.9           lubridate_1.9.0      timechange_0.2.0     PostcodesioR_0.3.1
# [25] stringi_1.7.12       stringr_1.5.0        data.table_1.14.6    mded_0.1-2           reshape2_1.4.4       ggridges_0.5.4
# [31] ggplot2_3.4.0        magrittr_2.0.3       dplyr_1.0.10         apollo_0.2.8         here_1.0.1
#
# loaded via a namespace (and not attached):
#   [1] googledrive_2.0.0    colorspace_2.0-3     deldir_1.0-6         class_7.3-20         ellipsis_0.3.2       rprojroot_2.0.3
# [7] fs_1.6.0             proxy_0.4-27         rstudioapi_0.14      farver_2.1.1         MatrixModels_0.5-1   fansi_1.0.3
# [13] mvtnorm_1.1-3        RSGHB_1.2.2          xml2_1.3.3           codetools_0.2-18     splines_4.2.0        mnormt_2.1.1
# [19] doParallel_1.0.17    knitr_1.41           jsonlite_1.8.4       mcmc_0.9-7           broom_1.0.2          dbplyr_2.3.0
# [25] compiler_4.2.0       httr_1.4.4           backports_1.4.1      assertthat_0.2.1     fastmap_1.1.0        gargle_1.2.1
# [31] cli_3.6.0            s2_1.1.2             htmltools_0.5.4      quantreg_5.94        tools_4.2.0          coda_0.19-4
# [37] gtable_0.3.1         glue_1.6.2           wk_0.7.1             cellranger_1.1.0     vctrs_0.5.1          nlme_3.1-157
# [43] iterators_1.0.14     randtoolbox_2.0.3    xfun_0.36            rvest_1.0.3          lifecycle_1.0.3      rngWELL_0.10-9
# [49] googlesheets4_1.0.1  LearnBayes_2.15.1    MASS_7.3-56          zoo_1.8-11           scales_1.2.1         miscTools_0.6-26
# [55] hms_1.1.2            sandwich_3.0-2       expm_0.999-7         SparseM_1.81         RColorBrewer_1.1-3   yaml_2.3.6
# [61] e1071_1.7-12         boot_1.3-28          intervals_0.15.2     rlang_1.0.6          pkgconfig_2.0.3      distributional_0.3.1
# [67] evaluate_0.20        lattice_0.20-45      tidyselect_1.2.0     plyr_1.8.8           R6_2.5.1             generics_0.1.3
# [73] DBI_1.1.3            pillar_1.8.1         haven_2.5.1          withr_2.5.0          units_0.8-1          xts_0.12.2
# [79] survival_3.3-1       modelr_0.1.10        crayon_1.5.2         KernSmooth_2.23-20   utf8_1.2.2           tzdb_0.3.0
# [85] rmarkdown_2.20       maxLik_1.5-2         grid_4.2.0           readxl_1.4.1         classInt_0.4-8       reprex_2.0.2
# [91] digest_0.6.31        numDeriv_2016.8-1.1  MCMCpack_1.6-3       munsell_0.5.0
# install.packages(c("doSNOW","doParallel","doMPI","foreach"),repos="http://cran.us.r-project.org")



## Libraries: ---------------------------------------------------------------
library(parallel)
library(spacetime)
library(dplyr)
library(sf)
library(stringi)
library(stringr)
library(PostcodesioR)
library(lubridate)
library(spdep)
library(spatialreg)
library(xtable)
library(here)
library(data.table)
library(Rfast)
library(matrixStats)
library(reshape2)
library(ggdist)
library(ggplot2)
library(ggridges)
library(foreach)
rm(list=ls())



##----------------------------------------------------------------------------------------------------------
#### Step One: Read in data frame with all respondents ####
##----------------------------------------------------------------------------------------------------------


# Data Import: ----
Winter <- data.frame(fread(here("OtherData","Winter_Step4.csv")))
Draws <- data.frame(fread(here("CEoutput/ModelTwo","Winter_MXL_ModelTwo_UnconWTP.csv")))



## Drop rows that have missing data in any of the following we use in the models:
Winter <- Winter %>% drop_na(Colour_WTP_Medium,WoodlandsScore,
                      MilesDistance,MostRecentVisit,
                      DummyAge,Gender,
                      IncomeDummy, Impairment,
                      GDHI,Density,Area_ha_median)



Data_Combined <- bind_cols(
  Winter, Draws
)

# GB_Winter <- st_read("GB_Winter_2022_07_30.gpkg")

Data <- Data_Combined
Data_Winter <- Data_Combined


#-----------------------------------------
# Section 2: Define Nearest Neighbours ####
#-----------------------------------------



K = sqrt(nrow(Data))
Data <- Data[!is.na(Data$LonH),]
Data <- Data[!is.na(Data$LatH),]
Data <- Data[which(!duplicated(Data$LatH)),]
Data <- Data[which(!duplicated(Data$LonH)),]


if(missing(K)) {
  K = sqrt(nrow(Data))
} else {
  K
}

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



#----------------------------------------------------------------------------------------------------------
# #### Section Alpha: Setup parallelisation ####
# #----------------------------------------------------------------------------------------------------------

list.of.packages <- c(
  "mded", "here","magrittr","dplyr",
  "microbenchmark",
  "foreach",
  "doParallel",
  "ranger",
  "palmerpenguins",
  "tidyverse","svMisc",
  "kableExtra","Rfast","matrixStats",
  "parallel","spacetime",
  "sf","stringi","stringr","PostcodesioR","lubridate",
  "spdep","spatialreg",
  "data.table"
)
#
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages) > 0){
  install.packages(new.packages, dep=TRUE,repos="http://cran.us.r-project.org")
}
#
#loading packages
for(package.i in list.of.packages){
  suppressPackageStartupMessages(
    library(
      package.i,
      character.only = TRUE
    )
  )
}
#
my.cluster <- parallel::makeCluster(
  10,
  type = "PSOCK"
)
#
#check cluster definition (optional)
print(my.cluster)
doParallel::registerDoParallel(cl = my.cluster)

#check if it is registered (optional)
foreach::getDoParRegistered()


# #----------------------------------------------------------------------------------------------------------
# #### Section 2: Define functions ####
# #----------------------------------------------------------------------------------------------------------
#

## Repeats LM tests
PVSimulator <- function(Model,Data) {


  # SLModel <- lagsarlm(Model, data=Data, NearestNeighbours) # Spatial lag model
  OLS_dummy_Attribute<-lm(Model, data=Data) # Dummy regression
  Test_Attribute <- lm.LMtests(OLS_dummy_Attribute,NearestNeighbours, test = "LMlag" )

  Test_Attribute$LMlag$p.value

}


## Calculates 1000 values the slow way:
PoeTester <- function(Attribute) {
  PValues <- matrix(0,1000,1) %>% as.data.frame()
  PValues <- foreach(i = 1:1000,.combine=c,.export=c("PVSimulator","PValues",
                                          "Data","NearestNeighbours"),
          .packages = list.of.packages)%dopar% {
  PVSimulator(as.formula(paste0(Attribute,i,"~WoodlandsScore+
                     MilesDistance+MostRecentVisit+DummyAge+
                     Gender+IncomeDummy")), Data) %>% as.numeric()
  }
  PValues %>% as.data.frame()
}



# ## Calculates 1000 values the slow way:
# PoeTester <- function(Attribute) {
#   PValues <- matrix(0,1000,1) %>% as.data.frame()
#   for (i in 1:1000){
#     PValues[i,] <- PVSimulator(as.formula(paste0(Attribute,i,"~WoodlandsScore+
#                      MilesDistance+MostRecentVisit+DummyAge+
#                      Gender+IncomeDummy")), Data) %>% as.numeric()
#                    }
#   PValues %>% as.data.frame()
# }




# #----------------------------------------------------------------------------------------------------------
# #### Section 3: Repeat test per variable ####
# #----------------------------------------------------------------------------------------------------------
#



### my attempt to foreach() this step, currently defunct ---------------------------------------------------------------------------
# VarList <- c("beta_Tax.",
#              "b_Colour.")

# VarList <- c("beta_Tax.",
#              "b_Colour.","b_Colour2.",
#              "b_Sound.","b_Sound2.",
#              "b_Smell.","b_Smell2.",
#              "b_Deadwood.","b_Deadwood2.")


# SLPValues <- matrix(0,1000,VarList %>% length()) %>% as.data.frame()
#   SLPValues <- foreach(i = 1:length(VarList),.combine=cbind,.export=c("VarList"),
#                      .packages = list.of.packages)%dopar% {
#                        PoeTester(VarList[i]) %>% as.numeric()
#                      }
#   SpatialLags <-  SLPValues %>% as.data.frame()
#   SpatialLags %>% dim()




## Just run each function here:
SpatialLags <- bind_cols(
  PoeTester("beta_Tax."),

  PoeTester("b_Colour."),
  PoeTester("b_Colour2."),

  PoeTester("b_Sound."),
  PoeTester("b_Sound2."),

  PoeTester("b_Smell."),
  PoeTester("b_Smell2."),

  PoeTester("b_Deadwood."),
  PoeTester("b_Deadwood2.")
)



## Export values:
fwrite(SpatialLags %>% data.frame(),sep=",",
       here("OtherOutput/Spatial","Table7_Simulations.csv"))
# parallel::stopCluster(cl = my.cluster)





# #----------------------------------------------------------------------------------------------------------
# #### Section 4: Now repeat for plots ####
# #----------------------------------------------------------------------------------------------------------
#


## Plot the distribution of P values here:
SpatialLagsPlot <- SpatialLags %>%
  reshape2::melt() %>%
  ggplot(aes(x=value,y=variable,group=variable,fill=variable))+
  stat_dist_halfeye()+ ## Love halfeye() from GGDist
  scale_x_continuous(name="P Values", limits=c(0,1),breaks = seq(0,1,0.05))+
  theme(legend.position = "none")+
  geom_vline(xintercept = 0.10)+
  geom_vline(xintercept = 0.05)+
  geom_vline(xintercept = 0.01)+
  theme_bw()+
  scale_y_discrete(name="Variables",
                   labels=c(
                     "Tax",
                     "ColourMedium",
                     "ColourHigh",
                     "SoundMedium",
                     "SoundHigh",
                     "SmellMedium",
                     "SmellHigh",
                     "DeadwoodMedium",
                     "DeadwoodHigh"
                   ))+
  ggtitle("Distribution of calculated spatial lag P values.")


## Export with high dpi, png format, and in correct location:
ggsave(SpatialLagsPlot, device = "png",
       filename = here("OtherOutput/Figures","Table7_Plots.png"),
       width=20,height=15,units = "cm",dpi=1000)


## End of script -------------------------------------------
