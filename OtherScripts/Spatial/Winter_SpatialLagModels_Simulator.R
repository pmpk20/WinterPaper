#### RELATE WP5: Winter Spatial Lag Models  ###############
# Script author: Peter King (p.m.king@kent.ac.uk)
# Last Edited: 01/02/2023
# Here I estimate the spatial lag models


#----------------------------------------------------------------------------------------------------------
#### Section 0: Setup ####
#----------------------------------------------------------------------------------------------------------



# install.packages(c("doSNOW","doParallel","doMPI","foreach"),repos="http://cran.us.r-project.org")
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
Data <-
  Data[!is.na(Data$LonH),]
Data <-
  Data[!is.na(Data$LatH),]
Data <-
  Data[which(!duplicated(Data$LatH)),]
Data <-
  Data[which(!duplicated(Data$LonH)),]


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


## Verifying that this loop works:
# Test <- PoeTester("beta_Tax.")
# Test %>% dim()
# Test %>% length()





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



SpatialLagsPlot <- SpatialLags %>%
  reshape2::melt() %>%
  ggplot(aes(x=value,y=variable,group=variable,fill=variable))+
  stat_dist_halfeye()+
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


## Export
ggsave(SpatialLagsPlot, device = "png",
       filename = here("OtherOutput/Figures","Table7_Plots.png"),
       width=20,height=15,units = "cm",dpi=1000)


## End of script -------------------------------------------
