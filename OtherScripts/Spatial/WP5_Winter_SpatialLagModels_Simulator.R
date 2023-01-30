#### RELATE WP5: Winter Spatial Lag Models  ###############
# Script author: Peter King (p.m.king@kent.ac.uk)
# Last Edited: 29/07/2022.
# Here I estimate the spatial lag models


#----------------------------------------------------------------------------------------------------------
#### Section 0: Setup ####
#----------------------------------------------------------------------------------------------------------


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
rm(list=ls())



##----------------------------------------------------------------------------------------------------------
#### Step One: Read in data frame with all respondents ####
##----------------------------------------------------------------------------------------------------------

# Data Import: ---- 
Winter <- data.frame(fread(here("OtherData","Winter_dataframe_2022-01-07.csv")))
Draws <- data.frame(fread("WP5_Winter_MXL_ModelOne_2022_07_29_UCWTP.csv"))

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



# #----------------------------------------------------------------------------------------------------------
# #### Section Alpha: Setup parallelisation and benchmarking ####
# #----------------------------------------------------------------------------------------------------------
#



# Function Definition -----------------------------------------------------


# 
# Model_Attribute <- ColourWTPNew ~ WoodlandsScore+MilesDistance+MostRecentVisit+DummyAge+Gender+IncomeDummy
## Example Call: SpatialLagModel(Model_Attribute,Data_Winter)

PVSimulator <- function(Model,Data) {
  

  # SLModel <- lagsarlm(Model, data=Data, NearestNeighbours) # Spatial lag model
  OLS_dummy_Attribute<-lm(Model, data=Data) # Dummy regression
  Test_Attribute <- lm.LMtests(OLS_dummy_Attribute,NearestNeighbours, test = "LMlag" )

  Test_Attribute$LMlag$p.value
  
}


# Function Output ---------------------------------------------------------

# Model_Attribute <-   ColourWTPNew ~ WoodlandsScore+MilesDistance+MostRecentVisit+DummyAge+Gender+IncomeDummy


############ Testing Colour:


# New faster more memory intensive version
PoeTester <- function(Attribute) {
  PValues <- matrix(0,1000,1) %>% as.data.frame()
  for (i in 1:1000){
    PValues[i,] <- PVSimulator(as.formula(paste0(Attribute,i,"~WoodlandsScore+
                     MilesDistance+MostRecentVisit+DummyAge+
                     Gender+IncomeDummy")), Data) %>% as.numeric()
                   }
  PValues %>% as.data.frame()
}


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

write.csv(SpatialLags,"SpatialLags_SimulatedPVS.csv")
# parallel::stopCluster(cl = my.cluster)

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


ggsave(SpatialLagsPlot, device = "jpeg",
       filename = "SpatialLagsPlot_2022_09_24.jpeg",
       width=20,height=15,units = "cm",dpi=1000)


# 
# # ## Spatial Lag Model for Colour low estimate:
# PVSimulator(as.formula("b_Colour.1~WoodlandsScore+
#                      MilesDistance+MostRecentVisit+DummyAge+
#                      Gender+IncomeDummy"), Data)
# 
# # ## Spatial Lag Model for Colour low estimate:
# PVSimulator(as.formula("ColourWTP2~WoodlandsScore+
#                      MilesDistance+MostRecentVisit+DummyAge+
#                      Gender+IncomeDummy+ Impairment + GDHI + Density + Area_ha_median
#                      "), Data)

