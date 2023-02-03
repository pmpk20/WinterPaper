#### RELATE WP5: Winter Spatial Lag Models  ###############
# Script author: Peter King (p.m.king@kent.ac.uk)
# Last Edited: 30/01/2023
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
rm(list=ls())



##----------------------------------------------------------------------------------------------------------
#### Step One: Read in data frame with all respondents ####
##----------------------------------------------------------------------------------------------------------


## The latest version with WTP appended is 2022_01_07
# Winter <- data.frame(read.csv(here("OtherData","Winter_dataframe_2022-01-07.csv")))
# ModelOne_WTP <- data.frame(read.csv(here("CEModelData","WP5_Winter_MXL_ModelOne_2022_07_29_WTP.csv")))
# ModelOne_WTP <- ModelOne_WTP %>% select(ends_with(".post.mean"))
# colnames(ModelOne_WTP) <- stri_replace_all_regex(colnames(ModelOne_WTP),
#                                                  pattern = c("b_",".post.mean","beta_"),
#                                                  replacement = c("","WTP",""),
#                                                  vectorise_all = FALSE)
#
# colnames(ModelOne_WTP) <- stri_replace_all_regex(colnames(ModelOne_WTP),
#                                                  pattern = c("2WTP"),
#                                                  replacement = c("WTP2"),
#                                                  vectorise_all = FALSE)
#
#
# Winter <- cbind(Winter[,c(1:211)],
#                 Winter[,c(226:230)],
#                 ModelOne_WTP)
#
#
# colnames(Winter)[which(names(Winter)=="DeadwoodWTP")] <- "DecompositionWTP"
# colnames(Winter)[which(names(Winter)=="DeadwoodWTP2")] <- "DecompositionWTP2"
#
#
# Winter$Impairment <- ifelse((Winter$SightIssues==1)|
#                                 (Winter$SmellIssues==1)|
#                                 (Winter$HearingIssues==1),1,0)


GB_Winter <- st_read(here("OtherData","GB_Winter_Step4.gpkg"))

Data <- GB_Winter
Data_Winter <- Data

#-----------------------------------------
# Section 2: Define Nearest Neighbours ####
#-----------------------------------------


K = sqrt(nrow(Data))
Data <- Data[!is.na(Data$LonH),]
Data <- Data[!is.na(Data$LatH),]
Data <- Data[which(!duplicated(Data$LatH)),]
Data <- Data[which(!duplicated(Data$LonH)),]


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



# Function Definition -----------------------------------------------------


#
# Model_Attribute <- ColourWTPNew ~ WoodlandsScore+MilesDistance+MostRecentVisit+DummyAge+Gender+IncomeDummy
## Example Call: SpatialLagModel(Model_Attribute,Data_Winter)

SpatialLagModel <- function(Model, WTP,Data, K) {

  # Data <- Data[!is.na(Data$Density),]
  # Data <- Data[!is.na(Data$Area_ha_median),]
  # Data <- Data[!is.na(Data$GDHI),]

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

  SLM_dummy_Attribute <- paste0("SLM_Dummy_", WTP)
  # SLM_dummy_Attribute <- lm(Model, data=Data)
  SLM_dummy_Attribute <- lagsarlm(Model, data=Data, NearestNeighbours)
  SLM_dummy_Attribute_S <- summary(SLM_dummy_Attribute)
  OLS_dummy_Attribute<-lm(Model, data=Data) # Dummy regression
  logLik(OLS_dummy_Attribute)
  OLS_dummy_Attribute_S <- summary(OLS_dummy_Attribute)
  Test_Attribute <- lm.LMtests(OLS_dummy_Attribute,NearestNeighbours, test = "LMlag" )
  Stat_Attribute <- round(t(cbind("Stat"=Test_Attribute$LMlag$statistic,"P.V"=Test_Attribute$LMlag$p.value)),3)
  An_Attribute <- anova(SLM_dummy_Attribute,OLS_dummy_Attribute)

  Result <- ifelse(
    Test_Attribute$LMlag$p.value < 0.05,
    paste0("Missing Spatial Lag with P: ",round(Test_Attribute$LMlag$p.value,3)),
    paste0("No Spatial Lag with P: ",round(Test_Attribute$LMlag$p.value,3))
  )

  saveRDS(SLM_dummy_Attribute, paste0("SLM_Dummy_",WTP,"_K", round(K),"_Model",".rds"))
  saveRDS(Test_Attribute, paste0("SLM_Dummy_",WTP,"_K", round(K),"_Result",".rds"))

  return(list(Result,SLM_dummy_Attribute_S))
}


# Function Output ---------------------------------------------------------

# Model_Attribute <-   ColourWTPNew ~ WoodlandsScore+MilesDistance+MostRecentVisit+DummyAge+Gender+IncomeDummy


############ Testing Colour:



# ## Spatial Lag Model for Colour low estimate:
Model <- as.formula("ColourWTP~WoodlandsScore+
                     MilesDistance+MostRecentVisit+DummyAge+
                     Gender+IncomeDummy+ Impairment + GDHI + Density + Area_ha_median
                     ")
SpatialLagModel(Model, "ColourWTP", Data_Winter,K = 5)

# + Impairment + GDHI + Density + Area_ha_median
#   GDHI+Density+Area_ha_median

# ## Spatial Lag Model for Colour high estimate:
Model <- as.formula("ColourWTP2~WoodlandsScore+
                     MilesDistance+MostRecentVisit+DummyAge+
                     Gender+IncomeDummy+ Impairment + GDHI + Density + Area_ha_median
                     ")
SpatialLagModel(Model, "ColourWTP2", Data_Winter,K = 5)


############ Testing Sound:

# ## Spatial Lag Model for Sound low estimate:
Model <- as.formula("SoundWTP~WoodlandsScore+
                     MilesDistance+MostRecentVisit+DummyAge+
                     Gender+IncomeDummy+ Impairment + GDHI + Density + Area_ha_median
                     ")
SpatialLagModel(Model, "SoundWTP", Data_Winter,K = 5)


# ## Spatial Lag Model for Sound high estimate:
Model <- as.formula("SoundWTP2~WoodlandsScore+
                     MilesDistance+MostRecentVisit+DummyAge+
                     Gender+IncomeDummy+ Impairment + GDHI + Density + Area_ha_median
                     ")
SpatialLagModel(Model, "SoundWTP2", Data_Winter,K = 5)


############ Testing Smell:




# ## Spatial Lag Model for Smell low estimate:
Model <- as.formula("SmellWTP~WoodlandsScore+
                     MilesDistance+MostRecentVisit+DummyAge+
                     Gender+IncomeDummy+ Impairment + GDHI + Density + Area_ha_median
                     ")
SpatialLagModel(Model, "SmellWTP", Data_Winter,K = 5)


# ## Spatial Lag Model for Smell high estimate:
Model <- as.formula("SmellWTP2~WoodlandsScore+
                     MilesDistance+MostRecentVisit+DummyAge+
                     Gender+IncomeDummy+ Impairment + GDHI + Density + Area_ha_median
                     ")
SpatialLagModel(Model, "SmellWTP2", Data_Winter,K = 5)


############ Testing Decomposition:

## Make the model formula:


# ## Spatial Lag Model for Decomposition low estimate:
Model <- as.formula("DecompositionWTP~WoodlandsScore+
                     MilesDistance+MostRecentVisit+DummyAge+
                     Gender+IncomeDummy+ Impairment + GDHI + Density + Area_ha_median
                     ")
SpatialLagModel(Model, "DecompositionWTP", Data_Winter,K = 5)


# ## Spatial Lag Model for Decomposition high estimate:
Model <- as.formula("DecompositionWTP2~WoodlandsScore+
                     MilesDistance+MostRecentVisit+DummyAge+
                     Gender+IncomeDummy+ Impairment + GDHI + Density + Area_ha_median
                     ")
SpatialLagModel(Model, "DecompositionWTP2", Data_Winter,K = 5)



#----------------------------------------------------------------------------------------------------------
#### Section 2: Outputs ####
#----------------------------------------------------------------------------------------------------------


# Read in Results: --------------------------------------------------------
# ColourWTPNewModel <- readRDS("SLM_Dummy_ColourWTPNew_K5_Model.rds")
ColourWTPModel <- readRDS("SLM_Dummy_ColourWTP_K5_Model.rds")
ColourWTP2Model <- readRDS("SLM_Dummy_ColourWTP2_K5_Model.rds")


# SoundWTPNewModel <- readRDS("SLM_Dummy_SoundWTPNew_K5_Model.rds")
SoundWTPModel <- readRDS("SLM_Dummy_SoundWTP_K5_Model.rds")
SoundWTP2Model <- readRDS("SLM_Dummy_SoundWTP2_K5_Model.rds")

# SmellWTPNewModel <- readRDS("SLM_Dummy_SmellWTPNew_K5_Model.rds")
SmellWTPModel <- readRDS("SLM_Dummy_SmellWTP_K5_Model.rds")
SmellWTP2Model <- readRDS("SLM_Dummy_SmellWTP2_K5_Model.rds")

# DecompositionWTPNewModel <- readRDS("SLM_Dummy_DecompositionWTPNew_K5_Model.rds")
DecompositionWTPModel <- readRDS("SLM_Dummy_DecompositionWTP_K5_Model.rds")
DecompositionWTP2Model <- readRDS("SLM_Dummy_DecompositionWTP2_K5_Model.rds")


# Read in Results: --------------------------------------------------------
# ColourWTPNewResult <- readRDS("SLM_Dummy_ColourWTPNew_K5_Result.rds")
ColourWTPResult <- readRDS("SLM_Dummy_ColourWTP_K5_Result.rds")
ColourWTP2Result <- readRDS("SLM_Dummy_ColourWTP2_K5_Result.rds")

# SoundWTPNewResult <- readRDS("SLM_Dummy_SoundWTPNew_K5_Result.rds")
SoundWTPResult <- readRDS("SLM_Dummy_SoundWTP_K5_Result.rds")
SoundWTP2Result <- readRDS("SLM_Dummy_SoundWTP2_K5_Result.rds")

# SmellWTPNewResult <- readRDS("SLM_Dummy_SmellWTPNew_K5_Result.rds")
SmellWTPResult <- readRDS("SLM_Dummy_SmellWTP_K5_Result.rds")
SmellWTP2Result <- readRDS("SLM_Dummy_SmellWTP2_K5_Result.rds")

# DecompositionWTPNewResult <- readRDS("SLM_Dummy_DecompositionWTPNew_K5_Result.rds")
DecompositionWTPResult <- readRDS("SLM_Dummy_DecompositionWTP_K5_Result.rds")
DecompositionWTP2Result <- readRDS("SLM_Dummy_DecompositionWTP2_K5_Result.rds")


# Output LM results: --------------------------------------------------------


## So this code outputs a LaTeX table of estimate, p.v stars and s.e in brackets ##
### You still need to change the LaTex by removing $\backslash$
ModelOutput <- function(Model,Result) {
  Estimates <- summary(Model)
  rbind(
      data.frame(
        "Value" = rownames(Estimates$Coef),
        "Data" = paste(ifelse(
          Estimates$Coef[, 4] < 0.01,
          paste0(round(Estimates$Coef[, 1], 3), "***"),
          ifelse(
            Estimates$Coef[, 4] < 0.05,
            paste0(round(Estimates$Coef[, 1], 3), "**"),
            ifelse(
              Estimates$Coef[, 4] < 0.1,
              paste0(round(Estimates$Coef[, 1], 3), "*"),
              round(Estimates$Coef[, 4], 3)))),
          "(",round(Estimates$Coef[,3],3),")")),

      data.frame("Value"=c("Stat"),"Data"=round(Result$LMlag$statistic,3)),

      data.frame("Value"=c("LR"),
                 "Data"=paste(ifelse(
                   Estimates$LR1$p.value < 0.01,
                   paste0(round(Estimates$rho, 3), "***"),
                   ifelse(
                     Estimates$LR1$p.value < 0.05,
                     paste0(round(Estimates$rho, 3), "**"),
                     ifelse(
                       Estimates$LR1$p.value < 0.1,
                       paste0(round(Estimates$rho, 3), "*"),
                       round(Estimates$LR1$p.value, 3)
                     )
                   )
                 ),
                 paste0("(", round(Estimates$rho.se, 3),")")))[1,],

      data.frame("Value"=c("logLik"),"Data"=round(logLik(Estimates),3)),
      data.frame("Value"=c("AIC"),"Data"=round(AIC(Estimates),3)))}


## Maybe a little ugly but outputs table in one go
cbind(
  "Attribute"=ModelOutput(ColourWTPModel,ColourWTPResult)[,1],
  "Colour: Medium"=ModelOutput(ColourWTPModel,ColourWTPResult)[,2],
  "Colour: High"=ModelOutput(ColourWTP2Model,ColourWTP2Result)[,2],

  "Smell: Medium"=ModelOutput(SmellWTPModel,SmellWTPResult)[,2],
  "Smell: High"=ModelOutput(SmellWTP2Model,SmellWTP2Result)[,2],

  "Sound: Medium"=ModelOutput(SoundWTPModel,SoundWTPResult)[,2],
  "Sound: High"=ModelOutput(SoundWTP2Model,SoundWTP2Result)[,2],

  "Decomposition: Medium"=ModelOutput(DecompositionWTPModel,DecompositionWTPResult)[,2],
  "Decomposition: High"=ModelOutput(DecompositionWTP2Model,DecompositionWTP2Result)[,2]) %>%
  noquote() %>%
  write.csv(quote=F,row.names=F)


# End of Script ----------------------------------------------------------------
