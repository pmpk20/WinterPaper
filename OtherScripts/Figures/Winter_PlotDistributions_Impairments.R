#### RELATE Winter Paper ####
## Function: Plots distributions of WTP by impairments
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 30/01/2023
## TODO: Change to be even more colour blind friendly


#------------------------------
# Replication Information: ####
#------------------------------


# R version 4.2.0 (2022-04-22 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19044)
#   [1] LC_COLLATE=English_United Kingdom.utf8  LC_CTYPE=English_United Kingdom.utf8
#   [1] here_1.0.1     reshape2_1.4.4 ggridges_0.5.3 ggplot2_3.3.6  magrittr_2.0.3 dplyr_1.0.9    apollo_0.2.7
#   [1] zoo_1.8-10          tidyselect_1.1.2    purrr_0.3.4         splines_4.2.0       lattice_0.20-45
# [6] colorspace_2.0-3    generics_0.1.2      vctrs_0.4.1         MCMCpack_1.6-3      utf8_1.2.2
# [11] survival_3.3-1      rlang_1.0.2         pillar_1.7.0        withr_2.5.0         glue_1.6.2
# [16] plyr_1.8.7          matrixStats_0.62.0  lifecycle_1.0.1     stringr_1.4.0       MatrixModels_0.5-0
# [21] munsell_0.5.0       gtable_0.3.0        mvtnorm_1.1-3       coda_0.19-4         miscTools_0.6-26
# [26] SparseM_1.81        RSGHB_1.2.2         quantreg_5.93       parallel_4.2.0      fansi_1.0.3
# [31] Rcpp_1.0.8.3        scales_1.2.0        tmvnsim_1.0-2       farver_2.1.0        mcmc_0.9-7
# [36] maxLik_1.5-2        mnormt_2.0.2        digest_0.6.29       stringi_1.7.6       rprojroot_2.0.3
# [41] numDeriv_2016.8-1.1 grid_4.2.0          cli_3.3.0           tools_4.2.0         sandwich_3.0-1
# [46] tibble_3.1.7        crayon_1.5.1        pkgconfig_2.0.3     MASS_7.3-56         ellipsis_0.3.2
# [51] Matrix_1.4-1        randtoolbox_1.31.1  R6_2.5.1            rngWELL_0.10-7      compiler_4.2.0


#------------------------------
# Setup Environment: ####
#------------------------------


library(magrittr)
library(dplyr)
library(apollo)
library(reshape2)
library(ggplot2)
library(ggridges)
library(distributional)
library(ggdist)
library(gridExtra)


#------------------------------
# Section 1: Import Data ####
#------------------------------


## The latest version with WTP appended is 2022_01_07
Winter <- data.frame(fread(here("WinterReplication/OtherData","Winter_dataframe_Step3.csv")))
WTP <- data.frame(fread(here("CEoutput/ModelTwo","Winter_MXL_ModelTwo_ConWTP.csv")))
Winter <- cbind(Winter,WTP)

#----------------------------------------------
# Section 2: Aggregate and Plot Smell Data ####
#----------------------------------------------


## Medium Level:
Smell_Medium_Density <- Winter %>%
  group_by(SmellIssues) %>%
  summarise(Mean = mean(SmellWTP),SD = sd(SmellWTP)) %>%
  ggplot()+
  aes(y=SmellIssues,xdist= dist_normal(Mean,SD))+
  stat_histinterval()+
  scale_y_continuous(name="Smell Impairment",
                     limits=c(0,2),
                     breaks = seq(0,1,1),
                     label=c("No Impairment","Some Impairment"))+
  scale_x_continuous(name="mWTP in GBP per HH per annum.",
                     limits=c(-10,10),
                     breaks = seq(-10,10,1))+
  ggtitle("Distribution of WTP for a medium variety of smells by impairments to smell.")


## High Level:
Smell_High_Density <- Winter %>%
  group_by(SmellIssues) %>%
  summarise(Mean = mean(SmellWTP2),SD = sd(SmellWTP2)) %>%
  ggplot()+
  aes(y=SmellIssues,xdist= dist_normal(Mean,SD))+
  stat_histinterval()+
  scale_y_continuous(name="Smell Impairment",
                     limits=c(0,2),
                     breaks = seq(0,1,1),
                     label=c("No Impairment","Some Impairment"))+
  scale_x_continuous(name="mWTP in GBP per HH per annum.",
                     limits=c(-10,10),
                     breaks = seq(-10,10,1))+
  ggtitle("Distribution of WTP for a high variety of smells by impairments to smell.")


grid.arrange(Smell_Medium_Density,Smell_High_Density)


#----------------------------------------------
# Section 3: Aggregate and Plot Sound Data ####
#----------------------------------------------


## Medium Level:
Sound_Medium_Density <- Winter %>%
  group_by(HearingIssues) %>%
  summarise(Mean = mean(SoundWTP),SD = sd(SoundWTP)) %>%
  ggplot()+
  aes(y=HearingIssues,xdist= dist_normal(Mean,SD))+
  stat_histinterval()+
  scale_y_continuous(name="Hearing Impairment",
                     limits=c(0,2),
                     breaks = seq(0,1,1),
                     label=c("No Impairment","Some Impairment"))+
  scale_x_continuous(name="mWTP in GBP per HH per annum.",
                     limits=c(-10,10),
                     breaks = seq(-10,10,1))+
  ggtitle("Distribution of WTP for a medium variety of sounds by hearing impairments.")


## High Level:
Sound_High_Density <- Winter %>%
  group_by(HearingIssues) %>%
  summarise(Mean = mean(SoundWTP2),SD = sd(SoundWTP2)) %>%
  ggplot()+
  aes(y=HearingIssues,xdist= dist_normal(Mean,SD))+
  stat_histinterval()+
  scale_y_continuous(name="Hearing Impairment",
                     limits=c(0,2),
                     breaks = seq(0,1,1),
                     label=c("No Impairment","Some Impairment"))+
  scale_x_continuous(name="mWTP in GBP per HH per annum.",
                     limits=c(-10,10),
                     breaks = seq(-10,10,1))+
  ggtitle("Distribution of WTP for a high variety of sounds by hearing impairments.")


grid.arrange(Sound_Medium_Density,Sound_High_Density)


#----------------------------------------------
# Section 4: Aggregate and Plot Colour Data ####
#----------------------------------------------


## Medium Level:
Colour_Medium_Density <- Winter %>%
  group_by(SightIssues) %>%
  summarise(Mean = mean(ColourWTP),SD = sd(ColourWTP)) %>%
  ggplot()+
  aes(y=SightIssues,xdist= dist_normal(Mean,SD))+
  stat_histinterval()+
  scale_y_continuous(name="Sight Impairment",
                     limits=c(0,2),
                     breaks = seq(0,1,1),
                     label=c("No Impairment","Some Impairment"))+
  scale_x_continuous(name="mWTP in GBP per HH per annum.",
                     limits=c(-10,10),
                     breaks = seq(-10,10,1))+
  ggtitle("Distribution of WTP for a medium variety of colours by sight impairments.")


## High Level:
Colour_High_Density <- Winter %>%
  group_by(SightIssues) %>%
  summarise(Mean = mean(ColourWTP2),SD = sd(ColourWTP2)) %>%
  ggplot()+
  aes(y=SightIssues,xdist= dist_normal(Mean,SD))+
  stat_histinterval()+
  scale_y_continuous(name="Sight Impairment",
                     limits=c(0,2),
                     breaks = seq(0,1,1),
                     label=c("No Impairment","Some Impairment"))+
  scale_x_continuous(name="mWTP in GBP per HH per annum.",
                     limits=c(-10,10),
                     breaks = seq(-10,10,1))+
  ggtitle("Distribution of WTP for a high variety of colours by sight impairments.")


grid.arrange(Colour_Medium_Density,Colour_High_Density)

#----------------------------------------------
# Section 5: Export arranged plots ####
#----------------------------------------------


## Arrange into 3 rows (colour, smell, sound) and 2 columns (medium, high).
### May need some relabelling to look nicer.
AllPlots <- grid.arrange(Colour_Medium_Density,Colour_High_Density,
             Smell_Medium_Density,Smell_High_Density,
             Sound_Medium_Density,Sound_High_Density,nrow=3,ncol=2)

## To record plot version
Date <- gsub(pattern = "-",replacement = "_",Sys.Date())


## Save output in highest DPI
ggsave(AllPlots, device = "jpeg",
       filename = paste0("Winter_ImpairmentPlot_",Date,".jpeg"),
       width=20,height=15,units = "cm",dpi=1000)


#----------------------------------------------
# Section 6: Bonus Code ####
#----------------------------------------------



# ## Bonus code which discretises continuous variable and maps distributions:
# Winter %>% mutate(Tree=case_when(
#   WoodlandsTree >=0 & WoodlandsTree < 20 ~'1',
#   WoodlandsTree >=20 & WoodlandsTree < 40 ~'2',
#   WoodlandsTree >=40 & WoodlandsTree < 60 ~'3',
#   WoodlandsTree >=60 & WoodlandsTree < 80 ~'4',
#   WoodlandsTree >=80 & WoodlandsTree <= 100 ~'5'
# )) %>%
#   group_by(Tree) %>%
#   summarise(Mean = mean(ColourWTP2),SD = sd(ColourWTP2)) %>%
# ggplot()+
#   aes(y=Tree,xdist= dist_normal(Mean,SD))+
#   stat_histinterval()


# End Of Script ------------------------------
