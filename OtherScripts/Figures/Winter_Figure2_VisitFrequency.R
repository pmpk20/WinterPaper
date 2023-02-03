#### RELATE WP5: Recovering unconditionals  ###############
# Script author: Peter King (p.m.king@kent.ac.uk)
# Last Edited: 01/02/2023
# Change: Removing figure minor y lines and adding pound sign.
# Description: So here we take all the unconditionals and use them to specify the
## mean, max, min, and quantiles for the distribution of each attribute in each
## VisitFrequency. So unconditionals > moments > summary > create plot.


#----------------------------------------------------------------------------------------------------------
#### Section 0: Setup and estimate models ####
#----------------------------------------------------------------------------------------------------------

# install.packages("data.table",repos="http://cran.us.r-project.org")
# options(scipen=90)
library(apollo)
library(tidyverse)
library(dplyr)
library(magrittr)
library(mded)
library(here)
library(data.table)
library(Rfast)


#----------------------------------------------------------------------------------------------------------
#### Section 1: Setup parallelisation and benchmarking ####
#----------------------------------------------------------------------------------------------------------



## The latest version with WTP appended is 2022_01_07
Winter <- data.frame(fread(here("OtherData","Winter_dataframe_Step3.csv")))

## Truncate to have same number of rows as in the models
Winter<- Winter[!is.na(Winter$MilesDistance),] ## Drop missing distances
Winter<- Winter[!is.na(Winter$Overall),] ## Drop respondents not completing BIOWELL




## This is the WTP from the model itself:
### Note: 1711 rows (one per respondent), 9001 variables (1000 per attribute level)
### Note: added all four datasets here so you can choose
# WTP <- data.frame(fread(here("WinterReplication/CEModelData","WP5_Winter_MXL_ModelOne_2022_07_29_WTP.csv")))
# WTP <- data.frame(fread(here("WinterReplication/CEModelData","WP5_Winter_MXL_ModelOne_2022_07_29_UCWTP.csv")))
# WTP <- data.frame(fread(here("WinterReplication/CEModelData","WP5_Winter_MXL_ModelTwo_2022_07_29_WTP.csv")))
WTP <- data.frame(fread(here("CEoutput/ModelTwo","Winter_MXL_ModelTwo_UnconWTP.csv")))

## If the conditionals are imported then run this to recover only useful variables
WTP <- WTP[WTP %>% select(-ends_with(c(".ID",".post.sd"))) %>% colnames()] %>% data.frame()


## Combine all for ease:
WinterWTPCombined <- cbind(Winter,WTP)


#----------------------------------------------------------------------------------------------------------
#### Section 2: Define useful variables ####
#----------------------------------------------------------------------------------------------------------


## So I'll use these later to name rows
Names <- c("Tax",
           "ColourMedium",
           "ColourHigh",
           "SoundMedium",
           "SoundHigh",
           "SmellMedium",
           "SmellHigh",
           "DeadwoodMedium",
           "DeadwoodHigh")


## Label X axis of ggplot boxplots
Labels <- c("Tax",
            "Colour\n Medium","Colour\n High",
            "Sound\n Medium","Sound\n High",
            "Smell\n Medium", "Smell\n High",
            "Deadwood\n Medium","Deadwood\n High")


## Label grouping variable
VisitFrequency <- c(0,1,2,3,4,5)


LegendLabels <- c(
  paste0("I did not visit\n(N = ",Winter %>% filter(MostRecentVisit==0) %>% nrow(),")"),
  paste0("Once or twice a season\n(N = " ,Winter %>% filter(MostRecentVisit==1) %>% nrow(),")"),
  paste0("Once or twice a month\n(N = ",Winter %>% filter(MostRecentVisit==2) %>% nrow(),")"),
  paste0("Once a week\n(N = " ,Winter %>% filter(MostRecentVisit==3) %>% nrow(),")"),
  paste0("Several times a week\n(N = " ,Winter %>% filter(MostRecentVisit==4) %>% nrow(),")"),
  paste0("Every day\n(N = " ,Winter %>% filter(MostRecentVisit==5) %>% nrow(),")"))


#----------------------------------------------------------------------------------------------------------
#### Section 2B or most likely not 2B: Trying to make the tails longer by using the entire distribution ####
#----------------------------------------------------------------------------------------------------------


## So this function calculates one stat per attribute per VisitFrequency
### So: rowmeans() not rowMeans() is actually insanely fast if you can be bothered to transform to and from matrices.
Summarizer <- function(Attribute) {

  bind_cols(
    "y0"=  WinterWTPCombined %>% select(starts_with(Attribute),"MostRecentVisit")  %>% group_by(MostRecentVisit) %>% summarise_all(quantile,c(0.05)) %>% as.matrix() %>% rowmeans(),
    "y25"= WinterWTPCombined %>% select(starts_with(Attribute),"MostRecentVisit")  %>% group_by(MostRecentVisit) %>% summarise_all(quantile,c(0.25)) %>% as.matrix() %>% rowmeans(),
    "y50"= WinterWTPCombined %>% select(starts_with(Attribute),"MostRecentVisit")  %>% group_by(MostRecentVisit) %>% summarise_all(mean) %>% as.matrix() %>% rowmeans(),
    "y75"= WinterWTPCombined %>% select(starts_with(Attribute),"MostRecentVisit")  %>% group_by(MostRecentVisit) %>% summarise_all(quantile,c(0.75)) %>% as.matrix() %>% rowmeans() ,
    "y100"=WinterWTPCombined %>% select(starts_with(Attribute),"MostRecentVisit")  %>% group_by(MostRecentVisit) %>% summarise_all(quantile,c(0.95)) %>% as.matrix() %>% rowmeans()) %>% data.frame()

}


## Binds the results for each attribute; maybe we can loop through this.
Summaries <- bind_rows(
  Summarizer("beta_Tax"),
  Summarizer("b_Colour."),
  Summarizer("b_Colour2."),
  Summarizer("b_Sound."),
  Summarizer("b_Sound2."),
  Summarizer("b_Smell."),
  Summarizer("b_Smell2."),
  Summarizer("b_Deadwood."),
  Summarizer("b_Deadwood2."))


## Bind the results with variable and MostRecentVisit ID for ease later
NewerData <- bind_cols(
  "variable"=rep(Names,each=6),
  "MostRecentVisit"=rep(0:5,times=9),
  Summaries)


## Without explicitly telling GGPLOT the factor levels it will plot in a horrible order!
NewerData$variable <- factor(NewerData$variable,levels=unique(NewerData$variable))


#----------------------------------------------------------------------------------------------------------
#### Section 3: Create Plot ####
#----------------------------------------------------------------------------------------------------------


print("Figure")
## Using this version now!
Figure2_VisitFrequency <-
  ggplot(NewerData,aes(x=variable, fill=as.factor(MostRecentVisit))) +
  geom_boxplot(varwidth = 0.5,outlier.shape = NA,
               aes(
                 ymin=y0,
                 lower=y25,
                 middle=y50,
                 upper=y75,
                 ymax=y100,
               ),stat="identity")+
  scale_x_discrete(name="Attribute",label=Labels)+
  theme_bw()+geom_hline(yintercept=0)+
  ylab("WTP ( \U00a3 GBP) Per Year Local Tax")+
  scale_y_continuous(limits=c(-10,25)
                     ,breaks = seq(-10,25,1))+
  scale_fill_brewer(name="Visit Frequency",type="seq",
                    label=LegendLabels,
                    guide=guide_legend(reverse = FALSE))+
  theme(legend.position = "bottom",
        legend.background=element_blank(),
        legend.box.background = element_rect(colour="black"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank())


#----------------------------------------------------------------------------------------------------------
#### Section 3: Export Plot ####
#----------------------------------------------------------------------------------------------------------


ggsave(Figure2_VisitFrequency, device = "png",
       filename = paste0(here(),"/OtherOutput/Figures/","Figure2_VisitFrequency.png"),
       width=20,height=15,units = "cm",dpi=500)


# End Of Script -----------------------------------------------------------
