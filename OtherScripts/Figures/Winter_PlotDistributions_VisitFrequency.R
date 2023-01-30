#### RELATE Winter Paper ####
## Function: Plots distributions of WTP by visit frequency
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 30/01/2023
## TODO: Maybe flip axis or make larger


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
library(here)


#------------------------------
# Section 1: Import Data ####
#------------------------------


## The latest version with WTP appended is 2022_01_07
Winter <- data.frame(fread(here("WinterReplication/OtherData","Winter_dataframe_Step3.csv")))
WTP <- data.frame(fread(here("CEoutput/ModelTwo","Winter_MXL_ModelTwo_ConWTP.csv")))
Winter <- cbind(Winter,WTP)


##Trim crazy WTPs
Winter <- Winter[Winter$TaxWTP >-20,]


#----------------------------------------------
# Section 2: Aggregate Data ####
#----------------------------------------------


WTP_Winter <-melt(data.frame(cbind(
  "TaxWTP"=Winter$TaxWTP,
  "SmellWTP"=Winter$SmellWTP,
  "SmellWTP2"=Winter$SmellWTP2,
  "ColourWTP"=Winter$ColourWTP,
  "ColourWTP2"=Winter$ColourWTP2,
  "SoundWTP"=Winter$SoundWTP,
  "SoundWTP2"=Winter$SoundWTP2,
  "DeadwoodWTP"=Winter$DeadwoodWTP,
  "DeadwoodWTP2"=Winter$DeadwoodWTP2,
  "MostRecentVisit"=Winter$MostRecentVisit)),id.vars="MostRecentVisit")


## Custom labels for Y axis
Labels <- c("Tax","Smell\n Medium", "Smell\n High","Colour\n Medium","Colour\n High", "Sound\n Medium","Sound\n High", "Deadwood\n Medium","Deadwood\n High")


#----------------------------------------------
# Section 3: Boxplot Data ####
#----------------------------------------------


Winter_Figure5_Boxplot <- WTP_Winter %>% ggplot(aes(y=variable,x=value,fill=factor(MostRecentVisit))) +
  geom_boxplot(varwidth = 0.85,outlier.shape = NA)+
  scale_y_discrete(name="Attribute",
                   label=Labels)+
  coord_cartesian()+theme_bw()+
  scale_x_continuous(name="WTP in Pounds Per Year Local Tax",
                     limits=c(-12.5,22.5)
                     ,breaks = seq(-12.5,22.5,2.5))+
  scale_fill_brewer(name="Visit Frequency",type="seq",
                    label=c("I did not visit\n(N = 536)", "Once or twice a season\n(N = 236)", "Once or twice a month\n(N = 276)", "Once a week\n(N = 259)","Several times a week\n(N = 310)","Every day\n(N = 94)"),
                    guide=guide_legend(reverse = TRUE))+
  theme(legend.background=element_blank(),
        legend.box.background = element_rect(colour="black"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        panel.grid.major.y=element_blank())




Winter_Figure5_Boxplot_NoGridLines <- WTP_Winter %>% ggplot(aes(y=variable,x=value,fill=factor(MostRecentVisit))) +
  geom_boxplot(varwidth = 0.85,outlier.shape = NA)+
  scale_y_discrete(name="Attribute",
                   label=Labels)+
  coord_cartesian()+theme_bw()+
  geom_vline(xintercept = 0)+
  scale_x_continuous(name="WTP in Pounds Per Year Local Tax",
                     limits=c(-10,15)
                     ,breaks = seq(-10,15,2.5))+
  scale_fill_brewer(name="Visit Frequency",type="seq",
                    label=c("I did not visit\n(N = 536)", "Once or twice a season\n(N = 236)", "Once or twice a month\n(N = 276)", "Once a week\n(N = 259)","Several times a week\n(N = 310)","Every day\n(N = 94)"),
                    guide=guide_legend(reverse = TRUE))+
  theme(legend.background=element_blank(),
        legend.box.background = element_rect(colour="black"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank())


#----------------------------------------------
# Section 4: Export plots ####
#----------------------------------------------


## To record plot version
Date <- gsub(pattern = "-",replacement = "_",Sys.Date())


## Save output in highest DPI
ggsave(Winter_Figure5_Boxplot_NoGridLines, device = "jpeg",
       filename = paste0("Winter_Figure5_Boxplot_NoGridLines_",Date,".jpeg"),
       width=25,height=20,units = "cm",dpi=1000)

#
# #----------------------------------------------
# # PS: Testing significance labels ####
# ### using GGSIGNIF but not currently functional
# #----------------------------------------------
#
# ## Poe tests:
# PoeTests <- function(WTP) {
#   c(mded(Winter[,c(WTP)][Winter$MostRecentVisit==0],Winter[,c(WTP)][Winter$MostRecentVisit==1])$stat,
#
#     mded(Winter[,c(WTP)][Winter$MostRecentVisit==1],Winter[,c(WTP)][Winter$MostRecentVisit==2])$stat,
#
#     mded(Winter[,c(WTP)][Winter$MostRecentVisit==2],Winter[,c(WTP)][Winter$MostRecentVisit==3])$stat,
#
#     mded(Winter[,c(WTP)][Winter$MostRecentVisit==3],Winter[,c(WTP)][Winter$MostRecentVisit==4])$stat,
#
#     mded(Winter[,c(WTP)][Winter$MostRecentVisit==4],Winter[,c(WTP)][Winter$MostRecentVisit==5])$stat)
# }
# PoeTests("ColourWTP")
# PoeTests("ColourWTP2")
#
# PoeTests("SoundWTP")
# PoeTests("SoundWTP2")
#
# PoeTests("SmellWTP")
# PoeTests("SmellWTP2")
#
# PoeTests("DeadwoodWTP")
# PoeTests("DeadwoodWTP2")
#
#
# MeanTests <- function(WTP) {
#   c(wilcox.test(Winter[,c(WTP)][Winter$MostRecentVisit==0],Winter[,c(WTP)][Winter$MostRecentVisit==1])$p.value,
#
#     wilcox.test(Winter[,c(WTP)][Winter$MostRecentVisit==1],Winter[,c(WTP)][Winter$MostRecentVisit==2])$p.value,
#
#     wilcox.test(Winter[,c(WTP)][Winter$MostRecentVisit==2],Winter[,c(WTP)][Winter$MostRecentVisit==3])$p.value,
#
#     wilcox.test(Winter[,c(WTP)][Winter$MostRecentVisit==3],Winter[,c(WTP)][Winter$MostRecentVisit==4])$p.value,
#
#     wilcox.test(Winter[,c(WTP)][Winter$MostRecentVisit==4],Winter[,c(WTP)][Winter$MostRecentVisit==5])$p.value)
# }
# MeanTests("DeadwoodWTP2")
#
# ## Poe test says no stat sig diffs.
#
# ## Graph
# WTP_Winter %>% mutate(x2 = interaction(MostRecentVisit,variable)) %>% ggplot(aes(y=x2,x=value,fill=factor(MostRecentVisit)))+geom_boxplot(varwidth = 0.85,outlier.shape = NA)+
#   geom_signif(comparisons = list(
#     c(1,2),c(2,3),c(3,4),c(4,5),c(5,6),
#
#     c(7,8),c(8,9),c(9,10),c(10,11),c(11,12),
#
#     c(13,14),c(14,15),c(15,16),c(16,17),c(17,18),
#
#     c(19,20),c(20,21),c(21,22),c(22,23),c(23,24),
#
#     c(25,26),c(26,27),c(27,28),c(28,29),c(29,30),
#
#     c(31,32),c(32,33),c(33,34),c(34,35),c(35,36),
#
#     c(37,38),c(38,39),c(39,40),c(40,41),c(41,42),
#
#     c(43,44),c(44,45),c(45,46),c(46,47),c(47,48),
#
#     c(49,50),c(50,51),c(51,52),c(52,53),c(53,54)
#   ))


# End Of Script ----------------------------------------
