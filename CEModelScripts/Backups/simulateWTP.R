#### RELATE Winter Paper ####
## Function: Literally plots one figure: distributions of each attributes' WTP
## Author: Dr Peter King (p.king1@leeds.ac.uk)
## Last change: 19/04/2024
## Changes:
## - Simulate WTP using mvrnorm


# ******************************
# Replication Information: ####
# ******************************


# > sessionInfo()
# R version 4.3.1 (2023-06-16 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 11 x64 (build 22631)
# Matrix products: default
# locale:
#   [1] C
# system code page: 65001
# time zone: Europe/London
# tzcode source: internal
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base
# other attached packages:
#   [1] Rfast_2.1.0        RcppParallel_5.1.7 RcppZiggurat_0.1.6
# [4] Rcpp_1.0.12        DCchoice_0.2.0     lubridate_1.9.3
# [7] forcats_1.0.0      stringr_1.5.0      purrr_1.0.2
# [10] readr_2.1.5        tidyr_1.3.0        tibble_3.2.1
# [13] tidyverse_2.0.0    MASS_7.3-60.0.1    data.table_1.15.2
# [16] RColorBrewer_1.1-3 here_1.0.1         mded_0.1-2
# [19] reshape2_1.4.4     ggridges_0.5.6     ggplot2_3.5.0
# [22] magrittr_2.0.3     dplyr_1.1.4        apollo_0.3.1
# loaded via a namespace (and not attached):
#   [1] gtable_0.3.4         lattice_0.21-8       tzdb_0.4.0
# [4] numDeriv_2016.8-1.1  vctrs_0.6.5          tools_4.3.1
# [7] generics_0.1.3       parallel_4.3.1       sandwich_3.1-0
# [10] fansi_1.0.6          pkgconfig_2.0.3      Matrix_1.6-5
# [13] distributional_0.4.0 lifecycle_1.0.4      truncnorm_1.0-9
# [16] compiler_4.3.1       farver_2.1.1         maxLik_1.5-2
# [19] MatrixModels_0.5-3   mcmc_0.9-8           interval_1.1-1.0
# [22] munsell_0.5.0        mnormt_2.1.1         SparseM_1.81
# [25] RSGHB_1.2.2          quantreg_5.97        bgw_0.1.2
# [28] Rsolnp_1.16          Formula_1.2-5        pillar_1.9.0
# [31] randtoolbox_2.0.4    Icens_1.72.0         tidyselect_1.2.0
# [34] digest_0.6.35        mvtnorm_1.2-4        stringi_1.7.12
# [37] labeling_0.4.3       splines_4.3.1        miscTools_0.6-28
# [40] rprojroot_2.0.4      grid_4.3.1           colorspace_2.1-0
# [43] cli_3.6.2            survival_3.5-7       utf8_1.2.3
# [46] withr_2.5.2          MLEcens_0.1-7        scales_1.3.0
# [49] timechange_0.2.0     matrixStats_1.2.0    rngWELL_0.10-9
# [52] hms_1.1.3            zoo_1.8-12           coda_0.19-4.1
# [55] ggdist_3.3.2         perm_1.0-0.4         rlang_1.1.3
# [58] MCMCpack_1.7-0       glue_1.7.0           rstudioapi_0.15.0
# [61] R6_2.5.1             plyr_1.8.9


# **********************************************************
# Setup Environment: ####
# **********************************************************


library(tidyverse)
library(magrittr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggridges)
library(here)
library(data.table)
library(Rfast)
library(MASS)



# **********************************************************
# Section 1: Import Data ####
# **********************************************************


## Read in model
Model <-
  here("CEoutput/ModelTwo",
       "Winter_MXL_ModelTwo_InText_WithASCRandom_model.rds") %>% readRDS()


## Calculate conditional WTP:
WTP <- here("CEoutput/ModelTwo",
            "Winter_MXL_ModelTwo_InText_WithASCRandom_ConWTP.csv") %>%
  fread() %>%
  data.frame()

Estimates <- here("CEoutput/ModelTwo",
            "Winter_MXL_ModelTwo_InText_WithASCRandom_estimates.csv") %>%
  fread() %>%
  data.frame()



# **********************************************************
# Section 2: Setup ####
# **********************************************************



nsim <- 100000
betas <- Estimates$Estimate[3:76]
vcmat <- Model$robvarcov


# 1. Take draws from a multivariate normal distribution described by the beta vector and the (robust) variance-covariance matrix to simulate the model nsim times

draws <- mvrnorm(nsim, mu = betas, Sigma = vcmat)


# 2. Do whatever you wish with each of the nsim draws, i.e. the simulated models. Here: Compute and store marg. WTP for some of the attributes

sim.wtp <- matrix(0, nrow = nsim, ncol = 8)    # use this matrix to store simulated WTP distributions

for(i in 1:nsim){
  sim.wtp[i,1] <- draws[i,4]
  sim.wtp[i,2] <- draws[i,5]
  sim.wtp[i,3] <- draws[i,6]
  sim.wtp[i,4] <- draws[i,7]
  sim.wtp[i,5] <- draws[i,8]
  sim.wtp[i,6] <- draws[i,9]
  sim.wtp[i,7] <- draws[i,10]
  sim.wtp[i,8] <- draws[i,11]

}


# 3. Extract 95% confidence intervals from the empirical WTP distributions in sim.wtp and display

wtp <-
  matrix(0, nrow = 8, ncol = 6) # use this matrix to store WTP means and CIs

for(i in 1:nrow(wtp)){
  wtp[i, 1] <- betas[i + 3]
  wtp[i, 2:6] <- quantile(sim.wtp[, i],
                          probs = c(0.025,
                                    0.25,
                                    0.5,
                                    0.75,
                                    0.975))
}


# round(wtp, digits = 2)
WTP <- wtp %>% data.frame()
colnames(WTP) <- c("mean",
                   "y025",
                   "y25",
                   "y50",
                   "y75",
                   "y975")
WTP$variable <-
  c(
    "mu_Colour2",
    "mu_Colour",
    "mu_Smell2",
    "mu_Smell",
    "mu_Sound2",
    "mu_Sound",
    "mu_Deadwood2",
    "mu_Deadwood"
  )



PlotData <- WTP %>%
  slice(match(
    c(
      "mu_Colour2",
      "mu_Colour",
      "mu_Smell2",
      "mu_Smell",
      "mu_Sound2",
      "mu_Sound",
      "mu_Deadwood2",
      "mu_Deadwood"
    ),
    variable
  )) %>%
  mutate(variable = factor(variable,
                           levels = unique(variable)))


TextSize <- 12


# ***************************************************************************
#### Section 3: Plot details ####
# ***************************************************************************

## I want this order on the plot:
Names <- c(
  "ColourHigh",
  "ColourMedium",
  "SmellHigh",
  "SmellMedium",
  "SoundHigh",
  "SoundMedium",
  "Deadwood\nDecomposition High",
  "Deadwood\nDecomposition Medium"
)



Labels <-
  Names %>% gsub(pattern = "Decomposition",
                 replacement = "decomposition") %>%
  gsub(pattern = "Medium", replacement = "\nmedium") %>%
  gsub(pattern = "High", replacement = "\nhigh") %>%
  gsub(pattern = "Colour", replacement = "Colours:") %>%
  gsub(pattern = "Smell", replacement = "Smells:") %>%
  gsub(pattern = "Sound", replacement = "Sounds:") %>%
  gsub(pattern = "Deadwood", replacement = "Deadwood:") %>%
  c()



## I got these from RColorBrewer but adding specifically here to
### (a) see the colours in the console with new RStudio,
### (b) make scale_fill_manual() easier
Colours <- c(
  "#C6DBEF",
  "#C6DBEF",
  "#08306B",
  "#08306B",
  "#6BAED6",
  "#6BAED6",
  "#2171B5",
  "#2171B5"
)

## Update all text sizes here for consistency
TextSize <- 12


# ***************************************************************************
#### Section 4: Create Plot ####
# ***************************************************************************

Figure2 <-
  PlotData %>%
  ggplot(aes(y = rev(variable),
             fill = as.factor(variable))) +
  geom_errorbar(aes(xmin = y025,
                    xmax = y975),
                width = 0.2) + ## errorbar means you can have the nicer whiskers
  geom_boxplot(
    varwidth = 0.5,
    outlier.shape = NA,
    aes(
      xmin = y025,
      xlower = y25,
      xmiddle = y50,
      xupper = y75,
      xmax = y975,
    ),
    stat = "identity" ## means you can specify moments as in AES()
  ) +
  theme_bw() +
  geom_vline(xintercept = 0, alpha = 0.5) +
  scale_x_continuous(name = "",
                     breaks = seq(-50, 50, 2.5)) +
  scale_y_discrete(name = "Attribute and level",
                   labels = rev(Labels)) +
  scale_fill_brewer(name = "",
                    type = "seq",
                    labels = rev(Labels),
                    guide = guide_legend(reverse = TRUE)) +
  theme(
    legend.position = "none",
    legend.text = element_text(
      size = 10,
      colour = "black",
      family = "serif"
    ),
    legend.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.title = element_text(
      size = TextSize,
      colour = "black",
      family = "serif"
    ),
    axis.text.x = element_text(
      size = TextSize,
      colour = "black",
      family = "serif"
    ),
    ## Change text to be clearer for reader
    axis.text.y = element_text(
      size = TextSize,
      colour = "black",
      family = "serif"
    ),
    axis.title.y = element_text(
      size = TextSize,
      colour = "black",
      family = "serif"
    ),
    axis.title.x = element_text(
      size = TextSize,
      colour = "black",
      family = "serif"
    )
  )



# **********************************************************
#### Section 5: Export Plot ####
## Removed sys.date() and changed save location
# **********************************************************



## Save output in highest DPI
ggsave(
  Figure2,
  device = "png",
  filename = paste0(
    here(),
    "/OtherOutput/Figures/",
    "Winter_Figure2_ModelTwo_Correlated_KR.png"
  ),
  width = 20,
  height = 15,
  units = "cm",
  dpi = 500
)



# PlotData %>%
#   ggplot(aes(
#     y = rev(variable),
#     x = mean)) +
#   geom_boxplot() +
#   geom_errorbar(aes(xmin = lb, xmax = ub),
#                 width = 0.25,
#                 position = position_dodge(width = 0.75))+
#   geom_boxplot(outlier.shape = NA) +
#   theme_bw() +
#   geom_vline(xintercept = 0, alpha = 0.5) +
#   scale_x_continuous(name = "",
#                      breaks = seq(-50, 50, 5)) +
#   scale_y_discrete(name = "Attribute and level",
#                    labels = rev(PlotData$variable)) +
#   scale_fill_brewer(
#     name = "",
#     type = "seq",
#     guide = guide_legend(reverse = TRUE)
#   ) +
#   theme(
#     legend.position = "bottom",
#     legend.text = element_text(size = 10,
#                                colour = "black",
#                                family = "serif"),
#     legend.background = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     legend.title = element_text(size = TextSize,
#                                 colour = "black",
#                                 family = "serif"),
#     axis.text.x = element_text(size = TextSize,
#                                colour = "black",
#                                family = "serif"), ## Change text to be clearer for reader
#     axis.text.y = element_text(size = TextSize,
#                                colour = "black",
#                                family = "serif"),
#     axis.title.y = element_text(size = TextSize,
#                                 colour = "black",
#                                 family = "serif"),
#     axis.title.x = element_text(size = TextSize,
#                                 colour = "black",
#                                 family = "serif")
#   )






