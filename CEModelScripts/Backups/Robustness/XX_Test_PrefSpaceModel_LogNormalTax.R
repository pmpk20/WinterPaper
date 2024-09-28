#### RELATE WP5: AllSeasons Spring Model for H3 ####

rm(list=ls())
library(here)

library(tidyr)
library(apollo)
library(ggridges)
library(ggplot2)
library(reshape2)
library(dplyr)
library(magrittr)
library(ggdist)
library(RColorBrewer)
library(data.table)


# *******************************************************************************
# Section 1: Import Data ####
# Selected output of 'sessionInfo()'
# *******************************************************************************



## Import reshaped survey data appropriate for Apollo
database <-
  here("CEModelData", "database_Winter_Step1.csv") %>%
  fread() %>%
  data.frame()


## Define here if not already
database$Impairment <- ifelse((database$SightIssues == 1) |
                                (database$SmellIssues == 1) |
                                (database$HearingIssues == 1),
                              1,
                              0
)


## Make dummy
database$CountryDummy <- ifelse(database$Country == 0, 0, 1)


## Keep only relevant columns
database <- database[, c(
  "Choice",
  "Respondent",
  "Season",
  "Task",
  "ID",
  "Tax1",
  "Tax2",
  "Sound1",
  "Sound2",
  "Smell1",
  "Smell2",
  "Colour1",
  "Colour2",
  "Deadwood1",
  "Deadwood2",
  "Gender",
  "DummyAge",
  "MilesDistance",
  "IncomeDummy",
  "Impairment",

  "SightIssues" ,
  "SmellIssues" ,
  "HearingIssues",

  "Country",
  "EthnicityDummyWhite",
  "Urbanicity",
  "MostRecentVisit",
  "WoodlandsScore"
)]

# database <- here("CEModelData", "database_Winter_Step1.csv") %>% fread() %>% data.frame()
apollo_initialise()


# *******************************************************************************
# Section 2: Estimation Statistics ####
# Selected output of 'sessionInfo()'
# *******************************************************************************


# apollo_initialise()
apollo_control = list(
  nCores    = 12,
  ## Watch out if not using HPC!
  mixing    = TRUE,
  modelDescr = "XX_Test_PrefSpaceModel_LogNormalTax",
  modelName  = "XX_Test_PrefSpaceModel_LogNormalTax",
  indivID    = "Respondent",
  outputDirectory = "CEoutput/Robustness"
)



# Define beta starting values:
apollo_beta = c(
  asc_A      = 0,
  asc_B      = 0,
  asc_C = -0.15137,
  mu_Tax    = -3,
  sig_Tax = -0.01,
  # beta_Tax = -0.02209,
  mu_Colour2 = 0.07410,
  mu_Colour = 0.13937,
  mu_Smell2 = 0.12855,
  mu_Smell = 0.08875,
  mu_Sound2 = 0.26818,
  mu_Sound = 0.25171,
  mu_Deadwood2 = 0.43725,
  mu_Deadwood = 0.24873,

  sig_Colour2 = -0.01,
  sig_Colour = -0.01,
  sig_Smell2 = -0.01,
  sig_Smell = -0.01,
  sig_Sound2   = -0.01,
  sig_Sound   = -0.01,
  sig_Deadwood2 = -0.01,
  sig_Deadwood = -0.01

)

apollo_fixed = c("asc_A", "asc_B")


# *******************************************************************************
# Section 3: Define random draws ####
# *******************************************************************************


### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "pmc",
  interNDraws    = 5000,
  interUnifDraws = c(),
  interNormDraws = c(
    "draws_Tax",
    "draws_Smell",
    "draws_Sound",
    "draws_Colour",
    "draws_Deadwood",
    "draws_Smell2",
    "draws_Sound2",
    "draws_Colour2",
    "draws_Deadwood2"
  ),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs) {
  randcoeff = list()
  randcoeff[["beta_Tax"]] = -exp(mu_Tax + sig_Tax * draws_Tax)
  randcoeff[["b_Smell"]] =  (mu_Smell + sig_Smell * draws_Smell)
  randcoeff[["b_Sound"]] =  (mu_Sound + sig_Sound * draws_Sound)
  randcoeff[["b_Colour"]] =  (mu_Colour + sig_Colour * draws_Colour)
  randcoeff[["b_Deadwood"]] =  (mu_Deadwood + sig_Deadwood * draws_Deadwood)
  randcoeff[["b_Smell2"]] =  (mu_Smell2 + sig_Smell2 * draws_Smell2)
  randcoeff[["b_Sound2"]] =  (mu_Sound2 + sig_Sound2 * draws_Sound2)
  randcoeff[["b_Colour2"]] =  (mu_Colour2 + sig_Colour2 * draws_Colour2)
  randcoeff[["b_Deadwood2"]] =  (mu_Deadwood2 + sig_Deadwood2 * draws_Deadwood2)
  return(randcoeff)
}

apollo_inputs = apollo_validateInputs()


# *******************************************************************************
# Section 4: Define model ####
# *******************************************************************************



apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){

  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  asc_C1 = asc_C



  P = list()

  V = list()
  V[['A']]  = asc_A +
    b_Sound  * (Sound1 == 1) + b_Sound2  * (Sound1 == 2) +
    b_Smell * (Smell1 == 1) + b_Smell2 * (Smell1 == 2) +
    b_Colour * (Colour1 == 1) + b_Colour2 * (Colour1 == 2) +
    b_Deadwood * (Deadwood1 == 7) + b_Deadwood2 * (Deadwood1 == 15) +
    beta_Tax * Tax1


  V[['B']]  =  asc_B +
    b_Sound  * (Sound2 == 1) + b_Sound2  * (Sound2 == 2)  +
    b_Smell * (Smell2 == 1) + b_Smell2 * (Smell2 == 2) +
    b_Colour * (Colour2 == 1) + b_Colour2 * (Colour2 == 2) +
    b_Deadwood * (Deadwood2 == 7) + b_Deadwood2 * (Deadwood2 == 15) +
    beta_Tax * Tax2

  V[['C']]  = asc_C1

  mnl_settings = list(
    alternatives = c(A = 1, B = 2, C = 3),
    avail        = list(A = 1, B = 1, C = 1),
    choiceVar    = Choice,
    V            = V
  )

  ## Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)

  ## Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)

  ## Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)

  ## Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


# *******************************************************************************
# Section 5: Model Estimation ####
# *******************************************************************************


# Actually estimates the model
XX_Test_PrefSpaceModel_LogNormalTax = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


## Model output and results here alongside saving information
apollo_modelOutput(XX_Test_PrefSpaceModel_LogNormalTax,modelOutput_settings = list(printPVal=TRUE))
apollo_saveOutput(XX_Test_PrefSpaceModel_LogNormalTax,saveOutput_settings = list(printPVal=TRUE))
# saveRDS(XX_Test_PrefSpaceModel_LogNormalTax, file="XX_Test_PrefSpaceModel_LogNormalTax.rds")


# ***********************************************
# Section 6: Output WTP ####
# ***********************************************
# apollo_modelOutput(model)
#Model <- here("CEOutputData/H3", "XX_Test_PrefSpaceModel_LogNormalTax_model.rds") %>% readRDS()


## Calculate conditional WTP:
XX_Test_PrefSpaceModel_LogNormalTax_ConWTP <- apollo_conditionals(XX_Test_PrefSpaceModel_LogNormalTax,apollo_probabilities,apollo_inputs )
fwrite(
  XX_Test_PrefSpaceModel_LogNormalTax_ConWTP %>% data.frame(),
  sep = ",",
  here("CEoutput/Robustness", "XX_Test_PrefSpaceModel_LogNormalTax_ConWTP.csv")
)

WTP <- XX_Test_PrefSpaceModel_LogNormalTax_ConWTP %>% data.frame()


## If the conditionals are imported then run this to recover only useful variables
WTP <- WTP[WTP %>% dplyr::select(-ends_with(c(".ID", ".post.sd"))) %>% colnames()] %>% data.frame()


WTP <-
  bind_cols(
    "ColourHigh" = WTP$b_Colour2.post.mean,
    "ColourMedium" = WTP$b_Colour.post.mean,
    "SmellHigh" = WTP$b_Smell2.post.mean,
    "SmellMedium" = WTP$b_Smell.post.mean,
    "SoundHigh" = WTP$b_Sound2.post.mean,
    "SoundMedium" = WTP$b_Sound.post.mean,
    "DeadwoodHigh" = WTP$b_Deadwood2.post.mean,
    "DeadwoodMedium" = WTP$b_Deadwood.post.mean
  )


# **********************************************************
# Section 2: Merge conditionals and survey data ####
# **********************************************************


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

PlotData <- WTP %>%
  pivot_longer(1:8) %>%
  mutate("YMIN" = min(value),
         "YMAX" = max(value))

## THis is an important step for the correct plot order!
PlotData$name <-
  factor(PlotData$name,
         levels = unique(PlotData$name))

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





Figure2 <- ggplot(PlotData,
                  aes(
                    x = value,
                    y = rev(name),
                    fill = rev(name),
                    group = rev(name)
                  )) +
  stat_boxplot(geom = "errorbar", width = 0.25, position = position_dodge(width = 0.75)) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  geom_vline(xintercept = 0, alpha = 0.5) +
  scale_x_continuous(name = "",
                     breaks = seq(-50, 50, 10)) +
  scale_y_discrete(name = "Attribute and level", labels = rev(Labels)) +
  scale_fill_brewer(
    name = "",
    type = "seq",
    label = rev(Labels),
    guide = guide_legend(reverse = TRUE)
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10,
                               colour = "black",
                               family = "serif"),
    legend.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.title = element_text(size = TextSize,
                                colour = "black",
                                family = "serif"),
    axis.text.x = element_text(size = TextSize,
                               colour = "black",
                               family = "serif"), ## Change text to be clearer for reader
    axis.text.y = element_text(size = TextSize,
                               colour = "black",
                               family = "serif"),
    axis.title.y = element_text(size = TextSize,
                                colour = "black",
                                family = "serif"),
    axis.title.x = element_text(size = TextSize,
                                colour = "black",
                                family = "serif")
  )+
  ggtitle("XX_Test_PrefSpaceModel_LogNormalTax_Plot")

# **********************************************************
#### Section 3: Export Plot ####
## Removed sys.date() and changed save location
# **********************************************************



## Save output in highest DPI
ggsave(
  Figure2,
  device = "png",
  filename = paste0(
    here(),
    "/OtherOutput/Figures/",
    "XX_Test_PrefSpaceModel_LogNormalTax_ConWTP_Plot.png"
  ),
  width = 20,
  height = 15,
  units = "cm",
  dpi = 500
)




# # End Of Script --------------------------------------------------------------
