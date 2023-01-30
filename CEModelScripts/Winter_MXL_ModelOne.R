#### WP5 Winter Paper: Table 3 Model One ####
## Function: Estimate Mixed Logit on Attributes Only
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 30/01/2023
## TODO: Make sure it always outputs exactly to table



#------------------------------
# Replication Information: ####
#------------------------------


# R version 4.1.3 (2022-03-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19043)
# Matrix products: default
#   [1] gridExtra_2.3  ggdist_3.2.0   here_1.0.1     mded_0.1-2     reshape2_1.4.4
# [6] ggridges_0.5.3 ggplot2_3.3.6  magrittr_2.0.3 dplyr_1.0.9    apollo_0.2.7


#------------------------------
# Setup Environment: ####
#------------------------------


## Clear workspace:
rm(list = ls())

## This sometimes fixes encoding issues using a HPC:
Sys.setlocale("LC_ALL","C")


## Note: My working directory is the zip file and then
###I specify extensions when running scripts or importing data:
# setwd("K:/WinterAnalysis1307/WP5/WinterReplication")


## Libraries that will come in handy later
library(apollo)
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggridges)
library(reshape2)
library(mded)
library(here)

#------------------------------
# Import Data: ####
#------------------------------

database <- data.frame(fread(here("CEModelData","database_Winter_Step1.csv")))
apollo_initialise()


#------------------------------
# Estimation Basics: ####
#------------------------------


## Note 10 cores as I'm using the University of Kent 'Tesla' HPC:
apollo_control = list(
  nCores    = 10,
  mixing    = TRUE,
  modelDescr = "Winter_MXL_ModelOne",
  modelName  = "Winter_MXL_ModelOne", ## Added dates last verified
  indivID    = "Respondent", ## This is the name of a column in the database indicating each unique respondent
  outputDirectory="CEoutput/ModelOne"
  )


## Define parameters starting values:
### Note starting mean of tax at -3 to avoid issues later when using the lognormal distribution
apollo_beta = c(
  asc_A      = 0,
  asc_B      = 0,
  asc_C = 0,
  mu_Tax    = -3,
  mu_Sound   = 0,
  mu_Smell = 0,
  mu_Colour = 0,
  mu_Deadwood = 0,
  sig_Tax = 0,
  sig_Sound   = 0,
  sig_Smell = 0,
  sig_Colour = 0,
  sig_Deadwood = 0,
  mu_Sound2   = 0,
  mu_Smell2 = 0,
  mu_Colour2 = 0,
  mu_Deadwood2 = 0,
  sig_Sound2   = 0,
  sig_Smell2 = 0,
  sig_Colour2 = 0,
  sig_Deadwood2 = 0
)


## Hold Alternative-Specific Constants for non-status-quo options at zero
apollo_fixed = c("asc_A","asc_B")


## Set parameters for generating draws
### Note that draws by attribute (5) or by level (9) a big difference!
apollo_draws = list(
  interDrawsType = "pmc",## Robust to using MLHS or Sobol draws
  interNDraws    = 1000, ## Same results if you use 5000 draws
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
  ))


## Create random parameters
### Note lognormal for tax attribute to impose negative signs
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["beta_Tax"]] = -exp(mu_Tax + sig_Tax * draws_Tax )
  randcoeff[["b_Smell"]] =  (mu_Smell + sig_Smell * draws_Smell )
  randcoeff[["b_Sound"]] =  (mu_Sound + sig_Sound * draws_Sound )
  randcoeff[["b_Colour"]] =  (mu_Colour + sig_Colour * draws_Colour )
  randcoeff[["b_Deadwood"]] =  (mu_Deadwood + sig_Deadwood * draws_Deadwood )
  randcoeff[["b_Smell2"]] =  (mu_Smell2 + sig_Smell2 * draws_Smell2 )
  randcoeff[["b_Sound2"]] =  (mu_Sound2 + sig_Sound2 * draws_Sound2 )
  randcoeff[["b_Colour2"]] =  (mu_Colour2 + sig_Colour2 * draws_Colour2 )
  randcoeff[["b_Deadwood2"]] =  (mu_Deadwood2 + sig_Deadwood2 * draws_Deadwood2 )
  return(randcoeff)
}


apollo_inputs = apollo_validateInputs() ## Required to check inputs are fine


#------------------------------
# Estimation Specification: ####
### Note: Model in WTP-space.
#------------------------------

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){

  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  P = list()

  V = list()
  V[['A']]  = asc_A  + beta_Tax*(Tax1 +
                                   b_Sound  * (Sound1==1) + b_Sound2  * (Sound1==2) +
                                   b_Smell*(Smell1==1) + b_Smell2*(Smell1==2) +
                                   b_Colour* (Colour1==1) +b_Colour2* (Colour1==2) +
                                   b_Deadwood*(Deadwood1==7) + b_Deadwood2*(Deadwood1==15) )
  V[['B']]  = asc_B  + beta_Tax*(Tax2 +
                                   b_Sound  * (Sound2==1) + b_Sound2  * (Sound2==2)  +
                                   b_Smell*(Smell2==1) + b_Smell2*(Smell2==2) +
                                   b_Colour*(Colour2==1) + b_Colour2* (Colour2==2) +
                                   b_Deadwood*(Deadwood2==7) +b_Deadwood2*(Deadwood2==15))
  V[['C']]  = asc_C

  mnl_settings = list(
    alternatives = c(A=1, B=2,C=3),
    avail        = list(A=1, B=1,C=1),
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


#------------------------------
# Model Outputs: ####
#------------------------------


## Actually estimates the model
Winter_MXL_ModelOne = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# Model output and results here alongside saving information
apollo_modelOutput(Winter_MXL_ModelOne,modelOutput_settings = list(printPVal=TRUE))
apollo_saveOutput(Winter_MXL_ModelOne,saveOutput_settings = list(printPVal=TRUE))
saveRDS(Winter_MXL_ModelOne, file="Winter_MXL_ModelOne.rds")


#------------------------------
# Summarise WTP: ####
#------------------------------


## Calculate conditional WTP:
Model <- readRDS(here("CEoutput/ModelOne","Winter_MXL_ModelOne.rds")) ## Enter model of interest RDS here


## Calculate conditional WTP:
Winter_MXL_ModelOne_ConWTP <- apollo_conditionals(Model,apollo_probabilities,apollo_inputs )
fwrite(Winter_MXL_ModelOne_ConWTP,here("CEoutput/ModelOne","Winter_MXL_ModelOne_ConWTP.csv"))


## Calculate unconditional WTP: (Needed for Fig.2. of the paper) [NOTE: Unconditionals make v large dataframes]
Winter_MXL_ModelOne_UnconWTP <- apollo_unconditionals(Model,apollo_probabilities,apollo_inputs )
fwrite(Winter_MXL_ModelOne_UnconWTP,here("CEoutput/ModelOne","Winter_MXL_ModelOne_UnconWTP.csv"))



## Output a summary table:
Winter_MXL_ModelOne_WTP <- data.frame(fread(here("CEoutput/ModelOne","Winter_MXL_ModelOne_WTP.csv")))
Winter_MXL_ModelOneSummary <-data.frame(cbind("TaxWTP_Test"=Winter_MXL_ModelOne_WTP$beta_Tax.post.mean,
                                                                         "SmellWTP_Test"=Winter_MXL_ModelOne_WTP$b_Smell.post.mean,
                                                                         "SmellWTP2_Test"=Winter_MXL_ModelOne_WTP$b_Smell2.post.mean,
                                                                         "SoundWTP_Test"=Winter_MXL_ModelOne_WTP$b_Sound.post.mean,
                                                                         "SoundWTP2_Test"=Winter_MXL_ModelOne_WTP$b_Sound2.post.mean,
                                                                         "ColourWTP_Test"=Winter_MXL_ModelOne_WTP$b_Colour.post.mean,
                                                                         "ColourWTP2_Test"=Winter_MXL_ModelOne_WTP$b_Colour2.post.mean,
                                                                         "DeadwoodWTP_Test"=Winter_MXL_ModelOne_WTP$b_Deadwood.post.mean,
                                                                         "DeadwoodWTP2_Test"=Winter_MXL_ModelOne_WTP$b_Deadwood2.post.mean))


# Summary <- Winter_MXL_ModelOneSummary %>% summarise(across(everything(),list(mean)))
#
# #------------------------------
# # WTP Plot: ####
# #------------------------------
#
#
# ## Add labels for later plot
# ### "\n" makes a new line
# Labels <- c("Tax",
#             "Smell\n Medium", "Smell\n High",
#             "Sound\n Medium","Sound\n High",
#             "Colour\n Medium","Colour\n High",
#             "Decomposing Trees \n Medium","Decomposing Trees\n High")
#
#
#
# ## Changed scale_fill_viridis_d to scale_fill_brewer
# ## And geom_density_ridges() from   stat_density_ridges(quantile_lines = TRUE, quantiles = 2)+
# AttributeWTPDensity <- Winter_MXL_ModelOneSummary %>%
#   melt() %>%
#   ggplot(aes(x=value,y=variable,group=variable,fill=variable))+
#   stat_density_ridges()+theme_bw()+
#   scale_x_continuous(name="mWTP in GBP per HH per annum.",
#                      limits=c(-10,10),
#                      breaks = seq(-10,10,1))+
#   scale_y_discrete(name="Attribute",
#                    label=Labels)+
#   ggtitle("Distribution of individual-level attribute WTP.")+
#   coord_cartesian(xlim = c(-10,10),clip='off')+
#   geom_vline(xintercept=0,linetype='dashed')+
#   scale_fill_brewer(name="Attributes",
#                     label=Labels,
#                     guide=guide_legend(reverse = TRUE))+
#   theme(legend.background=element_blank(),
#         legend.box.background = element_rect(colour="black"),
#         panel.grid.major.x=element_line(colour="lightgrey"),
#         panel.grid.major.y=element_blank())
#
# ## Okay this is silly but I like filenames with "_" not "-"
# ### Anyway the point is to add a date to the plot to know which version we're using
# Date <- gsub(pattern = "-",replacement = "_",Sys.Date())
#
#
# ## Save output in highest DPI
# ggsave(AttributeWTPDensity, device = "jpeg",
#        filename = paste0("Winter_Figure2_ModelOne_DensityPlot_",Date,".jpeg"),
#        width=20,height=15,units = "cm",dpi=1000)
#
# #------------------------------
# # Summarise Results: ####
# #------------------------------
#
#
#
#
# ## The latest version with WTP appended is 2022_01_07
# Winter <- data.frame(read.csv(here("OtherData","Winter_dataframe_2022-01-07.csv")))
# Winter <- cbind(Winter,
#                 Winter_MXL_ModelOneSummary)
#
# tWrap <- function(x) mded(x$Var1, x$Var2)$stat
#
#
# ## So this function calculates P.value per subgroup
# ### The output is formatted to 3 digits and with significance stars
# ApplyPoeTests <- function(WTP,Group) {
#   L <- split(Winter[,c(WTP)], Winter[,c(Group)])
#   pvals <- apply(expand.grid(L, L), 1, tWrap)
#   pvals_mat <- matrix(pvals,ncol = length(unique(Winter[,c(Group)])))
#   return(pvals_mat %>% data.frame() %>%  mutate(across(where(is.numeric), round,digits=3)) %>% lapply(function(x) ifelse(
#     x < 0.01,
#     paste0(round(x, 3), "***"),
#     ifelse(
#       x < 0.05,
#       paste0(round(x, 3), "**"),
#       ifelse(
#         x < 0.1,
#         paste0(round(x, 3), "*"),
#         round(x, 3))))) %>% as.data.frame()
#   )
# }
#
# ApplyPoeTests("ColourWTP_Test","MostRecentVisit")
# ApplyPoeTests("ColourWTP2_Test","MostRecentVisit")
#
# ApplyPoeTests("SmellWTP_Test","MostRecentVisit")
# ApplyPoeTests("SmellWTP2_Test","MostRecentVisit")
#
# ApplyPoeTests("SoundWTP_Test","MostRecentVisit")
# ApplyPoeTests("SoundWTP2_Test","MostRecentVisit")
#
# ApplyPoeTests("DeadwoodWTP_Test","MostRecentVisit")
# ApplyPoeTests("DeadwoodWTP2_Test","MostRecentVisit")



# End Of Script -------------------------------------
