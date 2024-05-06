########## SURVEY COMPARISON PROJECT MAIN FILE ##########
########## 
##########
# This main file calls and runs all subsequent R files in this analysis.
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2021-06-07
##########
##########


# Set-Up =======================================================================
library(here)


# Fish Dataframe Creation ======================================================

# This script reads in cleaned survey data from SVC, transect, and roving 
# surveys and creates a constant format between the three types before joining
# them into a single dataframe with all fish observations, grouped by species
# and size bin for each session. 

source(here("./src/fish_dataframes.R"))


# Reclassifying Size Bins ======================================================

# This script reclassifies the size bins created in the initial fish dataframe
# to utilize the mean lengths in each of the six bins (0-5cm, 5-10cm, 10-15cm, 
# 15-20cm, 20-30cm, and >30cm) as opposed to arbitrary values for further 
# analyses. 

source(here("./src/sizebin_reclassification.R"))


# Model Dataframe Creation =====================================================

# This script joins survey metadata, habitat trait, and species' trait values 
# onto the cleaned fish dataframe before splitting it into the dataframes
# used in further analyses. The script additionally calculates log-transformed 
# density differences between the survey types for each fish observation as well 
# as average depths between the surveys. 

source(here("./src/model_dataframes.R"))


# Model Selection ==============================================================

# This script runs linear mixed effects models on the dataframes comparing log 
# density differences between survey types which incorporate all habitat, trait, 
# and survey characteristics present in the dataframes. A global model is 
# created for each dataframe (SVC vs. transect surveys and SVC vs. roving 
# surveys) utilizing all appropriate traits and interactions, and a dredge is 
# performed on each to examine the models of the highest likelihood. Included 
# are tests of predictor collinearity and model fit. 

source(here("./src/model_selection.R"))


# Covariate Plots ==============================================================

# This script creates boxplots and scatterplots of significant covariates from
# the global linear mixed effects models and the model average performed on 
# the dredging results. 

source(here("./src/covariate_plots.R"))


# Heat Plots ===================================================================

# This script creates heat plots comparing the difference in fish densities 
# recorded between SVC and belt transect surveys across different levels of 
# species colouration, shape, size, and site stony coral cover. 

source(here("./src/heat_plots.R"))


# SVC vs. Transect Analyses Including Anguilliformes ===========================

# This script runs linear mixed effects models on the dataframes comparing log 
# density differences between SVC and belt transect surveys including all traits 
# from primary analyses and all Anguilliformes species (eels). A global model is 
# created utilizing all appropriate traits and interactions, and a dredge is 
# performed to examine the models of the highest likelihood. Included are tests 
# of predictor collinearity and model fit. Results of this analyses are 
# discussed in the Electronic Supplementary Material.

source(here("./src/anguilliform_model_selection.R"))


# Survey Area and Duration Analyses ============================================

# This script runs linear mixed effects models on the dataframes comparing log 
# density differences between survey types including all traits from primary
# analyses along with differences in total survey area and duration. A global 
# model is created for each dataframe (SVC vs. transect surveys and SVC vs. 
# roving surveys) utilizing all appropriate traits and interactions, and a 
# dredge is performed on each to examine the models of the highest likelihood. 
# Included are tests of predictor collinearity and model fit. Results of these 
# analyses are discussed in the Electronic Supplementary Material.

source(here("./src/survey_area_duration_models.R"))


# Density Comparison ===========================================================

# These scripts compare the abundances of traits recorded on survey types as 
# opposed to their densities. The first script examines differences in 
# the abundances of traits between SVC and belt transect surveys and the second
# examines differences between SVC and roving surveys. The traits examined are
# those that were found to have a significant affect on density differences
# between survey types from previous analyses. Differences in abundances are
# compared via Chi-Square tests. The results of these analyses are discussed
# in the Electronic Supplementary Material. 

source(here("./src/SVCprey_trait_presabs.R"))
source(here("./src/SVCpred_trait_presabs.R"))