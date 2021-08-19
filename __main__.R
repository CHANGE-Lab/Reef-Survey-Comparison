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
# onto the cleaned fish dataframe before splitting it into the two dataframes
# used in further analyses. One dataframe examines fish observations within 
# SVC and transect surveys, while the other explores observations in SVC and 
# roving surveys. The script additionally calculates log-transformed density 
# differences between the survey types for each fish observation as well as 
# average depths between the surveys. 

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


# Density Comparison ===========================================================

# This script creates a dataframe of the average densities of each fish species
# between SVC and transect surveys and SVC and roving surveys. It then tests
# for differences in the average densities of species between surveys using 
# Kruskal-Wallis one-way analysis of variance tests. 

source(here("./src/density_comparison.R"))


# SVC vs. Transect: Presence/Absence Comparison ================================

# This script creates a dataframe of the total number of sessions each of 8 SVC
# focal species was present in between SVC and transect surveys. The difference
# in the total number of sessions across species as well as for each species
# individually is then analyzed using Chi-Square tests and a barplot of the 
# totals is created. 

source(here("./src/SVCprey_presabs.R"))


# SVC vs. Transect: Focal Species Density Comparison ===========================

# This script creates a dataframe of the average density of each of 8 SVC focal
# species recorded in SVC and transect surveys. The difference in average 
# densities of all 8 species as well as each species individually between 
# surveys are determined using Kruskal-Wallis one-way analysis of variance 
# tests. A barplot of the average densities between surveys is also created. 

source(here("./src/SVCprey_density.R"))


# SVC vs. Roving: Presence/Absence Comparison ==================================

# This script creates a dataframe of presence/absence recordings of species
# across sessions for SVC and roving surveys in order to compare differences in 
# recordings. A Chi-Square test is performed to determine whether a difference
# between SVC and roving presence recordings exists. A barplot is created of 
# presence recordings in sessions across the surveys for every species. 

source(here("./src/SVCpred_chisquare.R"))


# SVC vs. Roving: Focal Species Density Comparison =============================

# This script creates a dataframe of the average density of each of 3 SVC focal
# species recorded in both SVC and roving surveys. The difference in average 
# densities of all 3 species as well as each species individually between 
# surveys are determined using Kruskal-Wallis one-way analysis of variance 
# tests. A barplot of the average densities between surveys is also created. 

source(here("./src/SVCpred_density.R"))


# Covariate Plots ==============================================================

# This script creates boxplots and scatterplots of significant covariates from
# the global linear mixed effects models and the model average performed on 
# the dredging results. 

source(here("./src/covariate_plots.R"))


# Density Barplots =============================================================

# This script creates and saves barplots of the density differences between 
# survey types averaged across families (for SVC compared to transect surveys) 
# and species (for SVC compared to roving surveys). 

source(here("./src/density_barplots.R"))