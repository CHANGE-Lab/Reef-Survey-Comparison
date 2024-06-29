########## SURVEY COMPARISON PROJECT ANGUILLIFORM RESULTS ##########
########## 
##########
# This file creates global linear mixed effects models to compare fish density 
# differences between SVC and transect surveys using species, habitat, and 
# survey traits as predictors. This data includes species of the order 
# Anguilliform which were removed from other analyses. It then performs a dredge 
# on each global model to determine which predictors result in the best model
# fit, based on AICc values. 
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2021-12-8
##########
##########


# Set-Up =======================================================================

# packages
library(tidyverse)
library(nlme)
library(lme4)
library(car)
library(arm)
library(MuMIn)
library(data.table)
library(emmeans)
library(ggplot2)
library(here)

# data
SVCprey_anguilliform_data <- 
  read_csv(here("./dataframes/SVCprey_dataframe_anguilliformes.csv"))


# SVC vs. Transect: Centring Variables =========================================

# The following centres all continuous and dummy variables from the global 
# model by subtracting the sample mean from all variable values.

# octocoral
SVCprey_anguilliform_data$octocoral_c <- SVCprey_anguilliform_data$octocoral - 
  mean(SVCprey_anguilliform_data$octocoral)

# stony coral
SVCprey_anguilliform_data$stony_c <- 
  SVCprey_anguilliform_data$stony - mean(SVCprey_anguilliform_data$stony)

# vertical relief
SVCprey_anguilliform_data$relief_c <- SVCprey_anguilliform_data$relief_cm - 
  mean(SVCprey_anguilliform_data$relief_cm)

# size bins
SVCprey_anguilliform_data$size_bin_c <- 
  SVCprey_anguilliform_data$size_bin_lengths - 
  mean(SVCprey_anguilliform_data$size_bin_lengths)

# maximum length
SVCprey_anguilliform_data$max_length_c <- SVCprey_anguilliform_data$max_length - 
  mean(SVCprey_anguilliform_data$max_length)

# depth
SVCprey_anguilliform_data$depth_c <- SVCprey_anguilliform_data$average_depth - 
  mean(SVCprey_anguilliform_data$average_depth)

# area difference
SVCprey_anguilliform_data$area_dif_c <- 
  SVCprey_anguilliform_data$SVCprey_area_dif - 
  mean(SVCprey_anguilliform_data$SVCprey_area_dif)


# SVC vs. Transect: Anguilliform Global Model ==================================

# In the following, a linear mixed model is created to compare fish density 
# differences between SVC and transect surveys in response to predictors 
# including species traits, habitat traits, and survey traits. All traits of 
# interest are included in the global model. Collinearity is explored through 
# the VIF values for each predictor, and model fit is determined through 
# random effects plots, residual plots, qq plots, and model plots. 

# global lme model
SVCprey_anguilliform_global <- lme(log_difference~habitat+octocoral_c+stony_c+
                                     relief_c+size_bin_c*colouration+nocturnal+
                                     position+max_length+behavior+
                                     cryptic_behaviour+depth_c+shape+area_dif_c, 
                      random = list(~1|site, ~1|species_order), 
                      SVCprey_anguilliform_data) 

# model summary
summary(SVCprey_anguilliform_global) 
AICc(SVCprey_anguilliform_global)

# covariate VIF values
vif(SVCprey_anguilliform_global) 

# remove shape
SVCprey_anguilliform_global2 <- lme(log_difference~habitat+octocoral+stony+
                                      relief_cm+size_bin_lengths*colouration+
                                      nocturnal+position+max_length+behavior+
                                      cryptic_behaviour+average_depth+
                                      area_dif_c, 
                                   random = list(~1|site, ~1|species_order), 
                                   SVCprey_anguilliform_data) 

# model summary
summary(SVCprey_anguilliform_global2) 
AICc(SVCprey_anguilliform_global2)

# covariate VIF values
vif(SVCprey_anguilliform_global2) 

# random effects plot
plot(ranef(SVCprey_anguilliform_global2))

# residuals plot
res_SVCprey_global = residuals(SVCprey_anguilliform_global2)
plot(res_SVCprey_global2) 

# qq plot
qqnorm(res_SVCprey_global2) 
qqline(res_SVCprey_global2)

# model plot
plot(SVCprey_anguilliform_global2) 


# SVC vs. Transect: Anguilliform Dredging ======================================

# The following performs a dredge on the global linear mixed effects model 
# comparing SVC fish densities to transect survey fish densities in order to 
# determine which combination of predictors results in the most likely model.

# dredge
SVCprey_anguilliform_dredge <- dredge(SVCprey_anguilliform_global2)
SVCprey_anguilliform_dredge

# save dredge output 
saveRDS(SVCprey_anguilliform_dredge, 
        here("./outputs/SVCprey_anguilliform_dredge.rds"))

# subset dredge
SVCprey_anguil_dredge_sub <- subset(SVCprey_anguilliform_dredge, delta < 4) 

# model average 
SVCprey_anguil_average <- model.avg(SVCprey_anguil_dredge_sub)
SVCprey_anguil_avg_summary <- summary(SVCprey_anguil_average)

# confidence intervals of predictors
SVCprey_anguil_confidence <- confint(SVCprey_anguil_average)