########## SURVEY COMPARISON PROJECT MODELS WITH ALTERNATE SIZE BINS ##########
########## 
##########
# This file creates global linear mixed effects models to compare fish density 
# differences between SVC and transect surveys and SVC and roving surveys using 
# species, habitat, and survey traits as predictors. It then performs a dredge 
# on each global model to determine which predictors result in the best model
# fit, based on AICc values. 
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2021-06-03
##########
##########


# Set-Up =======================================================================

# packages
library(tidyverse)
library(nlme)
library(car)
library(arm)
library(MuMIn)
library(here)

# data
SVCprey_model_data <- read_csv(here("./dataframes/SVCprey_model_data.csv"))
SVCpred_model_data <- read_csv(here("./dataframes/SVCpred_model_data.csv"))
SVC_lengths <- read_csv(here("./clean_data/SVC_lengths.csv"))
prey_lengths <- read_csv(here("./clean_data/prey_fish_data.csv"))
pred_lengths <- read_csv(here("./clean_data/pred_fish_data.csv"))


# Determining Size Bin 1 Classification ========================================

# The following determines the median length of size class 1 (<=5cm).

# extract lengths < 5cm 
SVC_1 <- SVC_lengths %>% filter(SVC_lengths$SVC_tl <= 5)
prey_1 <- prey_lengths %>% filter(prey_lengths$prey_tl <= 5)
pred_1 <- pred_lengths %>% filter(pred_lengths$pred_tl <= 5)

# round SVC abundances
SVC_1$abundance_round <- ifelse(SVC_1$SVC_abundance == 0.25, 1, 
                         ifelse(SVC_1$SVC_abundance == 0.5, 1, 
                         ifelse(SVC_1$SVC_abundance == 6.5, 7, 
                         round(SVC_1$SVC_abundance, digits = 0))))

# duplicate SVC length rows by abundance 
SVC_1 <- SVC_1[rep(row.names(SVC_1), SVC_1$abundance_round), 1:4]

# select length and species columns
SVC_1 <- SVC_1[,c(2,4)]
prey_1 <- prey_1[,3:4]
pred_1 <- pred_1[,3:4]

# re-name length columns
SVC_1 <- rename(SVC_1, length = SVC_tl)
prey_1 <- rename(prey_1, length = prey_tl)
pred_1 <- rename(pred_1, length = pred_tl)

# bind together
size_class_1 <- bind_rows(SVC_1, prey_1, pred_1)

# find median
median(size_class_1$length) # = 4cm 
mean(size_class_1$length) # = 3.63321cm


# Determining Size Bin 2 Classification ========================================

# The following determines the median length of size class 2 (>5cm, <=10cm).

# extract lengths >5cm, <=10cm
SVC_2 <- SVC_lengths %>% 
  filter(SVC_lengths$SVC_tl > 5 & SVC_lengths$SVC_tl <= 10)
prey_2 <- prey_lengths %>% 
  filter(prey_lengths$prey_tl > 5 & prey_lengths$prey_tl <= 10)
pred_2 <- pred_lengths %>% 
  filter(pred_lengths$pred_tl > 5 & pred_lengths$pred_tl <= 10)

# round SVC abundances
SVC_2$abundance_round <- ifelse(SVC_2$SVC_abundance == 0.25, 1, 
                         ifelse(SVC_2$SVC_abundance == 0.5, 1, 
                         ifelse(SVC_2$SVC_abundance == 6.5, 7, 
                         round(SVC_2$SVC_abundance, digits = 0))))

# duplicate SVC length rows by abundance 
SVC_2 <- SVC_2[rep(row.names(SVC_2), SVC_2$abundance_round), 1:4]

# select length and species columns
SVC_2 <- SVC_2[,c(2,4)]
prey_2 <- prey_2[,3:4]
pred_2 <- pred_2[,3:4]

# re-name length columns
SVC_2 <- rename(SVC_2, length = SVC_tl)
prey_2 <- rename(prey_2, length = prey_tl)
pred_2 <- rename(pred_2, length = pred_tl)

# bind together
size_class_2 <- bind_rows(SVC_2, prey_2, pred_2)

# find median
median(size_class_2$length) # = 7cm 
mean(size_class_2$length) # = 7.533217cm


# Determining Size Bin 3 Classification ========================================

# The following determines the median length of size class 3 (>10cm, <=15cm).

# extract lengths >10cm, <=15cm
SVC_3 <- SVC_lengths %>% 
  filter(SVC_lengths$SVC_tl > 10 & SVC_lengths$SVC_tl <= 15)
prey_3 <- prey_lengths %>% 
  filter(prey_lengths$prey_tl > 10 & prey_lengths$prey_tl <= 15)
pred_3 <- pred_lengths %>% 
  filter(pred_lengths$pred_tl > 10 & pred_lengths$pred_tl <= 15)

# round SVC abundances
SVC_3$abundance_round <- ifelse(SVC_3$SVC_abundance == 0.25, 1, 
                         ifelse(SVC_3$SVC_abundance == 0.5, 1, 
                         ifelse(SVC_3$SVC_abundance == 6.5, 7, 
                         round(SVC_3$SVC_abundance, digits = 0))))

# duplicate SVC length rows by abundance 
SVC_3 <- SVC_3[rep(row.names(SVC_3), SVC_3$abundance_round), 1:4]

# select length and species columns
SVC_3 <- SVC_3[,c(2,4)]
prey_3 <- prey_3[,3:4]
pred_3 <- pred_3[,3:4]

# re-name length columns
SVC_3 <- rename(SVC_3, length = SVC_tl)
prey_3 <- rename(prey_3, length = prey_tl)
pred_3 <- rename(pred_3, length = pred_tl)

# bind together
size_class_3 <- bind_rows(SVC_3, prey_3, pred_3)

# find median
median(size_class_3$length) # = 13cm 
mean(size_class_3$length) # = 12.91456cm


# Determining Size Bin 4 Classification ========================================

# The following determines the median length of size class 4 (>15cm, <=20cm).

# extract lengths >15cm, <=20cm
SVC_4 <- SVC_lengths %>% 
  filter(SVC_lengths$SVC_tl > 15 & SVC_lengths$SVC_tl <= 20)
prey_4 <- prey_lengths %>% 
  filter(prey_lengths$prey_tl > 15 & prey_lengths$prey_tl <= 20)
pred_4 <- pred_lengths %>% 
  filter(pred_lengths$pred_tl > 15 & pred_lengths$pred_tl <= 20)

# round SVC abundances
SVC_4$abundance_round <- ifelse(SVC_4$SVC_abundance == 0.25, 1, 
                         ifelse(SVC_4$SVC_abundance == 0.5, 1, 
                         ifelse(SVC_4$SVC_abundance == 6.5, 7, 
                         round(SVC_4$SVC_abundance, digits = 0))))

# duplicate SVC length rows by abundance 
SVC_4 <- SVC_4[rep(row.names(SVC_4), SVC_4$abundance_round), 1:4]

# select length and species columns
SVC_4 <- SVC_4[,c(2,4)]
prey_4 <- prey_4[,3:4]
pred_4 <- pred_4[,3:4]

# re-name length columns
SVC_4 <- rename(SVC_4, length = SVC_tl)
prey_4 <- rename(prey_4, length = prey_tl)
pred_4 <- rename(pred_4, length = pred_tl)

# bind together
size_class_4 <- bind_rows(SVC_4, prey_4, pred_4)

# find median
median(size_class_4$length) # = 18cm 
mean(size_class_4$length) # = 17.94356cm


# Determining Size Bin 5 Classification ========================================

# The following determines the median length of size class 5 (>20cm, <=30cm).

# extract lengths >20cm, <=30cm
SVC_5 <- SVC_lengths %>% 
  filter(SVC_lengths$SVC_tl > 20 & SVC_lengths$SVC_tl <= 30)
prey_5 <- prey_lengths %>% 
  filter(prey_lengths$prey_tl > 20 & prey_lengths$prey_tl <= 30)
pred_5 <- pred_lengths %>% 
  filter(pred_lengths$pred_tl > 20 & pred_lengths$pred_tl <= 30)

# round SVC abundances
SVC_5$abundance_round <- ifelse(SVC_5$SVC_abundance == 0.25, 1, 
                         ifelse(SVC_5$SVC_abundance == 0.5, 1, 
                         ifelse(SVC_5$SVC_abundance == 6.5, 7, 
                         round(SVC_5$SVC_abundance, digits = 0))))

# duplicate SVC length rows by abundance 
SVC_5 <- SVC_5[rep(row.names(SVC_5), SVC_5$abundance_round), 1:4]

# select length and species columns
SVC_5 <- SVC_5[,c(2,4)]
prey_5 <- prey_5[,3:4]
pred_5 <- pred_5[,3:4]

# re-name length columns
SVC_5 <- rename(SVC_5, length = SVC_tl)
prey_5 <- rename(prey_5, length = prey_tl)
pred_5 <- rename(pred_5, length = pred_tl)

# bind together
size_class_5 <- bind_rows(SVC_5, prey_5, pred_5)

# find median
median(size_class_5$length) # = 25cm 
mean(size_class_5$length) # = 24.92844cm


# Determining Size Bin 6 Classification ========================================

# The following determines the median length of size class 6 (>30cm).

# extract lengths > 30cm 
SVC_6 <- SVC_lengths %>% filter(SVC_lengths$SVC_tl > 30)
prey_6 <- prey_lengths %>% filter(prey_lengths$prey_tl > 30)
pred_6 <- pred_lengths %>% filter(pred_lengths$pred_tl > 30)

# round SVC abundances
SVC_6$abundance_round <- ifelse(SVC_6$SVC_abundance == 0.25, 1, 
                         ifelse(SVC_6$SVC_abundance == 0.5, 1, 
                         ifelse(SVC_6$SVC_abundance == 6.5, 7, 
                         round(SVC_6$SVC_abundance, digits = 0))))

# duplicate SVC length rows by abundance 
SVC_6 <- SVC_6[rep(row.names(SVC_6), SVC_6$abundance_round), 1:4]

# select length and species columns
SVC_6 <- SVC_6[,c(2,4)]
prey_6 <- prey_6[,3:4]
pred_6 <- pred_6[,3:4]

# re-name length columns
SVC_6 <- rename(SVC_6, length = SVC_tl)
prey_6 <- rename(prey_6, length = prey_tl)
pred_6 <- rename(pred_6, length = pred_tl)

# bind together
size_class_6 <- bind_rows(SVC_6, prey_6, pred_6)

# find median
median(size_class_6$length) # = 40cm 
mean(size_class_6$length) # = 47.70199cm


# Size Bin Reclassifying =======================================================

# The following reclassifies size bins using the mean length that each contains. 

# size bin 1 = 3.63321 cm
# size bin 2 = 7.533217 cm
# size bin 3 = 12.91456 cm
# size bin 4 = 17.94356 cm
# size bin 5 = 24.92844 cm
# size bin 6 = 47.70199 cm

# SVC vs. transect survey
SVCprey_model_data$size_bin_lengths <- 
  ifelse(SVCprey_model_data$size_bin == 1, 3.63321, 
  ifelse(SVCprey_model_data$size_bin == 2, 7.533217, 
  ifelse(SVCprey_model_data$size_bin == 3, 12.91456, 
  ifelse(SVCprey_model_data$size_bin == 4, 17.94356, 
  ifelse(SVCprey_model_data$size_bin == 5, 24.92844, 
  ifelse(SVCprey_model_data$size_bin == 6, 47.70199, NA))))))

# SVC vs. roving survey
SVCpred_model_data$size_bin_lengths <- 
  ifelse(SVCpred_model_data$size_bin == 1, 3.63321, 
  ifelse(SVCpred_model_data$size_bin == 2, 7.533217, 
  ifelse(SVCpred_model_data$size_bin == 3, 12.91456, 
  ifelse(SVCpred_model_data$size_bin == 4, 17.94356, 
  ifelse(SVCpred_model_data$size_bin == 5, 24.92844, 
  ifelse(SVCpred_model_data$size_bin == 6, 47.70199, NA))))))


# SVC vs. Transect: Global Model ===============================================

# In the following, a linear mixed model is created to compare fish density 
# differences between SVC and transect surveys in response to predictors 
# including species traits, habitat traits, and survey traits. All traits of 
# interest are included in the global model. Collinearity is explored through 
# the VIF values for each predictor, and model fit is determined through 
# random effects plots, residual plots, qq plots, and model plots. 

# global lme model
SVCpreylengths_global <- lme(log_difference~habitat+octocoral+stony+relief_cm+
                        size_bin_lengths*colouration2+nocturnal+position+
                        max_length+behavior+cryptic_behaviour+average_depth+
                        size_bin_lengths*shape, 
                      random = list(~1|site, ~1|species_order), 
                      SVCprey_model_data) 
# response is log density difference, random effects are site and species order

# model summary
summary(SVCpreylengths_global) 

# covariate VIF values
vif(SVCpreylengths_global) 

# random effects plot
plot(ranef(SVCpreylengths_global))

# residuals plot
res_SVCprey_global = residuals(SVCprey_global)
plot(res_SVCprey_global) 

# qq plot
qqnorm(res_SVCprey_global) 
qqline(res_SVCprey_global)

# model plot
plot(SVCprey_global) 


# SVC vs. Transect: Dredging ===================================================

# The following performs a dredge on the global linear mixed effects model 
# comparing SVC fish densities to transect survey fish densities in order to 
# determine which combination of predictors results in the most likely model.

# dredge
SVCprey_global_dredge <- dredge(SVCprey_global)
SVCprey_global_dredge
# First model (AICc = 29562.4): colouration, cryptic behaviour, habitat, max 
# length, shape, size bin, colouration*size bin, shape*size bin
# Second model ( AICc = : aggregation behaviour, colouration, cryptic behaviour, 
# habitat, max length, shape, size bin, colouration*size bin, shape*size bin
# Delta AIC = 1.82 between them and 2.10 between second and third model (third 
# has position)

# subset dredge
SVCprey_dredge_sub <- subset(SVCprey_global_dredge, delta < 4) 

# model average 
SVCprey_model_average <- model.avg(SVCprey_dredge_sub)
summary(SVCprey_model_average)

# confidence intervals of predictors
confint(SVCprey_model_average)

# save dredge output 
saveRDS(SVCprey_global_dredge, here("./outputs/SVCprey_global_dredge.rds"))


# SVC vs. Roving: Global Model =================================================

# In the following, a linear mixed model is created to compare fish density 
# differences between SVC and roving surveys in response to predictors 
# including species traits, habitat traits, and survey traits. All traits of 
# interest are included in the global model. Collinearity is explored through 
# the VIF values for each predictor, and model fit is determined through 
# random effects plots, residual plots, qq plots, and model plots. 

# global lme model
SVCpred_global <- lme(log_difference~habitat+octocoral+stony+relief_cm+
                        nocturnal+max_length+cryptic_behaviour+ average_depth+
                        colouration2+size_bin_lengths, 
                      random = list(~1|site, ~1|species_order),
                      SVCpred_model_data) 

# model summary 
summary(SVCpred_global) 
# AIC = 1695.676
# sig pos covariates = silvering
# sig neg covariates = stony, size bin

# covariate VIF values
vif(SVCpred_global)
# habitat GVIF = 6.832051
# position GVIF = 6.634015
# depth GVIF = 5.079790
# colouration GVIF = 21.218479
# shape GVIF = 9.864492

# random effects plot
plot(ranef(SVCpred_global))

# residuals plot
res_SVCpred_global = residuals(SVCpred_global)
plot(res_SVCpred_global)

# qq plot
qqnorm(res_SVCpred_global) 
qqline(res_SVCpred_global) 

# model plot
plot(SVCpred_global) 


# SVC vs. Roving: Dredging =====================================================

# The following performs a dredge on the global linear mixed effects model 
# comparing SVC fish densities to roving survey fish densities in order to 
# determine which combination of predictors results in the most likely model.

# dredge
SVCpred_dredge <- dredge(SVCpred_global)
SVCpred_dredge
# First model (AICc = 1631.5): colouration, shape
# Second model (AICc = 1631.6, delta AICc = 0.11): cryptic behaviour, shape 
# Third model (AICc = 1633.1, delta AICc = 1.64): colouration, position, shape 
# Fourth model (AICc = 1633.4, delta AICc = 1.90): colouration  

# subset dredge
SVCpred_dredge_sub <- subset(SVCpred_dredge, delta < 4) 

# model average 
SVCpred_model_average <- model.avg(SVCpred_dredge_sub)
summary(SVCpred_model_average)

# covariate confidence intervals
confint(SVCpred_model_average)

# save dredge results 
saveRDS(SVCpred_dredge, here("./outputs/SVCpred_dredge.rds"))