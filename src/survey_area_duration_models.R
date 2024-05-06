########## SURVEY COMPARISON PROJECT SURVEY AREA AND DURATION MODELS ##########
########## 
##########
# This file creates global linear mixed effects models to compare fish density 
# differences between SVC and transect surveys and SVC and roving surveys using 
# species, habitat, and survey traits as predictors, and specifically accounting 
# for differences in total survey arae and duration. It then performs a dredge 
# on each global model to determine which predictors result in the best model
# fit, based on AICc values.  
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2024-05-06
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
library(cowplot)
library(ggpubr)
library(patchwork)
library(performance)
library(here)

# data
SVCprey_model_data <- read_csv(here("./dataframes/SVCprey_dataframe.csv"))
SVCpred_model_data_ta <- 
  read_csv(here("./dataframes/SVCpred_dataframe_time_area.csv"))


# SVC vs. Transect: Centring Variables =========================================

# The following centres all continuous and dummy variables from the global 
# model by subtracting the sample mean from all variable values.

# octocoral
SVCprey_model_data$octocoral_c <- 
  SVCprey_model_data$octocoral - mean(SVCprey_model_data$octocoral)

# stony coral
SVCprey_model_data$stony_c <- 
  SVCprey_model_data$stony - mean(SVCprey_model_data$stony)

# vertical relief
SVCprey_model_data$relief_c <- 
  SVCprey_model_data$relief_cm - mean(SVCprey_model_data$relief_cm)

# size bins
SVCprey_model_data$size_bin_c <- SVCprey_model_data$size_bin_lengths - 
  mean(SVCprey_model_data$size_bin_lengths)

# maximum length
SVCprey_model_data$max_length_c <- 
  SVCprey_model_data$max_length - mean(SVCprey_model_data$max_length)

# depth
SVCprey_model_data$depth_c <- 
  SVCprey_model_data$average_depth - mean(SVCprey_model_data$average_depth)

# area difference 
SVCprey_model_data$area_dif_c <- SVCprey_model_data$SVCprey_area_dif - 
  mean(SVCprey_model_data$SVCprey_area_dif)


# SVC vs. Transect: Global Model (area difference included) ====================

# The following creates the SVC vs. Transect survey global model utilizing the 
# best fit global model obtained prior to centring.

# global lme model
SVCprey_global_a <- lme(log_difference~habitat+octocoral_c+stony_c+relief_c+
                          size_bin_c*colouration+nocturnal+position+
                          max_length_c+behavior+cryptic_behaviour+depth_c+
                          size_bin_c*shape+area_dif_c, 
                        random = list(~1|site, ~1|species_order), 
                        SVCprey_model_data) 

# model summary
summary(SVCprey_global_a)
AICc(SVCprey_global_a) # 33754.43

# covariate VIF values
vif(SVCprey_global_a) # aggregation behaviour VIF = 5.254327

# random effects plot
plot(ranef(SVCprey_global_a))

# residuals plot
res_SVCprey_global_a = residuals(SVCprey_global_a)
plot(res_SVCprey_global_a) 

# qq plot
qqnorm(res_SVCprey_global_a) 
qqline(res_SVCprey_global_a)

# model plot
plot(SVCprey_global_a) 

# SVC vs. Transect: Dredging (area difference included) ========================

# The following performs a dredge on the global linear mixed effects model 
# comparing SVC fish densities to transect survey fish densities in order to 
# determine which combination of predictors results in the most likely model.

# dredge
SVCprey_dredge_a <- dredge(SVCprey_global_a)
SVCprey_dredge_a

# subset dredge
SVCprey_dredge_sub_a <- subset(SVCprey_dredge_a, delta < 4) 

# model average 
SVCprey_model_average_a <- model.avg(SVCprey_dredge_sub_a)
SVCprey_model_avg_summary_a <- summary(SVCprey_model_average_a)


# SVC vs. Roving: Centring Variables in Time/Area DF ===========================

# The following centres all continuous and dummy variables in the SVC vs. roving
# dataframe containing area, time, and speed differences from the global 
# model by subtracting the sample mean from all variable values. 

# octocoral
SVCpred_model_data_ta$octocoral_c <- SVCpred_model_data_ta$octocoral - 
  mean(SVCpred_model_data_ta$octocoral)

# stony coral
SVCpred_model_data_ta$stony_c <- SVCpred_model_data_ta$stony - 
  mean(SVCpred_model_data_ta$stony)

# vertical relief
SVCpred_model_data_ta$relief_c <- SVCpred_model_data_ta$relief_cm - 
  mean(SVCpred_model_data_ta$relief_cm)

# size bins
SVCpred_model_data_ta$size_bin_c <- SVCpred_model_data_ta$size_bin_lengths - 
  mean(SVCpred_model_data_ta$size_bin_lengths)

# area difference
SVCpred_model_data_ta$area_dif_c <- SVCpred_model_data_ta$SVCpred_area_dif - 
  mean(SVCpred_model_data_ta$SVCpred_area_dif)

# time difference
SVCpred_model_data_ta$time_dif_c <- SVCpred_model_data_ta$SVCpred_time_dif - 
  mean(SVCpred_model_data_ta$SVCpred_time_dif)

# speed difference
SVCpred_model_data_ta$speed_dif_c <- SVCpred_model_data_ta$SVCpred_speed_dif - 
  mean(SVCpred_model_data_ta$SVCpred_speed_dif)


# SVC vs. Roving: Global Model (difference in time and area included) ==========

# The following creates the SVC vs. Roving survey global model utilizing the 
# best fit global model obtained prior to centering.

SVCpred_ta <- lme(log_difference~habitat+octocoral_c+stony_c+relief_c+nocturnal+
                    max_length+cryptic_behaviour+average_depth+size_bin_c+
                    colouration+shape+position+area_dif_c+time_dif_c, 
                  random = list(~1|site, ~1|species_order), 
                  SVCpred_model_data_ta) 
summary(SVCpred_ta) # AIC = 1289.737
AICc(SVCpred_ta) # AICc = 1291.691
vif(SVCpred_ta) 
# habitat, max_length, average_depth, colouration, shape, position >5

SVCpred_ta2 <- lme(log_difference~habitat+octocoral_c+stony_c+relief_c+
                     nocturnal+cryptic_behaviour+size_bin_c+colouration+shape+
                     area_dif_c+time_dif_c, 
                   random = list(~1|site, ~1|species_order), 
                   SVCpred_model_data_ta) 
summary(SVCpred_ta2) # AIC = 1267.223
AICc(SVCpred_ta2) # AICc = 1268.611
vif(SVCpred_ta2) # all good 

# random effects plot
plot(ranef(SVCpred_ta2))

# residuals plot
res_SVCpred_ta2 = residuals(SVCpred_ta2)
plot(res_SVCpred_ta2) 

# qq plot
qqnorm(res_SVCpred_ta2) 
qqline(res_SVCpred_ta2)

# model plot
plot(SVCpred_ta2) 


# SVC vs. Roving: Dredge (difference in time and area included) ================

# dredge
SVCpred_dredge_ta <- dredge(SVCpred_ta2)
SVCpred_dredge_ta

# save dredge results 
saveRDS(SVCpred_dredge_ta, here("./outputs/SVCpred_dredge_time_area.rds"))

# subset dredge
SVCpred_dredge_ta_sub <- subset(SVCpred_dredge_ta, delta < 4) 

# model average 
SVCpred_model_average_ta <- model.avg(SVCpred_dredge_ta_sub)
SVCpred_model_avg_summary_ta <- summary(SVCpred_model_average_ta)

# save model average
saveRDS(SVCpred_model_average_ta, 
        here("./outputs/SVCpred_dredge_average_time_area.rds"))

# read in saved average
SVCpred_model_average_ta <- 
  read_rds(here("./outputs/SVCpred_dredge_average_time_area.rds"))
SVCpred_model_avg_ta_summary <- summary(SVCpred_model_average_ta)

# covariate confidence intervals
SVCpred_confidence_ta <- confint(SVCpred_model_average_ta)
summary(SVCpred_confidence_ta)

# save confidence intervals
saveRDS(SVCpred_confidence_ta, 
        here("./outputs/SVCpred_dredge_CI_time_area.rds"))


# SVC vs. Roving: Global Model (difference in speed included) ==================

# The following creates the SVC vs. Roving survey global model utilizing the 
# best fit global model obtained prior to centering.

SVCpred_s <- lme(log_difference~habitat+octocoral_c+stony_c+relief_c+nocturnal+
                   max_length+cryptic_behaviour+average_depth+size_bin_c+
                   colouration+shape+position+speed_dif_c, 
                 random = list(~1|site, ~1|species_order), 
                 SVCpred_model_data_ta) 
summary(SVCpred_s) # AIC = 1276.7
AICc(SVCpred_s) # AICc = 1278.454
vif(SVCpred_s) 
# habitat, max_length, average_depth, colouration, shape, position >5

SVCpred_s2 <- lme(log_difference~habitat+octocoral_c+stony_c+relief_c+nocturnal+
                    cryptic_behaviour+size_bin_c+colouration+shape+speed_dif_c, 
                  random = list(~1|site, ~1|species_order), 
                  SVCpred_model_data_ta) 
summary(SVCpred_s2) # AIC = 1254.423
AICc(SVCpred_s2) # AICc = 1255.644
vif(SVCpred_s2) # all good 

# random effects plot
plot(ranef(SVCpred_s2))

# residuals plot
res_SVCpred_s2 = residuals(SVCpred_s2)
plot(res_SVCpred_s2) 

# qq plot
qqnorm(res_SVCpred_s2) 
qqline(res_SVCpred_s2)

# model plot
plot(SVCpred_s2) 


# SVC vs. Roving: Dredge (difference survey speed) =============================

# dredge
SVCpred_dredge_s <- dredge(SVCpred_s2)
SVCpred_dredge_s

# save dredge results 
saveRDS(SVCpred_dredge_s, here("./outputs/SVCpred_dredge_speed.rds"))

# subset dredge
SVCpred_dredge_s_sub <- subset(SVCpred_dredge_s, delta < 4) 

# model average 
SVCpred_model_average_s <- model.avg(SVCpred_dredge_s_sub)
SVCpred_model_avg_summary_s <- summary(SVCpred_model_average_s)

# save model average
saveRDS(SVCpred_model_average_s, 
        here("./outputs/SVCpred_dredge_average_speed.rds"))

# read in saved average
SVCpred_model_average_s <- 
  read_rds(here("./outputs/SVCpred_dredge_average_speed.rds"))
SVCpred_model_avg_s_summary <- summary(SVCpred_model_average_s)

# covariate confidence intervals
SVCpred_confidence_s <- confint(SVCpred_model_average_s)
summary(SVCpred_confidence_s)

# save confidence intervals
saveRDS(SVCpred_confidence_s, here("./outputs/SVCpred_dredge_CI_speed.rds"))