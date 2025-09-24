########## SVC VS. ROVING MODEL SELECTION ##########
########## 
##########
# This file goes through the workflow of optimizing fit on the top model 
# comparing density differences reported on SVC compared to roving surveys. 
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2025-04-22
##########
##########


# Set-Up =======================================================================

# packages
library(tidyverse)
library(lme4)
library(lmerTest)
library(car)
library(performance)
library(MuMIn)
library(AER)
library(here)

# data
SVCpred_model_data <- read_csv(here("./dataframes/SVCpred_dataframe.csv"))


# SVC vs. Roving: Centring & Scaling Variables =================================

# The following centres all continuous and dummy variables from the global 
# model by subtracting the sample mean from all variable values.

# make nocturnality and crypsis categorical
SVCpred_model_data$nocturnal <- as.character(SVCpred_model_data$nocturnal)
SVCpred_model_data$cryptic_behaviour <- 
  as.character(SVCpred_model_data$cryptic_behaviour)

# octocoral
SVCpred_model_data$octocoral_c <- SVCpred_model_data$octocoral - 
  mean(SVCpred_model_data$octocoral)

# stony coral
SVCpred_model_data$stony_c <- SVCpred_model_data$stony - 
  mean(SVCpred_model_data$stony)

# vertical relief
SVCpred_model_data$relief_c <- SVCpred_model_data$relief_cm - 
  mean(SVCpred_model_data$relief_cm)

# size bins
SVCpred_model_data$size_bin_c <- SVCpred_model_data$size_bin_lengths - 
  mean(SVCpred_model_data$size_bin_lengths)

# maximum length
SVCpred_model_data$max_length_c <- SVCpred_model_data$max_length - 
  mean(SVCpred_model_data$max_length)

# average depth
SVCpred_model_data$average_depth_c <- SVCpred_model_data$average_depth - 
  mean(SVCpred_model_data$average_depth)

# area difference
SVCpred_model_data$area_dif_c <- SVCpred_model_data$SVCpred_area_dif - 
  mean(SVCpred_model_data$SVCpred_area_dif)

# also re-scale variables 
# (due to warnings in models regarding differences in predictor scales)
SVCpred_model_data <- SVCpred_model_data %>%
  mutate(
    octocoral_scaled = scale(octocoral_c)[,1],
    stony_scaled = scale(stony_c)[,1],
    relief_scaled = scale(relief_c)[,1],
    max_length_scaled = scale(max_length_c)[,1],
    average_depth_scaled = scale(average_depth_c)[,1],
    size_bin_scaled = scale(size_bin_c)[,1],
    area_dif_scaled = scale(area_dif_c)[,1])


# SVC vs. Roving: Global Model =================================================

# The following creates the top SVC vs. Roving survey global model by accounting
# for predictor collinearity and evaluates global model fit using residual 
# plots.

# original global model: all predictors
SVCpred_global <- lmer(log_difference ~ habitat + octocoral_scaled + 
                         stony_scaled + relief_scaled + nocturnal + 
                         max_length_scaled + cryptic_behaviour + 
                         average_depth_scaled + size_bin_scaled +
                        colouration + shape + position + area_dif_scaled + 
                        (1 | site) + (1 | species_order), 
                 data = SVCpred_model_data) 
summary(SVCpred_global)
AICc(SVCpred_global) # AICc = 1368.984
vif(SVCpred_global) 
# habitat, max length, average_depth, colouration, shape, position >5

# We ran AICc model selection by removing all possible combinations of collinear
# predictors. This model, with depth and water column position removed, has
# the best performance based on AICc values. This is the global model that 
# we continue to use throughout this workflow. 

# top global model: depth and position removed
SVCpred_top <- lmer(log_difference ~ habitat + octocoral_scaled + 
                            stony_scaled + relief_scaled + nocturnal + 
                            max_length_scaled + cryptic_behaviour + 
                            size_bin_scaled + colouration + shape + 
                            area_dif_scaled + 
                      (1 | site) + (1 | species_order), 
                    data = SVCpred_model_data)
summary(SVCpred_top)
AICc(SVCpred_top) # AICc = 1363.277
vif(SVCpred_top) # all <5

# diagnostic plots for top global model: 

# residuals plot
res_SVCpred_top = residuals(SVCpred_top)
plot(res_SVCpred_top) 

# qq plot
qqnorm(res_SVCpred_top) 
qqline(res_SVCpred_top)

# histogram of residuals
hist(res_SVCpred_top)

# residual plots from performance package
check_model(SVCpred_top)

# the above summarizes our original model fit


# SVC vs. Roving: Alternate Random Effect Structures ===========================

# The following overviews edits made to the random effect structure of the SVC
# vs. roving global model in order to try to improve model fit. 


#### nest session in site ####

# nest session in site
SVCpred_v2 <- lmer(log_difference ~ habitat + octocoral_scaled + 
                     stony_scaled + relief_scaled + nocturnal + 
                     max_length_scaled + cryptic_behaviour + 
                     average_depth_scaled + size_bin_scaled +
                     colouration + shape + position + area_dif_scaled + 
                     (1 | site/session) + (1 | species_order), 
                   data = SVCpred_model_data)
summary(SVCpred_v2)
AICc(SVCpred_v2) # 1371.157

# residuals plot
res_SVCpred_v2 = residuals(SVCpred_v2)
plot(res_SVCpred_v2) 

# qq plot
qqnorm(res_SVCpred_v2) 
qqline(res_SVCpred_v2)

# histogram of residuals
hist(res_SVCpred_v2)

# residual plots from performance package
check_model(SVCpred_v2) 
# unable to render plots (seems due to nested REs or lme model type)

# CONCLUSION: overall shows no improvement to model fit 


#### nest date in site ####

# nest date in site
SVCpred_date_nest <- lmer(log_difference ~ habitat + octocoral_scaled + 
                            stony_scaled + relief_scaled + nocturnal + 
                            max_length_scaled + cryptic_behaviour + 
                            average_depth_scaled + size_bin_scaled +
                            colouration + shape + position + area_dif_scaled + 
                         (1|site/SVC_date) + (1 | species_order), 
                         data = SVCpred_model_data) 
summary(SVCpred_date_nest)
AICc(SVCpred_date_nest) # 1371.157

# residuals plot
res_SVCpred_date_nest = residuals(SVCpred_date_nest)
plot(res_SVCpred_date_nest) 

# qq plot
qqnorm(res_SVCpred_date_nest) 
qqline(res_SVCpred_date_nest)

# histogram of residuals
hist(res_SVCpred_date_nest)

# residual plots from performance package
check_model(SVCpred_date_nest) 
# unable to render plots (seems due to nested REs)

# CONCLUSION: shows no overall improvement to model fit 


#### nest season in site ####

# create season variable
SVCpred_model_data <- SVCpred_model_data %>%
  mutate(
    month = format(as.Date(SVC_date, format = "%Y-%m-%d"), "%m"), 
    season = case_when(
      month %in% c("12", "01", "02") ~ "Winter",
      month %in% c("03", "04", "05") ~ "Spring",
      month %in% c("06", "07", "08") ~ "Summer",
      month %in% c("09", "10", "11") ~ "Fall",
      TRUE ~ NA_character_ 
    )
  )

# nest season in site
SVCpred_season <- lmer(log_difference ~ habitat + octocoral_scaled + 
                         stony_scaled + relief_scaled + nocturnal + 
                         max_length_scaled + cryptic_behaviour + 
                         average_depth_scaled + size_bin_scaled +
                         colouration + shape + position + area_dif_scaled + 
                      (1|site/season) + (1 | species_order), 
                      data = SVCpred_model_data) 
summary(SVCpred_season)
AICc(SVCpred_season) # 1371.157

# residuals plot
res_SVCpred_season = residuals(SVCpred_season)
plot(res_SVCpred_season) 

# qq plot
qqnorm(res_SVCpred_season) 
qqline(res_SVCpred_season)

# histogram of residuals
hist(res_SVCpred_season)

# residual plots from performance package
check_model(SVCpred_season) # unable to render plots (seems due to nested REs)

# CONCLUSION: shows no overall improvement to model fit 


# SVC vs. Roving: Alternate Data Transformations ===============================

# The following models SVC vs. roving density differences using alternate data
# transformations, other than the log transformation used in prior models


#### no transformation ####

# histogram of raw density difference
hist(SVCpred_model_data$SVC_density - SVCpred_model_data$pred_density)

# make raw density difference variable
SVCpred_model_data$raw_difference <- 
  SVCpred_model_data$SVC_density - SVCpred_model_data$pred_density

# global square root model (from prior model selection)
SVCpred_raw <- lmer(raw_difference ~ habitat + octocoral_scaled + 
                      stony_scaled + relief_scaled + nocturnal + 
                      max_length_scaled + cryptic_behaviour + 
                      average_depth_scaled + size_bin_scaled +
                      colouration + shape + position + area_dif_scaled + 
                            (1|site) + (1|species_order), 
                          data = SVCpred_model_data) 
summary(SVCpred_raw)
AICc(SVCpred_raw) # AICc = -3102.906

# residuals plot
res_SVCpred_raw = residuals(SVCpred_raw)
plot(res_SVCpred_raw) 

# qq plot
qqnorm(res_SVCpred_raw) 
qqline(res_SVCpred_raw)

# histogram
hist(res_SVCpred_raw)

# diagnostic plots with performance package
check_model(SVCpred_raw, return = TRUE)

# CONCLUSION: worsens model fit 


#### square root transformation ####

# calculate square root SVC density in SVC vs. roving data frame
sqrt_SVCdensity <- sqrt(SVCpred_model_data$SVC_density)

# calculate square root roving density 
sqrt_preddensity <- sqrt(SVCpred_model_data$pred_density)

# histogram of SVC vs. roving square root density differences
hist(sqrt_SVCdensity-sqrt_preddensity)

# calculate SVC vs. roving square root density difference
SVCpred_model_data$sqrt_difference <- sqrt_SVCdensity - sqrt_preddensity

# global square root model (from prior model selection)
SVCpred_best_sqrt <- lmer(sqrt_difference ~ habitat + octocoral_scaled + 
                            stony_scaled + relief_scaled + nocturnal + 
                            max_length_scaled + cryptic_behaviour + 
                            average_depth_scaled + size_bin_scaled +
                            colouration + shape + position + area_dif_scaled + 
                         (1|site) + (1|species_order), 
                         data = SVCpred_model_data) 
summary(SVCpred_best_sqrt)
AICc(SVCpred_best_sqrt) # AICc = -1423.421

# residuals plot
res_SVCpred_global_sqrt = residuals(SVCpred_best_sqrt)
plot(res_SVCpred_global_sqrt) 

# qq plot
qqnorm(res_SVCpred_global_sqrt) 
qqline(res_SVCpred_global_sqrt)

# histogram
hist(res_SVCpred_global_sqrt)

# diagnostic plots with performance package
check_model(SVCpred_best_sqrt)

# CONCLUSIONS: worsens model fit 


#### double square root transformation ####

# calculate double square root SVC density in SVC vs. roving dataframe
sqrt2_SVCdensity <- sqrt(sqrt(SVCpred_model_data$SVC_density))

# calculate double square root roving density 
sqrt2_preddensity <- sqrt(sqrt(SVCpred_model_data$pred_density))

# histogram of SVC vs. roving double square root density differences
hist(sqrt2_SVCdensity-sqrt2_preddensity) 

# calculate SVC vs. roving double square root density difference
SVCpred_model_data$sqrt2_difference <- sqrt2_SVCdensity - sqrt2_preddensity

# global square root model (from prior model selection)
SVCpred_best_sqrt2 <- lmer(sqrt2_difference ~ habitat + octocoral_scaled + 
                             stony_scaled + relief_scaled + nocturnal + 
                             max_length_scaled + cryptic_behaviour + 
                             average_depth_scaled + size_bin_scaled +
                             colouration + shape + position + area_dif_scaled + 
                          (1|site) + (1|species_order), 
                          data = SVCpred_model_data) 
summary(SVCpred_best_sqrt2)
AICc(SVCpred_best_sqrt2) # AICc = -405.8912

# residuals plot
res_SVCpred_global_sqrt2 = residuals(SVCpred_best_sqrt2)
plot(res_SVCpred_global_sqrt2) 

# qq plot
qqnorm(res_SVCpred_global_sqrt2) 
qqline(res_SVCpred_global_sqrt2)

# histogram
hist(res_SVCpred_global_sqrt2)

# diagnostic plots with performance package
check_model(SVCpred_best_sqrt2)

# CONCLUSION: worsens model fit 


#### log transformation (post difference) ####

# scale to positive
SVCpred_model_data$pos_raw_difference <- SVCpred_model_data$raw_difference + 0.1

# histogram of log transformed positive raw density difference
hist(log(SVCpred_model_data$pos_raw_difference))

# create log transformed positive raw density difference
SVCpred_model_data$log_raw_difference <- 
  log(SVCpred_model_data$pos_raw_difference)

# log-transformed raw density difference model 
SVCpred_raw_log <- lmer(log_raw_difference ~ habitat + octocoral_scaled + 
                          stony_scaled + relief_scaled + nocturnal + 
                          max_length_scaled + cryptic_behaviour + 
                          average_depth_scaled + size_bin_scaled +
                          colouration + shape + position + area_dif_scaled + 
                      (1|site) + (1|species_order), 
                    data = SVCpred_model_data) 
summary(SVCpred_raw_log)
AICc(SVCpred_raw_log) # AICc = -1141.465

# residuals plot
res_SVCpred_raw_log = residuals(SVCpred_raw_log)
plot(res_SVCpred_raw_log) 

# qq plot
qqnorm(res_SVCpred_raw_log) 
qqline(res_SVCpred_raw_log)

# histogram
hist(res_SVCpred_raw_log)

# diagnostic plots with performance package
check_model(SVCpred_raw_log)

# CONCLUSION: worsens model fit 


#### sqrt transformation (post difference) ####

# histogram of sqrt transformed raw density differences
hist(sqrt(SVCpred_model_data$pos_raw_difference))

# make sqrt transformed raw density difference variable
SVCpred_model_data$sqrt_raw_difference <- 
  sqrt(SVCpred_model_data$pos_raw_difference)

# raw density difference square root model
SVCpred_raw_sqrt <- lmer(sqrt_raw_difference ~ habitat + octocoral_scaled + 
                           stony_scaled + relief_scaled + nocturnal + 
                           max_length_scaled + cryptic_behaviour + 
                           average_depth_scaled + size_bin_scaled +
                           colouration + shape + position + area_dif_scaled + 
                            (1|site) + (1|species_order), 
                          data = SVCpred_model_data) 
summary(SVCpred_raw_sqrt)
AICc(SVCpred_raw_sqrt) # AICc = -2753.432

# residuals plot
res_SVCpred_raw_sqrt = residuals(SVCpred_raw_sqrt)
plot(res_SVCpred_raw_sqrt) 

# qq plot
qqnorm(res_SVCpred_raw_sqrt) 
qqline(res_SVCpred_raw_sqrt)

# histogram
hist(res_SVCpred_raw_sqrt)

# diagnostic plots with performance package
check_model(SVCpred_raw_sqrt)

# CONCLUSION: worsens model fit 


#### double sqrt transformation (post difference) ####

# histogram of double sqrt transformed raw density differences 
hist(sqrt(sqrt(SVCpred_model_data$pos_raw_difference)))

# make double sqrt transformed raw density difference variable
SVCpred_model_data$sqrt2_raw_difference <- 
  sqrt(sqrt(SVCpred_model_data$pos_raw_difference))

# raw density difference double square root model
SVCpred_raw_sqrt2 <- lmer(sqrt2_raw_difference ~ habitat + octocoral_scaled + 
                            stony_scaled + relief_scaled + nocturnal + 
                            max_length_scaled + cryptic_behaviour + 
                            average_depth_scaled + size_bin_scaled +
                            colouration + shape + position + area_dif_scaled + 
                           (1|site) + (1|species_order), 
                         data = SVCpred_model_data) 
summary(SVCpred_raw_sqrt2)
AICc(SVCpred_raw_sqrt2) # AICc = -2890.233

# residuals plot
res_SVCpred_raw_sqrt2 = residuals(SVCpred_raw_sqrt2)
plot(res_SVCpred_raw_sqrt2) 

# qq plot
qqnorm(res_SVCpred_raw_sqrt2) 
qqline(res_SVCpred_raw_sqrt2)

# histogram
hist(res_SVCpred_raw_sqrt2)

# diagnostic plots with performance package
check_model(SVCpred_raw_sqrt2)

# CONCLUSION: does not improve model fit 


# SVC vs. Roving: Alternate Distributions ======================================

# The following fits GLM models to SVC and roving survey density differences
# and utilizes different model distributions to optimize fit. 


#### GLM with gaussian distribution ####

# try glm with gaussian distribution 
SVCpred_glm <- glmer(log_difference ~ habitat + octocoral_scaled + 
                       stony_scaled + relief_scaled + nocturnal + 
                       max_length_scaled + cryptic_behaviour + 
                       average_depth_scaled + size_bin_scaled +
                       colouration + shape + position + area_dif_scaled + 
                       (1|site) + (1|species_order), 
                     data = SVCpred_model_data,
                     family = gaussian()) 
summary(SVCpred_glm)
AICc(SVCpred_glm) # 1368.984

# residuals plot
res_SVCpred_glm = residuals(SVCpred_glm)
plot(res_SVCpred_glm) 

# qq plot
qqnorm(res_SVCpred_glm) 
qqline(res_SVCpred_glm)

# histogram
hist(res_SVCpred_glm)

# check model with performance package
check_model(SVCpred_glm)

# CONCLUSION: does not improve model fit (since it is the same as the original 
# lmer model)


#### GLM with gamma distribution ####

# add a constant to log-transformed values to allow for gamma distribution
SVCpred_model_data$pos_log_difference <- SVCpred_model_data$log_difference + 10

# try glm with gamma distribution 
SVCpred_gamma <- glmer(pos_log_difference ~ habitat + octocoral_scaled + 
                         stony_scaled + relief_scaled + nocturnal + 
                         max_length_scaled + cryptic_behaviour + 
                         average_depth_scaled + size_bin_scaled +
                         colouration + shape + position + area_dif_scaled +  
                         (1|site) + (1|species_order), 
                       data = SVCpred_model_data,
                       family = Gamma(link = "log")) 
summary(SVCpred_gamma)
AICc(SVCpred_gamma) # 1281.326

# residuals plot
res_SVCpred_glm = residuals(SVCpred_gamma)
plot(res_SVCpred_glm) 

# qq plot
qqnorm(res_SVCpred_glm) 
qqline(res_SVCpred_glm)

# histogram
hist(res_SVCpred_glm)

# check model with performance package
check_model(SVCpred_gamma)

# CONCLUSION: does not improve model fit 


#### GLM with gamma distribution (w/o log transformation) ####

# try glm with gamma distribution on positive, raw density differences 
# (no log transformation)
SVCpred_gamma_raw <- glmer(pos_raw_difference ~ habitat + octocoral_scaled + 
                             stony_scaled + relief_scaled + nocturnal + 
                             max_length_scaled + cryptic_behaviour + 
                             average_depth_scaled + size_bin_scaled +
                             colouration + shape + position + area_dif_scaled +  
                         (1|site) + (1|species_order), 
                       data = SVCpred_model_data,
                       family = Gamma(link = "log")) 
summary(SVCpred_gamma_raw)
AICc(SVCpred_gamma_raw) # -3388.122

# residuals plot
res_SVCpred_glm_raw = residuals(SVCpred_gamma_raw)
plot(res_SVCpred_glm_raw) 

# qq plot
qqnorm(res_SVCpred_glm_raw) 
qqline(res_SVCpred_glm_raw)

# histogram
hist(res_SVCpred_glm_raw)

# check model with performance package
check_model(SVCpred_gamma_raw)

# CONCLUSION: does not improve model fit 


#### scaling & rounding response variable ####

# the following scales the log-transformed survey density difference 
# values to positive, whole numbers to allow for poisson and negative binomial 
# distributions

# make raw density difference variable
SVCpred_model_data$raw_difference <- 
  SVCpred_model_data$SVC_density - SVCpred_model_data$pred_density

# look at distribution of raw differences
hist(SVCpred_model_data$raw_difference)

# find minimum raw density difference
min(SVCpred_model_data$raw_difference) # -0.02339181

# add 0.02439181 to all raw density differences (scaling to positive)
SVCpred_model_data$pos_raw_difference <- 
  SVCpred_model_data$raw_difference + 0.02439181

# multiply raw values by 1000
SVCpred_model_data$pos_raw_dif_scaled <- 
  SVCpred_model_data$pos_raw_difference*1000

# create new column of rounded density differences
SVCpred_model_data <- SVCpred_model_data %>%
  mutate(whole_scaled_raw_difference = round(pos_raw_dif_scaled))

# look at rounded density difference distribution
hist(SVCpred_model_data$whole_scaled_raw_difference)


#### GLM with gaussian distribution (scaled response) ####

# try model structure with gaussian distribution
SVCpred_gaus <- glmer(pos_raw_difference ~ habitat + octocoral_scaled + 
                        stony_scaled + relief_scaled + nocturnal + 
                        max_length_scaled + cryptic_behaviour + 
                        average_depth_scaled + size_bin_scaled +
                        colouration + shape + position + area_dif_scaled + 
                        (1 | site) + (1 | species_order),
                      data = SVCpred_model_data)
summary(SVCpred_gaus) 
AICc(SVCpred_gaus) # AICc = -3102.906

# residuals plot
res_SVCpred_gaus = residuals(SVCpred_gaus)
plot(res_SVCpred_gaus) 

# qq plot
qqnorm(res_SVCpred_gaus) 
qqline(res_SVCpred_gaus)

# histogram
hist(res_SVCpred_gaus)

# check model with performance package
check_model(SVCpred_gaus)

# CONCLUSION: did not improve model fit 


#### GLM with poisson distribution (scaled response) ####

# model 
SVCpred_raw_poisson <- glm(whole_scaled_raw_difference ~ habitat + 
                             octocoral_scaled + 
                             stony_scaled + relief_scaled + nocturnal + 
                             max_length_scaled + cryptic_behaviour + 
                             average_depth_scaled + size_bin_scaled +
                             colouration + shape + position + area_dif_scaled, 
                             data = SVCpred_model_data,
                             family = poisson)
summary(SVCpred_raw_poisson)
AICc(SVCpred_raw_poisson) # AICc = 3059.529
vif(SVCpred_raw_poisson)

# residuals plot
res_SVCpred_raw_poisson = residuals(SVCpred_raw_poisson)
plot(res_SVCpred_raw_poisson) 

# qq plot
qqnorm(res_SVCpred_raw_poisson) 
qqline(res_SVCpred_raw_poisson)

# histogram
hist(res_SVCpred_raw_poisson)

# check model with performance package
check_model(SVCpred_raw_poisson)

# calculate deviance goodness of fit test
pchisq(SVCpred_raw_poisson$deviance, df = SVCpred_raw_poisson$df.residual, 
       lower.tail = FALSE)
# significant, indicates issue with fit 

# check for overdispersion
dispersiontest(SVCpred_raw_poisson, trafo = 1, alternative = "greater") 
# significant, indicates overdispersion

# CONCLUSION: diagnostic plots show some fit improvement but tests show
# continuing fit issues 


#### GLM with negative binomial distribution (scaled response) ####

# model
SVCpred_raw_nb <- glm.nb(whole_scaled_raw_difference ~ habitat + 
                           octocoral_scaled + 
                           stony_scaled + relief_scaled + nocturnal + 
                           max_length_scaled + cryptic_behaviour + 
                           average_depth_scaled + size_bin_scaled +
                           colouration + shape + position + area_dif_scaled, 
                           data = SVCpred_model_data)
summary(SVCpred_raw_nb)
AICc(SVCpred_raw_nb) # AICc = 2994.263
vif(SVCpred_raw_nb)

# residuals plot
res_SVCpred_raw_nb = residuals(SVCpred_raw_nb)
plot(res_SVCpred_raw_nb) 

# qq plot
qqnorm(res_SVCpred_raw_nb) 
qqline(res_SVCpred_raw_nb)

# histogram
hist(res_SVCpred_raw_nb)

# check model with performance package
check_model(SVCpred_raw_nb)

# compare poisson and nb models with likelihood ratio test
pchisq(2 * (logLik(SVCpred_raw_nb) - logLik(SVCpred_raw_poisson)), 
       df = 1, lower.tail = FALSE)
# significant, indicates better performance of nb model 

# CONCLUSION: some improvement of model fit with better performance than 
# Poisson model