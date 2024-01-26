# Set-Up =======================================================================

# packages
library(readr)
library(tidyverse)
library(nlme)
library(lme4)
library(here)
library(car)
library(merTools)
library(glmmTMB)
library(ggplot2)

# data
SVCpred_model_data <- read_csv(here("dataframes/SVCpred_dataframe.csv"))


# Data Edits ===================================================================

# octocoral
SVCpred_model_data$octocoral_c <- SVCpred_model_data$octocoral - mean(SVCpred_model_data$octocoral)

# stony coral
SVCpred_model_data$stony_c <- SVCpred_model_data$stony - mean(SVCpred_model_data$stony)

# vertical relief
SVCpred_model_data$relief_c <- SVCpred_model_data$relief_cm - mean(SVCpred_model_data$relief_cm)

# size bins
SVCpred_model_data$size_bin_c <- SVCpred_model_data$size_bin_lengths - mean(SVCpred_model_data$size_bin_lengths)

# shape ratio
SVCpred_model_data$shape_ratio_c <- SVCpred_model_data$shape_ratio - mean(SVCpred_model_data$shape_ratio)


# SVC vs. Roving: Global Model =================================================

SVCpred_c <- lme(log_difference~habitat+octocoral_c+stony_c+relief_c+nocturnal+
                     cryptic_behaviour+size_bin_c+colouration+shape_ratio_c, 
                   random = list(~1|site, ~1|species_order), 
                   SVCpred_model_data) 
summary(SVCpred_c) # AIC = 1786.637
AICc(SVCpred_c) # AICc = 1787.36
vif(SVCpred_c) # all good 


# SVC vs. Roving: Prediction Grid ==============================================

# create unique id for each of the site / species order combos
svc_data <- SVCpred_model_data %>% 
  dplyr::group_by(site, species_order) %>% 
  dplyr::mutate(rand_id = cur_group_id())

# how many id's are there
dplyr::n_distinct(svc_data$rand_id)


## fit a model with few fixed effects ##

# make sure random effects are factors
svc_data$site <- as.factor(svc_data$site)
svc_data$species_order <- as.factor(svc_data$species_order)

# make sure the categorical fixed effects are factors
svc_data$habitat <- as.factor(svc_data$habitat)
svc_data$nocturnal <- as.factor(svc_data$nocturnal)
svc_data$cryptic_behaviour <- as.factor(svc_data$cryptic_behaviour)
svc_data$colouration <- as.factor(svc_data$colouration)

# fit with glmmTMB so we can get a standard error - predict.merMod doesn't do it
SVC_roving_mod <- glmmTMB(log_difference ~ habitat+octocoral_c+stony_c+relief_c+nocturnal+
                            cryptic_behaviour+size_bin_c+colouration+shape_ratio_c + 
                            (1|site) + (1|species_order), data = svc_data)
summary(SVC_roving_mod)

SVC_roving_mod_sig <- glmmTMB(log_difference ~ cryptic_behaviour+colouration + 
                            (1|site) + (1|species_order), data = svc_data)
summary(SVC_roving_mod_sig)


## prep prediction data ##

# get a representative sample from each of the variables
habitat <- unique(svc_data$habitat)
octocoral_c_range <- seq(round(min(svc_data$octocoral_c)), 
                         # note you don't have to round here I'm just doing it
                         # this way because it's easier
                         round(max(svc_data$octocoral_c)),
                         1)
stony_c_range <- seq(round(min(svc_data$stony_c)), 
                         round(max(svc_data$stony_c)),
                         1)
relief_c_range <- seq(round(min(svc_data$relief_c)), 
                     round(max(svc_data$relief_c)),
                     1)
nocturnal <- unique(svc_data$nocturnal)
cryptic <- unique(svc_data$cryptic_behaviour)
size_bin_c_range <- seq(round(min(svc_data$size_bin_c)), 
                      round(max(svc_data$size_bin_c)),
                      1)
colouration <- unique(svc_data$colouration)
shape_ratio_c_range <- seq(round(min(svc_data$shape_ratio_c)), 
                           round(max(svc_data$shape_ratio_c)),
                           1)

# expand the vectors into all possible combinations
predict_d <- expand.grid(octocoral_c = octocoral_c_range, 
                          # need to name them what they are in the model
                          nocturnal = as.factor(nocturnal),
                          site = NA,
                          species_order = NA
                          # site = unique(svc_filtered_data$site),
                          # species_order = 
                          #   unique(svc_filtered_data$species_order)
)

# expand predictors present in the dredge into all possible combinations
predict_df <- expand.grid(cryptic_behaviour = as.factor(cryptic),
                         colouration = as.factor(colouration),
                         site = NA,
                         species_order = NA)

## do the prediciton ===========================================================
mod_prediction <- data.frame(
  # i want to bind it to the prediction df for easy plotting
  cbind(
    predict_df
    ,
    # this is doing the actual prediction
    stats::predict(
      object = SVC_roving_mod_sig,
      newdata = predict_df,
      re.from = ~0,
      se.fit = TRUE,
      type = "response"
    ))
) %>% 
  # CI is μ +/- 1.96*SE
  dplyr::mutate(
    up_ci = fit + 1.96*se.fit,
    lo_ci = fit - 1.96*se.fit
  )


# Heat Maps ====================================================================
heatmap(mod_prediction)











