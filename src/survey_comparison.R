# Set-Up =======================================================================

# packages
library(tidyverse)
library(nlme)
library(lme4)
library(here)

# data
SVCpred_model_data <- read_csv(here("./dataframes/SVCpred_dataframe.csv"))

# Data Edits ===================================================================

# octocoral
SVCpred_model_data$octocoral_c <- SVCpred_model_data$octocoral - mean(SVCpred_model_data$octocoral)

# stony coral
SVCpred_model_data$stony_c <- SVCpred_model_data$stony - mean(SVCpred_model_data$stony)

# vertical relief
SVCpred_model_data$relief_c <- SVCpred_model_data$relief_cm - mean(SVCpred_model_data$relief_cm)

# size bins
SVCpred_model_data$size_bin_c <- SVCpred_model_data$size_bin_lengths - mean(SVCpred_model_data$size_bin_lengths)


# SVC vs. Roving: Global Model =================================================

SVCpred_c <- lme(log_difference~octocoral_c+stony_c+relief_c+nocturnal+
                   cryptic_behaviour+size_bin_c+colouration+shape, 
                 random = list(~1|site, ~1|species_order), 
                 SVCpred_model_data) 
summary(SVCpred_c) # AIC = 1749.976
AICc(SVCpred_c) # AICc = 1750.611 (note that this did not change with centring)
vif(SVCpred_c) # all good 


# What We've Tried So Far ======================================================

predictdf <- SVCpred_model_data %>% tidyr::expand(size_bin_c,colouration,shape) 
predictdf$site = NA
predictdf$species_order = NA
predictdf$octocoral_c = 10
predictdf$stony_c = 10
predictdf$relief_c = 10
predictdf$nocturnal = c(rep("1", 10), rep("0", 26))
predictdf$cryptic_behaviour = c(rep("1", 10), rep("0", 26))
modpredict <- stats::predict(SVCpred_c, newdata = predictdf, re.form = ~0, type = "response", se.fit = TRUE)

predictdffull <- SVCpred_model_data %>% tidyr::expand(octocoral_c,stony_c,relief_c,nocturnal,
                                                      cryptic_behaviour,size_bin_c,colouration,shape) 
predictdffull$site = NA
predictdffull$species_order = NA
predictdffull <- predictdffull[c(1:100), ]
modpredictfull <- stats::predict(object = SVCpred_c, newdata = predictdffull)
summary(SVCpred_c)

SVCpred_mod <- lmer(log_difference~octocoral_c+shape+(1|site)+(1|species_order), 
                    SVCpred_model_data) 
summary(SVCpred_mod)
predictdfmod <- SVCpred_model_data %>% tidyr::expand(octocoral_c,shape) 
predictdfmod$site = as.character(NA)
predictdfmod$species_order = as.character(NA)
modpredict <- predict(SVCpred_mod, newdata = predictdfmod, re.from = ~0, se.fit = TRUE)

