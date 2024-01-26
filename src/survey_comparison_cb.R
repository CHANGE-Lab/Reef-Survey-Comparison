# Set-Up =======================================================================

# packages
library(readr)
library(tidyverse)
library(nlme)
library(lme4)
library(here)
library(merTools)
library(glmmTMB)
library(ggplot2)

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
AIC(SVCpred_c) # AICc = 1750.611 (note that this did not change with centring)
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

# cole's tinkering =============================================================

# creat unique id for each of the site / species order combos
svc_data <- SVCpred_model_data %>% 
  dplyr::group_by(site, species_order) %>% 
  dplyr::mutate(rand_id = cur_group_id())

# how many id's are there
dplyr::n_distinct(svc_data$rand_id)

# find the levels of the random effects that are undersampled
svc_reff_table <- svc_data %>% 
  dplyr::group_by(rand_id) %>% 
  dplyr::summarize(count = n()) %>% 
  # filter to the ones that have greater than 2
  dplyr::filter(count >= 3)

svc_reff_table_too_few <- svc_data %>% 
  dplyr::group_by(rand_id) %>% 
  dplyr::summarize(count = n()) %>% 
  # filter to the ones that have greater than 2
  dplyr::filter(count < 3)

# how many id's have too few obs
n_distinct(svc_reff_table_too_few$rand_id)

# now filter out the levels of those two variables
svc_filtered_data <- svc_data %>% 
  dplyr::filter(
    rand_id %in% svc_reff_table$rand_id
  )

## fit a model with few fixed effects ==========================================

# make sure random effects are factors
svc_filtered_data$site <- as.factor(svc_filtered_data$site)
svc_filtered_data$species_order <- as.factor(svc_filtered_data$species_order)

# make sure the categorical fixed effect is factor
svc_filtered_data$nocturnal <- as.factor(svc_filtered_data$nocturnal)

# fit with glmmTMB so we can get a standard error - predict.merMod doesn't do it
mod1 <- glmmTMB(
  log_difference ~ octocoral_c + nocturnal + (1|site) + (1|species_order),
  data = svc_filtered_data
)
summary(mod1)

## prep prediction data ========================================================

# get a representative sample from each of the variables
octocoral_c_range <- seq(round(min(svc_filtered_data$octocoral_c)), 
                         # note you don't have to round here I'm just doing it
                         # this way because it's easier
                         round(max(svc_filtered_data$octocoral_c)),
                         1)
nocturnal <- unique(svc_filtered_data$nocturnal)

# expand the two vectors into all possible combinations
predict_df <- expand.grid(octocoral_c = octocoral_c_range, 
                          # need to name them what they are in the model
                          nocturnal = as.factor(nocturnal),
                          site = NA,
                          species_order = NA
                          # site = unique(svc_filtered_data$site),
                          # species_order = 
                          #   unique(svc_filtered_data$species_order)
                          )

## do the prediciton ===========================================================
mod_prediction <- data.frame(
  # i want to bind it to the prediction df for easy plotting
  cbind(
    predict_df
  ,
  # this is doing the actual prediction
  stats::predict(
    object = mod1,
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

# now you can plot it across some gradient =====================================

# NOTE ######
# Careful here, because the response variable is still in a log, we can change
# that if we want, but we need to do it to the CI as well
# END NOTE ##

exp_mod_prediction <- mod_prediction %>% 
  dplyr::mutate(
    fit_exp = exp(fit),
    up_exp = exp(up_ci), 
    lo_exp = exp(lo_ci)
  )

# so we can plot it on the log scale 
ggplot(data = mod_prediction) + 
  geom_line(aes(x = octocoral_c, y = fit, colour = nocturnal)) + 
  geom_ribbon(aes(x = octocoral_c, ymin = lo_ci, ymax = up_ci,
                  fill = nocturnal),
              alpha = 0.2) + 
  theme_bw()

# or on the original scale 
ggplot(data = exp_mod_prediction) + 
  geom_line(aes(x = octocoral_c, y = fit_exp, colour = nocturnal)) + 
  geom_ribbon(aes(x = octocoral_c, ymin = lo_exp, ymax = up_exp,
                  fill = nocturnal),
              alpha = 0.2) + 
  theme_bw()

# NOTE ########
# So we see here there's clearly no difference between the noctural and non-
# noctural levels, but we do see the curve going up for both of the measures
# for the response variable. So if you wanted, you could make a point
# estimate for any two levels and plot that. Here's an example comparing an
# octocoral level -20 and 40: 
# END NOTE ####

point_est_df <- exp_mod_prediction %>% 
  dplyr::filter(octocoral_c %in% c(-20, 40))

ggplot(data = point_est_df) + 
  geom_errorbar(aes(x = octocoral_c, ymin = lo_exp, ymax = up_exp,
                    colour = nocturnal),
                width = 0,
                # dodge the position so you can see them
                position = position_dodge(width = 2)) + 
  geom_point(aes(x = octocoral_c, y = fit_exp, colour = nocturnal),
             # dodge the position so you can see them
             position = position_dodge(width = 2)) + 
  theme_bw() + 
  labs(y = "Difference (NOT logged)")

# NOTE #######
# So here we can see that the CI's overall quite a bit both within the 
# nocturnal groups, and also across the two octocoral measures - so while 
# there is a difference in the estimate, we can't say there's a significant
# difference 
# END NOTE ###
