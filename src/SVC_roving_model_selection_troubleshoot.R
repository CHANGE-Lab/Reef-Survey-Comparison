########## SVC VS. ROVING MODEL SELECTION ##########


# Set-Up =======================================================================

# packages
library(tidyverse)
library(nlme)
library(lme4)
library(lmerTest)
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
library(DHARMa)

# data
SVCpred_model_data <- read_csv(here("./dataframes/SVCpred_dataframe.csv"))


# SVC vs. Roving: Centring Variables ===========================================

# The following centres all continuous and dummy variables from the global 
# model by subtracting the sample mean from all variable values.

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


# SVC vs. Roving: Original Global Model ========================================

# The following creates the SVC vs. Roving survey global model utilizing the 
# best fit global model obtained prior to centering.

# global model
SVCpred_c <- lme(log_difference~habitat+octocoral_c+stony_c+relief_c+nocturnal+
                   max_length_c+cryptic_behaviour+average_depth_c+size_bin_c+
                   colouration+shape+position+area_dif_c, 
                 random = list(~1|site, ~1|species_order), 
                 SVCpred_model_data) 
summary(SVCpred_c) # AIC = 1417.306
AICc(SVCpred_c) # AICc = 1418.83
vif(SVCpred_c) 
# habitat, average_depth, colouration, shape, position >5

# remove habitat from SVCpred_c
SVCpred_c2 <- lme(log_difference~octocoral_c+stony_c+relief_c+nocturnal+
                    max_length_c+cryptic_behaviour+average_depth_c+size_bin_c+
                    colouration+shape+position+area_dif_c, 
                  random = list(~1|site, ~1|species_order), 
                  SVCpred_model_data) 
summary(SVCpred_c2) # AIC = 1414.457 
AICc(SVCpred_c2) # AICc = 1415.817
vif(SVCpred_c2) # colouration, shape, position > 5 

# remove colouration from SVCpred_c
SVCpred_c3 <- lme(log_difference~habitat+octocoral_c+stony_c+relief_c+nocturnal+
                    max_length_c+cryptic_behaviour+average_depth_c+size_bin_c+
                    shape+position+area_dif_c, 
                  random = list(~1|site, ~1|species_order), 
                  SVCpred_model_data) 
summary(SVCpred_c3) # AIC = 1422.377
AICc(SVCpred_c3) # AICc = 1423.583
vif(SVCpred_c3) 
# habitat, average_depth, shape > 5

# remove average depth from SVCpred_c
SVCpred_c4 <- lme(log_difference~habitat+octocoral_c+stony_c+relief_c+nocturnal+
                    max_length_c+cryptic_behaviour+size_bin_c+colouration+
                    shape+position+area_dif_c, 
                  random = list(~1|site, ~1|species_order), 
                  SVCpred_model_data) 
summary(SVCpred_c4) # AIC = 1407.806
AICc(SVCpred_c4) # AICc = 1409.166
vif(SVCpred_c4) # colouration, shape, position VIF > 5 

# remove shape from SVCpred_c
SVCpred_c5 <- lme(log_difference~habitat+octocoral_c+stony_c+relief_c+nocturnal+
                    max_length_c+cryptic_behaviour+average_depth_c+size_bin_c+
                    colouration+position+area_dif_c, 
                  random = list(~1|site, ~1|species_order), 
                  SVCpred_model_data) 
summary(SVCpred_c5) # AIC = 1419.323
AICc(SVCpred_c5) # AICc = 1420.683
vif(SVCpred_c5) 
# habitat, average_depth >5

# remove position from SVCpred_c
SVCpred_c6 <- lme(log_difference~habitat+octocoral_c+stony_c+relief_c+nocturnal+
                    max_length_c+cryptic_behaviour+average_depth_c+size_bin_c+
                    colouration+shape+area_dif_c, 
                  random = list(~1|site, ~1|species_order), 
                  SVCpred_model_data) 
summary(SVCpred_c6) # AIC = 1415.684
AICc(SVCpred_c6) # AICc = 1417.044
vif(SVCpred_c6) 
# habitat, average_depth >5

# remove shape from SVCpred_c2
SVCpred_c7 <- lme(log_difference~octocoral_c+stony_c+relief_c+nocturnal+
                    max_length_c+cryptic_behaviour+average_depth_c+size_bin_c+
                    colouration+position+area_dif_c, 
                  random = list(~1|site, ~1|species_order), 
                  SVCpred_model_data) 
summary(SVCpred_c7) # AIC = 1416.43
AICc(SVCpred_c7) # AICc = 1417.637
vif(SVCpred_c7) # all good

# remove position from SVCpred_c2
SVCpred_c8 <- lme(log_difference~octocoral_c+stony_c+relief_c+nocturnal+
                    max_length_c+cryptic_behaviour+average_depth_c+size_bin_c+
                    colouration+shape+area_dif_c, 
                  random = list(~1|site, ~1|species_order), 
                  SVCpred_model_data) 
summary(SVCpred_c8) # AIC = 1412.787 
AICc(SVCpred_c8) # AICc = 1413.993
vif(SVCpred_c8) # all good

# remove shape from SVCpred_c4
SVCpred_c9 <- lme(log_difference~habitat+octocoral_c+stony_c+relief_c+nocturnal+
                    max_length_c+cryptic_behaviour+size_bin_c+colouration+
                    position+area_dif_c, 
                  random = list(~1|site, ~1|species_order), 
                  SVCpred_model_data) 
summary(SVCpred_c9) # AIC = 1409.528
AICc(SVCpred_c9) # AICc = 1410.734
vif(SVCpred_c9) # all good

# remove position from SVCpred_c4
SVCpred_c10 <- lme(log_difference~habitat+octocoral_c+stony_c+relief_c+nocturnal+
                     max_length_c+cryptic_behaviour+size_bin_c+colouration+
                     shape+area_dif_c, 
                   random = list(~1|site, ~1|species_order), 
                   SVCpred_model_data) 
summary(SVCpred_c10) # AIC = 1406.033
AICc(SVCpred_c10) # AICc = 1407.239
vif(SVCpred_c10) # all good 
# *BEST MODEL FIT*

# residuals plot
res_SVCpred_global_c = residuals(SVCpred_c10)
plot(res_SVCpred_global_c) 

# qq plot
qqnorm(res_SVCpred_global_c) 
qqline(res_SVCpred_global_c)
hist(res_SVCpred_global_c)

# diagnostic plots with performance package
check_model(SVCpred_c10)


# SVC vs. Roving: Original Dredge ==============================================

# dredge on c26
SVCpred_dredge_c <- dredge(SVCpred_c10)
SVCpred_dredge_c 

# subset dredge
SVCpred_dredge_c_sub <- subset(SVCpred_dredge_c, delta < 4) 

# model average 
SVCpred_model_average_c <- model.avg(SVCpred_dredge_c_sub)
SVCpred_model_avg_summary_c <- summary(SVCpred_model_average_c)

# covariate confidence intervals
SVCpred_confidence_c <- confint(SVCpred_model_average_c)
summary(SVCpred_confidence_c)


# SVC vs. Roving: Poisson Global Model (log) ===================================

# The following creates the SVC vs. Roving survey global model using a GLM
# with a Poisson distribution. 

# find minimum log density difference
min(SVCpred_model_data$log_difference) # -3.1942475

# add 3.1952475 to all log density differences (scaling to positive)
SVCpred_model_data$pos_log_difference <- 
  SVCpred_model_data$log_difference + 3.1952475

# create new column of rounded log density differences
SVCpred_model_data <- SVCpred_model_data %>%
  mutate(whole_log_difference = round(pos_log_difference))

# look at rounded density difference distribution
hist(SVCpred_model_data$whole_log_difference)

# re-scale variables 
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

# global model
SVCpred_p <- glmer(whole_log_difference~habitat+octocoral_scaled+stony_scaled+
                     relief_scaled+nocturnal+max_length_scaled+
                     cryptic_behaviour+average_depth_scaled + size_bin_scaled+
                     colouration+shape+position+area_dif_scaled + 
                     (1 | site) + (1 | species_order), 
                   data = SVCpred_model_data,
                   family = poisson) 
summary(SVCpred_p) # AIC = 1553.8
AICc(SVCpred_p) # AICc = 1555.153
vif(SVCpred_p) 
# habitat, max length, average_depth, colouration, shape, position >5

# remove habitat from SVCpred_p
SVCpred_p2 <- glmer(whole_log_difference~octocoral_scaled+stony_scaled+
                      relief_scaled+nocturnal+max_length_scaled+
                      cryptic_behaviour+average_depth_scaled+size_bin_scaled+
                      colouration+shape+position+area_dif_scaled + 
                      (1 | site) + (1 | species_order), 
                    data = SVCpred_model_data,
                    family = poisson) 
summary(SVCpred_p2) # AIC = 1553.8 
AICc(SVCpred_p2) # AICc = 1551.8
vif(SVCpred_p2) # max length, colouration, shape, position > 5 

# remove colouration from SVCpred_p
SVCpred_p3 <- glmer(whole_log_difference~habitat+octocoral_scaled+stony_scaled+
                      relief_scaled+nocturnal+max_length_scaled+
                      cryptic_behaviour+average_depth_scaled+size_bin_scaled+
                      shape+position+area_dif_scaled + 
                      (1 | site) + (1 | species_order), 
                    data = SVCpred_model_data,
                    family = poisson) 
summary(SVCpred_p3) # AIC = 1552.4
AICc(SVCpred_p3) # AICc = 1553.485
vif(SVCpred_p3) 
# habitat, average_depth, shape > 5

# remove average depth from SVCpred_p
SVCpred_p4 <- glmer(whole_log_difference~habitat+octocoral_scaled+stony_scaled+
                      relief_scaled+nocturnal+max_length_scaled+
                      cryptic_behaviour+size_bin_scaled+colouration+
                      shape+position+area_dif_scaled + 
                      (1 | site) + (1 | species_order), 
                    data = SVCpred_model_data,
                    family = poisson) 
summary(SVCpred_p4) # AIC = 1552.1
AICc(SVCpred_p4) # AICc = 1553.277
vif(SVCpred_p4) # max length, colouration, shape, position VIF > 5 

# remove shape from SVCpred_p
SVCpred_p5 <- glmer(whole_log_difference~habitat+octocoral_scaled+stony_scaled+
                      relief_scaled+nocturnal+max_length_scaled+
                      cryptic_behaviour+average_depth_scaled+size_bin_scaled+
                      colouration+position+area_dif_scaled + 
                      (1 | site) + (1 | species_order), 
                    data = SVCpred_model_data,
                    family = poisson) 
summary(SVCpred_p5) # AIC = 1553.5
AICc(SVCpred_p5) # AICc = 1554.671
vif(SVCpred_p5) 
# habitat, average_depth, colouration >5

# remove position from SVCpred_p
SVCpred_p6 <- glmer(whole_log_difference~habitat+octocoral_scaled+stony_scaled+
                      relief_scaled+nocturnal+max_length_scaled+
                      cryptic_behaviour+average_depth_scaled+size_bin_scaled+
                      colouration+shape+area_dif_scaled + 
                      (1 | site) + (1 | species_order), 
                    data = SVCpred_model_data,
                    family = poisson) 
summary(SVCpred_p6) # AIC = 1552.3
AICc(SVCpred_p6) # AICc = 1553.52
vif(SVCpred_p6) 
# habitat, average_depth >5

# remove max length from SVCpred_p
SVCpred_p7 <- glmer(whole_log_difference~habitat+octocoral_scaled+stony_scaled+
                      relief_scaled+nocturnal+
                      cryptic_behaviour+average_depth_scaled+size_bin_scaled+
                      colouration+shape+position+area_dif_scaled + 
                      (1 | site) + (1 | species_order), 
                    data = SVCpred_model_data,
                    family = poisson) 
summary(SVCpred_p7) # AIC = 1553.3
AICc(SVCpred_p7) # AICc = 1554.516
vif(SVCpred_p7) 
# habitat, average_depth, colouration, position >5

# remove max length from SVCpred_p2
SVCpred_p8 <- glmer(whole_log_difference~octocoral_scaled+stony_scaled+
                      relief_scaled+nocturnal+
                      cryptic_behaviour+average_depth_scaled+size_bin_scaled+
                      colouration+shape+position+area_dif_scaled + 
                      (1 | site) + (1 | species_order), 
                    data = SVCpred_model_data,
                    family = poisson) 
summary(SVCpred_p8) # AIC = 1551.3
AICc(SVCpred_p8) # AICc = 1552.374
vif(SVCpred_p8) # max length, colouration, shape, position > 5 

# remove colouration from SVCpred_p2
SVCpred_p9 <- glmer(whole_log_difference~octocoral_scaled+stony_scaled+
                      relief_scaled+nocturnal+max_length_scaled+
                      cryptic_behaviour+average_depth_scaled+size_bin_scaled+
                      shape+position+area_dif_scaled + 
                      (1 | site) + (1 | species_order), 
                    data = SVCpred_model_data,
                    family = poisson) 
summary(SVCpred_p9) # AIC = 1550.4 
AICc(SVCpred_p9) # AICc = 1551.366
vif(SVCpred_p9) # max length, colouration, shape, position > 5 

# remove shape from SVCpred_p2
SVCpred_p10 <- glmer(whole_log_difference~octocoral_scaled+stony_scaled+
                       relief_scaled+nocturnal+max_length_scaled+
                       cryptic_behaviour+average_depth_scaled+size_bin_scaled+
                       colouration+position+area_dif_scaled + 
                       (1 | site) + (1 | species_order), 
                     data = SVCpred_model_data,
                     family = poisson) 
summary(SVCpred_p10) # AIC = 1551.5
AICc(SVCpred_p10) # AICc = 1552.543
vif(SVCpred_p10) # max length, colouration, shape, position > 5 

# remove position from SVCpred_p2
SVCpred_p11 <- glmer(whole_log_difference~octocoral_scaled+stony_scaled+
                       relief_scaled+nocturnal+max_length_scaled+
                       cryptic_behaviour+average_depth_scaled+size_bin_scaled+
                       colouration+shape+area_dif_scaled + 
                       (1 | site) + (1 | species_order), 
                     data = SVCpred_model_data,
                     family = poisson) 
summary(SVCpred_p11) # AIC = 1550.3
AICc(SVCpred_p11) # AICc = 1551.391
vif(SVCpred_p11) # max length, colouration, shape, position > 5 

# remove habitat from SVCpred_p7
SVCpred_p12 <- glmer(whole_log_difference~octocoral_scaled+stony_scaled+
                       relief_scaled+nocturnal+
                       cryptic_behaviour+average_depth_scaled+size_bin_scaled+
                       colouration+shape+position+area_dif_scaled + 
                       (1 | site) + (1 | species_order), 
                     data = SVCpred_model_data,
                     family = poisson) 
summary(SVCpred_p12) # AIC = 1551.3
AICc(SVCpred_p12) # AICc = 1552.374
vif(SVCpred_p12) 
# habitat, average_depth, colouration, position >5

# remove depth from SVCpred_p7
SVCpred_p13 <- glmer(whole_log_difference~habitat+octocoral_scaled+stony_scaled+
                       relief_scaled+nocturnal+
                       cryptic_behaviour+size_bin_scaled+
                       colouration+shape+position+area_dif_scaled + 
                       (1 | site) + (1 | species_order), 
                     data = SVCpred_model_data,
                     family = poisson) 
summary(SVCpred_p13) # AIC = 1551.5
AICc(SVCpred_p13) # AICc = 1552.535
vif(SVCpred_p13) 
# habitat, average_depth, colouration, position >5

# remove position from SVCpred_p7
SVCpred_p14 <- glmer(whole_log_difference~habitat+octocoral_scaled+stony_scaled+
                       relief_scaled+nocturnal+
                       cryptic_behaviour+average_depth_scaled+size_bin_scaled+
                       colouration+shape+area_dif_scaled + 
                       (1 | site) + (1 | species_order), 
                     data = SVCpred_model_data,
                     family = poisson) 
summary(SVCpred_p14) # AIC = 1551.4
AICc(SVCpred_p14) # AICc = 1552.506
vif(SVCpred_p14) 
# habitat, average_depth, colouration, position >5

# remove colouration from SVCpred_p7
SVCpred_p15 <- glmer(whole_log_difference~habitat+octocoral_scaled+stony_scaled+
                       relief_scaled+nocturnal+
                       cryptic_behaviour+average_depth_scaled+size_bin_scaled+
                       shape+position+area_dif_scaled + 
                       (1 | site) + (1 | species_order), 
                     data = SVCpred_model_data,
                     family = poisson) 
summary(SVCpred_p15) # AIC = 1551.1
AICc(SVCpred_p15) # AICc = 1551.999
vif(SVCpred_p15) 
# habitat, average_depth, colouration, position >5

# remove position from SVCpred_p4
SVCpred_p16 <- glmer(whole_log_difference~habitat+octocoral_scaled+stony_scaled+
                       relief_scaled+nocturnal+max_length_scaled+
                       cryptic_behaviour+size_bin_scaled+colouration+
                       shape+area_dif_scaled + 
                       (1 | site) + (1 | species_order), 
                     data = SVCpred_model_data,
                     family = poisson) 
summary(SVCpred_p16) # AIC = 1550.5
AICc(SVCpred_p16) # AICc = 1551.593
vif(SVCpred_p16) # all good

# remove max length from SVCpred_p4
SVCpred_p17 <- glmer(whole_log_difference~habitat+octocoral_scaled+stony_scaled+
                       relief_scaled+nocturnal+
                       cryptic_behaviour+size_bin_scaled+colouration+
                       shape+position+area_dif_scaled + 
                       (1 | site) + (1 | species_order), 
                     data = SVCpred_model_data,
                     family = poisson) 
summary(SVCpred_p17) # AIC = 1551.5
AICc(SVCpred_p17) # AICc = 1552.535
vif(SVCpred_p17) # colouration, position VIF > 5 

# remove colouration from SVCpred_p4
SVCpred_p18 <- glmer(whole_log_difference~habitat+octocoral_scaled+stony_scaled+
                       relief_scaled+nocturnal+max_length_scaled+
                       cryptic_behaviour+size_bin_scaled+
                       shape+position+area_dif_scaled + 
                       (1 | site) + (1 | species_order), 
                     data = SVCpred_model_data,
                     family = poisson) 
summary(SVCpred_p18) # AIC = 1550.8
AICc(SVCpred_p18) # AICc = 1551.679
vif(SVCpred_p18) # shape VIF > 5 

# remove shape from SVCpred_p4
SVCpred_p19 <- glmer(whole_log_difference~habitat+octocoral_scaled+stony_scaled+
                       relief_scaled+nocturnal+max_length_scaled+
                       cryptic_behaviour+size_bin_scaled+colouration+
                       position+area_dif_scaled + 
                       (1 | site) + (1 | species_order), 
                     data = SVCpred_model_data,
                     family = poisson) 
summary(SVCpred_p19) # AIC = 1551.6
AICc(SVCpred_p19) # AICc = 1552.708
vif(SVCpred_p19) # colouration VIF > 5 

# remove habitat from SVCpred_p3
SVCpred_p20 <- glmer(whole_log_difference~octocoral_scaled+stony_scaled+
                       relief_scaled+nocturnal+max_length_scaled+
                       cryptic_behaviour+average_depth_scaled+size_bin_scaled+
                       shape+position+area_dif_scaled + 
                       (1 | site) + (1 | species_order), 
                     data = SVCpred_model_data,
                     family = poisson) 
summary(SVCpred_p20) # AIC = 1550.4
AICc(SVCpred_p20) # AICc = 1551.366
vif(SVCpred_p20) 
# shape > 5

# remove average depth from SVCpred_p3
SVCpred_p21 <- glmer(whole_log_difference~habitat+octocoral_scaled+stony_scaled+
                       relief_scaled+nocturnal+max_length_scaled+
                       cryptic_behaviour+size_bin_scaled+
                       shape+position+area_dif_scaled + 
                       (1 | site) + (1 | species_order), 
                     data = SVCpred_model_data,
                     family = poisson) 
summary(SVCpred_p21) # AIC = 1550.8
AICc(SVCpred_p21) # AICc = 1551.679
vif(SVCpred_p21) 
# shape > 5

# remove shape from SVCpred_p3
SVCpred_p22 <- glmer(whole_log_difference~habitat+octocoral_scaled+stony_scaled+
                       relief_scaled+nocturnal+max_length_scaled+
                       cryptic_behaviour+average_depth_scaled+size_bin_scaled+
                       position+area_dif_scaled + 
                       (1 | site) + (1 | species_order), 
                     data = SVCpred_model_data,
                     family = poisson) 
summary(SVCpred_p22) # AIC = 1551.3
AICc(SVCpred_p22) # AICc = 1552.226
vif(SVCpred_p22) 
# habitat, average_depth > 5

# remove habitat from SVCpred_p5
SVCpred_p23 <- glmer(whole_log_difference~octocoral_scaled+stony_scaled+
                       relief_scaled+nocturnal+max_length_scaled+
                       cryptic_behaviour+average_depth_scaled+size_bin_scaled+
                       colouration+position+area_dif_scaled + 
                       (1 | site) + (1 | species_order), 
                     data = SVCpred_model_data,
                     family = poisson) 
summary(SVCpred_p23) # AIC = 1551.5
AICc(SVCpred_p23) # AICc = 1552.543
vif(SVCpred_p23) 
# colouration >5

# remove average depth from SVCpred_p5
SVCpred_p24 <- glmer(whole_log_difference~habitat+octocoral_scaled+stony_scaled+
                       relief_scaled+nocturnal+max_length_scaled+
                       cryptic_behaviour+size_bin_scaled+
                       colouration+position+area_dif_scaled + 
                       (1 | site) + (1 | species_order), 
                     data = SVCpred_model_data,
                     family = poisson) 
summary(SVCpred_p24) # AIC = 1551.6
AICc(SVCpred_p24) # AICc = 1552.708
vif(SVCpred_p24) 
# colouration >5

# remove colouration from SVCpred_p5
SVCpred_p25 <- glmer(whole_log_difference~habitat+octocoral_scaled+stony_scaled+
                       relief_scaled+nocturnal+max_length_scaled+
                       cryptic_behaviour+average_depth_scaled+size_bin_scaled+
                       position+area_dif_scaled + 
                       (1 | site) + (1 | species_order), 
                     data = SVCpred_model_data,
                     family = poisson) 
summary(SVCpred_p25) # AIC = 1551.3
AICc(SVCpred_p25) # AICc = 1552.226
vif(SVCpred_p25) 
# habitat, average_depth >5

# remove habitat from SVCpred_p6
SVCpred_p26 <- glmer(whole_log_difference~octocoral_scaled+stony_scaled+
                       relief_scaled+nocturnal+max_length_scaled+
                       cryptic_behaviour+average_depth_scaled+size_bin_scaled+
                       colouration+shape+area_dif_scaled + 
                       (1 | site) + (1 | species_order), 
                     data = SVCpred_model_data,
                     family = poisson,
                     na.action = na.fail) 
summary(SVCpred_p26) # AIC = 1550.3
AICc(SVCpred_p26) # AICc = 1551.391
vif(SVCpred_p26) 
# all good 
# THIS IS ONE TOP MODEL (HABITAT & POSITION REMOVED)

# residuals plot
res_SVCpred_p26 = residuals(SVCpred_p26)
plot(res_SVCpred_p26) 

# qq plot
qqnorm(res_SVCpred_p26) 
qqline(res_SVCpred_p26)

# histogram
hist(res_SVCpred_p26)

# remove average depth from SVCpred_p6
SVCpred_p27 <- glmer(whole_log_difference~habitat+octocoral_scaled+stony_scaled+
                       relief_scaled+nocturnal+max_length_scaled+
                       cryptic_behaviour+size_bin_scaled+
                       colouration+shape+area_dif_scaled + 
                       (1 | site) + (1 | species_order), 
                     data = SVCpred_model_data,
                     family = poisson,
                     na.action = na.fail) 
summary(SVCpred_p27) # AIC = 1550.5
AICc(SVCpred_p27) # AICc = 1551.593
vif(SVCpred_p27) 
# all good 
# THIS IS THE SECOND TOP MODEL (DEPTH & POSITION REMOVED)

# residuals plot
res_SVCpred_p27 = residuals(SVCpred_p27)
plot(res_SVCpred_p27) 

# qq plot
qqnorm(res_SVCpred_p27) 
qqline(res_SVCpred_p27)

# histogram
hist(res_SVCpred_p27)


# SVC vs. Roving: Poisson Dredge ===============================================

# The following performs dredges on the two top SVC vs. Roving models with 
# a Poisson distribution. 

options(max.print = 10000)

# dredge on c26
SVCpred_dredge_p1 <- dredge(SVCpred_p26)
SVCpred_dredge_p1 
sum(get.models(SVCpred_dredge_p1, subset = delta < 4) %>% lengths() > 0)

# subset dredge
SVCpred_dredge_p1_sub <- subset(SVCpred_dredge_p1, delta < 4) 

# model average 
SVCpred_model_average_p1 <- model.avg(SVCpred_dredge_p1_sub)
SVCpred_model_avg_summary_p1 <- summary(SVCpred_model_average_p1)

# covariate confidence intervals
SVCpred_ponfidence_p1 <- confint(SVCpred_model_average_p1)
summary(SVCpred_ponfidence_p1)

# dredge on c27
SVCpred_dredge_p2 <- dredge(SVCpred_p27)
SVCpred_dredge_p2 
sum(get.models(SVCpred_dredge_p2, subset = delta < 4) %>% lengths() > 0)

# subset dredge
SVCpred_dredge_p2_sub <- subset(SVCpred_dredge_p2, delta < 4) 

# model average 
SVCpred_model_average_p2 <- model.avg(SVCpred_dredge_p2_sub)
SVCpred_model_avg_summary_p2 <- summary(SVCpred_model_average_p2)

# covariate confidence intervals
SVCpred_ponfidence_p2 <- confint(SVCpred_model_average_p2)
summary(SVCpred_ponfidence_p2)


# SVC vs. Roving: Negative Binomial Global Model ===============================

# make raw density difference variable
SVCpred_model_data$raw_difference <- 
  SVCpred_model_data$SVC_density - SVCpred_model_data$pred_density

# look at distribution of raw differences
hist(SVCpred_model_data$raw_difference)

# find minimum log density difference
min(SVCpred_model_data$raw_difference) # -0.02339181

# add 0.02439181 to all log density differences (scaling to positive)
SVCpred_model_data$pos_raw_difference <- 
  SVCpred_model_data$raw_difference + 0.02439181

# multiply raw values by 1000
SVCpred_model_data$pos_raw_dif_scaled <- SVCpred_model_data$pos_raw_difference*1000

# create new column of rounded density differences
SVCpred_model_data <- SVCpred_model_data %>%
  mutate(whole_scaled_raw_difference = round(pos_raw_dif_scaled))

# look at rounded density difference distribution
hist(SVCpred_model_data$whole_scaled_raw_difference)

# global model with negative binomial distribution
SVCpred_raw_nb_ran <- glmer.nb(whole_scaled_raw_difference ~ habitat+octocoral_scaled+stony_scaled+
                             relief_scaled+nocturnal+max_length_scaled+
                             cryptic_behaviour+average_depth_scaled + size_bin_scaled+
                             colouration+shape+position+area_dif_scaled+
                               (1 | site) + (1 | species_order),
                           data = SVCpred_model_data)
# model fit is singular
summary(SVCpred_raw_nb_ran)
AICc(SVCpred_raw_nb_ran) # AICc = 2998.58
vif(SVCpred_raw_nb_ran)

# global model with negative binomial distribution & scaled predictors
SVCpred_raw_nb_ran <- glm.nb(whole_scaled_raw_difference ~ habitat+scale(octocoral)+scale(stony)+
                                 scale(relief_cm)+nocturnal+scale(max_length)+
                                 cryptic_behaviour+scale(average_depth) + scale(size_bin_lengths)+
                                 colouration+shape+position+scale(SVCpred_area_dif),
                               data = SVCpred_model_data)
summary(SVCpred_raw_nb_ran)
AICc(SVCpred_raw_nb_ran) # AICc = 2998.58
vif(SVCpred_raw_nb_ran)

# global model with random effects removed 
SVCpred_raw_nb <- glm.nb(whole_scaled_raw_difference ~ habitat+octocoral_scaled+stony_scaled+
                             relief_scaled+nocturnal+max_length_scaled+
                             cryptic_behaviour+average_depth_scaled + size_bin_scaled+
                             colouration+shape+position+area_dif_scaled, 
                           data = SVCpred_model_data)
summary(SVCpred_raw_nb)
AICc(SVCpred_raw_nb) # AICc = 2994.263
vif(SVCpred_raw_nb)

# remove habitat from global
SVCpred_raw_nb_1 <- glm.nb(whole_scaled_raw_difference ~ octocoral_scaled+stony_scaled+
                           relief_scaled+nocturnal+max_length_scaled+
                           cryptic_behaviour+average_depth_scaled + size_bin_scaled+
                           colouration+shape+position+area_dif_scaled, 
                         data = SVCpred_model_data)
summary(SVCpred_raw_nb_1)
AICc(SVCpred_raw_nb_1) # AICc = 2992.873
vif(SVCpred_raw_nb_1)

# remove max length from global 
SVCpred_raw_nb_2 <- glm.nb(whole_scaled_raw_difference ~ habitat+octocoral_scaled+stony_scaled+
                           relief_scaled+nocturnal+
                           cryptic_behaviour+average_depth_scaled + size_bin_scaled+
                           colouration+shape+position+area_dif_scaled, 
                         data = SVCpred_model_data)
summary(SVCpred_raw_nb_2)
AICc(SVCpred_raw_nb_2) # AICc = 2993.511
vif(SVCpred_raw_nb_2)

# remove depth from global
SVCpred_raw_nb_3 <- glm.nb(whole_scaled_raw_difference ~ habitat+octocoral_scaled+stony_scaled+
                             relief_scaled+nocturnal+max_length_scaled+
                             cryptic_behaviour+ size_bin_scaled+
                             colouration+shape+position+area_dif_scaled, 
                           data = SVCpred_model_data)
summary(SVCpred_raw_nb_3)
AICc(SVCpred_raw_nb_3) # AICc = 2992.206
vif(SVCpred_raw_nb_3)

# remove colour from global
SVCpred_raw_nb_4 <- glm.nb(whole_scaled_raw_difference ~ habitat+octocoral_scaled+stony_scaled+
                           relief_scaled+nocturnal+max_length_scaled+
                           cryptic_behaviour+average_depth_scaled + size_bin_scaled+
                           shape+position+area_dif_scaled, 
                         data = SVCpred_model_data)
summary(SVCpred_raw_nb_4)
AICc(SVCpred_raw_nb_4) # AICc = 2991.313
vif(SVCpred_raw_nb_4)

# remove shape from global
SVCpred_raw_nb_5 <- glm.nb(whole_scaled_raw_difference ~ habitat+octocoral_scaled+stony_scaled+
                           relief_scaled+nocturnal+max_length_scaled+
                           cryptic_behaviour+average_depth_scaled + size_bin_scaled+
                           colouration+position+area_dif_scaled, 
                         data = SVCpred_model_data)
summary(SVCpred_raw_nb_5)
AICc(SVCpred_raw_nb_5) # AICc = 2996.495
vif(SVCpred_raw_nb_5)

# remove position from global
SVCpred_raw_nb_6 <- glm.nb(whole_scaled_raw_difference ~ habitat+octocoral_scaled+stony_scaled+
                           relief_scaled+nocturnal+max_length_scaled+
                           cryptic_behaviour+average_depth_scaled + size_bin_scaled+
                           colouration+shape+area_dif_scaled, 
                         data = SVCpred_model_data)
summary(SVCpred_raw_nb_6)
AICc(SVCpred_raw_nb_6) # AICc = 2993.336
vif(SVCpred_raw_nb_6)

# remove max length from 1
SVCpred_raw_nb_7 <- glm.nb(whole_scaled_raw_difference ~ octocoral_scaled+stony_scaled+
                             relief_scaled+nocturnal+
                             cryptic_behaviour+average_depth_scaled + size_bin_scaled+
                             colouration+shape+position+area_dif_scaled, 
                           data = SVCpred_model_data)
summary(SVCpred_raw_nb_7)
AICc(SVCpred_raw_nb_7) # AICc = 2992.143
vif(SVCpred_raw_nb_7)

# remove colour from 1
SVCpred_raw_nb_8 <- glm.nb(whole_scaled_raw_difference ~ octocoral_scaled+stony_scaled+
                             relief_scaled+nocturnal+max_length_scaled+
                             cryptic_behaviour+average_depth_scaled + size_bin_scaled+
                             shape+position+area_dif_scaled, 
                           data = SVCpred_model_data)
summary(SVCpred_raw_nb_8)
AICc(SVCpred_raw_nb_8) # AICc = 2989.855
vif(SVCpred_raw_nb_8)

# remove shape from 1
SVCpred_raw_nb_9 <- glm.nb(whole_scaled_raw_difference ~ octocoral_scaled+stony_scaled+
                             relief_scaled+nocturnal+max_length_scaled+
                             cryptic_behaviour+average_depth_scaled + size_bin_scaled+
                             colouration+position+area_dif_scaled, 
                           data = SVCpred_model_data)
summary(SVCpred_raw_nb_9)
AICc(SVCpred_raw_nb_9) # AICc = 2994.928
vif(SVCpred_raw_nb_9)

# remove position from 1
SVCpred_raw_nb_10 <- glm.nb(whole_scaled_raw_difference ~ octocoral_scaled+stony_scaled+
                             relief_scaled+nocturnal+max_length_scaled+
                             cryptic_behaviour+average_depth_scaled + size_bin_scaled+
                             colouration+shape+area_dif_scaled, 
                           data = SVCpred_model_data,
                           na.action = na.fail)
summary(SVCpred_raw_nb_10)
AICc(SVCpred_raw_nb_10) # AICc = 2991.792
vif(SVCpred_raw_nb_10)
# ALL GOOD 

# remove depth from 2
SVCpred_raw_nb_12 <- glm.nb(whole_scaled_raw_difference ~ habitat+octocoral_scaled+stony_scaled+
                             relief_scaled+nocturnal+
                             cryptic_behaviour+ size_bin_scaled+
                             colouration+shape+position+area_dif_scaled, 
                           data = SVCpred_model_data)
summary(SVCpred_raw_nb_12)
AICc(SVCpred_raw_nb_12) # AICc = 2991.409
vif(SVCpred_raw_nb_12)

# remove position from 2
SVCpred_raw_nb_13 <- glm.nb(whole_scaled_raw_difference ~ habitat+octocoral_scaled+stony_scaled+
                             relief_scaled+nocturnal+
                             cryptic_behaviour+average_depth_scaled + size_bin_scaled+
                             colouration+shape+area_dif_scaled, 
                           data = SVCpred_model_data)
summary(SVCpred_raw_nb_13)
AICc(SVCpred_raw_nb_13) # AICc = 2992
vif(SVCpred_raw_nb_13)

# remove colour from 3
SVCpred_raw_nb_15 <- glm.nb(whole_scaled_raw_difference ~ habitat+octocoral_scaled+stony_scaled+
                             relief_scaled+nocturnal+max_length_scaled+
                             cryptic_behaviour+ size_bin_scaled+
                             shape+position+area_dif_scaled, 
                           data = SVCpred_model_data)
summary(SVCpred_raw_nb_15)
AICc(SVCpred_raw_nb_15) # AICc = 2989.282
vif(SVCpred_raw_nb_15)

# remove shape from 3
SVCpred_raw_nb_16 <- glm.nb(whole_scaled_raw_difference ~ habitat+octocoral_scaled+stony_scaled+
                             relief_scaled+nocturnal+max_length_scaled+
                             cryptic_behaviour+ size_bin_scaled+
                             colouration+position+area_dif_scaled, 
                           data = SVCpred_model_data,
                           na.action = na.fail)
summary(SVCpred_raw_nb_16)
AICc(SVCpred_raw_nb_16) # AICc = 2994.38
vif(SVCpred_raw_nb_16)
# ALL GOOD

# remove position from 3
SVCpred_raw_nb_17 <- glm.nb(whole_scaled_raw_difference ~ habitat+octocoral_scaled+stony_scaled+
                             relief_scaled+nocturnal+max_length_scaled+
                             cryptic_behaviour+ size_bin_scaled+
                             colouration+shape+area_dif_scaled, 
                           data = SVCpred_model_data,
                           na.action = na.fail)
summary(SVCpred_raw_nb_17)
AICc(SVCpred_raw_nb_17) # AICc = 2991.242
vif(SVCpred_raw_nb_17)
# ALL GOOD

# remove shape from 4
SVCpred_raw_nb_20 <- glm.nb(whole_scaled_raw_difference ~ habitat+octocoral_scaled+stony_scaled+
                             relief_scaled+nocturnal+max_length_scaled+
                             cryptic_behaviour+average_depth_scaled + size_bin_scaled+
                             position+area_dif_scaled, 
                           data = SVCpred_model_data)
summary(SVCpred_raw_nb_20)
AICc(SVCpred_raw_nb_20) # AICc = 2992.676
vif(SVCpred_raw_nb_20)

# MODELS WITH MOST PREDICTORS AND VIFS <3:
# 10 -> habitat & position removed (AICc = 2991.792; stony sig, shape marginal)
# 16 -> depth & shape removed (AICc = 2994.38; crypsis sig, stony marginal)
# 17 -> depth & position removed (AICc = 2991.38; stony & shape marginal)
# Global -> shape sig, stony marginal 
# Based on significance in global model and hypotheses, think we should use 10


# SVC vs. Roving: Negative Binomial Dredge =====================================

options(max.print = 10000)

# dredge on nb 10 
SVCpred_dredge_nb_10 <- dredge(SVCpred_raw_nb_10)
SVCpred_dredge_nb_10 
sum(get.models(SVCpred_dredge_nb_10, subset = delta < 4) %>% lengths() > 0) # 46

# subset dredge
SVCpred_dredge_nb_10_sub <- subset(SVCpred_dredge_nb_10, delta < 4) 

# model average 
SVCpred_model_average_nb_10 <- model.avg(SVCpred_dredge_nb_10_sub)
SVCpred_model_avg_summary_nb_10 <- summary(SVCpred_model_average_nb_10)

# covariate confidence intervals
SVCpred_confidence_nb_10 <- confint(SVCpred_model_average_nb_10)
summary(SVCpred_confidence_nb_10)

# dredge on nb 16 
SVCpred_dredge_nb_16 <- dredge(SVCpred_raw_nb_16)
SVCpred_dredge_nb_16 
sum(get.models(SVCpred_dredge_nb_16, subset = delta < 4) %>% lengths() > 0) # 135

# subset dredge
SVCpred_dredge_nb_16_sub <- subset(SVCpred_dredge_nb_16, delta < 4) 

# model average 
SVCpred_model_average_nb_16 <- model.avg(SVCpred_dredge_nb_16_sub)
SVCpred_model_avg_summary_nb_16 <- summary(SVCpred_model_average_nb_16)

# covariate confidence intervals
SVCpred_ponfidence_nb_16 <- confint(SVCpred_model_average_nb_16)
summary(SVCpred_ponfidence_nb_16)

# dredge on nb 17 
SVCpred_dredge_nb_17 <- dredge(SVCpred_raw_nb_17)
SVCpred_dredge_nb_17
sum(get.models(SVCpred_dredge_nb_17, subset = delta < 4) %>% lengths() > 0) # 71

# subset dredge
SVCpred_dredge_nb_17_sub <- subset(SVCpred_dredge_nb_17, delta < 4) 

# model average 
SVCpred_model_average_nb_17 <- model.avg(SVCpred_dredge_nb_17_sub)
SVCpred_model_avg_summary_nb_17 <- summary(SVCpred_model_average_nb_17)

# covariate confidence intervals
SVCpred_ponfidence_nb_17 <- confint(SVCpred_model_average_nb_17)
summary(SVCpred_ponfidence_nb_17)


# SVC vs. Roving: Model Plot =================================================

# The following creates a visual representation of predictors in the top models
# comparing SVC and roving survey density differences. Predictor values are 
# displayed along with their confidence intervals and significance. 

# select conditional coefficient estimates 
SVCpred_model_avg_plot <- 
  as.data.frame(SVCpred_model_avg_summary_nb_10$coefmat.subset) 

# select confidence intervals 
SVCpred_CI <- as.data.frame(SVCpred_confidence_nb_10) 

# put confidence intervals into coefficient dataframe
SVCpred_model_avg_plot$CI.min <-SVCpred_CI$`2.5 %` 
SVCpred_model_avg_plot$CI.max <-SVCpred_CI$`97.5 %`

# put rownames in column
setDT(SVCpred_model_avg_plot, keep.rownames = "Coefficient") 

# remove spaces from column headers
names(SVCpred_model_avg_plot) <- gsub(" ", "", names(SVCpred_model_avg_plot)) 

# add significance column
SVCpred_model_avg_plot <- SVCpred_model_avg_plot %>%
  mutate(significance = case_when(
    `Pr(>|z|)` < 0.05 ~ "sig",
    `Pr(>|z|)` >= 0.05 & `Pr(>|z|)` <=0.1 ~ "margsig",
    TRUE ~ "nonsig"
  ))

# change names of coefficients
SVCpred_model_avg_plot[2,1] = "Crypsis"
SVCpred_model_avg_plot[3,1] = "Fusiform"
SVCpred_model_avg_plot[4,1] = "Stony Coral Cover"
SVCpred_model_avg_plot[5,1] = "Max. Length"
SVCpred_model_avg_plot[6,1] = "Avg. Depth"
SVCpred_model_avg_plot[7,1] = "Octocoral Cover"
SVCpred_model_avg_plot[8,1] = "Nocturnality"
SVCpred_model_avg_plot[9,1] = "Vertical Relief"
SVCpred_model_avg_plot[10,1] = "Size Class"
SVCpred_model_avg_plot[11,1] = "Area Dif."
SVCpred_model_avg_plot[12,1] = "Neutral"
SVCpred_model_avg_plot[13,1] = "Silvering"

# change order of rows
SVCpred_model_avg_plot <- SVCpred_model_avg_plot[c(4,7,9,11,6,13,12,3,10,8,5,2),]

# plot 
SVCpred_coef_CI <- ggplot(data=SVCpred_model_avg_plot[1:13], 
                          aes(x=Coefficient, y=Estimate))+ 
  geom_hline(yintercept=0, color = "grey40",linetype="dashed", lwd=1.5)+
  geom_errorbar(aes(ymin=CI.min, ymax=CI.max), colour="grey65", 
                width=.2,lwd=1) +
  geom_point(size=5, aes(shape = significance, color = significance))+ 
  theme_classic(base_size = 20)+ 
  scale_shape_manual(values = c("sig" = 16, "margsig" = 16, "nonsig" = 1))+
  scale_color_manual(values = c("sig" = "black", "margsig" = "grey65", "nonsig" = "black")) +
  ylim(c(-0.21, 0.21)) +
  coord_flip() +
  theme(legend.position = "none",
        axis.title.y =element_blank(),  #remove y axis labels
        axis.title.x =element_blank(),
        axis.title = element_text(size = 30),
        axis.text = element_text(size = 16, color = "black")) +
  scale_x_discrete(limits=SVCpred_model_avg_plot$Coefficient)

# save model plot 
ggsave(here("./figures/SVCpred_coef_plot_CIs_centred.png"), SVCpred_coef_CI)
