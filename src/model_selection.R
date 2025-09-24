########## SURVEY COMPARISON PROJECT MODEL CREATION AND SELECTION ##########
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
# DATE OF CREATION: 2021-05-28
##########
##########


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
SVCprey_model_data <- read_csv(here("./dataframes/SVCprey_dataframe.csv"))
SVCpred_model_data <- read_csv(here("./dataframes/SVCpred_dataframe.csv"))

# number of individual observations
colSums(SVCprey_model_data !=0)
colSums(SVCpred_model_data !=0)


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
SVCprey_model_data$area_dif_c <- 
  SVCprey_model_data$SVCprey_area_dif - 
  mean(SVCprey_model_data$SVCprey_area_dif)


# SVC vs. Transect: Global Model ===============================================

# The following creates the SVC vs. Transect survey global model utilizing the 
# best fit global model obtained prior to centring.

# global lme model
SVCprey_global_c <- lme(log_difference~habitat+octocoral_c+stony_c+relief_c+
                          size_bin_c*colouration+nocturnal+position+
                          max_length_c+behavior+cryptic_behaviour+depth_c+
                          size_bin_c*shape+area_dif_c, 
                        random = list(~1|site, ~1|species_order), 
                        SVCprey_model_data) 

# model summary
summary(SVCprey_global_c)
AICc(SVCprey_global_c) # 33754.79

# covariate VIF values
vif(SVCprey_global_c) # aggregation behaviour VIF = 5.253781
check_collinearity(SVCprey_global_c)

# random effects plot
plot(ranef(SVCprey_global_c))

# residuals plot
res_SVCprey_global_c = residuals(SVCprey_global_c)
plot(res_SVCprey_global_c) 

# qq plot
qqnorm(res_SVCprey_global_c) 
qqline(res_SVCprey_global_c)

# model plot
plot(SVCprey_global_c) 

# model diagnostic plots
check_model(SVCprey_global_c)


# SVC vs. Transect: Dredge =====================================================

# The following performs a dredge on the global linear mixed effects model 
# comparing SVC fish densities to transect survey fish densities in order to 
# determine which combination of predictors results in the most likely model.

# dredge
SVCprey_dredge_c <- dredge(SVCprey_global_c)
SVCprey_dredge_c

# save dredge output 
saveRDS(SVCprey_dredge_c, here("./outputs/SVCprey_global_dredge_centred.rds"))

# subset dredge
SVCprey_dredge_sub_c <- subset(SVCprey_dredge_c, delta < 4) 

# model average 
SVCprey_model_average_c <- model.avg(SVCprey_dredge_sub_c)
SVCprey_model_avg_summary_c <- summary(SVCprey_model_average_c)

# save model average
saveRDS(SVCprey_model_average_c, 
        here("./outputs/SVCprey_drege_average_centred.rds"))

# read in saved average and summary
SVCprey_model_average_c <- 
  read_rds(here("./outputs/SVCprey_drege_average_centred.rds"))
SVCprey_model_avg_summary_c <- summary(SVCprey_model_average_c)

# confidence intervals of predictors
SVCprey_confidence_c <- confint(SVCprey_model_average_c)

# save confidence intervals
saveRDS(SVCprey_confidence_c, here("./outputs/SVCprey_dredge_CI_centred.rds"))

# read in saved CIs
SVCprey_confidence_c <- 
  read_rds(here("./outputs/SVCprey_dredge_CI_centred.rds"))


# SVC vs. Transect: Model Plot =================================================

# The following creates a visual representation of predictors in the top models
# comparing SVC and transect survey density differences. Predictor values are 
# displayed along with their confidence intervals and significance. 

# select conditional coefficient estimates 
SVCprey_model_avg_plot <- 
  as.data.frame(SVCprey_model_avg_summary_c$coefmat.subset) 

# select confidence intervals 
SVCprey_CI <- as.data.frame(SVCprey_confidence_c) 

# put confidence intervals into coefficient dataframe
SVCprey_model_avg_plot$CI.min <-SVCprey_CI$`2.5 %` 
SVCprey_model_avg_plot$CI.max <-SVCprey_CI$`97.5 %`

# put rownames in column
setDT(SVCprey_model_avg_plot, keep.rownames = "Coefficient") 

# remove spaces from column headers
names(SVCprey_model_avg_plot) <- gsub(" ", "", names(SVCprey_model_avg_plot)) 

# add significance column
SVCprey_model_avg_plot <- SVCprey_model_avg_plot %>%
  mutate(significance = case_when(
    `Pr(>|z|)` < 0.05 ~ "sig",
    `Pr(>|z|)` >= 0.05 & `Pr(>|z|)` <=0.1 ~ "margsig",
    TRUE ~ "nonsig"
  ))

# change names of coefficients
SVCprey_model_avg_plot[2,1] = "Shoaling"
SVCprey_model_avg_plot[3,1] = "Solitary"
SVCprey_model_avg_plot[4,1] = "Colorful"
SVCprey_model_avg_plot[5,1] = "Neutral"
SVCprey_model_avg_plot[6,1] = "Silvering"
SVCprey_model_avg_plot[7,1] = "Patch"
SVCprey_model_avg_plot[8,1] = "Max. Length"
SVCprey_model_avg_plot[9,1] = "Octocoral Cover"
SVCprey_model_avg_plot[10,1] = "Elongated"
SVCprey_model_avg_plot[11,1] = "Fusiform"
SVCprey_model_avg_plot[12,1] = "Globiform"
SVCprey_model_avg_plot[13,1] = "Size Class"
SVCprey_model_avg_plot[14,1] = "Stony Coral Cover"
SVCprey_model_avg_plot[15,1] = "Elongated:Size Class"
SVCprey_model_avg_plot[16,1] = "Fusiform:Size Class"
SVCprey_model_avg_plot[17,1] = "Globiform:Size Class"
SVCprey_model_avg_plot[18,1] = "Colorful:Size Class"
SVCprey_model_avg_plot[19,1] = "Neutral:Size Class"
SVCprey_model_avg_plot[20,1] = "Silvering:Size Class"
SVCprey_model_avg_plot[21,1] = "Crypsis"
SVCprey_model_avg_plot[22,1] = "Demersal"

# change order of rows
SVCprey_model_avg_plot <- SVCprey_model_avg_plot[c(14,9,7,18,20,19,4,6,5,17,16,
                                                   15,12,11,10,13,22,8,2,3,21),]

# plot 
SVCprey_coef_CI <- ggplot(data=SVCprey_model_avg_plot[1:21], 
                          aes(x=Coefficient, y=Estimate))+ 
  geom_hline(yintercept=0, color = "grey40",linetype="dashed", lwd=1.5)+
  geom_errorbar(aes(ymin=CI.min, ymax=CI.max), colour="grey65", 
                width=.2,lwd=1) +
  geom_point(size=5, aes(shape = significance, color = significance))+ 
  theme_classic(base_size = 20)+ 
  scale_shape_manual(values = c("sig" = 16, "margsig" = 16, "nonsig" = 1))+
  scale_color_manual(values = c("sig" = "black", "margsig" = "grey65", 
                                "nonsig" = "black")) +
  ylim(c(-1.5, 1.5)) +
  coord_flip() +
  theme(legend.position = "none",
        axis.title.y =element_blank(),  #remove y axis labels
        axis.title.x =element_blank(),
        axis.title = element_text(size = 30),
        axis.text = element_text(size = 16, color = "black")) +
  scale_x_discrete(limits=SVCprey_model_avg_plot$Coefficient)

# save model plot 
ggsave(here("./figures/SVCprey_coef_plot_CIs_centred.png"), SVCprey_coef_CI)


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


# SVC vs. Roving: Global Model =================================================

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
SVCpred_model_data$pos_raw_dif_scaled <- 
  SVCpred_model_data$pos_raw_difference*1000

# create new column of rounded density differences
SVCpred_model_data <- SVCpred_model_data %>%
  mutate(whole_scaled_raw_difference = round(pos_raw_dif_scaled))

# look at rounded density difference distribution
hist(SVCpred_model_data$whole_scaled_raw_difference)

# global model with negative binomial distribution
SVCpred_raw_nb_ran <- glmer.nb(whole_scaled_raw_difference ~ habitat +
                                 octocoral_scaled + stony_scaled+
                                 relief_scaled+nocturnal + max_length_scaled +
                                 cryptic_behaviour + average_depth_scaled + 
                                 size_bin_scaled +
                                 colouration + shape + position + 
                                 area_dif_scaled +
                                 (1 | site) + (1 | species_order),
                               data = SVCpred_model_data)
# model fit is singular
summary(SVCpred_raw_nb_ran)
AICc(SVCpred_raw_nb_ran) # AICc = 2998.58
vif(SVCpred_raw_nb_ran)

# global model with negative binomial distribution & scaled predictors
SVCpred_raw_nb_ran <- glm.nb(whole_scaled_raw_difference ~ habitat+
                               scale(octocoral)+scale(stony)+
                               scale(relief_cm)+nocturnal+
                               scale(max_length)+
                               cryptic_behaviour+scale(average_depth) + 
                               scale(size_bin_lengths)+
                               colouration+shape+position+
                               scale(SVCpred_area_dif),
                             data = SVCpred_model_data)
summary(SVCpred_raw_nb_ran)
AICc(SVCpred_raw_nb_ran) # AICc = 2998.58
vif(SVCpred_raw_nb_ran)

# global model with random effects removed 
SVCpred_raw_nb <- glm.nb(whole_scaled_raw_difference ~ habitat+octocoral_scaled+
                           stony_scaled+relief_scaled+nocturnal+
                           max_length_scaled+cryptic_behaviour+
                           average_depth_scaled + size_bin_scaled+
                           colouration+shape+position+area_dif_scaled, 
                         data = SVCpred_model_data)
summary(SVCpred_raw_nb)
AICc(SVCpred_raw_nb) # AICc = 2994.263
vif(SVCpred_raw_nb)

# remove habitat from global
SVCpred_raw_nb_1 <- glm.nb(whole_scaled_raw_difference ~ octocoral_scaled+
                             stony_scaled+relief_scaled+nocturnal+
                             max_length_scaled+cryptic_behaviour+
                             average_depth_scaled + size_bin_scaled+
                             colouration+shape+position+area_dif_scaled, 
                           data = SVCpred_model_data)
summary(SVCpred_raw_nb_1)
AICc(SVCpred_raw_nb_1) # AICc = 2992.873
vif(SVCpred_raw_nb_1)

# remove max length from global 
SVCpred_raw_nb_2 <- glm.nb(whole_scaled_raw_difference ~ habitat+
                             octocoral_scaled+stony_scaled+
                             relief_scaled+nocturnal+
                             cryptic_behaviour+average_depth_scaled + 
                             size_bin_scaled+
                             colouration+shape+position+area_dif_scaled, 
                           data = SVCpred_model_data)
summary(SVCpred_raw_nb_2)
AICc(SVCpred_raw_nb_2) # AICc = 2993.511
vif(SVCpred_raw_nb_2)

# remove depth from global
SVCpred_raw_nb_3 <- glm.nb(whole_scaled_raw_difference ~ habitat+
                             octocoral_scaled+stony_scaled+
                             relief_scaled+nocturnal+max_length_scaled+
                             cryptic_behaviour+ size_bin_scaled+
                             colouration+shape+position+area_dif_scaled, 
                           data = SVCpred_model_data)
summary(SVCpred_raw_nb_3)
AICc(SVCpred_raw_nb_3) # AICc = 2992.206
vif(SVCpred_raw_nb_3)

# remove colour from global
SVCpred_raw_nb_4 <- glm.nb(whole_scaled_raw_difference ~ habitat+
                             octocoral_scaled+stony_scaled+
                             relief_scaled+nocturnal+max_length_scaled+
                             cryptic_behaviour+average_depth_scaled + 
                             size_bin_scaled+shape+position+area_dif_scaled, 
                           data = SVCpred_model_data)
summary(SVCpred_raw_nb_4)
AICc(SVCpred_raw_nb_4) # AICc = 2991.313
vif(SVCpred_raw_nb_4)

# remove shape from global
SVCpred_raw_nb_5 <- glm.nb(whole_scaled_raw_difference ~ habitat+
                             octocoral_scaled+stony_scaled+
                             relief_scaled+nocturnal+max_length_scaled+
                             cryptic_behaviour+average_depth_scaled + 
                             size_bin_scaled+colouration+position+
                             area_dif_scaled, 
                           data = SVCpred_model_data)
summary(SVCpred_raw_nb_5)
AICc(SVCpred_raw_nb_5) # AICc = 2996.495
vif(SVCpred_raw_nb_5)

# remove position from global
SVCpred_raw_nb_6 <- glm.nb(whole_scaled_raw_difference ~ habitat+
                             octocoral_scaled+stony_scaled+
                             relief_scaled+nocturnal+max_length_scaled+
                             cryptic_behaviour+average_depth_scaled + 
                             size_bin_scaled+
                             colouration+shape+area_dif_scaled, 
                           data = SVCpred_model_data)
summary(SVCpred_raw_nb_6)
AICc(SVCpred_raw_nb_6) # AICc = 2993.336
vif(SVCpred_raw_nb_6)

# remove max length from 1
SVCpred_raw_nb_7 <- glm.nb(whole_scaled_raw_difference ~ octocoral_scaled+
                             stony_scaled+relief_scaled+nocturnal+
                             cryptic_behaviour+average_depth_scaled + 
                             size_bin_scaled+
                             colouration+shape+position+area_dif_scaled, 
                           data = SVCpred_model_data)
summary(SVCpred_raw_nb_7)
AICc(SVCpred_raw_nb_7) # AICc = 2992.143
vif(SVCpred_raw_nb_7)

# remove colour from 1
SVCpred_raw_nb_8 <- glm.nb(whole_scaled_raw_difference ~ octocoral_scaled+
                             stony_scaled+
                             relief_scaled+nocturnal+max_length_scaled+
                             cryptic_behaviour+average_depth_scaled + 
                             size_bin_scaled+
                             shape+position+area_dif_scaled, 
                           data = SVCpred_model_data)
summary(SVCpred_raw_nb_8)
AICc(SVCpred_raw_nb_8) # AICc = 2989.855
vif(SVCpred_raw_nb_8)

# remove shape from 1
SVCpred_raw_nb_9 <- glm.nb(whole_scaled_raw_difference ~ octocoral_scaled+
                             stony_scaled+
                             relief_scaled+nocturnal+max_length_scaled+
                             cryptic_behaviour+average_depth_scaled + 
                             size_bin_scaled+
                             colouration+position+area_dif_scaled, 
                           data = SVCpred_model_data)
summary(SVCpred_raw_nb_9)
AICc(SVCpred_raw_nb_9) # AICc = 2994.928
vif(SVCpred_raw_nb_9)

# remove position from 1
SVCpred_raw_nb_10 <- glm.nb(whole_scaled_raw_difference ~ octocoral_scaled+
                              stony_scaled+
                              relief_scaled+nocturnal+max_length_scaled+
                              cryptic_behaviour+average_depth_scaled + 
                              size_bin_scaled+
                              colouration+shape+area_dif_scaled, 
                            data = SVCpred_model_data,
                            na.action = na.fail)
summary(SVCpred_raw_nb_10)
AICc(SVCpred_raw_nb_10) # AICc = 2991.792
vif(SVCpred_raw_nb_10)
# ALL GOOD 

# remove depth from 2
SVCpred_raw_nb_12 <- glm.nb(whole_scaled_raw_difference ~ habitat+
                              octocoral_scaled+stony_scaled+
                              relief_scaled+nocturnal+
                              cryptic_behaviour+ size_bin_scaled+
                              colouration+shape+position+area_dif_scaled, 
                            data = SVCpred_model_data)
summary(SVCpred_raw_nb_12)
AICc(SVCpred_raw_nb_12) # AICc = 2991.409
vif(SVCpred_raw_nb_12)

# remove position from 2
SVCpred_raw_nb_13 <- glm.nb(whole_scaled_raw_difference ~ habitat+
                              octocoral_scaled+stony_scaled+
                              relief_scaled+nocturnal+
                              cryptic_behaviour+average_depth_scaled + 
                              size_bin_scaled+
                              colouration+shape+area_dif_scaled, 
                            data = SVCpred_model_data)
summary(SVCpred_raw_nb_13)
AICc(SVCpred_raw_nb_13) # AICc = 2992
vif(SVCpred_raw_nb_13)

# remove colour from 3
SVCpred_raw_nb_15 <- glm.nb(whole_scaled_raw_difference ~ habitat+
                              octocoral_scaled+stony_scaled+
                              relief_scaled+nocturnal+max_length_scaled+
                              cryptic_behaviour+ size_bin_scaled+
                              shape+position+area_dif_scaled, 
                            data = SVCpred_model_data)
summary(SVCpred_raw_nb_15)
AICc(SVCpred_raw_nb_15) # AICc = 2989.282
vif(SVCpred_raw_nb_15)

# remove shape from 3
SVCpred_raw_nb_16 <- glm.nb(whole_scaled_raw_difference ~ habitat+
                              octocoral_scaled+stony_scaled+
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
SVCpred_raw_nb_17 <- glm.nb(whole_scaled_raw_difference ~ habitat+
                              octocoral_scaled+stony_scaled+
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
SVCpred_raw_nb_20 <- glm.nb(whole_scaled_raw_difference ~ habitat+
                              octocoral_scaled+stony_scaled+
                              relief_scaled+nocturnal+max_length_scaled+
                              cryptic_behaviour+average_depth_scaled + 
                              size_bin_scaled+
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


# SVC vs. Roving: Dredge =======================================================

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
SVCpred_model_avg_plot <- 
  SVCpred_model_avg_plot[c(4,7,9,11,6,13,12,3,10,8,5,2),]

# plot 
SVCpred_coef_CI <- ggplot(data=SVCpred_model_avg_plot[1:13], 
                          aes(x=Coefficient, y=Estimate))+ 
  geom_hline(yintercept=0, color = "grey40",linetype="dashed", lwd=1.5)+
  geom_errorbar(aes(ymin=CI.min, ymax=CI.max), colour="grey65", 
                width=.2,lwd=1) +
  geom_point(size=5, aes(shape = significance, color = significance))+ 
  theme_classic(base_size = 20)+ 
  scale_shape_manual(values = c("sig" = 16, "margsig" = 16, "nonsig" = 1))+
  scale_color_manual(values = c("sig" = "black", "margsig" = "grey65", 
                                "nonsig" = "black")) +
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