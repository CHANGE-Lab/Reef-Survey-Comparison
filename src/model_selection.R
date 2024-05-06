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


# SVC vs. Transect: Global Model ===============================================

# The following creates the SVC vs. Transect survey global model utilizing the 
# best fit global model obtained prior to centring.

# global lme model
SVCprey_global_c <- lme(log_difference~habitat+octocoral_c+stony_c+relief_c+
                          size_bin_c*colouration+nocturnal+position+
                          max_length_c+behavior+cryptic_behaviour+depth_c+
                          size_bin_c*shape, 
                        random = list(~1|site, ~1|species_order), 
                        SVCprey_model_data) 

# model summary
summary(SVCprey_global_c)
AICc(SVCprey_global_c) # 33739.94

# covariate VIF values
vif(SVCprey_global_c) # aggregation behaviour VIF = 5.259862 

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

# add binary significance column
SVCprey_model_avg_plot$significance <- 
  ifelse(SVCprey_model_avg_plot$`Pr(>|z|)` < 0.05, "sig", "nonsig")

# change names of coefficients
SVCprey_model_avg_plot[2,1] = "Shoaling"
SVCprey_model_avg_plot[3,1] = "Solitary"
SVCprey_model_avg_plot[4,1] = "Colorful"
SVCprey_model_avg_plot[5,1] = "Neutral"
SVCprey_model_avg_plot[6,1] = "Silvering"
SVCprey_model_avg_plot[7,1] = "Patch"
SVCprey_model_avg_plot[8,1] = "Max. length"
SVCprey_model_avg_plot[9,1] = "Octocoral cover"
SVCprey_model_avg_plot[10,1] = "Elongated"
SVCprey_model_avg_plot[11,1] = "Fusiform"
SVCprey_model_avg_plot[12,1] = "Globiform"
SVCprey_model_avg_plot[13,1] = "Size Class"
SVCprey_model_avg_plot[14,1] = "Stony Coral"
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
  geom_point(size=5, aes(shape = significance))+ 
  theme_classic(base_size = 20)+ 
  scale_shape_manual(values = c(1,16))+
  ylim(c(-2.5, 2.25)) +
  coord_flip() +
  theme(legend.position = "none",
        axis.title.y =element_blank(),  #remove y axis labels
        axis.title.x =element_blank(),
        axis.title = element_text(size = 30),
        axis.text = element_text(size = 16, color = "black")) +
  scale_x_discrete(limits=SVCprey_model_avg_plot$Coefficient)

# save model plot 
ggsave(here("./SVCprey_coef_plot_CIs_centred.png"), SVCprey_coef_CI)


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


# SVC vs. Roving: Global Model =================================================

# The following creates the SVC vs. Roving survey global model utilizing the 
# best fit global model obtained prior to centering.

SVCpred_c <- lme(log_difference~habitat+octocoral_c+stony_c+relief_c+nocturnal+
                   max_length+cryptic_behaviour+average_depth+size_bin_c+
                   colouration+shape+position, 
                 random = list(~1|site, ~1|species_order), 
                 SVCpred_model_data) 
summary(SVCpred_c) # AIC = 1800.308
AICc(SVCpred_c) # AICc = 1801.384
vif(SVCpred_c) 
# habitat, max_length, average_depth, colouration, shape, position >5

# remove water column position 
SVCpred_c2 <- lme(log_difference~habitat+octocoral_c+stony_c+relief_c+nocturnal+
                    max_length+cryptic_behaviour+average_depth+size_bin_c+
                    colouration+shape, 
                  random = list(~1|site, ~1|species_order), 
                  SVCpred_model_data) 
summary(SVCpred_c2) # AIC = 1796.458
AICc(SVCpred_c2) # AICc = 1800.291
vif(SVCpred_c2) # habitat, max_length, average_depth >5

# remove habitat
SVCpred_c3 <- lme(log_difference~octocoral_c+stony_c+relief_c+nocturnal+
                    max_length+cryptic_behaviour+average_depth+size_bin_c+
                    colouration+shape, 
                  random = list(~1|site, ~1|species_order), 
                  SVCpred_model_data) 
summary(SVCpred_c3) # AIC = 1796.458
AICc(SVCpred_c3) # AICc = 1797.299
vif(SVCpred_c3) # max_length >5

# remove max length
SVCpred_c4 <- lme(log_difference~octocoral_c+stony_c+relief_c+nocturnal+
                    cryptic_behaviour+average_depth+size_bin_c+colouration+
                    shape, 
                  random = list(~1|site, ~1|species_order), 
                  SVCpred_model_data) 
summary(SVCpred_c4) # AIC = 1786.854
AICc(SVCpred_c4) # AICc = 1787.588
vif(SVCpred_c4) # all good 

# remove habitat from SVCpred_c
SVCpred_c5 <- lme(log_difference~octocoral_c+stony_c+relief_c+nocturnal+
                    max_length+cryptic_behaviour+average_depth+size_bin_c+
                    colouration+shape+position, 
                  random = list(~1|site, ~1|species_order), 
                  SVCpred_model_data) 
summary(SVCpred_c5) # AIC = 1797.493
AICc(SVCpred_c5) # AICc = 1798.447
vif(SVCpred_c5) # max_length, colouration, shape, position >5

# remove max_length from SVCpred_c5 
SVCpred_c6 <- lme(log_difference~octocoral_c+stony_c+relief_c+nocturnal+
                    cryptic_behaviour+average_depth+size_bin_c+colouration+
                    shape+position, 
                  random = list(~1|site, ~1|species_order), 
                  SVCpred_model_data) 
summary(SVCpred_c6) # AIC = 1788.905
AICc(SVCpred_c6) # AICc = 1789.746
vif(SVCpred_c6) # colouration, position >5

# remove water column position from SVCpred_c6
SVCpred_c7 <- lme(log_difference~octocoral_c+stony_c+relief_c+nocturnal+
                    cryptic_behaviour+average_depth+size_bin_c+colouration+
                    shape, 
                  random = list(~1|site, ~1|species_order), 
                  SVCpred_model_data) 
summary(SVCpred_c7) # AIC = 1786.854
AICc(SVCpred_c7) # AICc = 1787.588
vif(SVCpred_c7) # all good 

# remove average depth from SVCpred_c
SVCpred_c8 <- lme(log_difference~habitat+octocoral_c+stony_c+relief_c+nocturnal+
                    max_length+cryptic_behaviour+size_bin_c+colouration+shape+
                    position, 
                  random = list(~1|site, ~1|species_order), 
                  SVCpred_model_data) 
summary(SVCpred_c8) # AIC = 1790.624
AICc(SVCpred_c8) # AICc = 1791.578
vif(SVCpred_c8) # max_length, colouration, shape, position >5

# remove water column position from SVCpred_c8
SVCpred_c9 <- lme(log_difference~habitat+octocoral_c+stony_c+relief_c+nocturnal+
                    max_length+cryptic_behaviour+size_bin_c+colouration+shape, 
                  random = list(~1|site, ~1|species_order), 
                  SVCpred_model_data) 
summary(SVCpred_c9) # AIC = 1763.979
AICc(SVCpred_c9) # AICc = 1764.82
vif(SVCpred_c9) # max_length >5

# remove max length from SVCpred_c9 
SVCpred_c10 <- lme(log_difference~habitat+octocoral_c+stony_c+relief_c+
                     nocturnal+cryptic_behaviour+size_bin_c+colouration+shape, 
                   random = list(~1|site, ~1|species_order), 
                   SVCpred_model_data) 
summary(SVCpred_c10) # AIC = 1789.521
AICc(SVCpred_c10) # AICc = 1754.549
vif(SVCpred_c10) # all good 
# *BEST GLOBAL MODEL FIT*

# residuals plot
res_SVCpred_global_c = residuals(SVCpred_c10)
plot(res_SVCpred_global_c) 

# qq plot
qqnorm(res_SVCpred_global_c) 
qqline(res_SVCpred_global_c)
hist(res_SVCpred_global_c)

# remove max length from SVCpred_8
SVCpred_c11 <- lme(log_difference~habitat+octocoral_c+stony_c+relief_c+
                     nocturnal+cryptic_behaviour+size_bin_c+colouration+shape+
                     position, 
                   random = list(~1|site, ~1|species_order), 
                   SVCpred_model_data) 
summary(SVCpred_c11) # AIC = 1781.708
AICc(SVCpred_c11) # AICc = 1782.549
vif(SVCpred_c11) # colouration, position >5


# SVC vs. Roving: Dredge =======================================================

# dredge
SVCpred_dredge_c <- dredge(SVCpred_c10)
SVCpred_dredge_c 

# save dredge results 
saveRDS(SVCpred_dredge_c, here("./outputs/SVCpred_dredge_centred.rds"))

# subset dredge
SVCpred_dredge_c_sub <- subset(SVCpred_dredge_c, delta < 4) 

# model average 
SVCpred_model_average_c <- model.avg(SVCpred_dredge_c_sub)
SVCpred_model_avg_summary_c <- summary(SVCpred_model_average_c)

# save model average
saveRDS(SVCpred_model_average_c, 
        here("./outputs/SVCpred_dredge_average_centred.rds"))

# read in saved average
SVCpred_model_average_c <- 
  read_rds(here("./outputs/SVCpred_dredge_average_centred.rds"))
SVCpred_model_avg_summary <- summary(SVCpred_model_average_c)

# covariate confidence intervals
SVCpred_confidence_c <- confint(SVCpred_model_average_c)
summary(SVCpred_confidence_c)

# save confidence intervals
saveRDS(SVCpred_confidence_c, here("./outputs/SVCpred_dredge_CI_centred.rds"))

# read in saved CIs
SVCpred_confidence_redo_cont <- 
  read_rds(here("./outputs/SVCpred_dredge_CI_centred.rds"))


# SVC vs. Roving: Model Plot ===================================================

# The following creates a visual representation of predictors in the top models
# comparing SVC and roving survey density differences. Predictor values are 
# displayed along with their confidence intervals and significance. 

# select conditional coefficient estimates 
SVCpred_model_avg_plot<-as.data.frame(SVCpred_model_avg_summary$coefmat.subset) 

# select confidence intervals 
SVCpred_CI <- as.data.frame(SVCpred_confidence_redo_cont) 

# put confidence intervals into coefficient dataframe
SVCpred_model_avg_plot$CI.min <-SVCpred_CI$`2.5 %` 
SVCpred_model_avg_plot$CI.max <-SVCpred_CI$`97.5 %`

# put rownames in column
setDT(SVCpred_model_avg_plot, keep.rownames = "Coefficient") 

# remove spaces from column headers
names(SVCpred_model_avg_plot) <- gsub(" ", "", names(SVCpred_model_avg_plot)) 

# add binary significance column
SVCpred_model_avg_plot$significance <- 
  ifelse(SVCpred_model_avg_plot$`Pr(>|z|)` < 0.05, "sig", "nonsig")

# change labels
SVCpred_model_avg_plot[2,1] <- "Neutral"
SVCpred_model_avg_plot[3,1] <- "Silvering"
SVCpred_model_avg_plot[4,1] <- "Fusiform"

# change order of rows
SVCpred_model_avg_plot <- SVCpred_model_avg_plot[c(3,2,4),]

# plot with confidence intervals 
SVCpred_coef_CI <- ggplot(data=SVCpred_model_avg_plot[1:3,], 
                          aes(x=Coefficient, y=Estimate))+ 
  geom_hline(yintercept=0, color = "grey40",linetype="dashed", lwd=1.5)+
  geom_errorbar(aes(ymin=CI.min, ymax=CI.max), colour="grey65", 
                width=.2,lwd=1) +
  geom_point(size=5, aes(shape = significance))+ 
  theme_classic(base_size = 20)+ 
  scale_shape_manual(values = c(1,16))+
  ylim(c(-2.5,2.25)) +
  coord_flip() +
  theme(legend.position = "none",
        axis.title.y=element_blank(),  #remove y axis labels
        axis.title = element_text(size = 34),
        axis.text = element_text(size = 16, color = "black")) +
  scale_x_discrete(limits=SVCpred_model_avg_plot$Coefficient)

# save model plot
ggsave(here("./visuals/SVCpred_coef_plot_CIs_centred.png"), SVCpred_coef_CI)