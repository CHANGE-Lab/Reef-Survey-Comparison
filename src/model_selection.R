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
library(here)

# data
SVCprey_model_data <- read_csv(here("./dataframes/SVCprey_dataframe.csv"))
SVCpred_model_data <- read_csv(here("./dataframes/SVCpred_dataframe.csv"))


# SVC vs. Transect: Global Model ===============================================

# In the following, a linear mixed model is created to compare fish density 
# differences between SVC and transect surveys in response to predictors 
# including species traits, habitat traits, and survey traits. All traits of 
# interest are included in the global model. Collinearity is explored through 
# the VIF values for each predictor, and model fit is determined through 
# random effects plots, residual plots, qq plots, and model plots. 

SVCprey_model_data$size_bin_lengths <- as.character(SVCprey_model_data$size_bin_lengths)

# global lme model
SVCprey_global <- lme(log_difference~habitat+octocoral+stony+relief_cm+
                      size_bin_lengths*colouration+nocturnal+position+
                      max_length+behavior+cryptic_behaviour+average_depth+
                      size_bin_lengths*shape, 
                      random = list(~1|site, ~1|species_order), 
                      SVCprey_model_data) 

# model summary
summary(SVCprey_global) 
AICc(SVCprey_global)

# comparison of predictor levels
emmeans(SVCprey_global, pairwise~behavior, adjust = "tukey")
emmeans(SVCprey_global, pairwise~colouration, adjust = "tukey")
emmeans(SVCprey_global, pairwise~size_bin_lengths, adjust = "tukey")
emmeans(SVCprey_global, pairwise~shape, adjust = "tukey")
emmeans_sizeshape <- pairs(emmeans(SVCprey_global, pairwise~size_bin_lengths*shape, adjust = "tukey"))
# contrast(emmeans_sizeshape, "consec", simple = "each", combine = FALSE, adjust = "mvt")
# joint_tests(SVCprey_global, by = "size_bin_lengths")
# emmip(SVCprey_global, size_bin_lengths~shape, mult.name = "variety", cov.reduce = FALSE)

# covariate VIF values
vif(SVCprey_global) # aggregation behaviour VIF = 5.872522

# random effects plot
plot(ranef(SVCprey_global))

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
SVCprey_dredge <- dredge(SVCprey_global)
SVCprey_dredge

# save dredge output 
saveRDS(SVCprey_dredge, here("./outputs/SVCprey_global_dredge.rds"))

# subset dredge
SVCprey_dredge_sub <- subset(SVCprey_dredge, delta < 4) 

# model average 
SVCprey_model_average <- model.avg(SVCprey_dredge_sub)
SVCprey_model_avg_summary <- summary(SVCprey_model_average)

# save model average
saveRDS(SVCprey_model_average, here("./outputs/SVCprey_drege_average.rds"))

# read in saved average and summary
SVCprey_model_average <- read_rds(here("./outputs/SVCprey_drege_average.rds"))
SVCprey_model_avg_summary <- summary(SVCprey_model_average)

# confidence intervals of predictors
SVCprey_confidence <- confint(SVCprey_model_average)

# save confidence intervals
saveRDS(SVCprey_confidence, here("./outputs/SVCprey_dredge_CI.rds"))

# read in saved CIs
SVCprey_confidence <- read_rds(here("./outputs/SVCprey_dredge_CI.rds"))


# SVC vs. Transect: Model Plot =================================================

# The following creates a visual representation of predictors in the top models
# comparing SVC and transect survey density differences. Predictor values are 
# displayed along with their confidence intervals and significance. 

# select conditional coefficient estimates 
SVCprey_model_avg_plot<-as.data.frame(SVCprey_model_avg_summary$coefmat.subset) 

# select confidence intervals 
SVCprey_CI <- as.data.frame(SVCprey_confidence) 

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
SVCprey_model_avg_plot[7,1] = "Crypsis"
SVCprey_model_avg_plot[8,1] = "Patch"
SVCprey_model_avg_plot[9,1] = "Max. length"
SVCprey_model_avg_plot[10,1] = "Octocoral cover"
SVCprey_model_avg_plot[11,1] = "Elongated"
SVCprey_model_avg_plot[12,1] = "Fusiform"
SVCprey_model_avg_plot[13,1] = "Globiform"
SVCprey_model_avg_plot[14,1] = "Size class"
SVCprey_model_avg_plot[15,1] = "Coral cover"
SVCprey_model_avg_plot[16,1] = "Elongated:Size class"
SVCprey_model_avg_plot[17,1] = "Fusiform:Size class"
SVCprey_model_avg_plot[18,1] = "Globiform:Size class"
SVCprey_model_avg_plot[19,1] = "Demersal"
SVCprey_model_avg_plot[20,1] = "Nocturnal"

# change order of rows
SVCprey_model_avg_plot$sort <- c(20,18,19,15,17,16,6,3,5,2,14,13,12,4,1,11,10,9,7,8)
SVCprey_model_avg_plot <- SVCprey_model_avg_plot %>% arrange(sort)
SVCprey_model_avg_plot$Coefficient <- factor(SVCprey_model_avg_plot$Coefficient, levels = SVCprey_model_avg_plot$Coefficient)

# plot with confidence intervals 
SVCprey_coef_CI <- ggplot(data=SVCprey_model_avg_plot[1:19], 
                          aes(x=Coefficient, y=Estimate))+ 
  geom_hline(yintercept=0, color = "grey40",linetype="dashed", lwd=1.5)+
  geom_errorbar(aes(ymin=CI.min, ymax=CI.max), colour="grey65", 
                width=.2,lwd=1) +
  geom_point(size=7, aes(shape = significance))+ 
  theme_classic(base_size = 20)+ 
  scale_shape_manual(values = c(16,8))+
  ylim(c(-2.2, 2.2)) +
  coord_flip() +
  theme(legend.position = "none",
        axis.title = element_text(size = 30),
        axis.text = element_text(size = 26, color = "black"))
ggsave(here("./visuals/SVCprey_coef_plot_CIs.png"), SVCprey_coef_CI)

# plot with adjusted standard error bars
SVCprey_coef_SE <- ggplot(data=SVCprey_model_avg_plot[2:20,], 
                          aes(x=Coefficient, y=Estimate))+ 
  geom_point(size=5, aes(shape = significance))+ 
  theme_classic(base_size = 20)+ 
  scale_shape_manual(values = c(16,8))+
  geom_errorbar(aes(ymin=Estimate-AdjustedSE, ymax=Estimate+AdjustedSE), 
                colour="grey65", 
                width=.2, lwd=1) +
  geom_hline(yintercept=0, color = "grey40",linetype="dashed", lwd=1.5)+
  coord_flip() +
  theme(legend.position = "none")
ggsave(here("./visuals/SVCprey_coef_plot_SEs.png"), SVCprey_coef_SE)


# SVC vs. Transect: Global Model W/O Crypsis ###################################

SVCprey_model_data$size_bin_lengths <- as.character(SVCprey_model_data$size_bin_lengths)

# global lme model
SVCprey_global_nocrypsis <- lme(log_difference~habitat+octocoral+stony+relief_cm+
                        size_bin_lengths*colouration+nocturnal+position+
                        max_length+behavior+average_depth+
                        size_bin_lengths*shape, 
                      random = list(~1|site, ~1|species_order), 
                      SVCprey_model_data) 

# model summary
summary(SVCprey_global_nocrypsis) 
AICc(SVCprey_global_nocrypsis)

# covariate VIF values
vif(SVCprey_global_nocrypsis) # aggregation behaviour VIF = 5.676662


# SVC vs. Transect: Dredge W/O Crypsis =========================================

# dredge
SVCprey_dredge_nocrypsis <- dredge(SVCprey_global_nocrypsis)
SVCprey_dredge_nocrypsis

# save dredge output 
saveRDS(SVCprey_dredge_nocrypsis, here("./outputs/SVCprey_dredge_nocrypsis.rds"))

# subset dredge
SVCprey_dredge_sub_nocrypsis <- subset(SVCprey_dredge_nocrypsis, delta < 4) 

# model average 
SVCprey_model_average_nocrypsis <- model.avg(SVCprey_dredge_sub_nocrypsis)
SVCprey_model_avg_summary_nocrypsis <- summary(SVCprey_model_average_nocrypsis)

# save model average
saveRDS(SVCprey_model_average_nocrypsis, here("./outputs/SVCprey_drege_average_nocrypsis.rds"))


# SVC vs. Roving: Global Model =================================================

# In the following, a linear mixed model is created to compare fish density 
# differences between SVC and roving surveys in response to predictors 
# including species traits, habitat traits, and survey traits. All traits of 
# interest are included in the global model. Collinearity is explored through 
# the VIF values for each predictor, and model fit is determined through 
# random effects plots, residual plots, qq plots, and model plots. 

# full model:
SVCpred_full <- lme(log_difference~habitat+octocoral+stony+relief_cm+nocturnal+
                    max_length+cryptic_behaviour+average_depth+colouration+
                    size_bin_lengths+shape+position, 
                    random = list(~1|site, ~1|species_order), 
                    SVCpred_model_data) 
summary(SVCpred_full)
AICc(SVCpred_full)
vif(SVCpred_full)

# Colouration Model:
SVCpred_colour <- lme(log_difference~habitat+octocoral+stony+relief_cm+
                      nocturnal+max_length+cryptic_behaviour+average_depth+
                      colouration+size_bin_lengths, 
                      random = list(~1|site, ~1|species_order), 
                      SVCpred_model_data) # removed shape and position
summary(SVCpred_colour) # AIC = 1795.605
AICc(SVCpred_colour) # 1796.432
vif(SVCpred_colour) # colouration GVIF = 5.877840

# Shape Model:
SVCpred_shape <- lme(log_difference~habitat+octocoral+stony+relief_cm+nocturnal+
                     position+max_length+cryptic_behaviour+average_depth+
                     shape+size_bin_lengths, 
                     random = list(~1|site, ~1|species_order), 
                     SVCpred_model_data) # removed colouration
summary(SVCpred_shape) # AIC = 1802.129 
AICc(SVCpred_shape) # 1803.188
vif(SVCpred_shape) # shape GVIF = 6.727650 

# Habitat Model (from colouration model):
SVCpred_hab <- lme(log_difference~habitat+octocoral+stony+relief_cm+nocturnal+
                   max_length+cryptic_behaviour+colouration+size_bin_lengths, 
                   random = list(~1|site, ~1|species_order), 
                   SVCpred_model_data) # removed shape, position, and depth
summary(SVCpred_hab) # AIC = 1785.537
AICc(SVCpred_hab)
vif(SVCpred_hab) # colouration GVIF = 5.843345, all else under 5

# Depth Model (from colouration model):
SVCpred_depth <- lme(log_difference~octocoral+stony+relief_cm+nocturnal+
                     max_length+cryptic_behaviour+average_depth+colouration+
                     size_bin_lengths, 
                     random = list(~1|site, ~1|species_order), 
                     SVCpred_model_data) # removed shape, position, and habitat
summary(SVCpred_depth) # AIC = 1792.709; higher than habitat model
AICc(SVCpred_depth)
vif(SVCpred_depth) # colouration GVIF = 5.844267 

# Habitat Model (from shape model):
SVCpred_hab2 <- lme(log_difference~habitat+octocoral+stony+relief_cm+nocturnal+
                    position+max_length+cryptic_behaviour+shape+
                    size_bin_lengths, 
                    random = list(~1|site, ~1|species_order), 
                    SVCpred_model_data) # removed colouration and depth
summary(SVCpred_hab2) # AIC = 1616.189
vif(SVCpred_hab2) # shape GVIF = 7.659572

# Depth Model (from shape model):
SVCpred_depth2 <- lme(log_difference~octocoral+stony+relief_cm+nocturnal+
                      position+max_length+cryptic_behaviour+average_depth+
                      shape+size_bin_lengths, 
                      random = list(~1|site, ~1|species_order), 
                      SVCpred_model_data) # removed colouration and habitat
summary(SVCpred_depth2) # AIC = 1623.182; higher than habitat model
vif(SVCpred_depth2) # shape GVIF = 7.659066


# global lme model: colouration and habitat model
SVCpred_global <- SVCpred_hab

# model summary 
summary(SVCpred_global) 

# post-hoc test on predictors
emmeans(SVCpred_global, pairwise~colouration, adjust = "tukey")

# covariate VIF values
vif(SVCpred_global)

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

# save dredge results 
saveRDS(SVCpred_dredge, here("./outputs/SVCpred_dredge.rds"))

# subset dredge
SVCpred_dredge_sub <- subset(SVCpred_dredge, delta < 4) 

# model average 
SVCpred_model_average <- model.avg(SVCpred_dredge_sub)
SVCpred_model_avg_summary <- summary(SVCpred_model_average)

# save model average
saveRDS(SVCpred_model_average, here("./outputs/SVCpred_dredge_average.rds"))

# read in saved average and summary
SVCpred_model_average <- read_rds(here("./outputs/SVCpred_dredge_average.rds"))
SVCpred_model_avg_summary <- summary(SVCpred_model_average)

# covariate confidence intervals
SVCpred_confidence <- confint(SVCpred_model_average)
summary(SVCpred_confidence)

# save confidence intervals
saveRDS(SVCpred_confidence, here("./outputs/SVCpred_dredge_CI.rds"))

# read in saved CIs
SVCpred_confidence <- read_rds(here("./outputs/SVCpred_dredge_CI.rds"))


# SVC vs. Roving: Model Plot ===================================================

# The following creates a visual representation of predictors in the top models
# comparing SVC and roving survey density differences. Predictor values are 
# displayed along with their confidence intervals and significance. 

# select conditional coefficient estimates 
SVCpred_model_avg_plot<-as.data.frame(SVCpred_model_avg_summary$coefmat.subset) 

# select confidence intervals 
SVCpred_CI <- as.data.frame(SVCpred_confidence) 

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
SVCpred_model_avg_plot[4,1] <- "Crypsis"

# change order of rows
SVCpred_model_avg_plot$sort <- c(1,3,4,2)
SVCpred_model_avg_plot <- SVCpred_model_avg_plot %>% arrange(sort)
SVCpred_model_avg_plot$Coefficient <- factor(SVCpred_model_avg_plot$Coefficient, levels = SVCpred_model_avg_plot$Coefficient)

# plot with confidence intervals 
SVCpred_coef_CI <- ggplot(data=SVCpred_model_avg_plot[2:4,], 
                          aes(x=Coefficient, y=Estimate))+ 
  geom_hline(yintercept=0, color = "grey40",linetype="dashed", lwd=1.5)+
  geom_errorbar(aes(ymin=CI.min, ymax=CI.max), colour="grey65", 
                width=.2,lwd=1) +
  geom_point(size=7, aes(shape = significance))+ 
  theme_classic(base_size = 20)+ 
  scale_shape_manual(values = c(16,8))+
  ylim(c(-2.2,2.2)) +
  coord_flip() +
  theme(legend.position = "none",
        axis.title = element_text(size = 34),
        axis.text = element_text(size = 30, color = "black"))
ggsave(here("./visuals/SVCpred_coef_plot_CIs.png"), SVCpred_coef_CI)

# plot with adjusted standard error bars
SVCpred_coef_SE <- ggplot(data=SVCpred_model_avg_plot[2:4,], 
                          aes(x=Coefficient, y=Estimate))+ 
  geom_point(size=5, aes(shape = significance))+ 
  theme_classic(base_size = 20)+ 
  scale_shape_manual(values = c(16,8))+
  geom_errorbar(aes(ymin=Estimate-AdjustedSE, ymax=Estimate+AdjustedSE), 
                colour="grey65", 
                width=.2, lwd=1) +
  geom_hline(yintercept=0, color = "grey40",linetype="dashed", lwd=1.5)+
  coord_flip() +
  theme(legend.position = "none")
ggsave(here("./visuals/SVCpred_coef_plot_SEs.png"), SVCpred_coef_SE)


# SVC vs. Roving: Global Model Re-Do ###########################################

SVCpred_model_data$size_bin_lengths <- as.character(SVCpred_model_data$size_bin_lengths)

# boxplot showing singularity in size*coloration
ggplot(SVCpred_model_data, aes(colouration, log_difference, 
                                               fill = size_bin_lengths)) + 
  geom_boxplot(show.legend = TRUE) + 
  theme_classic() + 
  xlab("Coloration") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 30)) +
  theme(axis.text= element_text(size = 28)) +
  theme(legend.text = element_text(size = 28)) +
  theme(legend.title = element_text(size = 30)) +
  scale_fill_brewer(name = "Size Class", labels = c("1 (0-5cm)", "2 (5-10cm)", "3 (10-15cm)", "4 (15-20cm)", "5 (20-30cm)", "6 (>30cm)"), palette = "YlGnBu") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")

# boxplot showing singularity in size*shape
ggplot(SVCpred_model_data, aes(shape, log_difference, 
                               fill = size_bin_lengths)) + 
  geom_boxplot(show.legend = TRUE) + 
  theme_classic() + 
  xlab("Body Shape") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 30)) +
  theme(axis.text= element_text(size = 28)) +
  theme(legend.text = element_text(size = 28)) +
  theme(legend.title = element_text(size = 30)) +
  scale_fill_brewer(name = "Size Class", labels = c("1 (0-5cm)", "2 (5-10cm)", "3 (10-15cm)", "4 (15-20cm)", "5 (20-30cm)", "6 (>30cm)"), palette = "YlGnBu") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")

# global model
SVCpred_1 <- lme(log_difference~habitat+octocoral+stony+relief_cm+nocturnal+
                      max_length+cryptic_behaviour+average_depth+
                      shape+position, 
                    random = list(~1|site, ~1|species_order), 
                    SVCpred_model_data) 
summary(SVCpred_1) # AIC = 1764.004
AICc(SVCpred_1) # AICc = 1773.015
vif(SVCpred_1) # habitat, depth, shape > 5

# remove habitat from 1
SVCpred_2 <- lme(log_difference~octocoral+stony+relief_cm+nocturnal+
                   max_length+cryptic_behaviour+average_depth+
                   shape+position, 
                 random = list(~1|site, ~1|species_order), 
                 SVCpred_model_data) 
summary(SVCpred_2) # AIC = 1761.087
AICc(SVCpred_2) # AICc = 1770.041
vif(SVCpred_2) # shape = 6.467980

# remove shape from 2
SVCpred_4 <- lme(log_difference~octocoral+stony+relief_cm+nocturnal+
                   max_length+cryptic_behaviour+average_depth+
                   position, 
                 random = list(~1|site, ~1|species_order), 
                 SVCpred_model_data) 
summary(SVCpred_4) # AIC = 1765.492
AICc(SVCpred_4) # AICc = 1769.909
vif(SVCpred_4) # all under 5 

# remove depth from 1
SVCpred_5 <- lme(log_difference~habitat+octocoral+stony+relief_cm+nocturnal+
                   max_length+cryptic_behaviour+
                   shape+position, 
                 random = list(~1|site, ~1|species_order), 
                 SVCpred_model_data) 
summary(SVCpred_5) # AIC = 1753.985
AICc(SVCpred_5) # AICc = 1763.278
vif(SVCpred_5) # shape = 6.450637

# remove shape from 5 
SVCpred_6 <- lme(log_difference~habitat+octocoral+stony+relief_cm+nocturnal+
                   max_length+cryptic_behaviour+
                   position, 
                 random = list(~1|site, ~1|species_order), 
                 SVCpred_model_data) 
summary(SVCpred_6) # AIC = 1762.442
AICc(SVCpred_6) # AICc = 1762.986
vif(SVCpred_6) # all good


# SVC vs. Roving: Dredge Re-Do #################################################

# dredge
SVCpred_dredge_redo <- dredge(SVCpred_6)
SVCpred_dredge_redo

# save dredge results 
saveRDS(SVCpred_dredge_redo, here("./outputs/SVCpred_dredge_redo.rds"))

# subset dredge
SVCpred_dredge_sub_redo <- subset(SVCpred_dredge_redo, delta < 4) 

# model average 
SVCpred_model_average_redo <- model.avg(SVCpred_dredge_sub_redo)
SVCpred_model_avg_summary_redo <- summary(SVCpred_model_average_redo)

# save model average
saveRDS(SVCpred_model_average_redo, here("./outputs/SVCpred_dredge_average_redo.rds"))

# read in saved average
SVCpred_model_average_redo <- read_rds(here("./outputs/SVCpred_dredge_average_redo.rds"))

# covariate confidence intervals
SVCpred_confidence_redo <- confint(SVCpred_model_average_redo)
summary(SVCpred_confidence_redo)

# save confidence intervals
saveRDS(SVCpred_confidence_redo, here("./outputs/SVCpred_dredge_CI_redo.rds"))

# read in saved CIs
SVCpred_confidence_redo <- read_rds(here("./outputs/SVCpred_dredge_CI_redo.rds"))


# SVC vs. Roving: Global Model W/O Crypsis #####################################

SVCpred_model_data$size_bin_lengths <- as.character(SVCpred_model_data$size_bin_lengths)

SVCpred_nocrypsis <- lme(log_difference~habitat+octocoral+stony+relief_cm+nocturnal+
                   max_length+average_depth+
                   shape+position, 
                 random = list(~1|site, ~1|species_order), 
                 SVCpred_model_data) 
summary(SVCpred_nocrypsis) # AIC = 1770.965
AICc(SVCpred_nocrypsis) # AICc = 1771.6
vif(SVCpred_nocrypsis) # habitat, depth > 5

# remove habitat from nocrypsis
SVCpred_nocrypsis2 <- lme(log_difference~octocoral+stony+relief_cm+nocturnal+
                           max_length+average_depth+
                           shape+position, 
                         random = list(~1|site, ~1|species_order), 
                         SVCpred_model_data) 
summary(SVCpred_nocrypsis2) # AIC = 1768.086
AICc(SVCpred_nocrypsis2) # AICc = 1768.63
vif(SVCpred_nocrypsis2) # all good

# remove depth from nocrypsis
SVCpred_nocrypsis3 <- lme(log_difference~habitat+octocoral+stony+relief_cm+nocturnal+
                           max_length+
                           shape+position, 
                         random = list(~1|site, ~1|species_order), 
                         SVCpred_model_data) 
summary(SVCpred_nocrypsis3) # AIC = 1761.36
AICc(SVCpred_nocrypsis3) # AICc = 1761.903
vif(SVCpred_nocrypsis3) # all good


# SVC vs. Roving: Dredge W/O Crypsis ###########################################

# dredge
SVCpred_dredge_nocrypsis <- dredge(SVCpred_nocrypsis3)
SVCpred_dredge_nocrypsis

# save dredge results 
saveRDS(SVCpred_dredge_nocrypsis, here("./outputs/SVCpred_dredge_nocrypsis.rds"))

# subset dredge
SVCpred_dredge_sub_nocrypsis <- subset(SVCpred_dredge_nocrypsis, delta < 4) 

# model average 
SVCpred_model_average_nocrypsis <- model.avg(SVCpred_dredge_sub_nocrypsis)
SVCpred_model_avg_summary_nocrypsis <- summary(SVCpred_model_average_nocrypsis)

# save model average
saveRDS(SVCpred_model_average_redo, here("./outputs/SVCpred_dredge_average_redo.rds"))

# read in saved average
SVCpred_model_average_redo <- read_rds(here("./outputs/SVCpred_dredge_average_redo.rds"))

# covariate confidence intervals
SVCpred_confidence_redo <- confint(SVCpred_model_average_redo)
summary(SVCpred_confidence_redo)

# save confidence intervals
saveRDS(SVCpred_confidence_redo, here("./outputs/SVCpred_dredge_CI_redo.rds"))

# read in saved CIs
SVCpred_confidence_redo <- read_rds(here("./outputs/SVCpred_dredge_CI_redo.rds"))











