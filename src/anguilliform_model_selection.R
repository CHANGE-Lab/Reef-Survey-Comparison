########## SURVEY COMPARISON PROJECT ANGUILLIFORM RESULTS ##########
########## 
##########
# This file creates global linear mixed effects models to compare fish density 
# differences between SVC and transect surveys using species, habitat, and 
# survey traits as predictors. This data includes species of the order 
# Anguilliform which were removed from other analyses. It then performs a dredge 
# on each global model to determine which predictors result in the best model
# fit, based on AICc values. 
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2021-12-8
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
SVCprey_anguilliform_data <- 
  read_csv(here("./dataframes/SVCprey_dataframe_anguilliformes.csv"))


# SVC vs. Transect: Centring Variables =========================================

# The following centres all continuous and dummy variables from the global 
# model by subtracting the sample mean from all variable values.

# octocoral
SVCprey_anguilliform_data$octocoral_c <- SVCprey_anguilliform_data$octocoral - mean(SVCprey_anguilliform_data$octocoral)

# stony coral
SVCprey_anguilliform_data$stony_c <- SVCprey_anguilliform_data$stony - mean(SVCprey_anguilliform_data$stony)

# vertical relief
SVCprey_anguilliform_data$relief_c <- SVCprey_anguilliform_data$relief_cm - mean(SVCprey_anguilliform_data$relief_cm)

# size bins
SVCprey_anguilliform_data$size_bin_c <- SVCprey_anguilliform_data$size_bin_lengths - mean(SVCprey_anguilliform_data$size_bin_lengths)

# maximum length
SVCprey_anguilliform_data$max_length_c <- SVCprey_anguilliform_data$max_length - mean(SVCprey_anguilliform_data$max_length)

# depth
SVCprey_anguilliform_data$depth_c <- SVCprey_anguilliform_data$average_depth - mean(SVCprey_anguilliform_data$average_depth)


# SVC vs. Transect: Anguilliform Global Model ==================================

# In the following, a linear mixed model is created to compare fish density 
# differences between SVC and transect surveys in response to predictors 
# including species traits, habitat traits, and survey traits. All traits of 
# interest are included in the global model. Collinearity is explored through 
# the VIF values for each predictor, and model fit is determined through 
# random effects plots, residual plots, qq plots, and model plots. 

# global lme model
SVCprey_anguilliform_global <- lme(log_difference~habitat+octocoral_c+stony_c+relief_c+
                                     size_bin_c*colouration+nocturnal+position+
                        max_length+behavior+cryptic_behaviour+depth_c+shape, 
                      random = list(~1|site, ~1|species_order), 
                      SVCprey_anguilliform_data) 

# model summary
summary(SVCprey_anguilliform_global) 
AICc(SVCprey_anguilliform_global)

# covariate VIF values
vif(SVCprey_anguilliform_global) 

# remove shape
SVCprey_anguilliform_global2 <- lme(log_difference~habitat+octocoral+stony+relief_cm+
                                     size_bin_lengths*colouration+nocturnal+position+
                                     max_length+behavior+cryptic_behaviour+average_depth, 
                                   random = list(~1|site, ~1|species_order), 
                                   SVCprey_anguilliform_data) 

# model summary
summary(SVCprey_anguilliform_global2) 
AICc(SVCprey_anguilliform_global2)

# covariate VIF values
vif(SVCprey_anguilliform_global2) 

# random effects plot
plot(ranef(SVCprey_anguilliform_global2))

# residuals plot
res_SVCprey_global = residuals(SVCprey_anguilliform_global2)
plot(res_SVCprey_global2) 

# qq plot
qqnorm(res_SVCprey_global2) 
qqline(res_SVCprey_global2)

# model plot
plot(SVCprey_anguilliform_global2) 


# SVC vs. Transect: Anguilliform Dredging ==========================================

# The following performs a dredge on the global linear mixed effects model 
# comparing SVC fish densities to transect survey fish densities in order to 
# determine which combination of predictors results in the most likely model.

# dredge
SVCprey_anguilliform_dredge <- dredge(SVCprey_anguilliform_global2)
SVCprey_anguilliform_dredge

# save dredge output 
saveRDS(SVCprey_anguilliform_dredge, here("./outputs/SVCprey_anguilliform_dredge.rds"))

# subset dredge
SVCprey_anguil_dredge_sub <- subset(SVCprey_anguilliform_dredge, delta < 4) 

# model average 
SVCprey_anguil_average <- model.avg(SVCprey_anguil_dredge_sub)
SVCprey_anguil_avg_summary <- summary(SVCprey_anguil_average)

# save model average
saveRDS(SVCprey_anguil_average, here("./outputs/SVCprey_anguil_average.rds"))
# SVCprey_model_average <- read_rds(here("./outputs/SVCprey_drege_average.rds"))
# SVCprey_model_avg_summary <- summary(SVCprey_model_average)

# confidence intervals of predictors
SVCprey_anguil_confidence <- confint(SVCprey_anguil_average)

# save confidence intervals
saveRDS(SVCprey_anguil_confidence, here("./outputs/SVCprey_anguil_dredge_CI.rds"))
# SVCprey_confidence <- read_rds(here("./outputs/SVCprey_dredge_CI.rds"))


# SVC vs. Transect: Model Plot =================================================

# The following creates a visual representation of predictors in the top models
# comparing SVC and transect survey density differences. Predictor values are 
# displayed along with their confidence intervals and significance. 

# select conditional coefficient estimates 
SVCprey_model_avg_plot<-as.data.frame(SVCprey_anguil_avg_summary$coefmat.subset) 

# select confidence intervals 
SVCprey_CI <- as.data.frame(SVCprey_anguil_confidence) 

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