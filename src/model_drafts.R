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


# SVC vs. Transect: Global Model (categorical size bin) ===============================================

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


# SVC vs. Transect: Dredging (categorical size bin)===================================================

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


# SVC vs. Transect: Model Plot (categorical size class)=================================================

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
SVCprey_model_avg_plot[7,1] = "Patch"
SVCprey_model_avg_plot[8,1] = "Max. length"
SVCprey_model_avg_plot[9,1] = "Octocoral cover"
SVCprey_model_avg_plot[10,1] = "Elongated"
SVCprey_model_avg_plot[11,1] = "Fusiform"
SVCprey_model_avg_plot[12,1] = "Globiform"
SVCprey_model_avg_plot[13,1] = "Size class 4"
SVCprey_model_avg_plot[14,1] = "Size class 5"
SVCprey_model_avg_plot[15,1] = "Size class 1"
SVCprey_model_avg_plot[16,1] = "Size class 6"
SVCprey_model_avg_plot[17,1] = "Size class 2"
SVCprey_model_avg_plot[18,1] = "Coral cover"
SVCprey_model_avg_plot[19,1] = "Colorful:SC 4"
SVCprey_model_avg_plot[20,1] = "Neutral:SC 4"
SVCprey_model_avg_plot[21,1] = "Silvering:SC 4"
SVCprey_model_avg_plot[22,1] = "Colorful:SC 5"
SVCprey_model_avg_plot[23,1] = "Neutral:SC 5"
SVCprey_model_avg_plot[24,1] = "Silvering:SC 5"
SVCprey_model_avg_plot[25,1] = "Colorful:SC 1"
SVCprey_model_avg_plot[26,1] = "Neutral:SC 1"
SVCprey_model_avg_plot[27,1] = "Silvering:SC 1"
SVCprey_model_avg_plot[28,1] = "Colorful:SC 6"
SVCprey_model_avg_plot[29,1] = "Neutral:SC 6"
SVCprey_model_avg_plot[30,1] = "Silvering:SC 6"
SVCprey_model_avg_plot[31,1] = "Colorful:SC 2"
SVCprey_model_avg_plot[32,1] = "Neutral:SC 2"
SVCprey_model_avg_plot[33,1] = "Silvering:SC 2"
SVCprey_model_avg_plot[34,1] = "Elongated:SC 4"
SVCprey_model_avg_plot[35,1] = "Fusiform:SC 4"
SVCprey_model_avg_plot[36,1] = "Globiform:SC 4"
SVCprey_model_avg_plot[37,1] = "Elongated:SC 5"
SVCprey_model_avg_plot[38,1] = "Fusiform:SC 5"
SVCprey_model_avg_plot[39,1] = "Globiform:SC 5"
SVCprey_model_avg_plot[40,1] = "Elongated:SC 1"
SVCprey_model_avg_plot[41,1] = "Fusiform:SC 1"
SVCprey_model_avg_plot[42,1] = "Globiform:SC 1"
SVCprey_model_avg_plot[43,1] = "Elongated:SC 6"
SVCprey_model_avg_plot[44,1] = "Fusiform:SC 6"
SVCprey_model_avg_plot[45,1] = "Globiform:SC 6"
SVCprey_model_avg_plot[46,1] = "Elongated:SC 2" 
SVCprey_model_avg_plot[47,1] = "Fusiform:SC 2"
SVCprey_model_avg_plot[48,1] = "Globiform:SC 2"
SVCprey_model_avg_plot[49,1] = "Nocturnality"
SVCprey_model_avg_plot[50,1] = "Demersal"
SVCprey_model_avg_plot[51,1] = "Crypsis"

# change order of rows
SVCprey_model_avg_plot$sort <- c(20,18,19,15,17,16,6,3,5,2,14,13,12,4,1,11,10,9,7,8,21,22,23,24,25,262,7,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51)
SVCprey_model_avg_plot <- SVCprey_model_avg_plot %>% arrange(sort)
SVCprey_model_avg_plot$Coefficient <- factor(SVCprey_model_avg_plot$Coefficient, levels = SVCprey_model_avg_plot$Coefficient)

# change order of rows
SVCprey_model_avg_plot$sort <- c(1,18,9,7,28,30,29,22,24,23,19,21,20,31,33,32,25,27,26,4,6,5,45,44,43,39,38,37,36,35,34,48,47,46,42,41,40,12,11,10,16,14,13,17,15,50,49,8,51,2,3)
SVCprey_model_avg_plot <- SVCprey_model_avg_plot %>% arrange(sort)
SVCprey_model_avg_plot$Coefficient <- factor(SVCprey_model_avg_plot$Coefficient, levels = SVCprey_model_avg_plot$Coefficient)

SVCprey_model_avg_plot <- SVCprey_model_avg_plot[c(18,9,7,28,30,29,22,24,23,19,21,20,31,33,32,25,27,26,4,6,5,45,44,43,39,38,37,36,35,34,48,47,46,42,41,40,12,11,10,16,14,13,17,15,50,49,8,2,3,51),]

# plot with confidence intervals 
SVCprey_coef_CI <- ggplot(data=SVCprey_model_avg_plot[1:50], 
                          aes(x=Coefficient, y=Estimate))+ 
  geom_hline(yintercept=0, color = "grey40",linetype="dashed", lwd=1.5)+
  geom_errorbar(aes(ymin=CI.min, ymax=CI.max), colour="grey65", 
                width=.2,lwd=1) +
  geom_point(size=5, aes(shape = significance))+ 
  theme_classic(base_size = 20)+ 
  scale_shape_manual(values = c(16,8))+
  ylim(c(-5.2, 5.6)) +
  coord_flip() +
  theme(legend.position = "none",
        axis.title = element_text(size = 30),
        axis.text = element_text(size = 16, color = "black")) +
  scale_x_discrete(limits=SVCprey_model_avg_plot$Coefficient)
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


# SVC vs. Transect: Global Model (continuous size bin) ===============================================

# In the following, a linear mixed model is created to compare fish density 
# differences between SVC and transect surveys in response to predictors 
# including species traits, habitat traits, and survey traits. All traits of 
# interest are included in the global model. Collinearity is explored through 
# the VIF values for each predictor, and model fit is determined through 
# random effects plots, residual plots, qq plots, and model plots. 

# data
SVCprey_model_data <- read_csv(here("./dataframes/SVCprey_dataframe.csv"))
SVCpred_model_data <- read_csv(here("./dataframes/SVCpred_dataframe.csv"))


# global lme model
SVCprey_global_cont <- lme(log_difference~habitat+octocoral+stony+relief_cm+
                        size_bin_lengths*colouration+nocturnal+position+
                        max_length+behavior+cryptic_behaviour+average_depth+
                        size_bin_lengths*shape, 
                      random = list(~1|site, ~1|species_order), 
                      SVCprey_model_data) 

# model summary
summary(SVCprey_global_cont) 
AICc(SVCprey_globa_cont)

# covariate VIF values
vif(SVCprey_global_cont) # aggregation behaviour VIF = 5.257051


# SVC vs. Transect: Dredging (size bin continuous)===================================================

# The following performs a dredge on the global linear mixed effects model 
# comparing SVC fish densities to transect survey fish densities in order to 
# determine which combination of predictors results in the most likely model.

# dredge
SVCprey_dredge_cont <- dredge(SVCprey_global_cont)
SVCprey_dredge_cont

# save dredge output 
saveRDS(SVCprey_dredge_cont, here("./outputs/SVCprey_global_dredge_cont.rds"))
SVCprey_dredge_cont <- read_rds(here("./outputs/SVCprey_global_dredge_cont.rds"))

# subset dredge
SVCprey_dredge_sub_cont <- subset(SVCprey_dredge_cont, delta < 4) 

# model average 
SVCprey_model_average_cont <- model.avg(SVCprey_dredge_sub_cont)
SVCprey_model_avg_summary_cont <- summary(SVCprey_model_average_cont)

# save model average
saveRDS(SVCprey_model_average_cont, here("./outputs/SVCprey_drege_average_cont.rds"))

# read in saved average and summary
SVCprey_model_average_cont <- read_rds(here("./outputs/SVCprey_drege_average_cont.rds"))
SVCprey_model_avg_summary_cont <- summary(SVCprey_model_average_cont)

# confidence intervals of predictors
SVCprey_confidence_cont <- confint(SVCprey_model_average_cont)

# save confidence intervals
saveRDS(SVCprey_confidence_cont, here("./outputs/SVCprey_dredge_CI_cont.rds"))

# read in saved CIs
SVCprey_confidence_cont <- read_rds(here("./outputs/SVCprey_dredge_CI_cont.rds"))


# SVC vs. Transect: Model Plot (continuous size class) =================================================

# The following creates a visual representation of predictors in the top models
# comparing SVC and transect survey density differences. Predictor values are 
# displayed along with their confidence intervals and significance. 

# select conditional coefficient estimates 
SVCprey_model_avg_plot_cont <- as.data.frame(SVCprey_model_avg_summary_cont$coefmat.subset) 

# select confidence intervals 
SVCprey_CI_cont <- as.data.frame(SVCprey_confidence_cont) 

# put confidence intervals into coefficient dataframe
SVCprey_model_avg_plot_cont$CI.min <-SVCprey_CI_cont$`2.5 %` 
SVCprey_model_avg_plot_cont$CI.max <-SVCprey_CI_cont$`97.5 %`

# put rownames in column
setDT(SVCprey_model_avg_plot_cont, keep.rownames = "Coefficient") 

# remove spaces from column headers
names(SVCprey_model_avg_plot_cont) <- gsub(" ", "", names(SVCprey_model_avg_plot_cont)) 

# add binary significance column
SVCprey_model_avg_plot_cont$significance <- 
  ifelse(SVCprey_model_avg_plot_cont$`Pr(>|z|)` < 0.05, "sig", "nonsig")

# change names of coefficients
SVCprey_model_avg_plot_cont[2,1] = "Shoaling"
SVCprey_model_avg_plot_cont[3,1] = "Solitary"
SVCprey_model_avg_plot_cont[4,1] = "Colorful"
SVCprey_model_avg_plot_cont[5,1] = "Neutral"
SVCprey_model_avg_plot_cont[6,1] = "Silvering"
SVCprey_model_avg_plot_cont[7,1] = "Patch"
SVCprey_model_avg_plot_cont[8,1] = "Max. length"
SVCprey_model_avg_plot_cont[9,1] = "Octocoral cover"
SVCprey_model_avg_plot_cont[10,1] = "Elongated"
SVCprey_model_avg_plot_cont[11,1] = "Fusiform"
SVCprey_model_avg_plot_cont[12,1] = "Globiform"
SVCprey_model_avg_plot_cont[13,1] = "Size Class"
SVCprey_model_avg_plot_cont[14,1] = "Stony Coral"
SVCprey_model_avg_plot_cont[15,1] = "Elongated:Size Class"
SVCprey_model_avg_plot_cont[16,1] = "Fusiform:Size Class"
SVCprey_model_avg_plot_cont[17,1] = "Globiform:Size Class"
SVCprey_model_avg_plot_cont[18,1] = "Colorful:Size Class"
SVCprey_model_avg_plot_cont[19,1] = "Neutral:Size Class"
SVCprey_model_avg_plot_cont[20,1] = "Silvering:Size Class"
SVCprey_model_avg_plot_cont[21,1] = "Crypsis"
SVCprey_model_avg_plot_cont[22,1] = "Demersal"

# change order of rows
SVCprey_model_avg_plot_cont <- SVCprey_model_avg_plot_cont[c(14,9,7,18,20,19,4,6,5,17,16,15,12,11,10,13,22,8,2,3,21,1),]

# plot with confidence intervals 
SVCprey_coef_CI_cont <- ggplot(data=SVCprey_model_avg_plot_cont[1:22], 
                          aes(x=Coefficient, y=Estimate))+ 
  geom_hline(yintercept=0, color = "grey40",linetype="dashed", lwd=1.5)+
  geom_errorbar(aes(ymin=CI.min, ymax=CI.max), colour="grey65", 
                width=.2,lwd=1) +
  geom_point(size=7, aes(shape = significance))+ 
  theme_classic(base_size = 20)+ 
  scale_shape_manual(values = c(16,8))+
  ylim(c(-2.5, 2)) +
  coord_flip() +
  theme(legend.position = "none",
        axis.title = element_text(size = 30),
        axis.text = element_text(size = 16, color = "black")) +
  scale_x_discrete(limits=SVCprey_model_avg_plot_cont$Coefficient)
ggsave(here("./visuals/SVCprey_coef_plot_CIs_cont.png"), SVCprey_coef_CI_cont)


# SVC vs. Transect: Creating Dummy Variables ===================================

# The following creates dummy variables for all categorical predictors included
# in the top global model.

# habitat:
levels(factor(SVCprey_model_data$habitat))

# create dummy variable for continuous
SVCprey_model_data <- SVCprey_model_data %>%
  mutate(continuous = if_else(habitat == "Continuous", 1, 0))

# create dummy variable for patch
SVCprey_model_data <- SVCprey_model_data %>%
  mutate(patch = if_else(habitat == "Patch", 1, 0))

# colouration:
levels(factor(SVCprey_model_data$colouration))

# create dummy variable for camouflage 
SVCprey_model_data <- SVCprey_model_data %>%
  mutate(camouflage = if_else(colouration == "camouflage", 1, 0))

# create dummy variable for colourful
SVCprey_model_data <- SVCprey_model_data %>%
  mutate(colourful = if_else(colouration == "colourful", 1, 0))

# create dummy variable for neutral 
SVCprey_model_data <- SVCprey_model_data %>%
  mutate(neutral = if_else(colouration == "neutral", 1, 0))

# create dummy variable for silvering
SVCprey_model_data <- SVCprey_model_data %>%
  mutate(silvering = if_else(colouration == "silvering", 1, 0))

# position:
levels(factor(SVCprey_model_data$position))

# create dummy variable for benthic
SVCprey_model_data <- SVCprey_model_data %>%
  mutate(benthic = if_else(position == "benthic", 1, 0))

# create dummy variable for demersal
SVCprey_model_data <- SVCprey_model_data %>%
  mutate(demersal = if_else(position == "demersal", 1, 0))

# behavior:
levels(factor(SVCprey_model_data$behavior))

# create dummy variable for schooling
SVCprey_model_data <- SVCprey_model_data %>%
  mutate(schooling = if_else(behavior == "schooling", 1, 0))

# create dummy variable for shoaling
SVCprey_model_data <- SVCprey_model_data %>%
  mutate(shoaling = if_else(behavior == "shoaling", 1, 0))

# create dummy variable for solitary
SVCprey_model_data <- SVCprey_model_data %>%
  mutate(solitary = if_else(behavior == "solitary", 1, 0))

# shape: 
levels(factor(SVCprey_model_data$shape))

# create dummy variable for compressiform
SVCprey_model_data <- SVCprey_model_data %>%
  mutate(compressiform = if_else(shape == "compressiform", 1, 0))

# create dummy variable for elongated
SVCprey_model_data <- SVCprey_model_data %>%
  mutate(elongated = if_else(shape == "elongated", 1, 0))

# create dummy variable for fusiform
SVCprey_model_data <- SVCprey_model_data %>%
  mutate(fusiform = if_else(shape == "fusiform", 1, 0))

# create dummy variable for globiform
SVCprey_model_data <- SVCprey_model_data %>%
  mutate(globiform = if_else(shape == "globiform", 1, 0))


# SVC vs. Transect: Centring Variables =========================================

# The following centres all continuous and dummy variables from the global 
# model by subtracting the sample mean from all variable values.

# habitat dummy variables
SVCprey_model_data$continuous_c <- SVCprey_model_data$continuous - mean(SVCprey_model_data$continuous)
SVCprey_model_data$patch_c <- SVCprey_model_data$patch - mean(SVCprey_model_data$patch)

# colouration dummy variables
SVCprey_model_data$camouflage_c <- SVCprey_model_data$camouflage - mean(SVCprey_model_data$camouflage)
SVCprey_model_data$colourful_c <- SVCprey_model_data$colourful - mean(SVCprey_model_data$colourful)
SVCprey_model_data$neutral_c <- SVCprey_model_data$neutral - mean(SVCprey_model_data$neutral)
SVCprey_model_data$silvering_c <- SVCprey_model_data$silvering - mean(SVCprey_model_data$silvering)

# position dummy variables
SVCprey_model_data$benthic_c <- SVCprey_model_data$benthic - mean(SVCprey_model_data$benthic)
SVCprey_model_data$demersal_c <- SVCprey_model_data$demersal - mean(SVCprey_model_data$demersal)

# behavior dummy variables
SVCprey_model_data$schooling_c <- SVCprey_model_data$schooling - mean(SVCprey_model_data$schooling)
SVCprey_model_data$shoaling_c <- SVCprey_model_data$shoaling - mean(SVCprey_model_data$shoaling)
SVCprey_model_data$solitary_c <- SVCprey_model_data$solitary - mean(SVCprey_model_data$solitary)

# shape dummy variables
SVCprey_model_data$compressiform_c <- SVCprey_model_data$compressiform - mean(SVCprey_model_data$compressiform)
SVCprey_model_data$elongated_c <- SVCprey_model_data$elongated - mean(SVCprey_model_data$elongated)
SVCprey_model_data$fusiform_c <- SVCprey_model_data$fusiform - mean(SVCprey_model_data$fusiform)
SVCprey_model_data$globiform_c <- SVCprey_model_data$globiform - mean(SVCprey_model_data$globiform)

# octocoral
SVCprey_model_data$octocoral_c <- SVCprey_model_data$octocoral - mean(SVCprey_model_data$octocoral)

# stony coral
SVCprey_model_data$stony_c <- SVCprey_model_data$stony - mean(SVCprey_model_data$stony)

# vertical relief
SVCprey_model_data$relief_c <- SVCprey_model_data$relief_cm - mean(SVCprey_model_data$relief_cm)

# size bins
SVCprey_model_data$size_bin_c <- SVCprey_model_data$size_bin_lengths - mean(SVCprey_model_data$size_bin_lengths)

# nocturnality
SVCprey_model_data$nocturnal_c <- SVCprey_model_data$nocturnal - mean(SVCprey_model_data$nocturnal)

# maximum length
SVCprey_model_data$max_length_c <- SVCprey_model_data$max_length - mean(SVCprey_model_data$max_length)

# crypsis
SVCprey_model_data$crypsis_c <- SVCprey_model_data$cryptic_behaviour - mean(SVCprey_model_data$cryptic_behaviour)

# depth
SVCprey_model_data$depth_c <- SVCprey_model_data$average_depth - mean(SVCprey_model_data$average_depth)


# SVC vs. Transect: Global Model (centred variables)############################

# The following creates the SVC vs. Transect survey global model utilizing the 
# best fit global model obtained prior to centring.

# note that continuous reefs, camouflage, benthic, schooling, and compressiform
# have been removed as levels in the model to avoid multicollinearity.

# global lme model
SVCprey_global_c <- lme(log_difference~patch_c+octocoral_c+stony_c+relief_c+
                             size_bin_c*colourful_c+
                             size_bin_c*neutral_c+
                             size_bin_c*silvering_c+
                             nocturnal_c+demersal_c+
                             max_length_c+shoaling_c+solitary_c+crypsis_c+depth_c+
                             size_bin_c*elongated_c+
                             size_bin_c*fusiform_c+
                             size_bin_c*globiform_c, 
                           random = list(~1|site, ~1|species_order), 
                           SVCprey_model_data) 

# model summary
summary(SVCprey_global_c) 
AICc(SVCprey_global_c)

# covariate VIF values
vif(SVCprey_global_c) # aggregation behaviour VIF = 5.257051


# SVC vs. Transect: Global Model (centred continuous variables)#################

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
AICc(SVCprey_global_c) # 33673.67

# covariate VIF values
vif(SVCprey_global_c) # aggregation behaviour VIF = 5.257051

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


# SVC vs. Transect: Global Model (centred continuous variables and no log transformation)#################

# The following creates the SVC vs. Transect survey global model utilizing the 
# best fit global model obtained prior to centring.

# calculate base density difference
SVCprey_model_data$density_difference <- (SVCprey_model_data$SVC_density)-(SVCprey_model_data$prey_density)

# global lme model
SVCprey_global_c2 <- lme(density_difference~habitat+octocoral_c+stony_c+relief_c+
                          size_bin_c*colouration+nocturnal+position+
                          max_length_c+behavior+cryptic_behaviour+depth_c+
                          size_bin_c*shape, 
                        random = list(~1|site, ~1|species_order), 
                        SVCprey_model_data) 

# model summary
summary(SVCprey_global_c2) 
AICc(SVCprey_global_c2) # 33673.67

# covariate VIF values
vif(SVCprey_global_c2) # aggregation behaviour VIF = 5.257051

# random effects plot
plot(ranef(SVCprey_global_c2))

# residuals plot
res_SVCprey_global_c2 = residuals(SVCprey_global_c2)
plot(res_SVCprey_global_c2) 

# qq plot
qqnorm(res_SVCprey_global_c2) 
qqline(res_SVCprey_global_c2)

# model plot
plot(SVCprey_global_c2) 


# SVC vs. Transect: Dredging (centred continuous variables)=====================

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
saveRDS(SVCprey_model_average_c, here("./outputs/SVCprey_drege_average_centred.rds"))

# read in saved average and summary
SVCprey_model_average_c <- read_rds(here("./outputs/SVCprey_drege_average_centred.rds"))
SVCprey_model_avg_summary_c <- summary(SVCprey_model_average_c)

# confidence intervals of predictors
SVCprey_confidence_c <- confint(SVCprey_model_average_c)

# save confidence intervals
saveRDS(SVCprey_confidence_c, here("./outputs/SVCprey_dredge_CI_centred.rds"))

# read in saved CIs
SVCprey_confidence_c <- read_rds(here("./outputs/SVCprey_dredge_CI_centred.rds"))



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


# SVC vs. Roving: Global Model Re-Do (size class categorical)###########################################

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

# boxplot showing singularity in aggregation behavior
ggplot(SVCpred_model_data, aes(behavior, log_difference)) + 
  geom_boxplot(show.legend = TRUE) + 
  theme_classic() + 
  xlab("Aggregation Behavior") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 30)) +
  theme(axis.text= element_text(size = 28)) +
  theme(legend.text = element_text(size = 28)) +
  theme(legend.title = element_text(size = 30)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")


# global model
SVCpred_1 <- SVCpred_full <- lme(log_difference~habitat+octocoral+stony+relief_cm+nocturnal+
                                   max_length+cryptic_behaviour+average_depth+colouration+
                                   size_bin_lengths+shape+position, 
                                 random = list(~1|site, ~1|species_order), 
                                 SVCpred_model_data) 
summary(SVCpred_1) # AIC = 1764.004
AICc(SVCpred_1) # AICc = 1766.115
vif(SVCpred_1) # habitat, max length, depth, coloration, shape, position > 5

# remove depth and max length
SVCpred_2 <- SVCpred_full <- lme(log_difference~habitat+octocoral+stony+relief_cm+nocturnal+
                                   cryptic_behaviour+colouration+
                                   size_bin_lengths+shape+position, 
                                 random = list(~1|site, ~1|species_order), 
                                 SVCpred_model_data) 
summary(SVCpred_2) # AIC = 1764.004
AICc(SVCpred_2) # AICc = 1746.184
vif(SVCpred_2) # coloration & position > 5

# remove position from 2
SVCpred_3 <- SVCpred_full <- lme(log_difference~habitat+octocoral+stony+relief_cm+nocturnal+
                                   cryptic_behaviour+colouration+
                                   size_bin_lengths+shape, 
                                 random = list(~1|site, ~1|species_order), 
                                 SVCpred_model_data) 
summary(SVCpred_3) # AIC = 1764.004
AICc(SVCpred_3) # AICc = 1743.824
vif(SVCpred_3) # all good

# remove coloration from 2
SVCpred_4 <- SVCpred_full <- lme(log_difference~habitat+octocoral+stony+relief_cm+nocturnal+
                                   cryptic_behaviour+
                                   size_bin_lengths+shape+position, 
                                 random = list(~1|site, ~1|species_order), 
                                 SVCpred_model_data) 
summary(SVCpred_4) # AIC = 1764.004
AICc(SVCpred_4) # AICc = 1748.347
vif(SVCpred_4) # all good


# SVC vs. Roving: Dredge Re-Do (size class categorical)#################################################

# dredge
SVCpred_dredge_redo <- dredge(SVCpred_3)
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
SVCpred_model_avg_summary_redo <- summary(SVCpred_model_average_redo)

# covariate confidence intervals
SVCpred_confidence_redo <- confint(SVCpred_model_average_redo)
summary(SVCpred_confidence_redo)

# save confidence intervals
saveRDS(SVCpred_confidence_redo, here("./outputs/SVCpred_dredge_CI_redo.rds"))

# read in saved CIs
SVCpred_confidence_redo <- read_rds(here("./outputs/SVCpred_dredge_CI_redo.rds"))


# SVC vs. Roving: Model Plot Re-Do (size class categorical)===================================================

# The following creates a visual representation of predictors in the top models
# comparing SVC and roving survey density differences. Predictor values are 
# displayed along with their confidence intervals and significance. 

# select conditional coefficient estimates 
SVCpred_model_avg_plot<-as.data.frame(SVCpred_model_avg_summary_redo$coefmat.subset) 

# select confidence intervals 
SVCpred_CI <- as.data.frame(SVCpred_confidence_redo) 

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
SVCpred_model_avg_plot[4,1] <- "Size Class 4"
SVCpred_model_avg_plot[5,1] <- "Size Class 5"
SVCpred_model_avg_plot[6,1] <- "Size Class 1"
SVCpred_model_avg_plot[7,1] <- "Size Class 6"
SVCpred_model_avg_plot[8,1] <- "Size Class 2"
SVCpred_model_avg_plot[9,1] <- "Fusiform"
SVCpred_model_avg_plot[10,1] <- "Crypsis"
SVCpred_model_avg_plot[11,1] <- "Nocturnality"

# change order of rows
SVCpred_model_avg_plot <- SVCpred_model_avg_plot[c(7,5,4,8,6,3,2,9,11,10),]
#SVCpred_model_avg_plot$sort <- c(1,7,5,4,8,6,3,2,9,11,10)
#SVCpred_model_avg_plot <- SVCpred_model_avg_plot %>% arrange(sort)
#SVCpred_model_avg_plot$Coefficient <- factor(SVCpred_model_avg_plot$Coefficient, levels = SVCpred_model_avg_plot$Coefficient)

# plot with confidence intervals 
SVCpred_coef_CI <- ggplot(data=SVCpred_model_avg_plot[1:10,], 
                          aes(x=Coefficient, y=Estimate))+ 
  geom_hline(yintercept=0, color = "grey40",linetype="dashed", lwd=1.5)+
  geom_errorbar(aes(ymin=CI.min, ymax=CI.max), colour="grey65", 
                width=.2,lwd=1) +
  geom_point(size=5, aes(shape = significance))+ 
  theme_classic(base_size = 20)+ 
  scale_shape_manual(values = c(16,8))+
  ylim(c(-2.2,2.2)) +
  coord_flip() +
  theme(legend.position = "none",
        axis.title = element_text(size = 34),
        axis.text = element_text(size = 30, color = "black")) +
  scale_x_discrete(limits=SVCpred_model_avg_plot$Coefficient)
ggsave(here("./visuals/SVCpred_coef_plot_CIs.png"), SVCpred_coef_CI)


# SVC vs. Roving: Global Model Re-Do (size class continuous)###########################################

# data
SVCprey_model_data <- read_csv(here("./dataframes/SVCprey_dataframe.csv"))
SVCpred_model_data <- read_csv(here("./dataframes/SVCpred_dataframe.csv"))

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
SVCpred_5 <- lme(log_difference~habitat+octocoral+stony+relief_cm+nocturnal+
                      max_length+cryptic_behaviour+average_depth+colouration+
                      size_bin_lengths+shape+position, 
                    random = list(~1|site, ~1|species_order), 
                    SVCpred_model_data) 
summary(SVCpred_5) # AIC = 1764.004
AICc(SVCpred_5) # AICc =1776.148
vif(SVCpred_5) # habitat, depth, shape, max length, coloration, position > 5

# remove depth and max length
SVCpred_6 <- lme(log_difference~octocoral+stony+relief_cm+nocturnal+
                   cryptic_behaviour+colouration+
                   size_bin_lengths+shape+position, 
                 random = list(~1|site, ~1|species_order), 
                 SVCpred_model_data) 
summary(SVCpred_6) # AIC = 1761.087
AICc(SVCpred_6) # AICc = 1752.796
vif(SVCpred_6) # coloration & position > 5 

# remove position from 6
SVCpred_7 <- lme(log_difference~octocoral+stony+relief_cm+nocturnal+
                   cryptic_behaviour+colouration+
                   size_bin_lengths+shape, 
                 random = list(~1|site, ~1|species_order), 
                 SVCpred_model_data) 
summary(SVCpred_7) # AIC = 1765.492
AICc(SVCpred_7) # AICc = 1750.611
vif(SVCpred_7) # all good 

# remove coloration from 6
SVCpred_8 <- lme(log_difference~octocoral+stony+relief_cm+nocturnal+
                   cryptic_behaviour+
                   size_bin_lengths+shape+position, 
                 random = list(~1|site, ~1|species_order), 
                 SVCpred_model_data) 
summary(SVCpred_8) # AIC = 1753.985
AICc(SVCpred_8) # AICc = 1756.377
vif(SVCpred_8) # all good


# SVC vs. Roving: Dredge Re-Do (size class cotinuous)#################################################

# dredge
SVCpred_dredge_redo_cont <- dredge(SVCpred_7)
SVCpred_dredge_redo_cont

# save dredge results 
saveRDS(SVCpred_dredge_redo_cont, here("./outputs/SVCpred_dredge_redo_cont.rds"))

# subset dredge
SVCpred_dredge_sub_redo_cont <- subset(SVCpred_dredge_redo_cont, delta < 4) 

# model average 
SVCpred_model_average_redo_cont <- model.avg(SVCpred_dredge_sub_redo_cont)
SVCpred_model_avg_summary_redo_cont <- summary(SVCpred_model_average_redo_cont)

# save model average
saveRDS(SVCpred_model_average_redo_cont, here("./outputs/SVCpred_dredge_average_redo_cont.rds"))

# read in saved average
SVCpred_model_average_redo_cont <- read_rds(here("./outputs/SVCpred_dredge_average_redo_cont.rds"))
SVCpred_model_avg_summary_redo_cont <- summary(SVCpred_model_average_redo_cont)

# covariate confidence intervals
SVCpred_confidence_redo_cont <- confint(SVCpred_model_average_redo_cont)
summary(SVCpred_confidence_redo_cont)

# save confidence intervals
saveRDS(SVCpred_confidence_redo_cont, here("./outputs/SVCpred_dredge_CI_redo.rds_cont"))

# read in saved CIs
SVCpred_confidence_redo_cont <- read_rds(here("./outputs/SVCpred_dredge_CI_redo_cont.rds"))


# SVC vs. Roving: Model Plot Re-Do (size class continuous)===================================================

# The following creates a visual representation of predictors in the top models
# comparing SVC and roving survey density differences. Predictor values are 
# displayed along with their confidence intervals and significance. 

# select conditional coefficient estimates 
SVCpred_model_avg_plot_cont<-as.data.frame(SVCpred_model_avg_summary_redo_cont$coefmat.subset) 

# select confidence intervals 
SVCpred_CI_cont <- as.data.frame(SVCpred_confidence_redo_cont) 

# put confidence intervals into coefficient dataframe
SVCpred_model_avg_plot_cont$CI.min <-SVCpred_CI_cont$`2.5 %` 
SVCpred_model_avg_plot_cont$CI.max <-SVCpred_CI_cont$`97.5 %`

# put rownames in column
setDT(SVCpred_model_avg_plot_cont, keep.rownames = "Coefficient") 

# remove spaces from column headers
names(SVCpred_model_avg_plot_cont) <- gsub(" ", "", names(SVCpred_model_avg_plot_cont)) 

# add binary significance column
SVCpred_model_avg_plot_cont$significance <- 
  ifelse(SVCpred_model_avg_plot_cont$`Pr(>|z|)` < 0.05, "sig", "nonsig")

# change labels
SVCpred_model_avg_plot_cont[2,1] <- "Neutral"
SVCpred_model_avg_plot_cont[3,1] <- "Silvering"
SVCpred_model_avg_plot_cont[4,1] <- "Fusiform"

# change order of rows
SVCpred_model_avg_plot_cont <- SVCpred_model_avg_plot_cont[c(3,2,4,1),]
#SVCpred_model_avg_plot$sort <- c(1,7,5,4,8,6,3,2,9,11,10)
#SVCpred_model_avg_plot <- SVCpred_model_avg_plot %>% arrange(sort)
#SVCpred_model_avg_plot$Coefficient <- factor(SVCpred_model_avg_plot$Coefficient, levels = SVCpred_model_avg_plot$Coefficient)

# plot with confidence intervals 
SVCpred_coef_CI <- ggplot(data=SVCpred_model_avg_plot_cont[1:4,], 
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
        axis.text = element_text(size = 30, color = "black")) +
  scale_x_discrete(limits=SVCpred_model_avg_plot_cont$Coefficient)
ggsave(here("./visuals/SVCpred_coef_plot_CIs.png"), SVCpred_coef_CI_cont)


# SVC vs. Roving: Creating Dummy Variables =====================================

# The following creates dummy variables for all categorical predictors included
# in the top global model.

# colouration:
levels(factor(SVCpred_model_data$colouration))

# create dummy variable for camouflage 
SVCpred_model_data <- SVCpred_model_data %>%
  mutate(camouflage = if_else(colouration == "camouflage", 1, 0))

# create dummy variable for neutral
SVCpred_model_data <- SVCpred_model_data %>%
  mutate(neutral = if_else(colouration == "neutral", 1, 0))

# create dummy variable for silvering
SVCpred_model_data <- SVCpred_model_data %>%
  mutate(silvering = if_else(colouration == "silvering", 1, 0))

# shape: 
levels(factor(SVCpred_model_data$shape))

# create dummy variable for anguilliform
SVCpred_model_data <- SVCpred_model_data %>%
  mutate(anguilliform = if_else(shape == "anguilliform", 1, 0))

# create dummy variable for fusiform
SVCpred_model_data <- SVCpred_model_data %>%
  mutate(fusiform = if_else(shape == "fusiform", 1, 0))


# SVC vs. Roving: Centring Variables ===========================================

# The following centres all continuous and dummy variables from the global 
# model by subtracting the sample mean from all variable values.

# colouration dummy variables
SVCpred_model_data$camouflage_c <- SVCpred_model_data$camouflage - mean(SVCpred_model_data$camouflage)
SVCpred_model_data$neutral_c <- SVCpred_model_data$neutral - mean(SVCpred_model_data$neutral)
SVCpred_model_data$silvering_c <- SVCpred_model_data$silvering - mean(SVCpred_model_data$silvering)

# shape dummy variables
SVCpred_model_data$anguilliform_c <- SVCpred_model_data$anguilliform - mean(SVCpred_model_data$anguilliform)
SVCpred_model_data$fusiform_c <- SVCpred_model_data$fusiform - mean(SVCpred_model_data$fusiform)

# octocoral
SVCpred_model_data$octocoral_c <- SVCpred_model_data$octocoral - mean(SVCpred_model_data$octocoral)

# stony coral
SVCpred_model_data$stony_c <- SVCpred_model_data$stony - mean(SVCpred_model_data$stony)

# vertical relief
SVCpred_model_data$relief_c <- SVCpred_model_data$relief_cm - mean(SVCpred_model_data$relief_cm)

# nocturnality
SVCpred_model_data$nocturnal_c <- SVCpred_model_data$nocturnal - mean(SVCpred_model_data$nocturnal)

# cryptic behaviour 
SVCpred_model_data$crypsis_c <- SVCpred_model_data$cryptic_behaviour - mean(SVCpred_model_data$cryptic_behaviour)

# size bins
SVCpred_model_data$size_bin_c <- SVCpred_model_data$size_bin_lengths - mean(SVCpred_model_data$size_bin_lengths)

# shape ratio
SVCpred_model_data$shape_ratio_c <- SVCpred_model_data$shape_ratio - mean(SVCpred_model_data$shape_ratio)


# SVC vs. Roving: Global Model (centred variables)##############################

# The following creates the SVC vs. Roving survey global model utilizing the 
# best fit global model obtained prior to centering.

SVCpred_c <- lme(log_difference~habitat+octocoral_c+stony_c+relief_c+nocturnal+
                   max_length+cryptic_behaviour+average_depth+size_bin_c+colouration+shape_ratio_c+position, 
                 random = list(~1|site, ~1|species_order), 
                 SVCpred_model_data) 
summary(SVCpred_c) # AIC = 1808.436
AICc(SVCpred_c) # AICc = 1809.495
vif(SVCpred_c) # habitat, max_length, average_depth, colouration, shape, position >5

# remove water column position 
SVCpred_c2 <- lme(log_difference~habitat+octocoral_c+stony_c+relief_c+nocturnal+
                   max_length+cryptic_behaviour+average_depth+size_bin_c+colouration+shape_ratio, 
                 random = list(~1|site, ~1|species_order), 
                 SVCpred_model_data) 
summary(SVCpred_c2) # AIC = 1807.391
AICc(SVCpred_c2) # AICc = 1808.33
vif(SVCpred_c2) # habitat, max_length, average_depth, shape >5

# remove habitat
SVCpred_c3 <- lme(log_difference~octocoral_c+stony_c+relief_c+nocturnal+
                    max_length+cryptic_behaviour+average_depth+size_bin_c+colouration+shape_ratio, 
                  random = list(~1|site, ~1|species_order), 
                  SVCpred_model_data) 
summary(SVCpred_c3) # AIC = 1804.436
AICc(SVCpred_c3) # AICc = 1805.264
vif(SVCpred_c3) # max_length, shape >5

# remove max length
SVCpred_c4 <- lme(log_difference~octocoral_c+stony_c+relief_c+nocturnal+
                    cryptic_behaviour+average_depth+size_bin_c+colouration+shape_ratio, 
                  random = list(~1|site, ~1|species_order), 
                  SVCpred_model_data) 
summary(SVCpred_c4) # AIC = 1793.86
AICc(SVCpred_c4) # AICc = 1794.583
vif(SVCpred_c4) # all good 

# remove habitat from SVCpred_c
SVCpred_c5 <- lme(log_difference~octocoral_c+stony_c+relief_c+nocturnal+
                   max_length+cryptic_behaviour+average_depth+size_bin_c+colouration+shape_ratio+position, 
                 random = list(~1|site, ~1|species_order), 
                 SVCpred_model_data) 
summary(SVCpred_c5) # AIC = 1805.481
AICc(SVCpred_c5) # AICc = 1806.421
vif(SVCpred_c5) # max_length, colouration, shape, position >5

# remove max_length from SVCpred_c5 
SVCpred_c6 <- lme(log_difference~octocoral_c+stony_c+relief_c+nocturnal+
                    cryptic_behaviour+average_depth+size_bin_c+colouration+shape_ratio+position, 
                  random = list(~1|site, ~1|species_order), 
                  SVCpred_model_data) 
summary(SVCpred_c6) # AIC = 1796.077
AICc(SVCpred_c6) # AICc = 1796.905
vif(SVCpred_c6) # colouration, shape, position >5

# remove water column position from SVCpred_c6
SVCpred_c7 <- lme(log_difference~octocoral_c+stony_c+relief_c+nocturnal+
                    cryptic_behaviour+average_depth+size_bin_c+colouration+shape_ratio, 
                  random = list(~1|site, ~1|species_order), 
                  SVCpred_model_data) 
summary(SVCpred_c7) # AIC = 1793.86
AICc(SVCpred_c7) # AICc = 1794.583
vif(SVCpred_c7) # all good 

# remove average depth from SVCpred_c
SVCpred_c8 <- lme(log_difference~habitat+octocoral_c+stony_c+relief_c+nocturnal+
                   max_length+cryptic_behaviour+size_bin_c+colouration+shape_ratio+position, 
                 random = list(~1|site, ~1|species_order), 
                 SVCpred_model_data) 
summary(SVCpred_c8) # AIC = 1798.519 
AICc(SVCpred_c8) # AICc = 1799.458
vif(SVCpred_c8) # max_length, colouration, shape, position >5

# remove water column position from SVCpred_c8
SVCpred_c9 <- lme(log_difference~habitat+octocoral_c+stony_c+relief_c+nocturnal+
                    max_length+cryptic_behaviour+size_bin_c+colouration+shape_ratio, 
                  random = list(~1|site, ~1|species_order), 
                  SVCpred_model_data) 
summary(SVCpred_c9) # AIC = 1797.363
AICc(SVCpred_c9) # AICc = 1798.19
vif(SVCpred_c9) # max_length, shape >5

# remove max length from SVCpred_c9 
SVCpred_c10 <- lme(log_difference~habitat+octocoral_c+stony_c+relief_c+nocturnal+
                    cryptic_behaviour+size_bin_c+colouration+shape_ratio, 
                  random = list(~1|site, ~1|species_order), 
                  SVCpred_model_data) 
summary(SVCpred_c10) # AIC = 1786.637
AICc(SVCpred_c10) # AICc = 1787.36
vif(SVCpred_c10) # all good 
# *BEST GLOBAL MODEL FIT*

# remove max length from SVCpred_8
SVCpred_c11 <- lme(log_difference~habitat+octocoral_c+stony_c+relief_c+nocturnal+
                    cryptic_behaviour+size_bin_c+colouration+shape_ratio+position, 
                  random = list(~1|site, ~1|species_order), 
                  SVCpred_model_data) 
summary(SVCpred_c11) # AIC = 1788.855
AICc(SVCpred_c11) # AICc = 1789.683
vif(SVCpred_c11) # colouration, shape, position >5


# SVC vs. Roving: Dredge (centred variables)####################################

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
saveRDS(SVCpred_model_average_c, here("./outputs/SVCpred_dredge_average_centred.rds"))

# read in saved average
SVCpred_model_average_c <- read_rds(here("./outputs/SVCpred_dredge_average_centred.rds"))
SVCpred_model_avg_summary <- summary(SVCpred_model_average_c)

# covariate confidence intervals
SVCpred_confidence_c <- confint(SVCpred_model_average_c)
summary(SVCpred_confidence_c)

# save confidence intervals
saveRDS(SVCpred_confidence_c, here("./outputs/SVCpred_dredge_CI_centred.rds"))

# read in saved CIs
SVCpred_confidence_redo_cont <- read_rds(here("./outputs/SVCpred_dredge_CI_redo_cont.rds"))


# SVC vs. Roving: Model Plot Re-Do (size class continuous)===================================================

# The following creates a visual representation of predictors in the top models
# comparing SVC and roving survey density differences. Predictor values are 
# displayed along with their confidence intervals and significance. 

# select conditional coefficient estimates 
SVCpred_model_avg_plot_cont<-as.data.frame(SVCpred_model_avg_summary_redo_cont$coefmat.subset) 

# select confidence intervals 
SVCpred_CI_cont <- as.data.frame(SVCpred_confidence_redo_cont) 

# put confidence intervals into coefficient dataframe
SVCpred_model_avg_plot_cont$CI.min <-SVCpred_CI_cont$`2.5 %` 
SVCpred_model_avg_plot_cont$CI.max <-SVCpred_CI_cont$`97.5 %`

# put rownames in column
setDT(SVCpred_model_avg_plot_cont, keep.rownames = "Coefficient") 

# remove spaces from column headers
names(SVCpred_model_avg_plot_cont) <- gsub(" ", "", names(SVCpred_model_avg_plot_cont)) 

# add binary significance column
SVCpred_model_avg_plot_cont$significance <- 
  ifelse(SVCpred_model_avg_plot_cont$`Pr(>|z|)` < 0.05, "sig", "nonsig")

# change labels
SVCpred_model_avg_plot_cont[2,1] <- "Neutral"
SVCpred_model_avg_plot_cont[3,1] <- "Silvering"
SVCpred_model_avg_plot_cont[4,1] <- "Fusiform"

# change order of rows
SVCpred_model_avg_plot_cont <- SVCpred_model_avg_plot_cont[c(3,2,4,1),]
#SVCpred_model_avg_plot$sort <- c(1,7,5,4,8,6,3,2,9,11,10)
#SVCpred_model_avg_plot <- SVCpred_model_avg_plot %>% arrange(sort)
#SVCpred_model_avg_plot$Coefficient <- factor(SVCpred_model_avg_plot$Coefficient, levels = SVCpred_model_avg_plot$Coefficient)

# plot with confidence intervals 
SVCpred_coef_CI <- ggplot(data=SVCpred_model_avg_plot_cont[1:4,], 
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
        axis.text = element_text(size = 30, color = "black")) +
  scale_x_discrete(limits=SVCpred_model_avg_plot_cont$Coefficient)
ggsave(here("./visuals/SVCpred_coef_plot_CIs.png"), SVCpred_coef_CI_cont)



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











