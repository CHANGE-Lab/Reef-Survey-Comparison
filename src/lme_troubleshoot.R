########## SURVEY COMPARISON PROJECT MODEL TROUBLESHOOTING ##########
########## 
##########
# This file creates linear mixed effects models to compare fish density 
# differences between SVC and transect surveys and SVC and roving surveys using 
# species, habitat, and survey traits as predictors. It creates numerous models
# each of which use a different combination of traits in order to determine the 
# most appropriate global models to use in further model selection. 
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2021-05-28
##########
##########


# Set-Up =======================================================================

# packages
library(plyr)
library(dplyr)
library(nlme)
library(car)
library(ggplot2)
library(dotwhisker)
library(arm)
library(MuMIn)
library(here)

# data
SVCprey <- read.csv(here("./dataframes/SVCprey_dataframe.csv"))
SVCpred <- read.csv(here("./dataframes/SVCpred_dataframe.csv"))
colouration <- read.csv(here("./clean_data/species_colouration.csv"))
shape <- read.csv(here("./clean_data/species_shape.csv"))
fnt_group <- read.csv(here("./clean_data/species_trophic.csv"))


# Dataframe Edits ==============================================================

# The following makes minor edits to the dataframes comparing fish density 
# estimates from SVC, transect, and roving surveys to make them appropriate 
# for further analyses. 

# remove juveniles from colouration 
colouration <- filter(colouration, lifestage == "adult") 

# remove lifestage column from colouration 
colouration <- colouration[,c(1,3)] 

# join colouration to dataframes
SVCprey_data <- join(SVCprey, colouration, by = NULL, type = "left", 
                     match = "all")
SVCpred_data <- join(SVCpred, colouration, by = NULL, type = "left", 
                     match = "all")

# remove juveniles from shape
shape <- filter(shape, lifestage == "adult") 

# remove lifestage column from shape
shape <- shape[,c(1,3)]

# join shape to dataframes
SVCprey_data <- join(SVCprey_data, shape, by = NULL, type = "left", 
                     match = "all")
SVCpred_data <- join(SVCpred_data, shape, by = NULL, type = "left", 
                     match = "all")

# remove juveniles from functional group
fnt_group <- filter(fnt_group, lifestage == "adult")

# remove lifestage column from functional group
fnt_group <- fnt_group[,c(1,3)]

# join functional group to dataframes
SVCprey_data <- join(SVCprey_data, fnt_group, by = NULL, type = "left", 
                     match = "all")
SVCpred_data <- join(SVCpred_data, fnt_group, by = NULL, type = "left", 
                     match = "all")

# rename species order column
SVCprey_model_data <- SVCprey_data %>% rename(species_order = order)
SVCpred_model_data <- SVCpred_data

# remove trumpetfish from all dataframes (single species order)
SVCprey_model_data <- SVCprey_model_data[SVCprey_model_data$species_order 
                                         !="Syngnathiformes",] 
SVCpred_model_data <- SVCpred_model_data[SVCpred_model_data$species_order 
                                         !="Syngnathiformes",] 

# remove silversides from SVC vs. transect (single species order)
SVCprey_model_data <- SVCprey_model_data[SVCprey_model_data$species_order 
                                         !="Atheriniformes",] 

# remove flounder from SVC vs. transect (only depressiform species)
SVCprey_model_data <- SVCprey_model_data[SVCprey_model_data$species_order 
                                         !="Pleuronectiformes",] 

# remove gray snapper from SVC vs. roving (inconsistently recorded)
SVCpred_model_data <- SVCpred_model_data[SVCpred_model_data$species 
                                         !="gray snapper",] 

# remove amberjack from SVC vs. roving surveys (only schooling species)
SVCpred_model_data <- SVCpred_model_data[SVCpred_model_data$species 
                                         !="amberjack",] 

# remove NA values from dataframes
SVCprey_model_data <- na.omit(SVCprey_model_data)
SVCpred_model_data <- na.omit(SVCpred_model_data)

# remove size bin = 0
SVCprey_model_data <- SVCprey_model_data[SVCprey_model_data$size_bin !=0,]
SVCpred_model_data <- SVCpred_model_data[SVCpred_model_data$size_bin !=0,]

# Change Variables to Character:
SVCprey_model_data$size_bin_char <- as.character(SVCprey_model_data$size_bin)
SVCpred_model_data$size_bin_char <- as.character(SVCpred_model_data$size_bin)


# SVC vs. Transect: Original Linear Mixed Effects Model ========================

# The following runs a linear mixed effects model on original predictors of 
# interest for density differences between SVC and transect surveys. 
# Collinearity between predictors is explored by examining VIF values and model 
# fit is determined by creating random effects plots, residual plots, qq plots,
# and model plots. 

# full model
SVCprey_model <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin+
                     nocturnal+position+max_length+colouration2+behavior+
                     cryptic_behaviour+average_depth, 
                     random = list(~1|site, ~1|species_order), 
                     na.action = na.omit, SVCprey_model_data) 

# model summary
summary(SVCprey_model) 
# AIC = 29939.76
# sig pos covariates = octocoral, stony, size bin, demersal, max_length, 
# colourful, neutral, silvering, solitary
# sig neg covariates = cryptic behaviour 

# VIF values 
vif(SVCprey_model) 
# all under GVIF = 5

# random effects plot
plot(ranef(SVCprey_model))

# residuals plot
res_SVCprey_model = residuals(SVCprey_model)
plot(res_SVCprey_model) 

# qq plot
qqnorm(res_SVCprey_model) 
qqline(res_SVCprey_model) 

# model plot
plot(SVCprey_model) 

# boxplots of significant covariates
plot(SVCprey_model_data$log_difference~SVCprey_model_data$octocoral)
plot(SVCprey_model_data$log_difference~SVCprey_model_data$stony)
boxplot(SVCprey_model_data$log_difference~SVCprey_model_data$size_bin)
boxplot(SVCprey_model_data$log_difference~SVCprey_model_data$nocturnal)
boxplot(SVCprey_model_data$log_difference~SVCprey_model_data$position)
plot(SVCprey_model_data$log_difference~SVCprey_model_data$max_length)
boxplot(SVCprey_model_data$log_difference~SVCprey_model_data$colouration2)
boxplot(SVCprey_model_data$log_difference~SVCprey_model_data$behavior)
boxplot(SVCprey_model_data$log_difference~SVCprey_model_data$cryptic_behaviour)

# dot-and-whisker plot
dwplot(SVCprey_model)


# SVC vs. Roving: Linear Mixed Effects Model ===================================

# The following runs a linear mixed effects model on original predictors of 
# interest for density differences between SVC and roving surveys. Collinearity 
# between predictors is explored by examining VIF values and model fit is 
# determined by creating random effects plots, residual plots, qq plots, and 
# model plots. 

# full model
SVCpred_model <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin+
                     nocturnal+position+max_length+colouration2+
                     cryptic_behaviour+average_depth, 
                     random = list(~1|site, ~1|species_order), 
                     na.action = na.omit, SVCpred_model_data) 
# behaviour not included because all species are solitary 

# model summary
summary(SVCpred_model) 
# AIC = 1696.275
# sig pos covariates = silvering
# sig neg covariates = stony, size bin

# VIF values 
vif(SVCpred_model) 
# habitat GVIF = 6.779778
# colouration GVIF = 17.144453
# depth GVIF = 5.029028

# random effects plot
plot(ranef(SVCpred_model))

# residuals plot
res_SVCpred_model = residuals(SVCpred_model)
plot(res_SVCpred_model) 

# qq plot
qqnorm(res_SVCpred_model) 
qqline(res_SVCpred_model) 

# model plot
plot(SVCpred_model) 

# boxplots of significant covariates
plot(SVCpred_model_data$log_difference~SVCpred_model_data$stony)
boxplot(SVCpred_model_data$log_difference~SVCpred_model_data$size_bin)
boxplot(SVCpred_model_data$log_difference~SVCpred_model_data$colouration2)

# dot-and-whisker plot
dwplot(SVCpred_model)


# SVC vs. Transect: Shape Model ================================================

# The following runs a linear mixed effects model on predictors of interest for 
# density differences between SVC and transect surveys including species' body 
# shape. Collinearity between predictors is explored by examining VIF values. 

# lme including body shape
SVCprey_shape_model <- lme(log_difference~habitat+octocoral+stony+relief_cm+
                           size_bin+nocturnal+position+max_length+
                           colouration2+behavior+cryptic_behaviour+
                           average_depth+shape,
                           random = list(~1|site, ~1|species_order), 
                           na.action = na.omit, SVCprey_model_data)

# model summary
summary(SVCprey_shape_model) 
# AIC = 29625.5 --> better fit than model without shape
# sig pos covariates = octocoral, stony, size bin, max length, colourful, 
# neutral, silvering, shoaling, solitary, compressiform, elongated, fusiform, 
# globiform
# sig neg covariates = habitat type, cryptic behaviour

# VIF values 
vif(SVCprey_shape_model) 
# colouration GVIF = 6.583216
# behaviour GVIF = 5.076665
# shape GVIF = 6.310282

# shape boxplot
boxplot(SVCprey_model_data$log_difference~SVCprey_model_data$shape)


# SVC vs. Roving: Shape Model ==================================================

# The following runs a linear mixed effects model on predictors of interest for 
# density differences between SVC and roving surveys including species' body 
# shape. Collinearity between predictors is explored by examining VIF values. 

# lme including body shape
SVCpred_shape_model <- lme(log_difference~habitat+octocoral+stony+relief_cm+
                           size_bin+nocturnal+position+max_length+
                           colouration2+cryptic_behaviour+average_depth+shape, 
                           random = list(~1|site, ~1|species_order), 
                           na.action = na.omit, SVCpred_model_data) 

# model summary
summary(SVCpred_shape_model) 
# AIC = 1683.789 --> better fit than previous model without shape
# sig pos covariates = silvering, fusiform
# sig neg covariates = stony, size bin

# VIF values 
vif(SVCpred_shape_model) 
# habitat GVIF = 6.905395
# position GVIF = 6.696934
# max length = 5.106350
# colouration GVIF = 21.925347
# depth GVIF = 5.106133
# shape GVIF = 10.005806

# shape boxplot
boxplot(SVCpred_model_data$log_difference~SVCpred_model_data$shape)


# SVC vs. Transect: Model without Eels =========================================

# The following runs a linear mixed effects model on predictors of interest for 
# density differences between SVC and transect surveys excluding eel species. 
# Collinearity between predictors is explored by examining VIF values. 

# Remove Eels:
SVCprey_noeels <- SVCprey_model_data[SVCprey_model_data$species_order !="Anguilliformes",] # remove eels

# Model:
SVCprey_noeel_model <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin+nocturnal+position+max_length+colouration2+behavior+cryptic_behaviour+average_depth+shape, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCprey_noeels) 
summary(SVCprey_noeel_model) 
# model summary:
# AIC = 29457.02 --> better fit than models with eels
# sig pos covariates = octocoral, stony, size bin, max length, colourful, neutral, silvering, shoaling, solitary
# sig neg covariates = habitat type, cryptic behaviour, elongated, fusiform, globiform
vif(SVCprey_shape_model) 
# colouration GVIF = 6.583216
# behaviour GVIF = 5.076665
# shape GVIF = 6.310282


# SVC vs. Transect: Functional Group Model =====================================

# Model:
SVCprey_functional_model <- lme(log_difference~size_bin+fnt_group, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCprey_model_data) 
summary(SVCprey_functional_model) 
# model summary:
# AIC = 30262.5
# sig pos covariates = size bin, herbivore
# sig neg covariates = omnivore, planktivore
vif(SVCprey_functional_model) 
# all under GVIF = 5

# Random Effects Plot:
plot(ranef(SVCprey_functional_model))

# Residuals Plots:
res_SVCprey_fnt_model = residuals(SVCprey_functional_model)
plot(res_SVCprey_fnt_model) # residuals plot
qqnorm(res_SVCprey_fnt_model) 
qqline(res_SVCprey_fnt_model) # qq plot
plot(SVCprey_functional_model) 

# Covariate Boxplots:
boxplot(SVCprey_model_data$log_difference~SVCprey_model_data$size_bin)
boxplot(SVCprey_model_data$log_difference~SVCprey_model_data$fnt_group)


# SVC vs. Roving: Functional Group Model =======================================

# ALL SPECIES ARE CARNIVORES! 


# SVC vs. Transect: Size Bin Interactions ======================================

# Size Bin x Colouration (without shape):
SVCprey_sxc_model <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin*colouration2+nocturnal+position+max_length+behavior+cryptic_behaviour+average_depth, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCprey_model_data) 
summary(SVCprey_sxc_model) 
# model summary
# AIC = 29898.08
# sig pos covariates = octocoral, stony, size bin, colourful, neutral, silvering, demersal, max length, solitary
# sig neg covariates = cryptic behaviour, size bin*colourful, size bin*neutral, size bin*silvering
vif(SVCprey_sxc_model)
# size bin GVIF = 13.645754
# colouration GVIF = 234.809002

# Size Bin x Colouration (with shape):
SVCprey_sxc_model2 <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin*colouration2+nocturnal+position+max_length+behavior+cryptic_behaviour+average_depth+shape, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCprey_model_data) 
summary(SVCprey_sxc_model2) 
# model summary:
# AIC = 29607.8
# sig pos covariates = octocoral, stony, size bin, colourful, neutral, silvering, max length, shoaling, solitary, compressiform, elongated, fusiform, globiform
# sig neg covariates = cryptic behaviour, eel-like, elongated, fusiform, size bin*colourful, size bin*neutral, size bin*silvering
vif(SVCprey_sxc_model2) 
# size bin GVIF = 4.658874e+06
# colourful GVIF = 8.250041e+02
# behaviour GVIF = 5.452498e+00
# shape GVIF = 9.540549e+00

# Size Bin x Colouration Boxplot:
boxplot(SVCprey_model_data$log_difference ~ (SVCprey_model_data$colouration2)*(SVCprey_model_data$size_bin))

ggplot(SVCprey_model_data, aes(colouration2, log_difference, fill = size_bin_char)) + geom_boxplot(show.legend = TRUE) + theme_classic() + scale_fill_manual(name = "Size Bin", values = c("yellow1", "gray65", "white", "blue", "red", "green")) + xlab("Colouration") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")

# Size Bin x Shape (without colouration):
SVCprey_sxs_model <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin*shape+nocturnal+position+max_length+behavior+cryptic_behaviour+average_depth, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCprey_noeels) 
summary(SVCprey_sxs_model) # model summary
vif(SVCprey_sxs_model) # don't want GVIF > 5

# Size Bin x Shape (with colouration):
SVCprey_sxs_model2 <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin*shape+nocturnal+position+max_length+behavior+cryptic_behaviour+average_depth+colouration2, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCprey_noeels) 
summary(SVCprey_sxs_model2) # model summary
vif(SVCprey_sxs_model2) # don't want GVIF > 5

# Size Bin x Shape Boxplot:
boxplot(SVCprey_model_data$log_difference ~ (SVCprey_model_data$shape)*(SVCprey_model_data$size_bin))

ggplot(SVCprey_noeels, aes(shape, log_difference, fill = size_bin_char)) + geom_boxplot(show.legend = TRUE) + theme_classic() + scale_fill_manual(name = "Size Bin", values = c("yellow1", "gray65", "white", "blue", "red", "green")) + xlab("Shape") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")


# SVC vs. Roving: Size Bin Interactions ========================================

# Size Bin x Colouration (without shape):
SVCpred_sxc_model <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin*colouration2+nocturnal+position+max_length+cryptic_behaviour+average_depth, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCpred_model_data) 
summary(SVCpred_sxc_model) # model summary
vif(SVCpred_sxc_model) # don't want GVIF > 5

# Size Bin x Colouration (with shape):
SVCpred_sxc_model2 <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin*colouration2+nocturnal+position+max_length+behavior+cryptic_behaviour+average_depth+shape, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCpred_model_data) 
summary(SVCpred_sxc_model2) # model summary
vif(SVCpred_sxc_model2) # don't want GVIF > 5

# Size Bin x Colouration Boxplot:
boxplot(SVCpred_model_data$log_difference ~ (SVCpred_model_data$colouration2)*(SVCpred_model_data$size_bin))

ggplot(SVCpred_model_data, aes(colouration2, log_difference, fill = size_bin_char)) + geom_boxplot(show.legend = TRUE) + theme_classic() + scale_fill_manual(name = "Size Bin", values = c("yellow1", "gray65", "white", "red", "blue", "green")) + xlab("Colouration") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")

# Size Bin x Shape (without colouration):
SVCpred_sxs_model <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin*shape+nocturnal+position+max_length+behavior+cryptic_behaviour+average_depth, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCpred_model_data) 
# Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : contrasts can be applied only to factors with 2 or more levels
# only one observation of anguilliform of size bin 4
summary(SVCpred_sxs_model) # model summary
vif(SVCpred_sxs_model) # don't want GVIF > 5

# Size Bin x Shape (with colouration):
SVCpred_sxs_model2 <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin*shape+nocturnal+position+max_length+behavior+cryptic_behaviour+average_depth+colouration2, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCpred_model_data) 
summary(SVCpred_sxs_model2) # model summary
vif(SVCpred_sxs_model2) # don't want GVIF > 5

# Size Bin x Shape Boxplot:
boxplot(SVCpred_model_data$log_difference ~ (SVCpred_model_data$shape)*(SVCpred_model_data$size_bin))

ggplot(SVCpred_model_data, aes(shape, log_difference, fill = size_bin_char)) + geom_boxplot(show.legend = TRUE) + theme_classic() + scale_fill_manual(name = "Size Bin", values = c("yellow1", "gray65", "white", "blue", "red", "green")) + xlab("Shape") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")


# SVC vs. Transect: Backwards Model Selection ==================================

# Full Model:
SVCprey_model1 <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin+nocturnal+position+max_length+colouration2+behavior+cryptic_behaviour+average_depth+shape, random = list(~1|site, ~1|species_order), SVCprey_model_data) 
summary(SVCprey_model1) # model summary
vif(SVCprey_model1) # don't want GVIF > 5

# Remove Nocturnal:
SVCprey_model2 <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin+position+max_length+colouration2+behavior+cryptic_behaviour+average_depth+shape, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCprey_model_data) 
summary(SVCprey_model2) # model summary
vif(SVCprey_model2) # don't want GVIF > 5

# Remove Position:
SVCprey_model3 <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin+max_length+colouration2+behavior+cryptic_behaviour+average_depth+shape, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCprey_model_data) 
summary(SVCprey_model3) # model summary
vif(SVCprey_model3) # don't want GVIF > 5

# Remove Relief:
SVCprey_model4 <- lme(log_difference~habitat+octocoral+stony+size_bin+max_length+colouration2+behavior+cryptic_behaviour+average_depth+shape, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCprey_model_data) 
summary(SVCprey_model4) # model summary
vif(SVCprey_model4) # don't want GVIF > 5

# Remove Average Depth:
SVCprey_model5 <- lme(log_difference~habitat+octocoral+stony+size_bin+max_length+colouration2+behavior+cryptic_behaviour+shape, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCprey_model_data) 
summary(SVCprey_model5) # model summary
vif(SVCprey_model5) # don't want GVIF > 5

# Remove Aggregation Behaviour:
SVCprey_model6 <- lme(log_difference~habitat+octocoral+stony+size_bin+max_length+colouration2+cryptic_behaviour+shape, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCprey_model_data) 
summary(SVCprey_model6) # model summary
vif(SVCprey_model6) # don't want GVIF > 5

# ALL COVARIATES ARE SIGNIFICANT NOW, STOP HERE?


# SVC vs. Roving: Backwards Model Selection ====================================

# Full Model:
SVCpred_model1 <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin+nocturnal+position+max_length+colouration2+behavior+cryptic_behaviour+average_depth+shape, random = list(~1|site, ~1|species_order), SVCpred_model_data) 
summary(SVCpred_model1) # model summary
vif(SVCpred_model1) 

# Remove Nocturnal:
SVCpred_model2 <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin+position+max_length+colouration2+behavior+cryptic_behaviour+average_depth+shape, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCpred_model_data) 
summary(SVCpred_model2) # model summary
vif(SVCpred_model2) 

# Remove Cryptic Behaviour:
SVCpred_model3 <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin+position+max_length+colouration2+behavior+average_depth+shape, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCpred_model_data) 
summary(SVCpred_model3) # model summary
vif(SVCpred_model3) 
# Difference between this and previous model's AIC is 2.113; very close 

# Remove Relief:
SVCpred_model4 <- lme(log_difference~habitat+octocoral+stony+size_bin+position+max_length+colouration2+behavior+average_depth+shape, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCpred_model_data) 
summary(SVCpred_model4) # model summary
vif(SVCpred_model4) 

# Remove Habitat:
SVCpred_model5 <- lme(log_difference~octocoral+stony+size_bin+position+max_length+colouration2+behavior+average_depth+shape, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCpred_model_data) 
summary(SVCpred_model5) # model summary
vif(SVCpred_model5) 

# Remove Shape:
SVCpred_model6 <- lme(log_difference~octocoral+stony+size_bin+position+max_length+colouration2+behavior+average_depth, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCpred_model_data) 
summary(SVCpred_model6) # model summary
vif(SVCpred_model6)
# Removing shape decreases model fit (increases AIC) --> should I stop there?


# SVC vs. Transect: Dredging ===================================================

# Global Model:
SVCprey_global <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin*colouration2+nocturnal+position+max_length+behavior+cryptic_behaviour+average_depth+size_bin*shape, random = list(~1|site, ~1|species_order), SVCprey_noeels) 
summary(SVCprey_global) 
# model summary:
# AIC = 29598.41
# sig pos covariates = octocoral, stony, size bin, colourful, neutral, silvering, max length, shoaling, size bin*elongated, size bin*fusiform
# sig neg covariates = habitat, cryptic behaviour, elongated, fusiform, size bin*colourful, size bin*neutral, size bin*silvering
vif(SVCprey_global) 
# size bin GVIF = 24.897697 
# colouration GVIF = 409.450179 
# shape GVIF = 40.288049
# size bin*colouration GVIF = 1266.492002
# size bin*shape GVIF = 31.333791

# Dredging: 
SVCprey_dredge <- dredge(SVCprey_model1) # model without all covariates but no interactions
SVCprey_dredge
# Best model: colouration, cryptic behaviour, habitat type, max length, shape, and size bin
# AICc = 29807.3, delta AIC = 2.42 (next model includes position)
saveRDS(SVCprey_dredge, "SVCprey_mini_dredge.rds")
saveRDS(setosa_model, here('./output/setosa_model_object.rds'))

SVCprey_global_dredge <- dredge(SVCprey_global)
SVCprey_global_dredge
model_average <- model.avg(SVCprey_global_dredge)
summary(model_average)
confint(model_average)

# First model (AICc = 29562.4): colouration, cryptic behaviour, habitat, max length, shape, size bin, colouration*size bin, shape*size bin
# Second model ( AICc = : aggregation behaviour, colouration, cryptic behaviour, habitat, max length, shape, size bin, colouration*size bin, shape*size bin
# Delta AIC = 1.82 between them and 2.10 between second and third model (third has position)
saveRDS(SVCprey_global_dredge, "SVCprey_global_dredge.rds")

# SVC vs. Roving: Global Model Creation ========================================

# full model:
SVCpred_full <- lme(log_difference~habitat+octocoral+stony+relief_cm+nocturnal+max_length+cryptic_behaviour+average_depth+colouration+size_bin_lengths+shape+position, random = list(~1|site, ~1|species_order), SVCpred_model_data) 
summary(SVCpred_full)
vif(SVCpred_full)
AICc(SVCpred_full)

# Colouration Model:
SVCpred_colour <- lme(log_difference~habitat+octocoral+stony+relief_cm+nocturnal+max_length+cryptic_behaviour+average_depth+colouration+size_bin, random = list(~1|site, ~1|species_order), SVCpred_model_data) # removed shape and position
summary(SVCpred_colour) # AIC = 1795.605
AICc(SVCpred_colour) # 1796.432
vif(SVCpred_colour) # colouration GVIF = 5.877840

# Shape Model:
SVCpred_shape <- lme(log_difference~habitat+octocoral+stony+relief_cm+nocturnal+position+max_length+cryptic_behaviour+average_depth+shape+size_bin_lengths, random = list(~1|site, ~1|species_order), SVCpred_model_data) # removed colouration
summary(SVCpred_shape) # AIC = 1802.129 
AICc(SVCpred_shape) # 1803.188
vif(SVCpred_shape) # shape GVIF = 6.727650 

# Habitat Model (from colouration model):
SVCpred_hab <- lme(log_difference~habitat+octocoral+stony+relief_cm+nocturnal+max_length+cryptic_behaviour+colouration+size_bin, random = list(~1|site, ~1|species_order), SVCpred_model_data) # removed shape, position, and depth
summary(SVCpred_hab) # AIC = 1785.537
AICc(SVCpred_hab)
vif(SVCpred_hab) # colouration GVIF = 5.843345, all else under 5

# Depth Model (from colouration model):
SVCpred_depth <- lme(log_difference~octocoral+stony+relief_cm+nocturnal+max_length+cryptic_behaviour+average_depth+colouration+size_bin, random = list(~1|site, ~1|species_order), SVCpred_model_data) # removed shape, position, and habitat
summary(SVCpred_depth) # AIC = 1792.709; higher than habitat model
AICc(SVCpred_depth)
vif(SVCpred_depth) # colouration GVIF = 5.844267 
1793.432-1786.26
# Habitat Model (from shape model):
SVCpred_hab2 <- lme(log_difference~habitat+octocoral+stony+relief_cm+nocturnal+position+max_length+cryptic_behaviour+shape+size_bin, random = list(~1|site, ~1|species_order), SVCpred_model_data) # removed colouration and depth
summary(SVCpred_hab2) # AIC = 1616.189
vif(SVCpred_hab2) # shape GVIF = 7.659572

# Depth Model (from shape model):
SVCpred_depth2 <- lme(log_difference~octocoral+stony+relief_cm+nocturnal+position+max_length+cryptic_behaviour+average_depth+shape+size_bin, random = list(~1|site, ~1|species_order), SVCpred_model_data) # removed colouration and habitat
summary(SVCpred_depth2) # AIC = 1623.182; higher than habitat model
vif(SVCpred_depth2) # shape GVIF = 7.659066

# HABITAT MODELS FOR BOTH SHAPE AND COLOURATION RESULT IN BEST FIT, BUT COLOURATION MODEL'S GVIF VALUES ARE LOWER AND COLOURATION HAS MORE ECOLOGICAL SIGNIFICANCE 

SVCpred <- lme(log_difference~octocoral+stony+relief_cm+nocturnal+position+max_length+cryptic_behaviour+average_depth+size_bin, random = list(~1|site, ~1|species_order), SVCpred_model_data) # removed colouration and habitat
summary(SVCpred) # AIC = 1623.182; higher than habitat model
vif(SVCpred) # shape GVIF = 7.659066

SVCpred2 <- lme(log_difference~octocoral+stony+relief_cm+nocturnal+position+max_length+cryptic_behaviour+average_depth+size_bin*colouration, random = list(~1|site, ~1|species_order), SVCpred_model_data) # removed colouration and habitat
summary(SVCpred2) # AIC = 1623.182; higher than habitat model
vif(SVCpred2) # shape GVIF = 7.659066


# SVC vs. Roving: Dredging =====================================================

# Global Model:
SVCpred_global <- lme(log_difference~habitat+octocoral+stony+relief_cm+nocturnal+position+max_length+cryptic_behaviour+average_depth+colouration+shape+size_bin_lengths, random = list(~1|site, ~1|species_order), SVCpred_model_data) 
summary(SVCpred_global) 
# model summary:
# AIC = 1695.676
# sig pos covariates = silvering
# sig neg covariates = stony, size bin
vif(SVCpred_global)
# habitat GVIF = 6.832051
# position GVIF = 6.634015
# depth GVIF = 5.079790
# colouration GVIF = 21.218479
# shape GVIF = 9.864492

# Dredging: 
SVCpred_dredge <- dredge(SVCpred_global)
SVCpred_dredge
# First model (AICc = 1631.5): colouration, shape
# Second model (AICc = 1631.6, delta AICc = 0.11): cryptic behaviour, shape 
# Third model (AICc = 1633.1, delta AICc = 1.64): colouration, position, shape 
# Fourth model (AICc = 1633.4, delta AICc = 1.90): colouration  
saveRDS(SVCpred_dredge, "SVCprey_dredge.rds")


#### SVC vs. Predator: Presence/Absence Model ####

# Creating presence/absence values:
SVCpred_model_data$pres_abs <- ifelse(SVCpred_model_data$SVC_density > 0 & SVCpred_model_data$pred_density > 0, 0, ifelse(SVCpred_model_data$SVC_density > 0 & SVCpred_model_data$pred_density <= 0, 1, ifelse(SVCpred_model_data$SVC_density <= 0 & SVCpred_model_data$pred_density > 0, -1, NA)))
# 0 = observed on SVC and pred surveys
# 1 = observed only on SVC surveys
# -1 = observed only on pred surveys 

# Model:
SVCpred_presabs <- lme(pres_abs~habitat+octocoral+stony+relief_cm+nocturnal+position+max_length+cryptic_behaviour+average_depth+colouration2+shape+size_bin, random = list(~1|site, ~1|species_order), SVCpred_model_data) 
summary(SVCpred_presabs) 
summary(SVCpred_global) # compare to density model: changes elongated to significant and stony to insignificant 
vif(SVCpred_presabs)
vif(SVCpred_global) # compare to density model: no change, does that make sense?

# Boxplots:
boxplot(SVCpred_model_data$pres_abs ~ SVCpred_model_data$colouration2)
boxplot(SVCpred_model_data$pres_abs ~ SVCpred_model_data$shape)
boxplot(SVCpred_model_data$pres_abs ~ SVCpred_model_data$size_bin)


write_csv(SVCprey_noeels, here("./dataframes/SVCprey_model_data.csv"))
write_csv(SVCpred_model_data, here("./dataframes/SVCpred_model_data.csv"))

