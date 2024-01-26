########## SURVEY COMPARISON PROJECT COVARIATE STANDARDIZATION ##########
########## 
##########

##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2023-05-02
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


# SVC vs. Roving: Global Model###########################################


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


# SVC vs. Roving: Standardization of Continuous Variables ######################

# Continuous variables: octocoral, stony, relief_cm, max_length, average_depth, 
# size_bin_lengths

# Octocoral
SVCpred_model_data$octocoral_st = ((SVCpred_model_data$octocoral)-mean(SVCpred_model_data$octocoral))/(2*sd(SVCpred_model_data$octocoral))

# Stony Coral
SVCpred_model_data$stony_st = ((SVCpred_model_data$stony)-mean(SVCpred_model_data$stony))/(2*sd(SVCpred_model_data$stony))

# Vertical Relief
SVCpred_model_data$relief_st = ((SVCpred_model_data$relief_cm)-mean(SVCpred_model_data$relief_cm))/(2*sd(SVCpred_model_data$relief_cm))

# Maximum Length
SVCpred_model_data$max_length_st = ((SVCpred_model_data$max_length)-mean(SVCpred_model_data$max_length))/(2*sd(SVCpred_model_data$max_length))

# Average Depth
SVCpred_model_data$depth_st = ((SVCpred_model_data$average_depth)-mean(SVCpred_model_data$average_depth))/(2*sd(SVCpred_model_data$average_depth))

# Size Bin
SVCpred_model_data$size_bin_st = ((SVCpred_model_data$size_bin_lengths)-mean(SVCpred_model_data$size_bin_lengths))/(2*sd(SVCpred_model_data$size_bin_lengths))


# SVC vs. Roving: Standardized Global Model#####################################

# Model fit with standardized continuous predictors and intercept removed 

# global model
SVCpred <- lme(log_difference~habitat+octocoral_st+stony_st+relief_st+nocturnal+
                   max_length_st+cryptic_behaviour+depth_st+colouration+
                   size_bin_st+shape+position-1, 
                 random = list(~1|site, ~1|species_order), 
                 SVCpred_model_data) 
summary(SVCpred) # AIC = 1730.111
AICc(SVCpred) # AICc = 1731.187
vif(SVCpred) # habitat, nocturnal, max_length, crypsis, depth, colour, shape, 
# position, all > 5
# REMOVING THE INTERCEPT CREATES REALLY LARGE VIF VALUES THAT ARE HARD TO GET RID OF

# global model
SVCpred2 <- lme(log_difference~habitat+octocoral_st+stony_st+relief_st+nocturnal+
                 max_length_st+cryptic_behaviour+depth_st+colouration+
                 size_bin_st+shape+position, 
               random = list(~1|site, ~1|species_order), 
               SVCpred_model_data) 
summary(SVCpred2) # AIC = 1730.111
AICc(SVCpred2) # AICc = 1731.187
vif(SVCpred2) # habitat, max_length, depth, colour, shape, position >5

# remove depth and max length
SVCpred3 <- lme(log_difference~octocoral_st+stony_st+relief_st+nocturnal+
                   cryptic_behaviour+colouration+
                   size_bin_st+shape+position, 
                 random = list(~1|site, ~1|species_order), 
                 SVCpred_model_data) 
summary(SVCpred3) # AIC = 1724.118
AICc(SVCpred3) # AICc = 1724.852
vif(SVCpred3) # colour, position > 5

# remove position from 3
SVCpred4 <- lme(log_difference~octocoral_st+stony_st+relief_st+nocturnal+
                   cryptic_behaviour+colouration+
                   size_bin_st+shape, 
                 random = list(~1|site, ~1|species_order), 
                 SVCpred_model_data) 
summary(SVCpred4) # AIC = 1722.032
AICc(SVCpred4) # AICc = 1722.667
vif(SVCpred4) # all good 

# remove coloration from 3
SVCpred5 <- lme(log_difference~octocoral_st+stony_st+relief_st+nocturnal+
                   cryptic_behaviour+
                   size_bin_st+shape+position, 
                 random = list(~1|site, ~1|species_order), 
                 SVCpred_model_data) 
summary(SVCpred5) # AIC = 1727.889
AICc(SVCpred5) # AICc = 1728.433
vif(SVCpred5) # all good


# SVC vs. Roving: Standardized Dredge ##########################################

# dredge
SVCpred_dredge <- dredge(SVCpred4)
SVCpred_dredge
# GET A LOT MORE PREDICTORS IN TOP MODELS 

# save dredge results 
saveRDS(SVCpred_dredge, here("./outputs/SVCpred_dredge_standardized.rds"))

# subset dredge
SVCpred_dredge_sub <- subset(SVCpred_dredge, delta < 4) 

# model average 
SVCpred_model_average <- model.avg(SVCpred_dredge_sub)
SVCpred_model_avg_summary <- summary(SVCpred_model_average)

# save model average
saveRDS(SVCpred_model_average, here("./outputs/SVCpred_dredge_average_standardized.rds"))

# read in saved average
SVCpred_model_average <- read_rds(here("./outputs/SVCpred_dredge_average_standardized.rds"))
SVCpred_model_avg_summary <- summary(SVCpred_model_average)

# covariate confidence intervals
SVCpred_confidence <- confint(SVCpred_model_average)
summary(SVCpred_confidence)

# save confidence intervals
saveRDS(SVCpred_confidence, here("./outputs/SVCpred_dredge_CI_standardized"))

# read in saved CIs
SVCpred_confidence <- read_rds(here("./outputs/SVCpred_dredge_CI_standardized.rds"))


# SVC vs. Roving: Model Plot with Standardized Variables =======================

# The following creates a visual representation of predictors in the top models
# comparing SVC and roving survey density differences. Predictor values are 
# displayed along with their confidence intervals and significance. 

# select conditional coefficient estimates 
SVCpred_model_avg_plot_cont<-as.data.frame(SVCpred_model_avg_summary$coefmat.subset) 

# select confidence intervals 
SVCpred_CI_cont <- as.data.frame(SVCpred_confidence) 

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
SVCpred_model_avg_plot_cont[5,1] <- "Size Class"
SVCpred_model_avg_plot_cont[6,1] <- "Crypsis"
SVCpred_model_avg_plot_cont[7,1] <- "Octocoral"

# change order of rows
SVCpred_model_avg_plot_cont <- SVCpred_model_avg_plot_cont[c(7,3,2,4,5,6,1),]
#SVCpred_model_avg_plot$sort <- c(1,7,5,4,8,6,3,2,9,11,10)
#SVCpred_model_avg_plot <- SVCpred_model_avg_plot %>% arrange(sort)
#SVCpred_model_avg_plot$Coefficient <- factor(SVCpred_model_avg_plot$Coefficient, levels = SVCpred_model_avg_plot$Coefficient)

# plot with confidence intervals 
SVCpred_coef_CI <- ggplot(data=SVCpred_model_avg_plot_cont[1:7,], 
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