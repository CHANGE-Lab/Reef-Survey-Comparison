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


# SVC vs. Transect Model Plot ==================================================

# read in saved average and summary
SVCprey_model_average_c <- read_rds(here("./outputs/SVCprey_drege_average_centred.rds"))
SVCprey_model_avg_summary_c <- summary(SVCprey_model_average_c)

# confidence intervals of predictors
SVCprey_confidence_c <- confint(SVCprey_model_average_c)

# select conditional coefficient estimates 
SVCprey_model_avg_plot <- as.data.frame(SVCprey_model_avg_summary_c$coefmat.subset) 

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
SVCprey_model_avg_plot <- SVCprey_model_avg_plot[c(14,9,7,18,20,19,4,6,5,17,16,15,12,11,10,13,22,8,2,3,21,1),]

# plot with confidence intervals 
SVCprey_coef_CI <- ggplot(data=SVCprey_model_avg_plot[1:22], 
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
  scale_x_discrete(limits=SVCprey_model_avg_plot$Coefficient)


# SVC vs. Roving Model Plot ====================================================

# read in saved average
SVCpred_model_average_c <- read_rds(here("./outputs/SVCpred_dredge_average_centred.rds"))
SVCpred_model_avg_summary <- summary(SVCpred_model_average_c)

# covariate confidence intervals
SVCpred_confidence_c <- confint(SVCpred_model_average_c)

# select conditional coefficient estimates 
SVCpred_model_avg_plot <- as.data.frame(SVCpred_model_avg_summary$coefmat.subset) 

# select confidence intervals 
SVCpred_CI <- as.data.frame(SVCpred_confidence_c) 

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
SVCpred_model_avg_plot <- SVCpred_model_avg_plot[c(3,2,4,1),]

# plot with confidence intervals 
SVCpred_coef_CI <- ggplot(data=SVCpred_model_avg_plot[1:4,], 
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
  scale_x_discrete(limits=SVCpred_model_avg_plot$Coefficient)

