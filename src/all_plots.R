########## SURVEY COMPARISON PROJECT ALL PLOTS ##########


# Set-Up =======================================================================

# packages
library(tidyverse)
library(nlme)
library(lme4)
library(car)
library(arm)
library(MuMIn)
library(data.table)
library(ggpubr)
library(emmeans)
library(ggplot2)
library(here)

# dataframes
SVCprey_model_data <- read_csv(here("./dataframes/SVCprey_dataframe.csv"))
SVCpred_model_data <- read_csv(here("./dataframes/SVCpred_dataframe.csv"))

# SVC vs. transect results required
SVCprey_model_average <- read_rds(here("./outputs/SVCprey_drege_average.rds"))
SVCprey_model_avg_summary <- summary(SVCprey_model_average)
SVCprey_confidence <- read_rds(here("./outputs/SVCprey_dredge_CI.rds"))

# SVC vs. roving results required 
SVCpred_model_average_redo <- read_rds(here("./outputs/SVCpred_dredge_average_redo.rds"))
SVCpred_model_avg_summary_redo <- summary(SVCpred_model_average_redo)
SVCpred_confidence_redo <- read_rds(here("./outputs/SVCpred_dredge_CI_redo.rds"))


# Dataframe Edits ==============================================================

# The following performs dataframe edits necessary for creation of accurate 
# plots. 

# change size bin to character variable
SVCprey_model_data$size_bin_char <- 
  as.character(SVCprey_model_data$size_bin)
SVCpred_model_data$size_bin_char <- 
  as.character(SVCpred_model_data$size_bin)

# change cryptic behaviour to character variable
SVCprey_model_data$cryptic_behaviour <- 
  as.character(SVCprey_model_data$cryptic_behaviour)
SVCpred_model_data$cryptic_behaviour <- 
  as.character(SVCpred_model_data$cryptic_behaviour)

# change nocturnality to character variable
SVCprey_model_data$nocturnal <- 
  as.character(SVCprey_model_data$nocturnal)
SVCpred_model_data$nocturnal <- 
  as.character(SVCpred_model_data$nocturnal)

# re-name cryptic behaviour column
SVCprey_model_data$cryptic_behaviour2 <- 
  ifelse(SVCprey_model_data$cryptic_behaviour == 1, 
         "Cryptic behaviour", "None")

# re-order colouration levels
SVCprey_model_data$coloration2 <- ifelse(SVCprey_model_data$colouration == "camouflage", 
                                         "Camouflage", ifelse(SVCprey_model_data$colouration == "neutral", "Neutral", ifelse(SVCprey_model_data$colouration == "silvering", "Silvering", ifelse(SVCprey_model_data$colouration == "colourful", "Colorful", NA))))
SVCprey_model_data$coloration2 <- 
  factor(SVCprey_model_data$coloration2, 
         levels = c("Camouflage", "Neutral", "Silvering", "Colorful"))

# re-order shape levels
SVCprey_model_data$shape2 <- ifelse(SVCprey_model_data$shape == "elongated", "Elongated", ifelse(SVCprey_model_data$shape == "fusiform", "Fusiform", ifelse(SVCprey_model_data$shape == "compressiform", "Compressiform", ifelse(SVCprey_model_data$shape == "globiform", "Globiform", NA))))
SVCprey_model_data$shape2 <- 
  factor(SVCprey_model_data$shape2, 
         levels = c("Elongated", "Fusiform", "Compressiform", "Globiform"))

# capitalize aggregation behaviour levels
SVCprey_model_data$behavior2 <- ifelse(SVCprey_model_data$behavior == "schooling", "Schooling", ifelse(SVCprey_model_data$behavior == "shoaling", "Shoaling", ifelse(SVCprey_model_data$behavior == "solitary", "Solitary", NA)))

# capitalize SVC vs roving coloration levels
SVCpred_model_data$coloration2 <- ifelse(SVCpred_model_data$colouration == "camouflage", 
                                         "Camouflage", ifelse(SVCpred_model_data$colouration == "neutral", "Neutral", ifelse(SVCpred_model_data$colouration == "silvering", "Silvering", NA)))


# SVC vs. Transect Survey Covariate Plots ======================================

# reef type 
prey_hab <- ggplot(SVCprey_model_data, aes(x = habitat, y = log_difference, 
                                           fill = habitat)) + 
  geom_boxplot() +
  theme_classic() + xlab("Reef Type") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 30)) +
  theme(axis.text= element_text(size = 28)) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray88", "gray44")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")

# octocoral
prey_octo <- ggplot(SVCprey_model_data, aes(x = octocoral, 
                                            y = log_difference)) + 
  geom_jitter(width = 2, height = 0.1) +
  theme_classic() + xlab("Percent Octocoral") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 30)) +
  theme(axis.text= element_text(size = 28)) +
  geom_smooth(method=lm, color = "gray44", se = FALSE) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")

# stony coral
prey_stony <- ggplot(SVCprey_model_data, aes(x = stony, y = log_difference)) + 
  geom_jitter(width = 2, height = 0.1) +
  theme_classic() + xlab("Percent Stony Coral") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 30)) +
  theme(axis.text= element_text(size = 28)) +
  geom_smooth(method=lm, color = "gray44", se = FALSE) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")

# aggregation behavior
prey_behav <- ggplot(SVCprey_model_data, aes(x = behavior2, y = log_difference, 
                                             fill = behavior2)) + 
  geom_boxplot(show.legend = FALSE) + 
  theme_classic() + 
  xlab("Aggregation Behaviour") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 30)) + 
  theme(axis.text= element_text(size = 28)) + 
  theme(legend.position = "none") + 
  scale_fill_brewer(palette = "Greys") + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40")

# crypsis
prey_cryptic <- ggplot(SVCprey_model_data, aes(x = cryptic_behaviour2, 
                                               y = log_difference,fill = cryptic_behaviour2)) + 
  geom_boxplot() +
  theme_classic() + xlab("Cryptic Behaviour") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 30)) +
  theme(axis.text= element_text(size = 28)) +
  theme(legend.position = "none") +
  # scale_fill_brewer(palette = "YlGnBu") +
  scale_fill_manual(values = c("gray88", "gray44")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")

# maximum length
prey_max <- ggplot(SVCprey_model_data, aes(x = max_length, 
                                           y = log_difference)) + 
  geom_jitter(width = 1, height = 0.1)  +
  theme_classic() + xlab("Maximum Length") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 30)) +
  theme(axis.text= element_text(size = 28)) +
  geom_smooth(method=lm, color = "gray44", se = FALSE) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")

# nocturnality 
prey_nocturn <- ggplot(SVCprey_model_data, aes(x = nocturnal, y = log_difference, 
                                               fill = nocturnal)) + 
  geom_boxplot(show.legend = FALSE) + 
  theme_classic() + 
  xlab("Nocturnality") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 30)) + 
  theme(axis.text= element_text(size = 28)) + 
  theme(legend.position = "none") + 
  scale_fill_brewer(palette = "Greys") + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40")

# position
prey_position <- ggplot(SVCprey_model_data, aes(x = position, y = log_difference, 
                                                fill = position)) + 
  geom_boxplot(show.legend = FALSE) + 
  theme_classic() + 
  xlab("Water Column Position") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 30)) + 
  theme(axis.text= element_text(size = 28)) + 
  theme(legend.position = "none") + 
  scale_fill_brewer(palette = "Greys") + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40")

# size class
prey_size <- ggplot(SVCprey_model_data, aes(x = size_bin_char, 
                                            y = log_difference, fill = size_bin_char)) + 
  geom_boxplot(show.legend = FALSE) + 
  theme_classic() + 
  xlab("Size Class") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 48)) + 
  theme(axis.text= element_text(size = 44)) + 
  theme(legend.position = "none") + 
  scale_fill_brewer(palette = "YlGnBu") + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40")

# coloration
prey_colour <- ggplot(SVCprey_model_data, aes(x = coloration2, 
                                              y = log_difference, fill = coloration2)) + 
  geom_boxplot(show.legend = FALSE) + 
  theme_classic() + 
  xlab("Coloration") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 30)) + 
  theme(axis.text= element_text(size = 28)) + 
  theme(legend.position = "none") + 
  scale_fill_manual(name = "Coloration", values = c("goldenrod4", "wheat1", 
                                                    "gray85", "yellow")) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40")

# shape
prey_shape <- ggplot(SVCprey_model_data, aes(x = shape2, y = log_difference, 
                                             fill = shape2)) + 
  geom_boxplot(show.legend = FALSE) + 
  theme_classic() + 
  xlab("Body Shape") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 48)) + 
  theme(axis.text= element_text(size = 44)) + 
  theme(legend.position = "none") + 
  scale_fill_brewer(palette = "Greys") + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40")

# coloration*size class
prey_sizecol <- ggplot(SVCprey_model_data, aes(colouration, log_difference, 
                                               fill = size_bin_char)) + 
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

# shape*size class
prey_sizesha <- ggplot(SVCprey_model_data, aes(shape2, log_difference, 
                                               fill = size_bin_char)) + 
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


# SVC vs. Transect Survey Effect Size Plot =====================================

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
SVCprey_model_avg_plot[13,1] = "Size class 4 (15-20cm)"
SVCprey_model_avg_plot[14,1] = "Size class 5 (20-30cm)"
SVCprey_model_avg_plot[15,1] = "Size class 1 (0-5cm)"
SVCprey_model_avg_plot[16,1] = "Size class 6 (>30cm)"
SVCprey_model_avg_plot[17,1] = "Size class 2 (5-10cm)"
SVCprey_model_avg_plot[18,1] = "Coral cover"
SVCprey_model_avg_plot[19,1] = "Colorful:Size class 4"
SVCprey_model_avg_plot[20,1] = "Neutral:Size class 4"
SVCprey_model_avg_plot[21,1] = "Silvering:Size class 4"
SVCprey_model_avg_plot[22,1] = "Colorful:Size class 5"
SVCprey_model_avg_plot[23,1] = "Neutral:Size class 5"
SVCprey_model_avg_plot[24,1] = "Silvering:Size class 5"
SVCprey_model_avg_plot[25,1] = "Colorful:Size class 1"
SVCprey_model_avg_plot[26,1] = "Neutral:Size class 1"
SVCprey_model_avg_plot[27,1] = "Silvering:Size class 1"
SVCprey_model_avg_plot[28,1] = "Colorful:Size class 6"
SVCprey_model_avg_plot[29,1] = "Neutral:Size class 6"
SVCprey_model_avg_plot[30,1] = "Silvering:Size class 6"
SVCprey_model_avg_plot[31,1] = "Colorful:Size class 2"
SVCprey_model_avg_plot[32,1] = "Neutral:Size class 2"
SVCprey_model_avg_plot[33,1] = "Silvering:Size class 2"
SVCprey_model_avg_plot[34,1] = "Elongated:Size class 4"
SVCprey_model_avg_plot[35,1] = "Fusiform:Size class 4"
SVCprey_model_avg_plot[36,1] = "Globiform:Size class 4"
SVCprey_model_avg_plot[37,1] = "Elongated:Size class 5"
SVCprey_model_avg_plot[38,1] = "Fusiform:Size class 5"
SVCprey_model_avg_plot[39,1] = "Globiform:Size class 5"
SVCprey_model_avg_plot[40,1] = "Elongated:Size class 1"
SVCprey_model_avg_plot[41,1] = "Fusiform:Size class 1"
SVCprey_model_avg_plot[42,1] = "Globiform:Size class 1"
SVCprey_model_avg_plot[43,1] = "Elongated:Size class 6"
SVCprey_model_avg_plot[44,1] = "Fusiform:Size class 6"
SVCprey_model_avg_plot[45,1] = "Globiform:Size class 6"
SVCprey_model_avg_plot[46,1] = "Elongated:Size class 2" 
SVCprey_model_avg_plot[47,1] = "Fusiform:Size class 2"
SVCprey_model_avg_plot[48,1] = "Globiform:Size class 2"
SVCprey_model_avg_plot[49,1] = "Nocturnality"
SVCprey_model_avg_plot[50,1] = "Demersal"
SVCprey_model_avg_plot[51,1] = "Crypsis"

# change order of rows
SVCprey_model_avg_plot <- SVCprey_model_avg_plot[c(18,9,7,28,30,29,22,24,23,19,21,20,31,33,32,25,27,26,4,6,5,45,44,43,39,38,37,36,35,34,48,47,46,42,41,40,12,11,10,16,14,13,17,15,50,49,8,2,3,51,1),]

# plot with confidence intervals 
SVCprey_coef_CI <- ggplot(data=SVCprey_model_avg_plot[1:51], 
                          aes(x=Coefficient, y=Estimate))+ 
  geom_hline(yintercept=0, color = "grey40",linetype="dashed", lwd=1.5)+
  geom_errorbar(aes(ymin=CI.min, ymax=CI.max), colour="grey65", 
                width=.2,lwd=1) +
  geom_point(size=7, aes(shape = significance))+ 
  theme_classic(base_size = 20)+ 
  scale_shape_manual(values = c(16,8))+
  ylim(c(-5.2, 5.6)) +
  coord_flip() +
  theme(legend.position = "none",
        axis.title = element_text(size = 30),
        axis.text = element_text(size = 16, color = "black")) +
  scale_x_discrete(limits=SVCprey_model_avg_plot$Coefficient)
ggsave(here("./visuals/SVCprey_coef_plot_CIs.png"), SVCprey_coef_CI)


# SVC vs. Roving Survey Covariate Plots ========================================

# crypsis
pred_cryptic <- ggplot(SVCpred_model_data, aes(x = cryptic_behaviour, y = log_difference, 
                                               fill = cryptic_behaviour)) + 
  geom_boxplot(show.legend = FALSE) + 
  theme_classic() + 
  xlab("Presence of Cryptic Behavior") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 30)) + 
  theme(axis.text= element_text(size = 28)) + 
  theme(legend.position = "none") + 
  scale_fill_brewer(palette = "Greys") + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40")

# nocturnality 
pred_nocturn <- ggplot(SVCpred_model_data, aes(x = nocturnal, y = log_difference, 
                                               fill = nocturnal)) + 
  geom_boxplot(show.legend = FALSE) + 
  theme_classic() + 
  xlab("Nocturnality") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 30)) + 
  theme(axis.text= element_text(size = 28)) + 
  theme(legend.position = "none") + 
  scale_fill_brewer(palette = "Greys") + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40")

# size class
pred_size <- ggplot(SVCpred_model_data, aes(x = size_bin_char, y = log_difference, 
                                            fill = size_bin_char)) + 
  geom_boxplot(show.legend = FALSE) + 
  theme_classic() + 
  xlab("Size Class") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 26)) + 
  theme(axis.text= element_text(size = 26)) + 
  theme(legend.position = "none") + 
  scale_fill_brewer(palette = "Greys") + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40")

# coloration
pred_colour <- ggplot(SVCpred_model_data, aes(x = colouration, y = log_difference, 
                                              fill = colouration)) + 
  geom_boxplot(show.legend = FALSE) + 
  theme_classic() + 
  xlab("Coloration") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 26)) + 
  theme(axis.text= element_text(size = 26)) + 
  theme(legend.position = "none") + 
  scale_fill_brewer(palette = "Greys") + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40")

# shape
pred_shape <- ggplot(SVCpred_model_data, aes(x = shape, y = log_difference, 
                                             fill = shape)) + 
  geom_boxplot(show.legend = FALSE) + 
  theme_classic() + 
  xlab("Shape") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 26)) + 
  theme(axis.text= element_text(size = 26)) + 
  theme(legend.position = "none") + 
  scale_fill_brewer(palette = "Greys") + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40")


# SVC vs. Roving Survey Effect Size Plot =======================================

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
SVCpred_model_avg_plot <- SVCpred_model_avg_plot[c(7,5,4,8,6,3,2,9,11,10,1),]

# plot with confidence intervals 
SVCpred_coef_CI <- ggplot(data=SVCpred_model_avg_plot[1:11,], 
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
ggsave(here("./visuals/SVCpred_coef_plot_CIs.png"), SVCpred_coef_CI)


