########## SURVEY COMPARISON PROJECT COVARIATE PLOTS ##########
########## 
##########
# This file creates scatterplots and boxplots of predictors from linear mixed
# effects models run to compare fish density differences between SVC, transect,
# and roving reef surveys in relation to species, habitat, and survey traits.
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2021-05-31
##########
##########


# Set-Up =======================================================================

# packages
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(here)

# data
SVCprey_model_data <- read_csv(here("./dataframes/SVCprey_dataframe.csv"))
SVCpred_model_data <- read_csv(here("./dataframes/SVCpred_dataframe.csv"))


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

# re-name cryptic behaviour column
SVCprey_model_data$cryptic_behaviour2 <- 
  ifelse(SVCprey_model_data$cryptic_behaviour == 1, 
                                 "cryptic behaviour", "none")

# re-order colouration levels
SVCprey_model_data$colouration <- 
  factor(SVCprey_model_data$colouration, 
         levels = c("camouflage", "neutral", "silvering", "colourful"))

# re-order shape levels
SVCprey_model_data$shape <- 
  factor(SVCprey_model_data$shape, 
         levels = c("elongated", "fusiform", "compressiform", "globiform"))



# SVC vs. Transect Survey Plots ================================================

# The following creates scatterplots and boxplots of significant predictors from 
# the SVC vs. transect survey linear mixed effects model. 

# habitat type boxplot
TukeyHSD(aov(log_difference~habitat, SVCprey_model_data))
prey_hab <- ggplot(SVCprey_model_data, aes(x = habitat, y = log_difference, 
                               fill = habitat)) + 
  geom_boxplot() +
  theme_classic() + xlab("Reef Type") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 24)) +
  theme(axis.text= element_text(size = 22)) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray88", "gray44")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")
ggsave(here("./visuals/SVCprey_habitat_box.png"), prey_hab)

# stony coral scatterplot
prey_stony <- ggplot(SVCprey_model_data, aes(x = stony, y = log_difference)) + 
  geom_jitter(width = 2, height = 0.1) +
  theme_classic() + xlab("Percent Stony Coral") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 24)) +
  theme(axis.text= element_text(size = 22)) +
  geom_smooth(method=lm, color = "gray44", se = FALSE) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")
ggsave(here("./visuals/SVCprey_stony_scatter.png"), prey_stony)

# octocoral scatterplot
prey_octo <- ggplot(SVCprey_model_data, aes(x = octocoral, 
                                            y = log_difference)) + 
  geom_jitter(width = 2, height = 0.1) +
  theme_classic() + xlab("Percent Octocoral") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 24)) +
  theme(axis.text= element_text(size = 22)) +
  geom_smooth(method=lm, color = "gray44", se = FALSE) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")
ggsave(here("./visuals/SVCprey_octocoral_scatter.png"), prey_octo)

# cryptic behaviour boxplot
prey_cryptic <- ggplot(SVCprey_model_data, aes(x = cryptic_behaviour2, 
                y = log_difference,fill = cryptic_behaviour2)) + 
  geom_boxplot() +
  theme_classic() + xlab("Cryptic Behaviour") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 24)) +
  theme(axis.text= element_text(size = 22)) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "YlGnBu") +
  # scale_fill_manual(values = c("gray88", "gray44")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")
ggsave(here("./visuals/SVCprey_cryptic_box.png"), prey_cryptic)

# size bin boxplot
TukeyHSD(aov(log_difference~size_bin_char, SVCprey_model_data))
prey_size <- ggplot(SVCprey_model_data, aes(x = size_bin_char, 
             y = log_difference, fill = size_bin_char)) + 
  geom_boxplot(show.legend = FALSE) + 
  theme_classic() + 
  xlab("Size Bin") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 28)) + 
  theme(axis.text= element_text(size = 26)) + 
  theme(legend.position = "none") + 
  scale_fill_brewer(palette = "YlGnBu") + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40")
ggsave(here("./visuals/SVCprey_size_box.png"), prey_size)

# colouration boxplot
TukeyHSD(aov(log_difference~colouration, SVCprey_model_data))
prey_colour <- ggplot(SVCprey_model_data, aes(x = colouration, 
               y = log_difference, fill = colouration)) + 
  geom_boxplot(show.legend = FALSE) + 
  theme_classic() + 
  xlab("Colouration") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 24)) + 
  theme(axis.text= element_text(size = 22)) + 
  theme(legend.position = "none") + 
  scale_fill_manual(name = "Colouration", values = c("goldenrod4", "wheat1", 
                                                     "gray85", "yellow")) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40")
ggsave(here("./visuals/SVCprey_colouration_box.png"), prey_colour)

# behaviour boxplot
TukeyHSD(aov(log_difference~behavior, SVCprey_model_data))
prey_behav <- ggplot(SVCprey_model_data, aes(x = behavior, y = log_difference, 
                               fill = behavior)) + 
  geom_boxplot(show.legend = FALSE) + 
  theme_classic() + 
  xlab("Aggregation Behaviour") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 24)) + 
  theme(axis.text= element_text(size = 22)) + 
  theme(legend.position = "none") + 
  scale_fill_brewer(palette = "Greys") + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40")
ggsave(here("./visuals/SVCprey_behaviour_box.png"), prey_behav)

# maximum lengths scatterplot
prey_max <- ggplot(SVCprey_model_data, aes(x = max_length, 
                                           y = log_difference)) + 
  geom_jitter(width = 1, height = 0.1)  +
  theme_classic() + xlab("Maximum Length") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 24)) +
  theme(axis.text= element_text(size = 22)) +
  geom_smooth(method=lm, color = "gray44", se = FALSE) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")
ggsave(here("./visuals/SVCprey_maxlength_scatter.png"), prey_max)

# shape boxplot
TukeyHSD(aov(log_difference~shape, SVCprey_model_data))
prey_shape <- ggplot(SVCprey_model_data, aes(x = shape, y = log_difference, 
                               fill = shape)) + 
  geom_boxplot(show.legend = FALSE) + 
  theme_classic() + 
  xlab("Body Shape") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 28)) + 
  theme(axis.text= element_text(size = 26)) + 
  theme(legend.position = "none") + 
  scale_fill_brewer(palette = "Greys") + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40")
ggsave(here("./visuals/SVCprey_shape_box.png"), prey_shape)

# colouration*size bin boxplot
prey_sizecol <- ggplot(SVCprey_model_data, aes(colouration, log_difference, 
                               fill = size_bin_char)) + 
  geom_boxplot(show.legend = TRUE) + 
  theme_classic() + 
  xlab("Colouration") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 24)) +
  theme(axis.text= element_text(size = 22)) +
  theme(legend.text = element_text(size = 22)) +
  theme(legend.title = element_text(size = 24)) +
  scale_fill_brewer(name = "Size Bin", palette = "YlGnBu") +
  geom_hline(yintercept = 0,
               linetype = "dashed",
               colour = "grey40")
ggsave(here("./visuals/SVCprey_sizecolour_box.png"), prey_sizecol)

# shape*size bin boxplot
options(max.print = 10000)
TukeyHSD(aov(log_difference~shape*size_bin_char, SVCprey_model_data))
prey_sizesha <- ggplot(SVCprey_model_data, aes(shape, log_difference, 
                               fill = size_bin_char)) + 
  geom_boxplot(show.legend = TRUE) + 
  theme_classic() + 
  xlab("Body Shape") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 24)) +
  theme(axis.text= element_text(size = 22)) +
  theme(legend.text = element_text(size = 22)) +
  theme(legend.title = element_text(size = 24)) +
  scale_fill_brewer(name = "Size Bin", palette = "YlGnBu") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")
ggsave(here("./visuals/SVCprey_sizeshape_box.png"), prey_sizesha)

# arrange habitat traits on one page
habitat_trait_plots <- ggarrange(prey_hab, prey_stony, prey_octo, 
          labels = c("", "", "", ""), ncol = 2, nrow = 2)
ggsave(here("./visuals/SVCprey_habitat_trait_plots.png"), habitat_trait_plots)

# arrange fish traits on one page
fish_trait_plots <- ggarrange(prey_cryptic, prey_max, prey_behav, prey_colour,  
          labels = c("", "", "", ""), ncol = 2, nrow = 2)
ggsave(here("./visuals/SVCprey_fish_trait_plots.png"), fish_trait_plots)

# arrange size*colouration on one page
sizecol_plots <- ggarrange(prey_sizecol,
          ggarrange(prey_size, prey_colour, ncol = 2, labels = c("","")),
          nrow = 2,
          labels = "")
ggsave(here("./visuals/SVCprey_sizecolour_plots.png"), sizecol_plots)

# arrange size*shape on one page
sizesha_plots <- ggarrange(prey_sizesha,
          ggarrange(prey_size, prey_shape, ncol = 2, labels = c("","")),
          nrow = 2,
          labels = "")
ggsave(here("./visuals/SVCprey_sizeshape_plots.png"), sizesha_plots)


# SVC vs. Roving Survey Plots ==================================================

# The following creates scatterplots and boxplots of significant predictors 
# from the SVC vs. roving survey linear mixed effects model.

# stony coral scatterplot
pred_stony <- ggplot(SVCpred_model_data, aes(x = stony, y = log_difference)) + 
  geom_jitter(width = 2, height = 1)  +
  theme_classic() + xlab("Percent Stony Coral") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 14)) +
  theme(axis.text= element_text(size = 14)) +
  geom_smooth(method=lm, se = FALSE) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")
ggsave(here("./visuals/SVCpred_stony_scatter.png"), pred_stony)

# size bin boxplot
pred_size <- ggplot(SVCpred_model_data, aes(x = size_bin_char, 
                    y = log_difference, fill = size_bin_char)) + 
  geom_boxplot() +
  theme_classic() + xlab("Size Bin") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 14)) +
  theme(axis.text= element_text(size = 14)) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "YlGnBu") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")
ggsave(here("./visuals/SVCpred_size_box.png"), pred_size)

# colouration boxplot
TukeyHSD(aov(log_difference~colouration, SVCpred_model_data))
pred_colour <- ggplot(SVCpred_model_data, aes(x = colouration, 
                      y = log_difference, fill = colouration)) + 
  geom_boxplot() +
  theme_classic() + xlab("Colouration") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 24)) +
  theme(axis.text= element_text(size = 22)) +
  theme(legend.position = "none") +
  scale_fill_manual(name = "Colouration", values = c("goldenrod4", "wheat1", 
                                                     "gray85")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")
ggsave(here("./visuals/SVCpred_colour_box.png"), pred_colour)

# shape boxplot
pred_shape <- ggplot(SVCpred_model_data, aes(x = shape, y = log_difference, 
                               fill = shape)) + 
  geom_boxplot() +
  theme_classic() + xlab("Body Shape") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 14)) +
  theme(axis.text= element_text(size = 14)) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "YlGnBu") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")
ggsave(here("./visuals/SVCpred_shape_box.png"), pred_shape)

# arrange on one page
SVCpred_plots <- ggarrange(pred_stony, pred_size, pred_colour, pred_shape, 
          labels = c("", "", "", ""), ncol = 2, nrow = 2)
ggsave(here("./visuals/SVCpred_plots.png"), SVCpred_plots)

pred_sizecol <- ggplot(SVCpred_model_data, aes(colouration, log_difference, 
                                               fill = size_bin_char)) + 
  geom_boxplot(show.legend = TRUE) + 
  theme_classic() + 
  xlab("Colouration") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 14)) +
  theme(axis.text= element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  theme(legend.title = element_text(size = 14)) +
  scale_fill_brewer(name = "Size Bin", palette = "YlGnBu") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")