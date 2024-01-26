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
  theme(axis.title = element_text(size = 30)) +
  theme(axis.text= element_text(size = 28)) +
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
  theme(axis.title = element_text(size = 30)) +
  theme(axis.text= element_text(size = 28)) +
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
  theme(axis.title = element_text(size = 30)) +
  theme(axis.text= element_text(size = 28)) +
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
  theme(axis.title = element_text(size = 30)) +
  theme(axis.text= element_text(size = 28)) +
  theme(legend.position = "none") +
  # scale_fill_brewer(palette = "YlGnBu") +
  scale_fill_manual(values = c("gray88", "gray44")) +
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
  xlab("Size Class") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 48)) + 
  theme(axis.text= element_text(size = 44)) + 
  theme(legend.position = "none") + 
  scale_fill_brewer(palette = "YlGnBu") + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40")
ggsave(here("./visuals/SVCprey_size_box.png"), prey_size)

# colouration boxplot
TukeyHSD(aov(log_difference~coloration2, SVCprey_model_data))
prey_colour <- ggplot(SVCprey_model_data, aes(x = coloration2, 
               y = log_difference, fill = coloration2)) + 
  geom_boxplot(show.legend = FALSE) + 
  theme_classic() + 
  xlab("Coloration") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 30)) + 
  theme(axis.text= element_text(size = 20)) + 
  theme(legend.position = "none") + 
  scale_fill_manual(values = c("chocolate4", "navajowhite1", "grey90", "yellow")) +
  #scale_fill_brewer(palette = "Greys") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40")
ggsave(here("./visuals/SVCprey_colouration_box.png"), prey_colour)

# behaviour boxplot
TukeyHSD(aov(log_difference~behavior, SVCprey_model_data))
prey_behav <- ggplot(SVCprey_model_data, aes(x = behavior2, y = log_difference, 
                               fill = behavior2)) + 
  geom_boxplot(show.legend = FALSE) + 
  theme_classic() + 
  xlab("Aggregation Behaviour") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 30)) + 
  theme(axis.text= element_text(size = 20)) + 
  theme(legend.position = "none") + 
  scale_fill_brewer(palette = "Greys") + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40")
ggsave(here("./visuals/SVCprey_behaviour_box.png"), prey_behav)

# nocturnality boxplot
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

# position boxplot
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

# maximum lengths scatterplot
prey_max <- ggplot(SVCprey_model_data, aes(x = max_length, 
                                           y = log_difference)) + 
  geom_jitter(width = 1, height = 0.1)  +
  theme_classic() + xlab("Maximum Length") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 30)) +
  theme(axis.text= element_text(size = 20)) +
  geom_smooth(method=lm, color = "gray44", se = FALSE) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")
ggsave(here("./visuals/SVCprey_maxlength_scatter.png"), prey_max)

# shape boxplot
TukeyHSD(aov(log_difference~shape, SVCprey_model_data))
prey_shape <- ggplot(SVCprey_model_data, aes(x = shape2, y = log_difference, 
                               fill = shape2)) + 
  geom_boxplot(show.legend = FALSE) + 
  theme_classic() + 
  xlab("Body Shape") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 30)) + 
  theme(axis.text= element_text(size = 20)) + 
  theme(legend.position = "none") + 
  scale_fill_brewer(palette = "Greys") + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40")
ggsave(here("./visuals/SVCprey_shape_box.png"), prey_shape)

# shape*size bin boxplot
options(max.print = 10000)
TukeyHSD(aov(log_difference~shape*size_bin_char, SVCprey_model_data))
prey_sizesha <- ggplot(SVCprey_model_data, aes(shape2, log_difference, 
                               fill = size_bin_char)) + 
  geom_boxplot(show.legend = TRUE) + 
  theme_classic() + 
  xlab("Body Shape") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 30)) +
  theme(axis.text= element_text(size = 20)) +
  theme(legend.text = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  scale_fill_manual(name = "Size Class", labels = c("1 (0-5cm)", "2 (5-10cm)", "3 (10-15cm)", "4 (15-20cm)", "5 (20-30cm)", "6 (>30cm)"), values = c("cornsilk1", "khaki1", "lightgreen", "cyan1", "darkturquoise", "turquoise4")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")
ggsave(here("./visuals/SVCprey_sizeshape_box.png"), prey_sizesha)

# colouration*size bin boxplot
options(max.print = 10000)
TukeyHSD(aov(log_difference~colouration*size_bin_char, SVCprey_model_data))
prey_sizecol <- ggplot(SVCprey_model_data, aes(colouration, log_difference, 
                                               fill = size_bin_char)) + 
  geom_boxplot(show.legend = TRUE) + 
  theme_classic() + 
  xlab("Coloration") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 30)) +
  theme(axis.text= element_text(size = 20)) +
  theme(legend.text = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  scale_fill_manual(name = "Size Class", labels = c("1 (0-5cm)", "2 (5-10cm)", "3 (10-15cm)", "4 (15-20cm)", "5 (20-30cm)", "6 (>30cm)"), values = c("cornsilk1", "khaki1", "lightgreen", "cyan1", "darkturquoise", "turquoise4")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")

# elongated vs fusiform density differences for size classes 1-4
mean(SVCprey_model_data$log_difference[SVCprey_model_data$shape == 'elongated' & SVCprey_model_data$size_bin <= 4]) # -2.712606
mean(SVCprey_model_data$log_difference[SVCprey_model_data$shape == 'fusiform' & SVCprey_model_data$size_bin <= 4]) # -1.089659 

# elongated mean density differences for small vs. large size classes
mean(SVCprey_model_data$SVC_density[SVCprey_model_data$shape == 'elongated' & SVCprey_model_data$size_bin <= 4]) # 0.007240227
mean(SVCprey_model_data$prey_density[SVCprey_model_data$shape == 'elongated' & SVCprey_model_data$size_bin <= 4]) # 0.1256763
# prey is 1635.81% larger density than SVC
mean(SVCprey_model_data$SVC_density[SVCprey_model_data$shape == 'elongated' & SVCprey_model_data$size_bin >=5]) # 0.005192337
mean(SVCprey_model_data$prey_density[SVCprey_model_data$shape == 'elongated' & SVCprey_model_data$size_bin >=5]) # 0.004320175
# prey is 16.8% smaller density than SVC

# fusiform mean density differences for small vs. large size classes
mean(SVCprey_model_data$SVC_density[SVCprey_model_data$shape == 'fusiform' & SVCprey_model_data$size_bin <= 4]) # 0.04535966
mean(SVCprey_model_data$prey_density[SVCprey_model_data$shape == 'fusiform' & SVCprey_model_data$size_bin <= 4]) # 0.0870417
# prey is 91.89% larger density than SVC
mean(SVCprey_model_data$SVC_density[SVCprey_model_data$shape == 'fusiform' & SVCprey_model_data$size_bin >=5]) # 0.03065944
mean(SVCprey_model_data$prey_density[SVCprey_model_data$shape == 'fusiform' & SVCprey_model_data$size_bin >=5]) # 0.01974114
# prey is 35.61% smaller density than SVC


# arrange habitat traits on one page
habitat_trait_plots <- ggarrange(prey_stony, prey_octo, prey_hab,
          labels = c("", "", "", ""), ncol = 2, nrow = 2)
ggsave(here("./visuals/SVCprey_habitat_trait_plots.png"), habitat_trait_plots)

# arrange fish traits on one page
fish_trait_plots <- ggarrange(prey_max, prey_colour, prey_behav, prey_cryptic,  
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
pred_colour <- ggplot(SVCpred_model_data, aes(x = coloration2, 
                      y = log_difference, fill = coloration2)) + 
  geom_boxplot() +
  theme_classic() + xlab("Coloration") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size =  30)) +
  theme(axis.text= element_text(size = 20)) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Greys") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")
ggsave(here("./visuals/SVCpred_colour_box.png"), pred_colour)

# shape boxplot
TukeyHSD(aov(log_difference~shape, SVCpred_model_data))
pred_shape <- ggplot(SVCpred_model_data, aes(x = shape, y = log_difference, 
                               fill = shape)) + 
  geom_boxplot() +
  theme_classic() + xlab("Body Shape") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 30)) +
  theme(axis.text= element_text(size = 20)) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Greys") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")
ggsave(here("./visuals/SVCpred_shape_box.png"), pred_shape)

# reef type boxplot
pred_habitat <- ggplot(SVCpred_model_data, aes(x = habitat, y = log_difference, 
                                                fill = habitat)) + 
  geom_boxplot(show.legend = FALSE) + 
  theme_classic() + 
  xlab("Reef Type") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 30)) + 
  theme(axis.text= element_text(size = 28)) + 
  theme(legend.position = "none") + 
  scale_fill_brewer(palette = "Greys") + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40")

# cryptic behaviour boxplot
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

# nocturnality boxplot
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

# position boxplot
pred_position <- ggplot(SVCpred_model_data, aes(x = position, y = log_difference, 
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

# colouration boxplot
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

# shape boxplot
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

# size class boxplot
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