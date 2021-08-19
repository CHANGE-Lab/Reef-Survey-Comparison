########## SURVEY COMPARISON PROJECT SVC VS. ROVING DENSITY COMPARISON #########
########## 
##########
# This file creates a dataframe outlining the average density of 3 RVC focal 
# species which were recorded in both SVC and roving surveys in within each 
# session between SVC and roving surveys. The dataframe created is used to 
# compare average densities of each species between surveys as well as average 
# densities of each species individually using Kruskal-Wallis one-way analysis 
# of variance tests. A barplot of the average recorded density for each species 
# in SVC and roving surveys is also created. 
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2021-07-04
##########
##########


# Set-Up =======================================================================

# packages
library(plyr)
library(tidyverse)
library(ggplot2)
library(here)

# data
SVC_data <- read_csv(here("./clean_data/SVC_data.csv"))
pred_fish <- read_csv(here("./clean_data/pred_fish_data.csv"))
pred_meta <- read_csv(here("./clean_data/pred_metadata.csv"))


# Obtain Average Densities =====================================================

# The following compiles average density values for each species between SVC
# and roving surveys 

# filter for SVC focal species
SVC_data <- filter(SVC_data, species == "mutton snapper"|
                     species == "red grouper"|
                     species == "black grouper"|species == "lionfish")
pred_fish <- filter(pred_fish, species == "mutton snapper"|
                     species == "red grouper"|
                     species == "black grouper"|species == "lionfish")

# select roving species and session columns
pred_species <- pred_fish[,c(1,3)]

# aggregate species by session
pred_species <- pred_fish %>% group_by(session, species) %>% tally()

# rename abundance column
pred_species <- pred_species %>% rename(pred_abundance = n)

# select roving session and area columns
pred_area <- pred_meta[,c(1,21)]

# aggregate area by session
pred_area <- aggregate(.~session, pred_area, sum)

# join area to fish data
pred_density <- join(pred_species, pred_area, by = NULL, type = "full", 
                     match = "all")

# select SVC session, species, and abundance columns
SVC_density <- SVC_data[,c(1,12,37,38)]

# roving density calculation
pred_density$pred_density <- 
  (pred_density$pred_abundance)/(pred_density$pred_trans_area)

# SVC density calculation 
SVC_density$SVC_density <- 
  (SVC_density$SVC_abundance)/(SVC_density$SVC_cylinder_area)

# add survey column
pred_density$survey <- "roving"
SVC_density$survey <- "SVC"

# select session, survey, species, and density columns
pred_density <- pred_density[,c(1,6,2,5)]
SVC_density <- SVC_density[,c(1,6,3,5)]

# rename density columns
pred_density <- rename(pred_density, density = pred_density)
SVC_density <- rename(SVC_density, density = SVC_density)

# bind dataframes
SVCpred_density <- bind_rows(SVC_density, pred_density)

# remove NA values
SVCpred_density <- na.omit(SVCpred_density)


# Across Species ===============================================================

# The following performs an ANOVA across all SVC focal species to determine
# differences in their average densities between SVC and roving surveys. 

# shapiro-wilk normality test
with(SVCpred_density, shapiro.test(density[survey == "SVC"]))
with(SVCpred_density, shapiro.test(density[survey == "roving"]))

# kruskal-wallis test
SVCpred_density_kruskal <- kruskal.test(density~survey, data = SVCpred_density)
# Kruskal-Wallis chi-squared = 91.044, df = 1, p-value < 2.2e-16

# average SVC density 
mean(SVCpred_density$density[SVCpred_density$survey == "SVC"])
# 0.01646725

# average roving density 
mean(SVCpred_density$density[SVCpred_density$survey == "roving"])
# 0.001850097


# Mutton Snapper ANOVA =========================================================

# The following performs an ANOVA on the average density of mutton snapper 
# observed between SVC and roving surveys. 

# filter for mutton snapper
mutton_snapper_density <- filter(SVCpred_density, species == "mutton snapper")

# shapiro-wilk normality test
with(mutton_snapper_density, shapiro.test(density[survey == "SVC"]))
with(mutton_snapper_density, shapiro.test(density[survey == "roving"]))

# kruskal-wallis test
mutton_snapper_density_kruskal <- kruskal.test(density~survey, 
                                               data = mutton_snapper_density)
# Kruskal-Wallis chi-squared = 28.832, df = 1, p-value = 7.894e-08

# average SVC density 
mean(mutton_snapper_density$density[mutton_snapper_density$survey == "SVC"])
# 0.007670234

# average roving density 
mean(mutton_snapper_density$density[mutton_snapper_density$survey == "roving"])
# 0.0005641187


# Red Grouper ANOVA ============================================================

# The following performs an ANOVA on the average density of red grouper 
# observed between SVC and roving surveys. 

# filter for red grouper
red_grouper_density <- filter(SVCpred_density, species == "red grouper")

# shapiro-wilk normality test
with(red_grouper_density, shapiro.test(density[survey == "SVC"]))
with(red_grouper_density, shapiro.test(density[survey == "roving"]))

# kruskal-wallis test
red_grouper_density_kruskal <- kruskal.test(density~survey, 
                                            data = red_grouper_density)
# Kruskal-Wallis chi-squared = 20.208, df = 1, p-value = 6.947e-06

# average SVC density 
mean(red_grouper_density$density[red_grouper_density$survey == "SVC"])
# 0.02786595

# average roving density 
mean(red_grouper_density$density[red_grouper_density$survey == "roving"])
# 0.003325002


# Black Grouper ANOVA ==========================================================

# The following performs an ANOVA on the average density of black grouper 
# observed between SVC and roving surveys. 

# filter for black grouper
black_grouper_density <- filter(SVCpred_density, species == "black grouper")

# shapiro-wilk normality test
with(black_grouper_density, shapiro.test(density[survey == "SVC"]))
with(black_grouper_density, shapiro.test(density[survey == "roving"]))

# kruskal-wallis test
black_grouper_density_kruskal <- kruskal.test(density~survey, 
                                              data = black_grouper_density)
# Kruskal-Wallis chi-squared = 29.447, df = 1, p-value = 5.746e-08

# average SVC density 
mean(black_grouper_density$density[black_grouper_density$survey == "SVC"])
# 0.01044265

# average roving density 
mean(black_grouper_density$density[black_grouper_density$survey == "roving"])
# 0.001088917


# Lionfish ANOVA ===============================================================

# The following performs an ANOVA on the average density of lionfish 
# observed between SVC and roving surveys. 

# filter for lionfish
lionfish_density <- filter(SVCpred_density, species == "lionfish")

# shapiro-wilk normality test
with(lionfish_density, shapiro.test(density[survey == "SVC"]))
with(lionfish_density, shapiro.test(density[survey == "roving"]))

# kruskal-wallis test
lionfish_density_kruskal <- kruskal.test(density~survey, 
                                              data = lionfish_density)
# Kruskal-Wallis chi-squared = 12.426, df = 1, p-value = 0.0004233

# average SVC density 
mean(lionfish_density$density[lionfish_density$survey == "SVC"])
# 0.03116534

# average roving density 
mean(lionfish_density$density[lionfish_density$survey == "roving"])
# 0.002072866


# Density Barplot ==============================================================

# The following creates a barplot of the average density of each SVC focal 
# species between SVC and roving surveys.

# remove session column
SVCpred_density_bar <- SVCpred_density[,2:4]

# aggregate by species 
SVCpred_density_bar <- aggregate(.~species+survey, SVCpred_density_bar, mean)

# sort by species
SVCpred_density_bar <- SVCpred_density_bar[order(SVCpred_density_bar$species),]

# re-order surveys
SVCpred_density_bar$survey <- factor(SVCpred_density_bar$survey, 
                                     levels = c("SVC", "roving"))

# barplot
SVCpred_density_barplot <- ggplot(SVCpred_density_bar, aes(x = species, 
                           y = density, fill = survey)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  theme_classic() +
  xlab("Species") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  # scale_fill_manual(values = c("gray88", "gray44")) +
  scale_fill_brewer(palette = "YlGnBu") +
  theme(axis.title = element_text(size = 24)) +
  theme(axis.text = element_text(size = 22)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  theme(legend.text = element_text(size = 22)) +
  theme(legend.title = element_text(size = 24)) 
ggsave(here("./visuals/SVCpred_density_bar.png"), SVCpred_density_barplot)
