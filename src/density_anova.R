########## SURVEY COMPARISON PROJECT DENSITY COMPARISON ##########
########## 
##########
# This file creates a dataframe outlining the density of 8 RVC focal species in 
# each session between SVC and transect surveys. The dataframe created is used 
# to compare average densities of each species between surveys using an ANOVA, 
# as well as average densities of each species individually using ANOVAs. A 
# barplot of the average recorded density for each species in SVC and transect 
# surveys is also created. 
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2021-07-02
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
prey_fish <- read_csv(here("./clean_data/prey_fish_data.csv"))
prey_meta <- read_csv(here("./clean_data/prey_metadata.csv"))
pred_fish <- read_csv(here("./clean_data/pred_fish_data.csv"))
pred_meta <- read_csv(here("./clean_data/pred_metadata.csv"))
traits <- read_csv(here("./clean_data/fish_traits.csv"))


# Obtain Average Densities =====================================================

# The following compiles average density values for each species between survey 
# types 

# select transect species and session columns
prey_species <- prey_fish[,c(1,3)]

# select roving species and session columns
pred_species <- pred_fish[,c(1,3)]

# aggregate transect species by session
prey_species <- prey_species %>% group_by(session, species) %>% tally()

# aggregate roving species by session
pred_species <- pred_species %>% group_by(session, species) %>% tally()

# rename abundance columns
prey_species <- prey_species %>% rename(prey_abundance = n)
pred_species <- pred_species %>% rename(pred_abundance = n)

# select transect session and area columns
prey_area <- prey_meta[,c(1,15)]

# select roving session and area columns
pred_area <- pred_meta[,c(1,21)]

# aggregate transect area by session
prey_area <- aggregate(.~session, prey_area, sum)

# aggregate roving area by session
pred_area <- aggregate(.~session, pred_area, sum)

# join transect area to fish data
prey_density <- join(prey_species, prey_area, by = NULL, type = "full", 
                     match = "all")

# join roving area to fish data
pred_density <- join(pred_species, pred_area, by = NULL, type = "full", 
                     match = "all")

# select SVC session, species, and abundance columns
SVC_density <- SVC_data[,c(1,12,37,38)]

# transect density calculation
prey_density$prey_density <- 
  (prey_density$prey_abundance)/(prey_density$prey_tran_area)

# roving density calculation
pred_density$pred_density <- 
  (pred_density$pred_abundance)/(pred_density$pred_trans_area)

# SVC density calculation 
SVC_density$SVC_density <- 
  (SVC_density$SVC_abundance)/(SVC_density$SVC_cylinder_area)

# add survey column
prey_density$survey <- "transect"
pred_density$survey <- "roving"
SVC_density$survey <- "SVC"

# select session, survey, species, and density columns
prey_density <- prey_density[,c(1,6,2,5)]
pred_density <- pred_density[,c(1,6,2,5)]
SVC_density <- SVC_density[,c(1,6,3,5)]

# rename density columns
prey_density <- rename(prey_density, density = prey_density)
pred_density <- rename(pred_density, density = pred_density)
SVC_density <- rename(SVC_density, density = SVC_density)

# bind dataframes
SVCprey_density <- bind_rows(SVC_density, prey_density)
SVCpred_density <- bind_rows(SVC_density, pred_density)

# remove SVC/transect problem species
SVCprey_density <- SVCprey_density[SVCprey_density$species !="silverside",]

# select roving species
pred_presence <- traits[,c(4,7)]

# rename columns
pred_presence <- pred_presence %>% rename(species = common_name)

# join predator presence to SVC and roving survey dataframe
SVCpred_density <- join(SVCpred_density, pred_presence, by = NULL, 
                         type = "full", match = "all")

# filter for species present in predator/roving surveys
SVCpred_density <- SVCpred_density %>% filter(predator_presence == 1)

# SVC/roving problem species
SVCpred_density <- SVCpred_density[SVCpred_density$species !="gray snapper",]

# remove NA values
SVCprey_density <- na.omit(SVCprey_density)
SVCpred_density <- na.omit(SVCpred_density)


# SVC vs. Transect Density ANOVA ===============================================

# The following performs an ANOVA across all SVC focal species to determine
# differences in their average densities between survey types.

# one-way ANOVA
SVCprey_density_anova <- aov(density~survey, SVCprey_density)

# one-way Tukey Test 
TukeyHSD(SVCprey_density_anova)

# two-way ANOVA
SVCprey_density_twaov <- aov(density~survey+species, SVCprey_density)

# two-way Tukey Test
TukeyHSD(SVCprey_density_twaov)

# shapiro-wilk normality test
with(SVCprey_density, shapiro.test(density[survey == "SVC"]))
with(SVCprey_density, shapiro.test(density[survey == "transect"]))

# kruskal-wallis test
SVCprey_density_kruskal <- kruskal.test(density~survey, data = SVCprey_density)
SVCprey_density_mann <- wilcox.test(density~survey, data = SVCprey_density)

# average SVC density 
mean(SVCprey_density$density[SVCprey_density$survey == "SVC"])

# average transect density 
mean(SVCprey_density$density[SVCprey_density$survey == "transect"])


# SVC vs. Roving Density ANOVA =================================================

# The following performs an ANOVA across all SVC focal species to determine
# differences in their average densities between survey types.

# one-way ANOVA
SVCpred_density_anova <- aov(density~survey, SVCpred_density)

# one-way Tukey Test 
TukeyHSD(SVCpred_density_anova)

# two-way ANOVA
SVCpred_density_twaov <- aov(density~survey+species, SVCpred_density)

# two-way Tukey Test
TukeyHSD(SVCpred_density_twaov)

# shapiro-wilk normality test
with(SVCpred_density, shapiro.test(density[survey == "SVC"]))
with(SVCpred_density, shapiro.test(density[survey == "roving"]))

# kruskal-wallis test
SVCpred_density_kruskal <- kruskal.test(density~survey, data = SVCpred_density)
SVCpred_density_mann <- wilcox.test(density~survey, data = SVCpred_density)

# average SVC density 
mean(SVCpred_density$density[SVCpred_density$survey == "SVC"])

# average transect density 
mean(SVCpred_density$density[SVCpred_density$survey == "roving"])
