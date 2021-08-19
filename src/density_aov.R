########## SURVEY COMPARISON PROJECT DENSITY COMPARISON ##########
########## 
##########
# This file creates a dataframe outlining the density of all species in 
# each session between SVC, transect, and roving surveys. The dataframe created 
# is used to compare average densities of each species between surveys using an 
# ANOVA. An additional dataframe is created outlining the density of 8 RVC focal 
# species across each session in each survey type, and these are also compared 
# using an ANOVA. A barplot of the average recorded density for each of the 8 
# focal species in each survey type is also created. 
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2021-06-29
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
survey_density <- bind_rows(SVC_density, prey_density, pred_density)

# remove NA values
SVCprey_density <- na.omit(survey_density)


# ANOVA Across Species =========================================================

# The following performs an ANOVA across all SVC focal species to determine
# differences in their average densities between survey types.

# one-way ANOVA
survey_density_anova <- aov(density~survey, survey_density)

# one-way Tukey Test 
TukeyHSD(survey_density_anova)

# two-way ANOVA
twoway_density_aov <- aov(density~survey+species, survey_density)

# two-way Tukey Test
TukeyHSD(twoway_density_aov)


# ANOVA Across Focal Species ===================================================

# The following performs an ANOVA across all SVC focal species to determine
# differences in their average densities between survey types. 

# filter for SVC focal species
focal_density <- filter(survey_density, species == "white grunt"|
                     species == "bluestriped grunt"|species == "hogfish"|
                     species == "mutton snapper"|species == "yellowtail snapper"|
                     species == "yellowtail snapper"|species == "red grouper"|
                     species == "black grouper")

# ANOVA
focal_density_anova <- aov(density~survey, focal_density)

# Tukey Test 
TukeyHSD(focal_density_anova)


# Density Barplot ==============================================================

# The following creates a barplot of the average density of each SVC focal 
# species between SVC, transect, and roving surveys.

# remove session column
focal_density_bar <- focal_density[,2:4]

# aggregate by species 
focal_density_bar <- aggregate(.~species+survey, focal_density_bar, mean)

# sort by species
focal_density_bar <- focal_density_bar[order(focal_density_bar$species),]

# barplot
focal_density_barplot <- ggplot(focal_density_bar, aes(x = species, 
                         y = density, fill = survey)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  theme_classic() +
  xlab("Species") + 
  ylab("Average Density") +
  scale_fill_manual(values = c("lemonchiffon1", "aquamarine3", "navyblue")) +
  theme(axis.title = element_text(size = 24)) +
  theme(axis.text = element_text(size = 22)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.text = element_text(size = 22)) +
  theme(legend.title = element_text(size = 24)) 
ggsave(here("./visuals/SVCprey_density_bar.png"), SVCprey_density_barplot)