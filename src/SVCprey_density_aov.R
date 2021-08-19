########## SURVEY COMPARISON PROJECT SVC VS. TRANSECT DENSITY COMPARISON #######
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
# DATE OF CREATION: 2021-06-24
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


# Obtain Average Densities =====================================================

# The following compiles average density values for each species between SVC
# and transect surveys 

# filter for SVC focal species
SVC_data <- filter(SVC_data, species == "white grunt"|
                     species == "bluestriped grunt"|species == "hogfish"|
                     species == "mutton snapper"|species == "yellowtail snapper"|
                     species == "gray snapper"|species == "red grouper"|
                     species == "black grouper")
prey_fish <- filter(prey_fish, species == "white grunt"|
                      species == "bluestriped grunt"|species == "hogfish"|
                      species == "mutton snapper"|species == "yellowtail snapper"|
                      species == "gray snapper"|species == "red grouper"|
                      species == "black grouper")

# select transect species and session columns
prey_species <- prey_fish[,c(1,3)]

# aggregate species by session
prey_species <- prey_fish %>% group_by(session, species) %>% tally()

# rename abundance column
prey_species <- prey_species %>% rename(prey_abundance = n)

# select transect session and area columns
prey_area <- prey_meta[,c(1,15)]

# aggregate area by session
prey_area <- aggregate(.~session, prey_area, sum)

# join area to fish data
prey_density <- join(prey_species, prey_area, by = NULL, type = "full", 
                     match = "all")

# select SVC session, species, and abundance columns
SVC_density <- SVC_data[,c(1,12,37,38)]

# transect density calculation
prey_density$prey_density <- 
  (prey_density$prey_abundance)/(prey_density$prey_tran_area)

# SVC density calculation 
SVC_density$SVC_density <- 
  (SVC_density$SVC_abundance)/(SVC_density$SVC_cylinder_area)

# add survey column
prey_density$survey <- "transect"
SVC_density$survey <- "SVC"

# select session, survey, species, and density columns
prey_density <- prey_density[,c(1,6,2,5)]
SVC_density <- SVC_density[,c(1,6,3,5)]

# rename density columns
prey_density <- rename(prey_density, density = prey_density)
SVC_density <- rename(SVC_density, density = SVC_density)

# bind dataframes
SVCprey_density <- bind_rows(SVC_density, prey_density)

# remove NA values
SVCprey_density <- na.omit(SVCprey_density)


# ANOVA Across Species =========================================================

# The following performs an ANOVA across all SVC focal species to determine
# differences in their average densities between SVC and transect surveys. 

# shapiro-wilk normality test
with(SVCprey_density, shapiro.test(density[survey == "SVC"]))
with(SVCprey_density, shapiro.test(density[survey == "transect"]))

# kruskal-wallis test
SVCprey_density_kruskal <- kruskal.test(density~survey, data = SVCprey_density)
# Kruskal-Wallis chi-squared = 6.2476, df = 1, p-value = 0.01244

# average SVC density 
mean(SVCprey_density$density[SVCprey_density$survey == "SVC"])
# 0.2035143

# average transect density 
mean(SVCprey_density$density[SVCprey_density$survey == "transect"])
# 0.2952237


# White Grunt ANOVA ============================================================

# The following performs an ANOVA on the average density of white grunts 
# observed between SVC and transect surveys. 

# filter for white grunts
white_grunt_density <- filter(SVCprey_density, species == "white grunt")

# shapiro-wilk normality test
with(white_grunt_density, shapiro.test(density[survey == "SVC"]))
with(white_grunt_density, shapiro.test(density[survey == "transect"]))

# kruskal-wallis test
white_grunt_density_kruskal <- kruskal.test(density~survey, data = white_grunt_density)
# Kruskal-Wallis chi-squared = 9.1079, df = 1, p-value = 0.002545

# average SVC density 
mean(white_grunt_density$density[white_grunt_density$survey == "SVC"])
# 0.4663638

# average transect density 
mean(white_grunt_density$density[white_grunt_density$survey == "transect"])
# 0.8134522


# Bluestriped Grunt ANOVA ======================================================

# The following performs an ANOVA on the average density of bluestriped grunts 
# observed between SVC and transect surveys. 

# filter for bluestriped grunts
bluestriped_grunt_density <- filter(SVCprey_density, 
                                    species == "bluestriped grunt")

# shapiro-wilk normality test
with(bluestriped_grunt_density, shapiro.test(density[survey == "SVC"]))
with(bluestriped_grunt_density, shapiro.test(density[survey == "transect"]))

# kruskal-wallis test
bluestriped_grunt_density_kruskal <- kruskal.test(density~survey, data = bluestriped_grunt_density)
# Kruskal-Wallis chi-squared = 2.8361, df = 1, p-value = 0.09217


# Hogfish ANOVA ================================================================

# The following performs an ANOVA on the average density of hogfish 
# observed between SVC and transect surveys. 

# filter for hogfish
hogfish_density <- filter(SVCprey_density, species == "hogfish")

# shapiro-wilk normality test
with(hogfish_density, shapiro.test(density[survey == "SVC"]))
with(hogfish_density, shapiro.test(density[survey == "transect"]))

# kruskal-wallis test
hogfish_density_kruskal <- kruskal.test(density~survey, data = hogfish_density)
# Kruskal-Wallis chi-squared = 0.72481, df = 1, p-value = 0.3946


# Mutton Snapper ANOVA =========================================================

# The following performs an ANOVA on the average density of mutton snapper 
# observed between SVC and transect surveys. 

# filter for mutton snapper
mutton_snapper_density <- filter(SVCprey_density, species == "mutton snapper")

# shapiro-wilk normality test
with(mutton_snapper_density, shapiro.test(density[survey == "SVC"]))
with(mutton_snapper_density, shapiro.test(density[survey == "transect"]))

# kruskal-wallis test
mutton_snapper_density_kruskal <- kruskal.test(density~survey, data = mutton_snapper_density)
# Kruskal-Wallis chi-squared = 0.15633, df = 1, p-value = 0.6926


# Gray Snapper ANOVA ===========================================================

# The following performs an ANOVA on the average density of gray snapper 
# observed between SVC and transect surveys. 

# filter for gray snapper
gray_snapper_density <- filter(SVCprey_density, species == "gray snapper")

# shapiro-wilk normality test
with(gray_snapper_density, shapiro.test(density[survey == "SVC"]))
with(gray_snapper_density, shapiro.test(density[survey == "transect"]))

# kruskal-wallis test
gray_snapper_density_kruskal <- kruskal.test(density~survey, data = gray_snapper_density)
# Kruskal-Wallis chi-squared = 0.065888, df = 1, p-value = 0.7974


# Yellowtail Snapper ANOVA =====================================================

# The following performs an ANOVA on the average density of yellowtail snapper 
# observed between SVC and transect surveys. 

# filter for yellowtail snapper
yellowtail_snapper_density <- filter(SVCprey_density, 
                                     species == "yellowtail snapper")

# shapiro-wilk normality test
with(yellowtail_snapper_density, shapiro.test(density[survey == "SVC"]))
with(yellowtail_snapper_density, shapiro.test(density[survey == "transect"]))

# kruskal-wallis test
yellowtail_snapper_density_kruskal <- kruskal.test(density~survey, data = yellowtail_snapper_density)
# Kruskal-Wallis chi-squared = 0.03663, df = 1, p-value = 0.8482


# Red Grouper ANOVA ============================================================

# The following performs an ANOVA on the average density of red grouper 
# observed between SVC and transect surveys. 

# filter for red grouper
red_grouper_density <- filter(SVCprey_density, species == "red grouper")

# shapiro-wilk normality test
with(red_grouper_density, shapiro.test(density[survey == "SVC"]))
with(red_grouper_density, shapiro.test(density[survey == "transect"]))

# kruskal-wallis test
red_grouper_density_kruskal <- kruskal.test(density~survey, data = red_grouper_density)
# Kruskal-Wallis chi-squared = 0.12142, df = 1, p-value = 0.7275


# Black Grouper ANOVA ==========================================================

# The following performs an ANOVA on the average density of black grouper 
# observed between SVC and transect surveys. 

# filter for black grouper
black_grouper_density <- filter(SVCprey_density, species == "black grouper")

# shapiro-wilk normality test
with(black_grouper_density, shapiro.test(density[survey == "SVC"]))
with(black_grouper_density, shapiro.test(density[survey == "transect"]))

# kruskal-wallis test
black_grouper_density_kruskal <- kruskal.test(density~survey, data = black_grouper_density)
# Kruskal-Wallis chi-squared = 0.021821, df = 1, p-value = 0.8826


# Density Barplot ==============================================================

# The following creates a barplot of the average density of each SVC focal 
# species between SVC and transect surveys.

# remove session column
SVCprey_density_bar <- SVCprey_density[,2:4]

# aggregate by species 
SVCprey_density_bar <- aggregate(.~species+survey, SVCprey_density_bar, mean)

# sort by species
SVCprey_density_bar <- SVCprey_density_bar[order(SVCprey_density_bar$species),]

# barplot
SVCprey_density_barplot <- ggplot(SVCprey_density_bar, aes(x = species, 
                           y = density, fill = survey)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  theme_classic() +
  xlab("Species") + 
  ylab("Average Density") +
  scale_fill_manual(values = c("lemonchiffon1", "navyblue")) +
  theme(axis.title = element_text(size = 24)) +
  theme(axis.text = element_text(size = 22)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.text = element_text(size = 22)) +
  theme(legend.title = element_text(size = 24)) 
ggsave(here("./visuals/SVCprey_density_bar.png"), SVCprey_density_barplot)