########## SURVEY COMPARISON PROJECT TRAIT PROPORTIONS ##########
########## 
##########
# This file calculates the proportion of each trait level present in the data
# for each of the three visual survey techniques: SVC, belt transect, and 
# roving.
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2024-06-04
##########
##########

# Set-Up =======================================================================

# packages
library(plyr)
library(tidyverse)
library(here)

# data
SVCprey_data <- 
  read_csv(here("./dataframes/SVCprey_dataframe_anguilliformes.csv"))
SVCpred_data <- read_csv(here("./dataframes/SVCpred_dataframe.csv"))


# SVC Trait Proportions ========================================================

# The following calculates the proportion of each trait level recorded in SVC
# surveys.

# Total SVC abundance
sum(SVCprey_data$SVC_prey_abundance) # 21223

# Nocturnality proportions
sum(SVCprey_data[which(SVCprey_data$nocturnal == 1), "SVC_prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$nocturnal == 1), "SVC_prey_abundance"]) / 
  sum(SVCprey_data$SVC_prey_abundance)

sum(SVCprey_data[which(SVCprey_data$nocturnal == 0), "SVC_prey_abundance"]) 
sum(SVCprey_data[which(SVCprey_data$nocturnal == 0), "SVC_prey_abundance"]) / 
  sum(SVCprey_data$SVC_prey_abundance)

# Position proportions
sum(SVCprey_data[which(SVCprey_data$position == "demersal"), 
                 "SVC_prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$position == "demersal"), 
                 "SVC_prey_abundance"]) / sum(SVCprey_data$SVC_prey_abundance)

sum(SVCprey_data[which(SVCprey_data$position == "benthic"), 
                 "SVC_prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$position == "benthic"), 
                 "SVC_prey_abundance"]) / sum(SVCprey_data$SVC_prey_abundance)

# Aggregation behaviour proportions
sum(SVCprey_data[which(SVCprey_data$behavior == "solitary"), 
                 "SVC_prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$behavior == "solitary"), 
                 "SVC_prey_abundance"]) / sum(SVCprey_data$SVC_prey_abundance)

sum(SVCprey_data[which(SVCprey_data$behavior == "shoaling"), 
                 "SVC_prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$behavior == "shoaling"), 
                 "SVC_prey_abundance"]) / sum(SVCprey_data$SVC_prey_abundance)

sum(SVCprey_data[which(SVCprey_data$behavior == "schooling"), 
                 "SVC_prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$behavior == "schooling"), 
                 "SVC_prey_abundance"]) / sum(SVCprey_data$SVC_prey_abundance)

# Crypsis proportions
sum(SVCprey_data[which(SVCprey_data$cryptic_behaviour == 1), 
                 "SVC_prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$cryptic_behaviour == 1), 
                 "SVC_prey_abundance"]) / sum(SVCprey_data$SVC_prey_abundance)

sum(SVCprey_data[which(SVCprey_data$cryptic_behaviour == 0), 
                 "SVC_prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$cryptic_behaviour == 0), 
                 "SVC_prey_abundance"]) / sum(SVCprey_data$SVC_prey_abundance)

# Colouration proportions
sum(SVCprey_data[which(SVCprey_data$colouration == "camouflage"), 
                 "SVC_prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$colouration == "camouflage"), 
                 "SVC_prey_abundance"]) / sum(SVCprey_data$SVC_prey_abundance)

sum(SVCprey_data[which(SVCprey_data$colouration == "neutral"), 
                 "SVC_prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$colouration == "neutral"), 
                 "SVC_prey_abundance"]) / sum(SVCprey_data$SVC_prey_abundance)

sum(SVCprey_data[which(SVCprey_data$colouration == "silvering"), 
                 "SVC_prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$colouration == "silvering"), 
                 "SVC_prey_abundance"]) / sum(SVCprey_data$SVC_prey_abundance)

sum(SVCprey_data[which(SVCprey_data$colouration == "colourful"), 
                 "SVC_prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$colouration == "colourful"), 
                 "SVC_prey_abundance"]) / sum(SVCprey_data$SVC_prey_abundance)

# Shape proportions
sum(SVCprey_data[which(SVCprey_data$shape == "anguilliform"), 
                 "SVC_prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$shape == "anguilliform"), 
                 "SVC_prey_abundance"]) / sum(SVCprey_data$SVC_prey_abundance)

sum(SVCprey_data[which(SVCprey_data$shape == "elongated"), 
                 "SVC_prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$shape == "elongated"), 
                 "SVC_prey_abundance"]) / sum(SVCprey_data$SVC_prey_abundance)

sum(SVCprey_data[which(SVCprey_data$shape == "fusiform"), 
                 "SVC_prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$shape == "fusiform"), 
                 "SVC_prey_abundance"]) / sum(SVCprey_data$SVC_prey_abundance)

sum(SVCprey_data[which(SVCprey_data$shape == "compressiform"), 
                 "SVC_prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$shape == "compressiform"), 
                 "SVC_prey_abundance"]) / sum(SVCprey_data$SVC_prey_abundance)

sum(SVCprey_data[which(SVCprey_data$shape == "globiform"), 
                 "SVC_prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$shape == "globiform"), 
                 "SVC_prey_abundance"]) / sum(SVCprey_data$SVC_prey_abundance)

# Size class proportions
sum(SVCprey_data[which(SVCprey_data$size_bin == 1), "SVC_prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$size_bin == 1), "SVC_prey_abundance"]) / 
  sum(SVCprey_data$SVC_prey_abundance)

sum(SVCprey_data[which(SVCprey_data$size_bin == 2), "SVC_prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$size_bin == 2), "SVC_prey_abundance"]) / 
  sum(SVCprey_data$SVC_prey_abundance)

sum(SVCprey_data[which(SVCprey_data$size_bin == 3), "SVC_prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$size_bin == 3), "SVC_prey_abundance"]) / 
  sum(SVCprey_data$SVC_prey_abundance)

sum(SVCprey_data[which(SVCprey_data$size_bin == 4), "SVC_prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$size_bin == 4), "SVC_prey_abundance"]) / 
  sum(SVCprey_data$SVC_prey_abundance)

sum(SVCprey_data[which(SVCprey_data$size_bin == 5), "SVC_prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$size_bin == 5), "SVC_prey_abundance"]) / 
  sum(SVCprey_data$SVC_prey_abundance)

sum(SVCprey_data[which(SVCprey_data$size_bin == 6), "SVC_prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$size_bin == 6), "SVC_prey_abundance"]) / 
  sum(SVCprey_data$SVC_prey_abundance)

# Reef type proportions
sum(SVCprey_data[which(SVCprey_data$habitat == "Patch"), 
                 "SVC_prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$habitat == "Patch"), 
                 "SVC_prey_abundance"]) / sum(SVCprey_data$SVC_prey_abundance)

sum(SVCprey_data[which(SVCprey_data$habitat == "Continuous"), 
                 "SVC_prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$habitat == "Continuous"), 
                 "SVC_prey_abundance"]) / sum(SVCprey_data$SVC_prey_abundance)


# Belt Trait Proportions =======================================================

# The following calculates the proportion of each trait level recorded in belt
# transect surveys.

# Total belt abundance
sum(SVCprey_data$prey_abundance) # 67251

# Nocturnality proportions
sum(SVCprey_data[which(SVCprey_data$nocturnal == 1), "prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$nocturnal == 1), "prey_abundance"]) / 
  sum(SVCprey_data$prey_abundance)

sum(SVCprey_data[which(SVCprey_data$nocturnal == 0), "prey_abundance"]) 
sum(SVCprey_data[which(SVCprey_data$nocturnal == 0), "prey_abundance"]) / 
  sum(SVCprey_data$prey_abundance)

# Position proportions
sum(SVCprey_data[which(SVCprey_data$position == "demersal"), "prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$position == "demersal"), 
                 "prey_abundance"]) / sum(SVCprey_data$prey_abundance)

sum(SVCprey_data[which(SVCprey_data$position == "benthic"), "prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$position == "benthic"), 
                 "prey_abundance"]) / sum(SVCprey_data$prey_abundance)

# Aggregation behaviour proportions
sum(SVCprey_data[which(SVCprey_data$behavior == "solitary"), "prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$behavior == "solitary"), 
                 "prey_abundance"]) / sum(SVCprey_data$prey_abundance)

sum(SVCprey_data[which(SVCprey_data$behavior == "shoaling"), "prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$behavior == "shoaling"), 
                 "prey_abundance"]) / sum(SVCprey_data$prey_abundance)

sum(SVCprey_data[which(SVCprey_data$behavior == "schooling"), "prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$behavior == "schooling"), 
                 "prey_abundance"]) / sum(SVCprey_data$prey_abundance)

# Crypsis proportions
sum(SVCprey_data[which(SVCprey_data$cryptic_behaviour == 1), "prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$cryptic_behaviour == 1), 
                 "prey_abundance"]) / sum(SVCprey_data$prey_abundance)

sum(SVCprey_data[which(SVCprey_data$cryptic_behaviour == 0), "prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$cryptic_behaviour == 0), 
                 "prey_abundance"]) / sum(SVCprey_data$prey_abundance)

# Colouration proportions
sum(SVCprey_data[which(SVCprey_data$colouration == "camouflage"), 
                 "prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$colouration == "camouflage"), 
                 "prey_abundance"]) / sum(SVCprey_data$prey_abundance)

sum(SVCprey_data[which(SVCprey_data$colouration == "neutral"), 
                 "prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$colouration == "neutral"), 
                 "prey_abundance"]) / sum(SVCprey_data$prey_abundance)

sum(SVCprey_data[which(SVCprey_data$colouration == "silvering"), 
                 "prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$colouration == "silvering"), 
                 "prey_abundance"]) / sum(SVCprey_data$prey_abundance)

sum(SVCprey_data[which(SVCprey_data$colouration == "colourful"), 
                 "prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$colouration == "colourful"), 
                 "prey_abundance"]) / sum(SVCprey_data$prey_abundance)

# Shape proportions
sum(SVCprey_data[which(SVCprey_data$shape == "anguilliform"), "prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$shape == "anguilliform"), 
                 "prey_abundance"]) / sum(SVCprey_data$prey_abundance)

sum(SVCprey_data[which(SVCprey_data$shape == "elongated"), "prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$shape == "elongated"), 
                 "prey_abundance"]) / sum(SVCprey_data$prey_abundance)

sum(SVCprey_data[which(SVCprey_data$shape == "fusiform"), "prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$shape == "fusiform"), 
                 "prey_abundance"]) / sum(SVCprey_data$prey_abundance)

sum(SVCprey_data[which(SVCprey_data$shape == "compressiform"), 
                 "prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$shape == "compressiform"), 
                 "prey_abundance"]) / sum(SVCprey_data$prey_abundance)

sum(SVCprey_data[which(SVCprey_data$shape == "globiform"), "prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$shape == "globiform"), 
                 "prey_abundance"]) / sum(SVCprey_data$prey_abundance)

# Size class proportions
sum(SVCprey_data[which(SVCprey_data$size_bin == 1), "prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$size_bin == 1), "prey_abundance"]) / 
  sum(SVCprey_data$prey_abundance)

sum(SVCprey_data[which(SVCprey_data$size_bin == 2), "prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$size_bin == 2), "prey_abundance"]) / 
  sum(SVCprey_data$prey_abundance)

sum(SVCprey_data[which(SVCprey_data$size_bin == 3), "prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$size_bin == 3), "prey_abundance"]) / 
  sum(SVCprey_data$prey_abundance)

sum(SVCprey_data[which(SVCprey_data$size_bin == 4), "prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$size_bin == 4), "prey_abundance"]) / 
  sum(SVCprey_data$prey_abundance)

sum(SVCprey_data[which(SVCprey_data$size_bin == 5), "prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$size_bin == 5), "prey_abundance"]) / 
  sum(SVCprey_data$prey_abundance)

sum(SVCprey_data[which(SVCprey_data$size_bin == 6), "prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$size_bin == 6), "prey_abundance"]) / 
  sum(SVCprey_data$prey_abundance)

# Reef type proportions
sum(SVCprey_data[which(SVCprey_data$habitat == "Patch"), "prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$habitat == "Patch"), 
                 "prey_abundance"]) / sum(SVCprey_data$prey_abundance)

sum(SVCprey_data[which(SVCprey_data$habitat == "Continuous"), "prey_abundance"])
sum(SVCprey_data[which(SVCprey_data$habitat == "Continuous"), 
                 "prey_abundance"]) / sum(SVCprey_data$prey_abundance)


# Roving Trait Proportions =====================================================

# The following calculates the proportion of each trait level recorded in roving
# surveys.

# Total belt abundance
sum(SVCpred_data$pred_abundance) # 870

# Nocturnality proportions
sum(SVCpred_data[which(SVCpred_data$nocturnal == 1), "pred_abundance"])
sum(SVCpred_data[which(SVCpred_data$nocturnal == 1), "pred_abundance"]) / 
  sum(SVCpred_data$pred_abundance)

sum(SVCpred_data[which(SVCpred_data$nocturnal == 0), "pred_abundance"]) 
sum(SVCpred_data[which(SVCpred_data$nocturnal == 0), "pred_abundance"]) / 
  sum(SVCpred_data$pred_abundance)

# Position proportions
sum(SVCpred_data[which(SVCpred_data$position == "demersal"), "pred_abundance"])
sum(SVCpred_data[which(SVCpred_data$position == "demersal"), 
                 "pred_abundance"]) / sum(SVCpred_data$pred_abundance)

sum(SVCpred_data[which(SVCpred_data$position == "benthic"), "pred_abundance"])
sum(SVCpred_data[which(SVCpred_data$position == "benthic"), 
                 "pred_abundance"]) / sum(SVCpred_data$pred_abundance)

# Aggregation behaviour proportions
sum(SVCpred_data[which(SVCpred_data$behavior == "solitary"), "pred_abundance"])
sum(SVCpred_data[which(SVCpred_data$behavior == "solitary"), 
                 "pred_abundance"]) / sum(SVCpred_data$pred_abundance)

sum(SVCpred_data[which(SVCpred_data$behavior == "shoaling"), "pred_abundance"])
sum(SVCpred_data[which(SVCpred_data$behavior == "shoaling"), 
                 "pred_abundance"]) / sum(SVCpred_data$pred_abundance)

sum(SVCpred_data[which(SVCpred_data$behavior == "schooling"), "pred_abundance"])
sum(SVCpred_data[which(SVCpred_data$behavior == "schooling"), 
                 "pred_abundance"]) / sum(SVCpred_data$pred_abundance)

# Crypsis proportions
sum(SVCpred_data[which(SVCpred_data$cryptic_behaviour == 1), "pred_abundance"])
sum(SVCpred_data[which(SVCpred_data$cryptic_behaviour == 1), 
                 "pred_abundance"]) / sum(SVCpred_data$pred_abundance)

sum(SVCpred_data[which(SVCpred_data$cryptic_behaviour == 0), "pred_abundance"])
sum(SVCpred_data[which(SVCpred_data$cryptic_behaviour == 0), 
                 "pred_abundance"]) / sum(SVCpred_data$pred_abundance)

# Colouration proportions
sum(SVCpred_data[which(SVCpred_data$colouration == "camouflage"), 
                 "pred_abundance"])
sum(SVCpred_data[which(SVCpred_data$colouration == "camouflage"), 
                 "pred_abundance"]) / sum(SVCpred_data$pred_abundance)

sum(SVCpred_data[which(SVCpred_data$colouration == "neutral"), 
                 "pred_abundance"])
sum(SVCpred_data[which(SVCpred_data$colouration == "neutral"), 
                 "pred_abundance"]) / sum(SVCpred_data$pred_abundance)

sum(SVCpred_data[which(SVCpred_data$colouration == "silvering"), 
                 "pred_abundance"])
sum(SVCpred_data[which(SVCpred_data$colouration == "silvering"), 
                 "pred_abundance"]) / sum(SVCpred_data$pred_abundance)

sum(SVCpred_data[which(SVCpred_data$colouration == "colourful"), 
                 "pred_abundance"])
sum(SVCpred_data[which(SVCpred_data$colouration == "colourful"), 
                 "pred_abundance"]) / sum(SVCpred_data$pred_abundance)

# Shape proportions
sum(SVCpred_data[which(SVCpred_data$shape == "anguilliform"), "pred_abundance"])
sum(SVCpred_data[which(SVCpred_data$shape == "anguilliform"), 
                 "pred_abundance"]) / sum(SVCpred_data$pred_abundance)

sum(SVCpred_data[which(SVCpred_data$shape == "elongated"), "pred_abundance"])
sum(SVCpred_data[which(SVCpred_data$shape == "elongated"), 
                 "pred_abundance"]) / sum(SVCpred_data$pred_abundance)

sum(SVCpred_data[which(SVCpred_data$shape == "fusiform"), "pred_abundance"])
sum(SVCpred_data[which(SVCpred_data$shape == "fusiform"), 
                 "pred_abundance"]) / sum(SVCpred_data$pred_abundance)

sum(SVCpred_data[which(SVCpred_data$shape == "compressiform"), 
                 "pred_abundance"])
sum(SVCpred_data[which(SVCpred_data$shape == "compressiform"), 
                 "pred_abundance"]) / sum(SVCpred_data$pred_abundance)

sum(SVCpred_data[which(SVCpred_data$shape == "globiform"), "pred_abundance"])
sum(SVCpred_data[which(SVCpred_data$shape == "globiform"), 
                 "pred_abundance"]) / sum(SVCpred_data$pred_abundance)

# Size class proportions
sum(SVCpred_data[which(SVCpred_data$size_bin == 1), "pred_abundance"])
sum(SVCpred_data[which(SVCpred_data$size_bin == 1), "pred_abundance"]) / 
  sum(SVCpred_data$pred_abundance)

sum(SVCpred_data[which(SVCpred_data$size_bin == 2), "pred_abundance"])
sum(SVCpred_data[which(SVCpred_data$size_bin == 2), "pred_abundance"]) / 
  sum(SVCpred_data$pred_abundance)

sum(SVCpred_data[which(SVCpred_data$size_bin == 3), "pred_abundance"])
sum(SVCpred_data[which(SVCpred_data$size_bin == 3), "pred_abundance"]) / 
  sum(SVCpred_data$pred_abundance)

sum(SVCpred_data[which(SVCpred_data$size_bin == 4), "pred_abundance"])
sum(SVCpred_data[which(SVCpred_data$size_bin == 4), "pred_abundance"]) / 
  sum(SVCpred_data$pred_abundance)

sum(SVCpred_data[which(SVCpred_data$size_bin == 5), "pred_abundance"])
sum(SVCpred_data[which(SVCpred_data$size_bin == 5), "pred_abundance"]) / 
  sum(SVCpred_data$pred_abundance)

sum(SVCpred_data[which(SVCpred_data$size_bin == 6), "pred_abundance"])
sum(SVCpred_data[which(SVCpred_data$size_bin == 6), "pred_abundance"]) / 
  sum(SVCpred_data$pred_abundance)

# Reef type proportions
sum(SVCpred_data[which(SVCpred_data$habitat == "Patch"), "pred_abundance"])
sum(SVCpred_data[which(SVCpred_data$habitat == "Patch"), 
                 "pred_abundance"]) / sum(SVCpred_data$pred_abundance)

sum(SVCpred_data[which(SVCpred_data$habitat == "Continuous"), "pred_abundance"])
sum(SVCpred_data[which(SVCpred_data$habitat == "Continuous"), 
                 "pred_abundance"]) / sum(SVCpred_data$pred_abundance)