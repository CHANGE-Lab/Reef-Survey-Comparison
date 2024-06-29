########## SURVEY COMPARISON PROJECT FISH DATAFRAME CREATION ##########
########## 
##########
# This file compiles all individual fish observations from SVC, transect, and 
# roving surveys into a matching format with associated size bins and 
# abundances. It splits these data into two dataframes for further analyses:
# SVC vs. belt transect survey fish data and SVC vs. roving survey fish data.
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2021-06-07
##########
##########

# Set-Up =======================================================================

# Packages: 
library(plyr)
library(tidyverse)
library(data.table) 
library(here)

# Data:
SVC <- read_csv(here("./clean_data/SVC_data.csv"))
prey_fish <- read_csv(here("./clean_data/prey_fish_data.csv"))
pred_fish <- read_csv(here("./clean_data/pred_fish_data.csv"))

# SVC Lengths ==================================================================

# The following data estimates individual fish lengths from the SVC data using a 
# triangle distribution. This accounts for the abundance of a species observed, 
# the maximum length of the group, the minimum length of the group, and their 
# mean length. This is based on the rvc package from Ganz and Blondeau (2015).

# calculate length of species with abundance = 1
c2 <- SVC %>%
  filter(SVC_abundance == 1) %>%
  mutate(
    L1 = NA,
    A1 = NA,
    L2 = NA,
    A2 = NA,
    L3 = SVC_mean_tl,
    A3 = SVC_abundance,
    L4 = NA,
    A4 = NA,
    L5 = NA,
    A5 = NA)

# calculate lengths of species with abundance = 2
c3 <-SVC %>%
  filter(SVC_abundance == 2) %>%
  mutate(
    L1 = SVC_min_tl,
    A1 = 1,
    L2 = NA,
    A2 = NA,
    L3 = NA,
    A3 = NA,
    L4 = NA,
    A4 = NA,
    L5 = SVC_max_tl,
    A5 = 1)

# calculate lengths of species with abundance = 3
c4 <- SVC %>%
  filter(SVC_abundance == 3) %>%
  mutate(
    L1 = SVC_min_tl,
    A1 = 1,
    L2 = NA,
    A2 = NA,
    L3 = SVC_mean_tl,
    A3 = 1,
    L4 = NA,
    A4 = NA,
    L5 = SVC_max_tl,
    A5 = 1)

# calculate lengths of species with abundance = 4
c5 <- SVC %>%
  filter(SVC_abundance == 4) %>%
  mutate(
    L1 = SVC_min_tl,
    A1 = 1,
    L2 = NA,
    A2 = NA,
    L3 = SVC_mean_tl,
    A3 = 2,
    L4 = NA,
    A4 = NA,
    L5 = SVC_max_tl,
    A5 = 1)

# calculate lengths of species with abundance >= 5
c6 <- SVC %>%
  filter(SVC_abundance >= 5 &
           SVC_abundance <= 99) %>%
  mutate(
    L1 = SVC_min_tl,
    A1 = 1,
    L3 = SVC_mean_tl,
    A3 = (.5 * SVC_abundance),
    L5 = SVC_max_tl,
    A5 = 1,
    L2 = (L3 + L1) / 2,
    A2 = (SVC_abundance - (A1 + A3 + A5)) / 2,
    L4 = (L3 + L5) / 2,
    A4 = A2)

# calculate lengths of species with abundance > 99
c7 <- SVC %>%
  filter(SVC_abundance > 99) %>%
  mutate(
    L1 = SVC_min_tl,
    A1 = (.01 * SVC_abundance),
    L3 = SVC_mean_tl,
    A3 = (.5 * SVC_abundance),
    L5 = SVC_max_tl,
    A5 = (.01 * SVC_abundance),
    L2 = (L3 + L1) / 2,
    A2 = (SVC_abundance - (A1 + A3 + A5)) / 2,
    L4 = (L3 + L5) / 2,
    A4 = A2)

# bind SVC lengths
SVC_lengths <- bind_rows(c2, c3, c4, c5, c6, c7)


# SVC Dataframe ================================================================

# The following section transforms the SVC length data into a format matching 
# that of the belt transect and roving survey data. 

# select session, species, length, and abundance columns
SVC_fish <- SVC_lengths[,c(1, 37, 42:51)] 

# take from wide to long format, pairing length and abundance columns
SVC_melt <- melt(setDT(SVC_fish), measure.vars = list(c(3,5,7,9,11), 
                                                      c(4,6,8,10,12)),
                 variable.name = "var", 
                 value.name = c("length", "abundance"))[order(session)]

# remove NA values
SVC_melt <- na.omit(SVC_melt) 

# rename columns
SVC_melt <- SVC_melt %>% rename(SVC_tl = length) 
SVC_melt <- SVC_melt %>% rename(SVC_abundance = abundance) 

# export SVC length data
write_csv(SVC_melt, here("./clean_data/SVC_lengths.csv"))


# Size Bins ====================================================================

# The following groups every fish length into one of six size bins: bin 1 <= 5,
# 5 < bin 2 <= 10, 10 < bin 3 <= 15, 15 < bin 4 <= 20, 20 < bin 5 <= 30, 
# bin 6 > 30. 

# SVC size bin column (for SVC vs. belt analysis)
SVC_melt$size_bin_belt = ifelse(SVC_melt$SVC_tl <= 5, 1, 
                         ifelse(SVC_melt$SVC_tl > 5 & SVC_melt$SVC_tl <= 10, 2, 
                         ifelse(SVC_melt$SVC_tl > 10 & SVC_melt$SVC_tl <= 15, 3, 
                         ifelse(SVC_melt$SVC_tl > 15 & SVC_melt$SVC_tl <= 20, 4, 
                         ifelse(SVC_melt$SVC_tl > 20 & SVC_melt$SVC_tl <= 30, 5, 
                         ifelse(SVC_melt$SVC_tl > 30, 6, NA)))))) 

# SVC size bin column (for SVC vs. roving analysis)
SVC_melt$size_bin_roving = ifelse(SVC_melt$SVC_tl < 5, 1, 
                         ifelse(SVC_melt$SVC_tl >= 5 & SVC_melt$SVC_tl < 10, 2, 
                         ifelse(SVC_melt$SVC_tl >= 10 & SVC_melt$SVC_tl < 15, 3, 
                         ifelse(SVC_melt$SVC_tl >= 15 & SVC_melt$SVC_tl < 20, 4, 
                         ifelse(SVC_melt$SVC_tl >= 20 & SVC_melt$SVC_tl < 30, 5, 
                         ifelse(SVC_melt$SVC_tl >= 30, 6, NA)))))) 

# selected session, species, size_bin, and abundance columns from SVC (for
# belt analysis)
SVC_bins_prey <- SVC_melt[,c(1,2,5,6)] 

# selected session, species, size_bin, and abundance columns from SVC (for 
# roving analysis)
SVC_bins_pred <- SVC_melt[,c(1,2,5,7)] 

# transect size bin column
prey_fish$size_bin = ifelse(prey_fish$prey_tl <= 5, 1, 
   ifelse(prey_fish$prey_tl > 5 & prey_fish$prey_tl <= 10, 2, 
   ifelse(prey_fish$prey_tl > 10 & prey_fish$prey_tl <= 15, 3, 
   ifelse(prey_fish$prey_tl > 15 & prey_fish$prey_tl <= 20, 4, 
   ifelse(prey_fish$prey_tl > 20 & prey_fish$prey_tl <= 30, 5, 
   ifelse(prey_fish$prey_tl > 30, 6, NA))))))

# selecting session, species, and size_bin columns from transect
prey_bins <- prey_fish[,c(1,3,5)] 

# remove all roving individuals <15cm 
pred_fish <- subset(pred_fish, pred_tl >= 15)

# roving size bin column
pred_fish$size_bin = ifelse(pred_fish$pred_tl < 5, 1, 
    ifelse(pred_fish$pred_tl >= 5 & pred_fish$pred_tl < 10, 2, 
    ifelse(pred_fish$pred_tl >= 10 & pred_fish$pred_tl < 15, 3, 
    ifelse(pred_fish$pred_tl >= 15 & pred_fish$pred_tl < 20, 4, 
    ifelse(pred_fish$pred_tl >= 20 & pred_fish$pred_tl < 30, 5, 
    ifelse(pred_fish$pred_tl >= 30, 6, NA))))))

# selecting session, species, and size_bin columns from roving
pred_bins <- pred_fish[,c(1,3,14)] 


# Adding Abundance =============================================================

# The following combines all matching species' size bins within every session
# and creates an additional column with the number of individuals in each size
# bin. 

# SVC abundance column (for belt analysis)
SVC_abun_prey <- SVC_bins_prey[, lapply(.SD, sum), 
                               by=list(session, species, size_bin_belt)] 

# rename SVC abundance column
SVC_abun_prey  <- SVC_abun_prey %>% rename(SVC_prey_abundance = SVC_abundance)

# rename SVC size bin column
SVC_abun_prey  <- SVC_abun_prey %>% rename(size_bin = size_bin_belt)

# SVC abundance column (for roving analysis)
SVC_abun_pred <- SVC_bins_pred[, lapply(.SD, sum), 
                               by=list(session, species, size_bin_roving)] 

# rename SVC abundance column
SVC_abun_pred  <- SVC_abun_pred %>% rename(SVC_pred_abundance = SVC_abundance)

# rename SVC size bin column
SVC_abun_pred  <- SVC_abun_pred %>% rename(size_bin = size_bin_roving)

# transect abundance column
prey_abun <- prey_bins %>% group_by(session, species, size_bin) %>% tally()

# rename transect abundance column
prey_abun <- prey_abun %>% rename(prey_abundance = n)

# roving abundance column
pred_abun <- pred_bins %>% group_by(session, species, size_bin) %>% tally() 

# rename transect abundance column
pred_abun <- pred_abun %>% rename(pred_abundance = n) 


# Joining Survey Fish Dataframes ===============================================

# The following joins all four survey dataframes (SVC, SVC [roving lengths] 
# transect, and roving) with their size bin and abundance columns into a single 
# dataframe with all individual fish observations. 

# join SVC and roving data
SVCpred_fish_data <- full_join(pred_abun, SVC_abun_pred, by = NULL, 
                               copy = FALSE) 

# remove size bins 1-3 from SVC vs. roving fish data (only individuals >=15cm)
SVCpred_fish_data <- SVCpred_fish_data[SVCpred_fish_data$size_bin !="1",]
SVCpred_fish_data <- SVCpred_fish_data[SVCpred_fish_data$size_bin !="2",]
SVCpred_fish_data <- SVCpred_fish_data[SVCpred_fish_data$size_bin !="3",]

# join SVC and belt data
SVCprey_fish_data <- full_join(SVC_abun_prey, prey_abun, by = NULL, 
                           copy = FALSE) 

# replace NAs with values of 0
SVCpred_fish_data[is.na(SVCpred_fish_data)] <- 0
SVCprey_fish_data[is.na(SVCprey_fish_data)] <- 0

# order by session
SVCpred_fish_data <- SVCpred_fish_data[order(SVCpred_fish_data$session),]
SVCprey_fish_data <- SVCprey_fish_data[order(session),]

# export fish dataframes
write_csv(SVCpred_fish_data, here("./dataframes/SVCpred_fish_dataframe.csv"))
write_csv(SVCprey_fish_data, here("./dataframes/SVCprey_fish_dataframe.csv"))