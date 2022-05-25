########## SURVEY COMPARISON PROJECT RECLASSIFYING SIZE BINS ##########
########## 
##########
# This file reclassifies the six size bins previously designed to categorize 
# fish lengths observed in SVC, transect, and roving surveys for further 
# analyses. Mean and median lengths are determined from all observations in 
# each bin (0-5cm, 5-10cm, 10-15cm, 15-20cm, 20-30cm, and >30cm), and the mean
# lengths are used as alternate classifications for each bin. 
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2021-06-03
##########
##########


# Set-Up =======================================================================

# packages
library(tidyverse)
library(here)

# data
fish_dataframe <- read_csv(here("./dataframes/fish_dataframe.csv"))
SVC_lengths <- read_csv(here("./clean_data/SVC_lengths.csv"))
prey_lengths <- read_csv(here("./clean_data/prey_fish_data.csv"))
pred_lengths <- read_csv(here("./clean_data/pred_fish_data.csv"))


# Determining Size Bin 1 Classification ========================================

# The following determines the mean and median lengths of size class 1 (<=5cm).

# extract lengths < 5cm 
SVC_1 <- SVC_lengths %>% filter(SVC_lengths$SVC_tl <= 5)
prey_1 <- prey_lengths %>% filter(prey_lengths$prey_tl <= 5)
pred_1 <- pred_lengths %>% filter(pred_lengths$pred_tl <= 5)

# round SVC abundances
SVC_1$abundance_round <- ifelse(SVC_1$SVC_abundance == 0.25, 1, 
                         ifelse(SVC_1$SVC_abundance == 0.5, 1, 
                         ifelse(SVC_1$SVC_abundance == 6.5, 7, 
                         round(SVC_1$SVC_abundance, digits = 0))))

# duplicate SVC length rows by abundance 
SVC_1 <- SVC_1[rep(row.names(SVC_1), SVC_1$abundance_round), 1:4]

# select length and species columns
SVC_1 <- SVC_1[,c(2,4)]
prey_1 <- prey_1[,3:4]
pred_1 <- pred_1[,3:4]

# re-name length columns
SVC_1 <- rename(SVC_1, length = SVC_tl)
prey_1 <- rename(prey_1, length = prey_tl)
pred_1 <- rename(pred_1, length = pred_tl)

# bind together
size_class_1 <- bind_rows(SVC_1, prey_1, pred_1)

# find median
median(size_class_1$length) # = 4cm 

# find mean
mean(size_class_1$length) # = 3.63321cm


# Determining Size Bin 2 Classification ========================================

# The following determines the mean and median lengths of size class 2 (>5cm, 
# <=10cm).

# extract lengths >5cm, <=10cm
SVC_2 <- SVC_lengths %>% 
  filter(SVC_lengths$SVC_tl > 5 & SVC_lengths$SVC_tl <= 10)
prey_2 <- prey_lengths %>% 
  filter(prey_lengths$prey_tl > 5 & prey_lengths$prey_tl <= 10)
pred_2 <- pred_lengths %>% 
  filter(pred_lengths$pred_tl > 5 & pred_lengths$pred_tl <= 10)

# round SVC abundances
SVC_2$abundance_round <- ifelse(SVC_2$SVC_abundance == 0.25, 1, 
                         ifelse(SVC_2$SVC_abundance == 0.5, 1, 
                         ifelse(SVC_2$SVC_abundance == 6.5, 7, 
                         round(SVC_2$SVC_abundance, digits = 0))))

# duplicate SVC length rows by abundance 
SVC_2 <- SVC_2[rep(row.names(SVC_2), SVC_2$abundance_round), 1:4]

# select length and species columns
SVC_2 <- SVC_2[,c(2,4)]
prey_2 <- prey_2[,3:4]
pred_2 <- pred_2[,3:4]

# re-name length columns
SVC_2 <- rename(SVC_2, length = SVC_tl)
prey_2 <- rename(prey_2, length = prey_tl)
pred_2 <- rename(pred_2, length = pred_tl)

# bind together
size_class_2 <- bind_rows(SVC_2, prey_2, pred_2)

# find median
median(size_class_2$length) # = 7cm 

# find mean
mean(size_class_2$length) # = 7.533217cm


# Determining Size Bin 3 Classification ========================================

# The following determines the mean and median lengths of size class 3 (>10cm, 
# <=15cm).

# extract lengths >10cm, <=15cm
SVC_3 <- SVC_lengths %>% 
  filter(SVC_lengths$SVC_tl > 10 & SVC_lengths$SVC_tl <= 15)
prey_3 <- prey_lengths %>% 
  filter(prey_lengths$prey_tl > 10 & prey_lengths$prey_tl <= 15)
pred_3 <- pred_lengths %>% 
  filter(pred_lengths$pred_tl > 10 & pred_lengths$pred_tl <= 15)

# round SVC abundances
SVC_3$abundance_round <- ifelse(SVC_3$SVC_abundance == 0.25, 1, 
                         ifelse(SVC_3$SVC_abundance == 0.5, 1, 
                         ifelse(SVC_3$SVC_abundance == 6.5, 7, 
                         round(SVC_3$SVC_abundance, digits = 0))))

# duplicate SVC length rows by abundance 
SVC_3 <- SVC_3[rep(row.names(SVC_3), SVC_3$abundance_round), 1:4]

# select length and species columns
SVC_3 <- SVC_3[,c(2,4)]
prey_3 <- prey_3[,3:4]
pred_3 <- pred_3[,3:4]

# re-name length columns
SVC_3 <- rename(SVC_3, length = SVC_tl)
prey_3 <- rename(prey_3, length = prey_tl)
pred_3 <- rename(pred_3, length = pred_tl)

# bind together
size_class_3 <- bind_rows(SVC_3, prey_3, pred_3)

# find median
median(size_class_3$length) # = 13cm 

# find mean
mean(size_class_3$length) # = 12.91456cm


# Determining Size Bin 4 Classification ========================================

# The following determines the mean and median lengths of size class 4 (>15cm, 
# <=20cm).

# extract lengths >15cm, <=20cm
SVC_4 <- SVC_lengths %>% 
  filter(SVC_lengths$SVC_tl > 15 & SVC_lengths$SVC_tl <= 20)
prey_4 <- prey_lengths %>% 
  filter(prey_lengths$prey_tl > 15 & prey_lengths$prey_tl <= 20)
pred_4 <- pred_lengths %>% 
  filter(pred_lengths$pred_tl > 15 & pred_lengths$pred_tl <= 20)

# round SVC abundances
SVC_4$abundance_round <- ifelse(SVC_4$SVC_abundance == 0.25, 1, 
                         ifelse(SVC_4$SVC_abundance == 0.5, 1, 
                         ifelse(SVC_4$SVC_abundance == 6.5, 7, 
                         round(SVC_4$SVC_abundance, digits = 0))))

# duplicate SVC length rows by abundance 
SVC_4 <- SVC_4[rep(row.names(SVC_4), SVC_4$abundance_round), 1:4]

# select length and species columns
SVC_4 <- SVC_4[,c(2,4)]
prey_4 <- prey_4[,3:4]
pred_4 <- pred_4[,3:4]

# re-name length columns
SVC_4 <- rename(SVC_4, length = SVC_tl)
prey_4 <- rename(prey_4, length = prey_tl)
pred_4 <- rename(pred_4, length = pred_tl)

# bind together
size_class_4 <- bind_rows(SVC_4, prey_4, pred_4)

# find median
median(size_class_4$length) # = 18cm 

# find mean
mean(size_class_4$length) # = 17.94356cm


# Determining Size Bin 5 Classification ========================================

# The following determines the mean and median lengths of size class 5 (>20cm, 
# <=30cm).

# extract lengths >20cm, <=30cm
SVC_5 <- SVC_lengths %>% 
  filter(SVC_lengths$SVC_tl > 20 & SVC_lengths$SVC_tl <= 30)
prey_5 <- prey_lengths %>% 
  filter(prey_lengths$prey_tl > 20 & prey_lengths$prey_tl <= 30)
pred_5 <- pred_lengths %>% 
  filter(pred_lengths$pred_tl > 20 & pred_lengths$pred_tl <= 30)

# round SVC abundances
SVC_5$abundance_round <- ifelse(SVC_5$SVC_abundance == 0.25, 1, 
                         ifelse(SVC_5$SVC_abundance == 0.5, 1, 
                         ifelse(SVC_5$SVC_abundance == 6.5, 7, 
                         round(SVC_5$SVC_abundance, digits = 0))))

# duplicate SVC length rows by abundance 
SVC_5 <- SVC_5[rep(row.names(SVC_5), SVC_5$abundance_round), 1:4]

# select length and species columns
SVC_5 <- SVC_5[,c(2,4)]
prey_5 <- prey_5[,3:4]
pred_5 <- pred_5[,3:4]

# re-name length columns
SVC_5 <- rename(SVC_5, length = SVC_tl)
prey_5 <- rename(prey_5, length = prey_tl)
pred_5 <- rename(pred_5, length = pred_tl)

# bind together
size_class_5 <- bind_rows(SVC_5, prey_5, pred_5)

# find median
median(size_class_5$length) # = 25cm 

# find mean
mean(size_class_5$length) # = 24.92844cm


# Determining Size Bin 6 Classification ========================================

# The following determines the mean and median lengths of size class 6 (>30cm).

# extract lengths > 30cm 
SVC_6 <- SVC_lengths %>% filter(SVC_lengths$SVC_tl > 30)
prey_6 <- prey_lengths %>% filter(prey_lengths$prey_tl > 30)
pred_6 <- pred_lengths %>% filter(pred_lengths$pred_tl > 30)

# round SVC abundances
SVC_6$abundance_round <- ifelse(SVC_6$SVC_abundance == 0.25, 1, 
                         ifelse(SVC_6$SVC_abundance == 0.5, 1, 
                         ifelse(SVC_6$SVC_abundance == 6.5, 7, 
                         round(SVC_6$SVC_abundance, digits = 0))))

# duplicate SVC length rows by abundance 
SVC_6 <- SVC_6[rep(row.names(SVC_6), SVC_6$abundance_round), 1:4]

# select length and species columns
SVC_6 <- SVC_6[,c(2,4)]
prey_6 <- prey_6[,3:4]
pred_6 <- pred_6[,3:4]

# re-name length columns
SVC_6 <- rename(SVC_6, length = SVC_tl)
prey_6 <- rename(prey_6, length = prey_tl)
pred_6 <- rename(pred_6, length = pred_tl)

# bind together
size_class_6 <- bind_rows(SVC_6, prey_6, pred_6)

# find median
median(size_class_6$length) # = 40cm 

# find mean
mean(size_class_6$length) # = 47.70199cm


# Size Bin Reclassifying =======================================================

# The following reclassifies size bins using the mean length within each bin. 

# size bin 1 = 3.63321 cm
# size bin 2 = 7.533217 cm
# size bin 3 = 12.91456 cm
# size bin 4 = 17.94356 cm
# size bin 5 = 24.92844 cm
# size bin 6 = 47.70199 cm

# reclassifying fish dataframe size bins
fish_dataframe$size_bin_lengths <- 
  ifelse(fish_dataframe$size_bin == 1, 3.63321, 
  ifelse(fish_dataframe$size_bin == 2, 7.533217, 
  ifelse(fish_dataframe$size_bin == 3, 12.91456, 
  ifelse(fish_dataframe$size_bin == 4, 17.94356, 
  ifelse(fish_dataframe$size_bin == 5, 24.92844, 
  ifelse(fish_dataframe$size_bin == 6, 47.70199, NA))))))

# update csv of fish dataframe
write_csv(fish_dataframe, here("./dataframes/fish_dataframe.csv"))
