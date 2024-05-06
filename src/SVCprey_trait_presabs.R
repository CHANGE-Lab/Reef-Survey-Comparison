########## SURVEY COMPARISON PROJECT SVC VS. TRANSECT PRESENCE ANALYSIS ########
########## 
##########
# This file creates a dataframe outlining the total number of sessions 8 RVC
# focal species observed in SVC and transect surveys was present in. The 
# dataframe created is used to compare presence recordings between SVC and 
# transect surveys across species using a Chi-Square test in order to explore if 
# significant differences are present and how these differences compare to 
# differences in density across species explored previously. Potential presence 
# differences for each species are also analyzed using Chi-Square tests 
# individually. A barplot of the number of sessions present for each species in 
# SVC and roving surveys is also created. 
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2021-08-17
##########
##########


# Set-Up =======================================================================

# packages
library(plyr)
library(tidyverse)
library(ggplot2)
library(here)
library(chisq.posthoc.test)

# data
SVCprey_data <- read_csv(here("./dataframes/SVCprey_dataframe.csv"))


# Aggregation Behaviour Presence/Absence Analysis ==============================

# The following compares the frequency of occurrence of different levels of 
# species' aggregation (solitary, shoaling, and schooling) between SVC and 
# transect surveys by conducting a Chi-Square Test. 

# select behaviour columns and SVC density 
SVC_behaviour_presence <- SVCprey_data[,c(1,16,26)]

# rename density column
SVC_behaviour_presence <- rename(SVC_behaviour_presence, density = SVC_density)

# aggregate by session and behaviour
SVC_behaviour_presence <- 
  aggregate(.~session+behavior, SVC_behaviour_presence, sum)

# create presence column
SVC_behaviour_presence$presence <- 
  ifelse(SVC_behaviour_presence$density > 0, 1, 0)

# create survey column
SVC_behaviour_presence$survey <- "SVC"

# select behaviour columns and transect density
prey_behaviour_presence <- SVCprey_data[,c(1,16,30)]

# rename density column
prey_behaviour_presence <- 
  rename(prey_behaviour_presence, density = prey_density)

# aggregate by session and behaviour
prey_behaviour_presence <- 
  aggregate(.~session+behavior, prey_behaviour_presence, sum)

# create presence column
prey_behaviour_presence$presence <- 
  ifelse(prey_behaviour_presence$density > 0, 1, 0)

# create survey column
prey_behaviour_presence$survey <- "transect"

# bind SVC and transect presence values together 
SVCprey_behaviour_presence <- 
  bind_rows(SVC_behaviour_presence, prey_behaviour_presence)

# remove session and density columns
SVCprey_behaviour_presence <- SVCprey_behaviour_presence[,c(2,4,5)]

# remove rows where presence = 0
SVCprey_behaviour_presence <- 
  SVCprey_behaviour_presence[!(SVCprey_behaviour_presence$presence == 0),]

# convert dataframe to table
SVCprey_behaviour_chi <- table(SVCprey_behaviour_presence$behavior, 
                     SVCprey_behaviour_presence$survey)

# Chi-Square Test
SVCprey_behaviour_chi_result <- chisq.test(SVCprey_behaviour_chi)
# X-squared = 0.065153, df = 2, p-value = 0.9679

# save Chi-Square results
saveRDS(SVCprey_behaviour_chi_result, 
        here("./outputs/SVCprey_behaviour_chi.rds"))

# post-hoc test
chisq.posthoc.test(SVCprey_behaviour_chi, method = "bonferroni")


# Colouration Presence/Absence Analysis ==================================

# The following compares the frequency of occurrence of different levels of 
# species' colouration between SVC and transect surveys by conducting a 
# Chi-Square Test. 

# select colouration columns and SVC density 
SVC_colouration_presence <- SVCprey_data[,c(1,18,26)]

# rename density column
SVC_colouration_presence <- 
  rename(SVC_colouration_presence, density = SVC_density)

# aggregate by session and colouration
SVC_colouration_presence <- 
  aggregate(.~session+colouration, SVC_colouration_presence, sum)

# create presence column
SVC_colouration_presence$presence <- 
  ifelse(SVC_colouration_presence$density > 0, 1, 0)

# create survey column
SVC_colouration_presence$survey <- "SVC"

# select colouration columns and transect density
prey_colouration_presence <- SVCprey_data[,c(1,18,30)]

# rename density column
prey_colouration_presence <- 
  rename(prey_colouration_presence, density = prey_density)

# aggregate by session and colouration
prey_colouration_presence <- 
  aggregate(.~session+colouration, prey_colouration_presence, sum)

# create presence column
prey_colouration_presence$presence <- 
  ifelse(prey_colouration_presence$density > 0, 1, 0)

# create survey column
prey_colouration_presence$survey <- "transect"

# bind SVC and transect presence values together 
SVCprey_colouration_presence <- 
  bind_rows(SVC_colouration_presence, prey_colouration_presence)

# remove session and density columns
SVCprey_colouration_presence <- SVCprey_colouration_presence[,c(2,4,5)]

# remove rows where presence = 0
SVCprey_colouration_presence <- 
  SVCprey_colouration_presence[!(SVCprey_colouration_presence$presence == 0),]

# convert dataframe to table
SVCprey_colouration_chi <- table(SVCprey_colouration_presence$colouration, 
                               SVCprey_colouration_presence$survey)

# Chi-Square Test
SVCprey_colouration_chi_result <- chisq.test(SVCprey_colouration_chi)
# X-squared = 10.567, df = 3, p-value = 0.01431

# save Chi-Square results
saveRDS(SVCprey_colouration_chi_result, 
        here("./outputs/SVCprey_colouration_chi.rds"))

# post-hoc test
chisq.posthoc.test(SVCprey_colouration_chi, method = "bonferroni")


# Shape Presence/Absence Analysis ==============================================

# The following compares the frequency of occurrence of different levels of 
# species' shape (compressiform, elongated, fusiform, or globiform) between 
# SVC and transect surveys by conducting a Chi-Square Test. 

# select shape columns and SVC density 
SVC_shape_presence <- SVCprey_data[,c(1,21,26)]

# rename density column
SVC_shape_presence <- rename(SVC_shape_presence, density = SVC_density)

# aggregate by session and shape
SVC_shape_presence <- aggregate(.~session+shape, SVC_shape_presence, sum)

# create presence column
SVC_shape_presence$presence <- ifelse(SVC_shape_presence$density > 0, 1, 0)

# create survey column
SVC_shape_presence$survey <- "SVC"

# select shape columns and transect density
prey_shape_presence <- SVCprey_data[,c(1,21,30)]

# rename density column
prey_shape_presence <- rename(prey_shape_presence, density = prey_density)

# aggregate by session and shape
prey_shape_presence <- aggregate(.~session+shape, prey_shape_presence, sum)

# create presence column
prey_shape_presence$presence <- ifelse(prey_shape_presence$density > 0, 1, 0)

# create survey column
prey_shape_presence$survey <- "transect"

# bind SVC and transect presence values together 
SVCprey_shape_presence <- bind_rows(SVC_shape_presence, prey_shape_presence)

# remove session and density columns
SVCprey_shape_presence <- SVCprey_shape_presence[,c(2,4,5)]

# remove rows where presence = 0
SVCprey_shape_presence <- 
  SVCprey_shape_presence[!(SVCprey_shape_presence$presence == 0),]

# convert dataframe to table
SVCprey_shape_chi <- table(SVCprey_shape_presence$shape, 
                                 SVCprey_shape_presence$survey)

# Chi-Square Test
SVCprey_shape_chi_result <- chisq.test(SVCprey_shape_chi)
# X-squared = 5.1083, df = 3, p-value = 0.164

# save Chi-Square results
saveRDS(SVCprey_shape_chi_result, here("./outputs/SVCprey_shape_chi.rds"))

# post-hoc test
chisq.posthoc.test(SVCprey_shape_chi, method = "bonferroni")


# Maximum Length Presence/Absence Analysis =====================================

# The following compares the frequency of occurrence of different levels of 
# species' maximum lengths, after binning the continuous variable, between 
# SVC and transect surveys by conducting a Chi-Square Test. 

# plot maximum length
plot(SVCprey_data$log_difference ~ SVCprey_data$max_length)

# bin by 10cm to 100cm
SVCprey_data$max_length_bins <- ifelse(SVCprey_data$max_length <= 10, 1, 
                                ifelse(SVCprey_data$max_length > 10 & 
                                    SVCprey_data$max_length <= 20, 2, 
                                ifelse(SVCprey_data$max_length > 20 & 
                                    SVCprey_data$max_length <= 30, 3, 
                                ifelse(SVCprey_data$max_length > 30 & 
                                    SVCprey_data$max_length <= 40, 4, 
                                ifelse(SVCprey_data$max_length > 40 & 
                                    SVCprey_data$max_length <= 50, 5, 
                                ifelse(SVCprey_data$max_length > 50 & 
                                    SVCprey_data$max_length <= 60, 6, 
                                ifelse(SVCprey_data$max_length > 60 & 
                                    SVCprey_data$max_length <= 70, 7, 
                                ifelse(SVCprey_data$max_length > 70 & 
                                    SVCprey_data$max_length <= 80, 8, 
                                ifelse(SVCprey_data$max_length > 80 & 
                                    SVCprey_data$max_length <= 90, 9, 
                                ifelse(SVCprey_data$max_length > 90 & 
                                    SVCprey_data$max_length <= 100, 10, 
                                ifelse(SVCprey_data$max_length > 100, 11, 
                                       NA)))))))))))

# make maximum length bins categorical
SVCprey_data$max_length_bins <- as.character(SVCprey_data$max_length_bins)

# number of observations of each maximum length bin 
sum(SVCprey_data$max_length_bins == 1)
sum(SVCprey_data$max_length_bins == 2)
sum(SVCprey_data$max_length_bins == 3)
sum(SVCprey_data$max_length_bins == 4)
sum(SVCprey_data$max_length_bins == 5)
sum(SVCprey_data$max_length_bins == 6)
sum(SVCprey_data$max_length_bins == 7)
sum(SVCprey_data$max_length_bins == 8)
sum(SVCprey_data$max_length_bins == 9)
sum(SVCprey_data$max_length_bins == 10)
sum(SVCprey_data$max_length_bins == 11)

# select max_length columns and SVC density 
SVC_max_length_presence <- SVCprey_data[,c(1,36,26)]

# rename density column
SVC_max_length_presence <- 
  rename(SVC_max_length_presence, density = SVC_density)

# aggregate by session and max_length
SVC_max_length_presence <- 
  aggregate(.~session+max_length_bins, SVC_max_length_presence, sum)

# create presence column
SVC_max_length_presence$presence <- 
  ifelse(SVC_max_length_presence$density > 0, 1, 0)

# create survey column
SVC_max_length_presence$survey <- "SVC"

# select max_length columns and transect density
prey_max_length_presence <- SVCprey_data[,c(1,36,30)]

# rename density column
prey_max_length_presence <- 
  rename(prey_max_length_presence, density = prey_density)

# aggregate by session and max_length
prey_max_length_presence <- 
  aggregate(.~session+max_length_bins, prey_max_length_presence, sum)

# create presence column
prey_max_length_presence$presence <- 
  ifelse(prey_max_length_presence$density > 0, 1, 0)

# create survey column
prey_max_length_presence$survey <- "transect"

# bind SVC and transect presence values together 
SVCprey_max_length_presence <- 
  bind_rows(SVC_max_length_presence, prey_max_length_presence)

# remove session and density columns
SVCprey_max_length_presence <- SVCprey_max_length_presence[,c(2,4,5)]

# remove rows where presence = 0
SVCprey_max_length_presence <- 
  SVCprey_max_length_presence[!(SVCprey_max_length_presence$presence == 0),]

# convert dataframe to table
SVCprey_max_length_chi <- table(SVCprey_max_length_presence$max_length, 
                           SVCprey_max_length_presence$survey)

# Chi-Square Test
SVCprey_max_length_chi_result <- chisq.test(SVCprey_max_length_chi)
# X-squared = 8.7777, df = 10, p-value = 0.5533

# save Chi-Square results
saveRDS(SVCprey_max_length_chi_result, 
        here("./outputs/SVCprey_max_length_chi.rds"))

# post-hoc test
chisq.posthoc.test(SVCprey_max_length_chi, method = "bonferroni")