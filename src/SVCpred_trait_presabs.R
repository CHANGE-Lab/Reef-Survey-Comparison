########## SURVEY COMPARISON PROJECT SVC VS. ROVING PRESENCE ANALYSIS ########
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

# data
SVCpred_data <- read_csv(here("./dataframes/SVCpred_dataframe.csv"))


# Colouration Presence/Absence Analysis ========================================

# The following compares the frequency of occurrence of different levels of 
# species' colouration (camouflaged, neutral, silvering, or colourful) between 
# SVC and transect surveys by conducting a Chi-Square Test. 

# select colouration columns and SVC density 
SVC_colouration_presence <- SVCpred_data[,c(1,18,25)]

# rename density column
SVC_colouration_presence <- rename(SVC_colouration_presence, density = SVC_density)

# aggregate by session and colouration
SVC_colouration_presence <- aggregate(.~session+colouration, SVC_colouration_presence, sum)

# create presence column
SVC_colouration_presence$presence <- ifelse(SVC_colouration_presence$density > 0, 1, 0)

# create survey column
SVC_colouration_presence$survey <- "SVC"

# select colouration columns and transect density
pred_colouration_presence <- SVCpred_data[,c(1,18,29)]

# rename density column
pred_colouration_presence <- rename(pred_colouration_presence, density = pred_density)

# aggregate by session and colouration
pred_colouration_presence <- aggregate(.~session+colouration, pred_colouration_presence, sum)

# create presence column
pred_colouration_presence$presence <- ifelse(pred_colouration_presence$density > 0, 1, 0)

# create survey column
pred_colouration_presence$survey <- "transect"

# bind SVC and transect presence values together 
SVCpred_colouration_presence <- bind_rows(SVC_colouration_presence, pred_colouration_presence)

# remove session and density columns
SVCpred_colouration_presence <- SVCpred_colouration_presence[,c(2,4,5)]

# remove rows where presence = 0
SVCpred_colouration_presence <- SVCpred_colouration_presence[!(SVCpred_colouration_presence$presence == 0),]

# convert dataframe to table
SVCpred_colouration_chi <- table(SVCpred_colouration_presence$colouration, 
                                 SVCpred_colouration_presence$survey)

# Chi-Square Test
SVCpred_colouration_chi <- chisq.test(SVCpred_colouration_chi)
# X-squared = 18.157, df = 2, p-value = 0.0001141

# save Chi-Square results
saveRDS(SVCpred_colouration_chi, here("./outputs/SVCpred_colouration_chi.rds"))

# post-hoc test
chisq.posthoc.test(SVCpred_colouration_chi, method = "bonferroni")

