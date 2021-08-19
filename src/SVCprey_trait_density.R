########## SURVEY COMPARISON PROJECT SVC VS. TRANSECT DENSITY COMPARISON #######
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
# DATE OF CREATION: 2021-08-17
##########
##########


# Set-Up =======================================================================

# packages
library(plyr)
library(tidyverse)
library(ggplot2)
library(car)
library(here)

# data
SVCprey_data <- read_csv(here("./dataframes/SVCprey_dataframe.csv"))


# Aggregation Behaviour Density Differences ====================================

# The following explores density differences between SVC and transect surveys
# across different levels of aggregation behaviour (solitary, shoaling, and 
# schoolling) using Kruskal-Wallis tests and a Dunn post-hoc test.

# select SVC columns
SVC_behaviour <- SVCprey_data[,c(1,16,25)]

# aggregate by session and behaviour
SVC_behaviour <- aggregate(.~session+behavior, SVC_behaviour, sum)

# add survey column
SVC_behaviour$survey <- "SVC"

# rename density column
SVC_behaviour <- rename(SVC_behaviour, density = SVC_density)

# select transect columns
prey_behaviour <- SVCprey_data[,c(1,16,29)]

# aggregate by session and behaviour
prey_behaviour <- aggregate(.~session+behavior, prey_behaviour, sum)

# add survey column
prey_behaviour$survey <- "transect"

# rename density column
prey_behaviour <- rename(prey_behaviour, density = prey_density)

# join SVC and prey together 
SVCprey_behaviour <- join(SVC_behaviour, prey_behaviour, by = NULL, type = "full", match = "all")

# make survey+behaviour column
SVCprey_behaviour$survey_behaviour <- paste(SVCprey_behaviour$survey, SVCprey_behaviour$behavior)

# shapiro-wilk normality tests
with(SVCprey_behaviour, shapiro.test(density[survey == "SVC" & behavior == "solitary"]))
with(SVCprey_behaviour, shapiro.test(density[survey == "SVC" & behavior == "shoaling"]))
with(SVCprey_behaviour, shapiro.test(density[survey == "SVC" & behavior == "schooling"]))
with(SVCprey_behaviour, shapiro.test(density[survey == "transect" & behavior == "solitary"]))
with(SVCprey_behaviour, shapiro.test(density[survey == "transect" & behavior == "shoaling"]))
with(SVCprey_behaviour, shapiro.test(density[survey == "transect" & behavior == "schooling"]))
# all but shoaling on transect are non-normal

# Kruskal-Wallis test
SVCprey_behaviour_kruskal <- wilcox.test(density~survey_behaviour, data = SVCprey_behaviour)

# two-way ANOVA
SVCprey_behaviour_aov <- aov(density~survey*behavior, data = SVCprey_behaviour)

# Tukey Test
TukeyHSD(SVCprey_behaviour_aov)

# check homogeneity of variance
plot(SVCprey_behaviour_aov, 1) # three outliers
leveneTest(density~survey*behavior, data = SVCprey_behaviour) # p < 2.2e-16, do not have homogeneity of variance

# check normality 
plot(SVCprey_behaviour_aov, 2)
SVCprey_behaviour_residuals <- residuals(object = SVCprey_behaviour_aov)
shapiro.test(x = SVCprey_behaviour_residuals) # p < 2.2e-16, not normal 

# calculate log densities
SVCprey_behaviour$log_density <- log((SVCprey_behaviour$density)+0.001)

# two-way ANOVA with log densities 
SVCprey_behaviour_logaov <- aov(log_density~survey*behavior, data = SVCprey_behaviour)

# Tukey Test
TukeyHSD(SVCprey_behaviour_logaov)

# check homogeneity of variance
plot(SVCprey_behaviour_logaov, 1) # no outliers
leveneTest(log_density~survey*behavior, data = SVCprey_behaviour) # p < 2.2e-16, do not have homogeneity of variance

# check normality 
plot(SVCprey_behaviour_logaov, 2)
SVCprey_behaviour_logresiduals <- residuals(object = SVCprey_behaviour_logaov)
shapiro.test(x = SVCprey_behaviour_logresiduals) # p < 2.2e-16, not normal 
