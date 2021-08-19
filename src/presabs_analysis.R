########## SURVEY COMPARISON PROJECT PRESENCE/ABSENCE COMPARISON ##########
########## 
##########
# This file creates a dataframe outlining the total number of sessions each 
# species observed in SVC and roving surveys was present in. Only a subset of 
# large, predatory reef fish species are extracted as these were the only group
# surveyed for by roving surveys. The dataframe created is used to compare 
# presence recordings between SVC and roving surveys within and across species
# in order to explore if significant differences are present and how these
# differences compare to differences in density across species explored 
# previously. 
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2021-06-04
##########
##########


# Set-Up =======================================================================

# packages
library(plyr)
library(tidyverse)
library(ggplot2)
library(data.table)
install.packages("chisq.posthoc.test")
library(chisq.posthoc.test)
library(here)

# data
SVC_fish_data <- read_csv(here("./clean_data/SVC_data.csv"))
pred_fish_data <- read_csv(here("./clean_data/pred_fish_data.csv"))
traits <- read_csv(here("./clean_data/fish_traits.csv"))


# Obtain Presence Values =======================================================

# The following compiles all presence observations across sessions for each 
# species.

# extract SVC observations
SVC_fish <- SVC_fish_data[,c(1,37)]

# extract roving observations 
pred_fish <- pred_fish_data[,c(1,3)]

# remove duplicate rows 
pred_presence <- unique(pred_fish[,1:2])

# remove session column
SVC_presence <- SVC_fish[,2]
pred_presence <- pred_presence[,2]

# add presence column
SVC_presence$presence <- 1
pred_presence$presence <- 1

# aggregate species by sum
# SVC_presence <- aggregate(.~species, SVC_presence, sum)
# pred_presence <- aggregate(.~species, pred_presence, sum)

# extract roving presence traits column
pred_fish_list <- traits[,c(4,7)]

# change column name
pred_fish_list <- rename(pred_fish_list, species = common_name)

# select species recorded on roving surveys
pred_fish_list <- filter(pred_fish_list, pred_fish_list$predator_presence == 1)

# join roving presence to fish lists
SVC_presence <- join(SVC_presence, pred_fish_list, by = NULL, type = "full", 
                     match = "all")
pred_presence <- join(pred_presence, pred_fish_list, by = NULL, type = "full", 
                     match = "all")

# replace NA values with 0
SVC_presence[is.na(SVC_presence)] <- 0
pred_presence[is.na(pred_presence)] <- 0

# filter for roving survey species
SVC_presence <- filter(SVC_presence, predator_presence == 1) 
pred_presence <- filter(pred_presence, predator_presence == 1) 

# remove roving presence column
SVC_presence <- SVC_presence[,1:2]
pred_presence <- pred_presence[1:2]

# remove gray snapper 
SVC_presence <- SVC_presence[SVC_presence$species !="gray snapper",]
pred_presence <- pred_presence[pred_presence$species !="gray snapper",]

# add "survey" column
SVC_presence$survey <- "SVC"
pred_presence$survey <- "roving"

# bind SVC and roving presence values together 
SVCroving_presence_long <- bind_rows(SVC_presence, pred_presence)

# take from long to wide
SVCroving_presence_wide <- spread(SVCroving_presence_long, species, presence)

# change column names
SVC_presence <- rename(SVC_presence, SVC_presence = presence)
pred_presence <- rename(pred_presence, pred_presence = presence)

# remove survey columns
SVC_presence <- SVC_presence[,1:2]
pred_presence <- pred_presence[,1:2]

# join SVC and roving presence
SVCroving_presence_chi <- join(SVC_presence, pred_presence, by = NULL, 
                           type = "full", match = "all")

SVCroving_chi <- table(SVCroving_presence_long$species, SVCroving_presence_long$survey)
chisq.test(table(SVCroving_presence_long$species, SVCroving_presence_long$survey))
chisq.posthoc.test(SVCroving_chi)


# Barplot ======================================================================

# The following creates a barplot of the number of sessions each species was 
# recorded in between SVC and roving surveys. 

# barplot
SVCpred_presabs_bar <- ggplot(SVCroving_presence_long, aes(x = species, 
                       y = presence, fill = survey)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  theme_classic() + 
  xlab("Species") + 
  ylab("Number of Sessions Present") +
  scale_fill_manual(values = c("lemonchiffon1", "navyblue")) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 14)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) 
ggsave(here("./visuals/SVCpred_presabs_barplot.png"), SVCpred_presabs_bar)


# T-Test on Species' Presence ==================================================

# The following performs a two-sample t-test to test the effect of survey type
# on the number of sessions species were recorded in across all species.

# variance calculation
var(SVCroving_presence_chi$SVC_presence) # = 153.6957
var(SVCroving_presence_chi$pred_presence) # = 318.419

# F-test for equal variance
var.test(presence ~ survey, data = SVCroving_presence_long)
# p = 0.09472; no significant difference between variances 

# SVC presence normal distribution check
shapiro.test(SVCroving_presence_chi$SVC_presence) # p = 9.729e-07
hist(SVCroving_presence_chi$SVC_presence)
# non-normal

# roving presence normal distribution check
shapiro.test(SVCroving_presence_chi$pred_presence) # p = 0.001012
hist(SVCroving_presence_chi$pred_presence)
# non-normal

# t-test
presabs_ttest <- t.test(presence ~ survey, data = SVCroving_presence_long, 
                 var.equal = TRUE)
presabs_ttest
# t = 2.1688, df = 44, p-value = 0.03554

# kruskal-wallace test
kruskal.test(presence ~ survey, data = SVCroving_presence_long)

# mann-whitney U test
wilcox.test(presence ~ survey, data = SVCroving_presence_long)

# chi-square test
chisq.test(SVCroving_presence_chi$SVC_presence, SVCroving_presence_chi$pred_presence)


# Linear Model on Presence Values ==============================================

# Linear model using presence/absence values as response variable and species
# and survey type as the predictors.

# remove duplicate rows 
pred_fish <- unique(pred_fish[,1:2])

# add roving presence column
pred_fish$presence <- 1

# remove presence column
pred_fish_list <- pred_fish_list[,1]

# join roving species to survey data
SVC_presabs <- join(SVC_fish, pred_fish_list, by = NULL, type = "full", 
                    match = "all")


# Chi-Square Test: Red Grouper =================================================

# The following performs a Chi-Square test on presence/absence recordings of 
# red grouper across sessions between SVC and roving surveys. 

# extract roving sessions
pred_sessions <- pred_fish[,1]

# remove duplicate sessions 
pred_sessions <- unique(pred_sessions[,1])

# add species column for red grouper
red_grouper <- pred_sessions
red_grouper$species <- "red grouper"

# add presence column
SVC_fish$SVC_presence <- 1
pred_fish$pred_presence <- 1

# join red grouper presence values to each session
red_grouper_SVC <- join(red_grouper, SVC_fish, by = NULL, type = "left", 
                    match = "first")
red_grouper_pred <- join(red_grouper, pred_fish, by = NULL, type = "left", 
                    match = "first")

# add survey column
red_grouper_SVC$survey <- "SVC"
red_grouper_pred$survey <- "roving"

# rename columns
red_grouper_SVC <- rename(red_grouper_SVC, presence = SVC_presence)
red_grouper_pred <- rename(red_grouper_pred, presence = pred_presence)

# bind
SVCroving_presence_long <- bind_rows(SVC_presence, pred_presence)
red_grouper_lm <- bind_rows(red_grouper_SVC, red_grouper_pred)

# replace NA values with 0
red_grouper_lm[is.na(red_grouper_lm)] <- 0
red_grouper[is.na(red_grouper)] <- 0

# convert to table
red_grouper_chi <- table(red_grouper$SVC_presence, red_grouper$pred_presence)

# chi-square test
chisq.test(red_grouper_chi)
# X-squared = 17.72, df = 1, p-value = 2.559e-05

# F-test for equal variance
var.test(red_grouper$SVC_presence, red_grouper$pred_presence)
# F = 0.43243, num df = 106, denom df = 106, p-value = 2.187e-05
# unequal variance

# SVC presence normal distribution check
shapiro.test(red_grouper$SVC_presence) # p < 2.2e-16
hist(red_grouper$SVC_presence)
# non-normal

# roving presence normal distribution check
shapiro.test(red_grouper$pred_presence) # p = 5.368e-16
hist(red_grouper$pred_presence)
# non-normal

# t test
red_grouper_ttest <- t.test(red_grouper$SVC_presence, red_grouper$pred_presence)
red_grouper_ttest
# t = -3.8297, df = 183.23, p-value = 0.000176

# mann-whitney U test
red_grouper$SVC_presence <- as.numeric(red_grouper$SVC_presence)
red_grouper$pred_presence <- as.numeric(red_grouper$pred_presence)
wilcox.test(SVC_presence, pred_presence, data = red_grouper)

# Fischer's exact test
sum(red_grouper$SVC_presence) # 11 present out of 107 sessions
sum(red_grouper$pred_presence) # 33 present out of 107 sessions
fisher.test(matrix(c(11, 107-11, 33, 107-33), ncol=2))

# sum presence
red_grouper$presence_sum <- red_grouper$SVC_presence + red_grouper$pred_presence

# remove rows where presence = 0 for both surveys
red_grouper_chi <- red_grouper[red_grouper$presence_sum !=0,] 

# convert to table
red_grouper_chi <- table(red_grouper_chi$SVC_presence, red_grouper_chi$pred_presence)
red_grouper_chi2 <- table(red_grouper_lm$presence, red_grouper_lm$survey)

# chi-square test
chisq.test(red_grouper_chi)
chisq.test(red_grouper_chi2)

# proportion test
prop.test(c(11,33), c(107,107), alternative = "two.sided", correct = FALSE)
# X-squared = 13.847, df = 1, p-value = 0.0001983
# http://www.sthda.com/english/wiki/two-proportions-z-test-in-r 

# lm
red_grouper_model <- lm(presence ~ survey, data = red_grouper_lm)
summary(red_grouper_model)


# Chi-Square Test: Black Grouper ===============================================

# The following performs a Chi-Square test on presence/absence recordings of 
# black grouper across sessions between SVC and roving surveys. 

# add species column for black grouper
black_grouper <- pred_sessions
black_grouper$species <- "black grouper"

# join black grouper presence values to each session
black_grouper <- join(black_grouper, SVC_fish, by = NULL, type = "left", 
                    match = "first")
black_grouper <- join(black_grouper, pred_fish, by = NULL, type = "left", 
                    match = "first")

# replace NA values with 0
black_grouper[is.na(black_grouper)] <- 0

# convert to table
black_grouper_chi <- table(black_grouper$SVC_presence, 
                           black_grouper$pred_presence)

# chi-square test
chisq.test(black_grouper_chi)
# X-squared = 5.33, df = 1, p-value = 0.02096

# F-test for equal variance
var.test(black_grouper$SVC_presence, black_grouper$pred_presence)
# F = 0.39819, num df = 106, denom df = 106, p-value = 3.328e-06
# unequal variance

# SVC presence normal distribution check
shapiro.test(black_grouper$SVC_presence) # p < 2.2e-16
hist(black_grouper$SVC_presence)
# non-normal

# roving presence normal distribution check
shapiro.test(black_grouper$pred_presence) # p = 1.895e-15
hist(black_grouper$pred_presence)
# non-normal

# t test
black_grouper_ttest <- t.test(black_grouper$SVC_presence, 
                              black_grouper$pred_presence)
black_grouper_ttest
# t = -4.7341, df = 178.86, p-value = 4.459e-06

# Fischer's exact test
sum(black_grouper$SVC_presence) # 11 present out of 107 sessions
sum(black_grouper$pred_presence) # 39 present out of 107 sessions
fisher.test(matrix(c(11, 107-11, 39, 107-39), ncol=2))


# Chi-Square Test: Mutton Snapper ==============================================

# The following performs a Chi-Square test on presence/absence recordings of 
# mutton snapper across sessions between SVC and roving surveys. 

# add species column for mutton snapper
mutton_snapper <- pred_sessions
mutton_snapper$species <- "mutton snapper"

# join mutton snapper presence values to each session
mutton_snapper <- join(mutton_snapper, SVC_fish, by = NULL, type = "left", 
                      match = "first")
mutton_snapper <- join(mutton_snapper, pred_fish, by = NULL, type = "left", 
                      match = "first")

# replace NA values with 0
mutton_snapper[is.na(mutton_snapper)] <- 0

# convert to table
mutton_snapper_chi <- table(mutton_snapper$SVC_presence, 
                           mutton_snapper$pred_presence)

# chi-square test
chisq.test(mutton_snapper_chi)
# X-squared = 15.174, df = 1, p-value = 9.806e-05

# F-test for equal variance
var.test(mutton_snapper$SVC_presence, mutton_snapper$pred_presence)
# F = 0.76412, num df = 106, denom df = 106, p-value = 0.1677
# unequal variance

# SVC presence normal distribution check
shapiro.test(mutton_snapper$SVC_presence) # p < 2.2e-16
hist(mutton_snapper$SVC_presence)
# non-normal

# roving presence normal distribution check
shapiro.test(mutton_snapper$pred_presence) # p < 2.2e-16
hist(mutton_snapper$pred_presence)
# non-normal

# t test
mutton_snapper_ttest <- t.test(mutton_snapper$SVC_presence, 
                              mutton_snapper$pred_presence)
mutton_snapper_ttest
# t = -1.0944, df = 208.28, p-value = 0.275

# Fischer's exact test
sum(mutton_snapper$SVC_presence) # 15 present out of 107 sessions
sum(mutton_snapper$pred_presence) # 21 present out of 107 sessions
fisher.test(matrix(c(15, 107-15, 21, 107-21), ncol=2))


# Chi-Square Test: Lionfish ====================================================

# The following performs a Chi-Square test on presence/absence recordings of 
# lionfish across sessions between SVC and roving surveys. 

# add species column for lionfish
lionfish <- pred_sessions
lionfish$species <- "lionfish"

# join lionfish presence values to each session
lionfish <- join(lionfish, SVC_fish, by = NULL, type = "left", 
                       match = "first")
lionfish <- join(lionfish, pred_fish, by = NULL, type = "left", 
                       match = "first")

# replace NA values with 0
lionfish[is.na(lionfish)] <- 0

# convert to table
lionfish_chi <- table(lionfish$SVC_presence, 
                            lionfish$pred_presence)

# chi-square test
chisq.test(lionfish_chi)
# X-squared = 0.62845, df = 1, p-value = 0.4279 

# F-test for equal variance
var.test(lionfish$SVC_presence, lionfish$pred_presence)
# F = 0.19953, num df = 106, denom df = 106, p-value = 3.116e-15
# unequal variance

# SVC presence normal distribution check
shapiro.test(lionfish$SVC_presence) # p < 2.2e-16
hist(lionfish$SVC_presence)
# non-normal

# roving presence normal distribution check
shapiro.test(lionfish$pred_presence) # p < 2.2e-16
hist(lionfish$pred_presence)
# non-normal

# t test
lionfish_ttest <- t.test(lionfish$SVC_presence, 
                               lionfish$pred_presence)
lionfish_ttest
# t = -5.7641, df = 146.68, p-value = 4.673e-08

# Fischer's exact test
sum(lionfish$SVC_presence) # 5 present out of 107 sessions
sum(lionfish$pred_presence) # 36 present out of 107 sessions
fisher.test(matrix(c(5, 107-5, 36, 107-36), ncol=2))