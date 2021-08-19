########## SURVEY COMPARISON PROJECT SVC VS. ROVING CHI SQUARE ##########
########## 
##########
# This file creates performs Chi-Square Tests on presence/absence values for 
# large predatory species across sessions for SVC and roving surveys. This 
# complements previous analyses performed on the full presence/absence 
# dataframe across all species. 
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2021-06-14
##########
##########


# Set-Up =======================================================================

# packages
library(plyr)
library(tidyverse)
library(ggplot2)
library(here)

# data
SVC_fish_data <- read_csv(here("./clean_data/SVC_data.csv"))
pred_fish_data <- read_csv(here("./clean_data/pred_fish_data.csv"))


# Chi-Square Test: Goldentail Moray ============================================

# The following performs a Chi-Square test on presence/absence recordings of 
# goldentail moray across sessions between SVC and roving surveys. 

# extract SVC observations
SVC_fish <- SVC_fish_data[,c(1,37)]

# extract roving observations 
pred_fish <- pred_fish_data[,c(1,3)]

# extract roving sessions
pred_sessions <- pred_fish[,1]

# remove duplicate sessions 
pred_sessions <- unique(pred_sessions[,1])

# remove un-matching dates
pred_sessions <- pred_sessions[pred_sessions$session !=178,]
pred_sessions <- pred_sessions[pred_sessions$session !=179,]
pred_sessions <- pred_sessions[pred_sessions$session !=180,]

# add species column for goldentail moray
goldentail_moray <- pred_sessions
goldentail_moray$species <- "goldentail moray"

# add presence column
SVC_fish$SVC_presence <- 1
pred_fish$pred_presence <- 1

# join goldentail moray presence values to each session
goldentail_moray_SVC <- join(goldentail_moray, SVC_fish, by = NULL, 
                             type = "left", match = "first")
goldentail_moray_pred <- join(goldentail_moray, pred_fish, by = NULL, 
                              type = "left", match = "first")

# add survey column
goldentail_moray_SVC$survey <- "SVC"
goldentail_moray_pred$survey <- "roving"

# rename columns
goldentail_moray_SVC <- rename(goldentail_moray_SVC, presence = SVC_presence)
goldentail_moray_pred <- rename(goldentail_moray_pred, 
                                presence = pred_presence)

# bind
goldentail_moray_chi <- bind_rows(goldentail_moray_SVC, goldentail_moray_pred)

# replace NA values with 0
goldentail_moray_chi[is.na(goldentail_moray_chi)] <- 0

# convert to table
goldentail_moray_chi <- table(goldentail_moray_chi$presence, 
                              goldentail_moray_chi$survey)

# chi-square test
goldentail_moray_result <- chisq.test(goldentail_moray_chi) 
# X-squared = 5.322, df = 1, p-value = 0.02106

# save chi-square results
saveRDS(goldentail_moray_result, here("./outputs/goldentail_moray_chi.rds"))


# Chi-Square Test: Green Moray =================================================

# The following performs a Chi-Square test on presence/absence recordings of 
# green moray across sessions between SVC and roving surveys. 

# add species column for green moray
green_moray <- pred_sessions
green_moray$species <- "green moray"

# add presence column
SVC_fish$SVC_presence <- 1
pred_fish$pred_presence <- 1

# join green moray presence values to each session
green_moray_SVC <- join(green_moray, SVC_fish, by = NULL, type = "left", 
                        match = "first")
green_moray_pred <- join(green_moray, pred_fish, by = NULL, type = "left", 
                         match = "first")

# add survey column
green_moray_SVC$survey <- "SVC"
green_moray_pred$survey <- "roving"

# rename columns
green_moray_SVC <- rename(green_moray_SVC, presence = SVC_presence)
green_moray_pred <- rename(green_moray_pred, presence = pred_presence)

# bind
green_moray_chi <- bind_rows(green_moray_SVC, green_moray_pred)

# replace NA values with 0
green_moray_chi[is.na(green_moray_chi)] <- 0

# convert to table
green_moray_chi <- table(green_moray_chi$presence, green_moray_chi$survey)

# chi-square test
green_moray_result <- chisq.test(green_moray_chi) 
# X-squared = 15.987, df = 1, p-value = 6.377e-05

# save chi-square results
saveRDS(green_moray_result, here("./outputs/green_moray_chi.rds"))


# Chi-Square Test: Purplemouth Moray ===========================================

# The following performs a Chi-Square test on presence/absence recordings of 
# purplemouth moray across sessions between SVC and roving surveys. 

# add species column for purplemouth moray
purplemouth_moray <- pred_sessions
purplemouth_moray$species <- "purplemouth moray"

# add presence column
SVC_fish$SVC_presence <- 1
pred_fish$pred_presence <- 1

# join purplemouth moray presence values to each session
purplemouth_moray_SVC <- join(purplemouth_moray, SVC_fish, by = NULL, 
                              type = "left", match = "first")
purplemouth_moray_pred <- join(purplemouth_moray, pred_fish, by = NULL, 
                               type = "left", match = "first")

# add survey column
purplemouth_moray_SVC$survey <- "SVC"
purplemouth_moray_pred$survey <- "roving"

# rename columns
purplemouth_moray_SVC <- rename(purplemouth_moray_SVC, presence = SVC_presence)
purplemouth_moray_pred <- rename(purplemouth_moray_pred, 
                                 presence = pred_presence)

# bind
purplemouth_moray_chi <- bind_rows(purplemouth_moray_SVC, 
                                   purplemouth_moray_pred)

# replace NA values with 0
purplemouth_moray_chi[is.na(purplemouth_moray_chi)] <- 0

# convert to table
purplemouth_moray_chi <- table(purplemouth_moray_chi$presence, 
                               purplemouth_moray_chi$survey)

# chi-square test
purplemouth_moray_result <- chisq.test(purplemouth_moray_chi) 
# X-squared = 10.701, df = 1, p-value = 0.001071

# save chi-square results
saveRDS(purplemouth_moray_result, here("./outputs/purplemouth_moray_chi.rds"))


# Chi-Square Test: Spotted Moray ===============================================

# The following performs a Chi-Square test on presence/absence recordings of 
# spotted moray across sessions between SVC and roving surveys. 

# add species column for spotted moray
spotted_moray <- pred_sessions
spotted_moray$species <- "spotted moray"

# add presence column
SVC_fish$SVC_presence <- 1
pred_fish$pred_presence <- 1

# join spotted moray presence values to each session
spotted_moray_SVC <- join(spotted_moray, SVC_fish, by = NULL, type = "left", 
                        match = "first")
spotted_moray_pred <- join(spotted_moray, pred_fish, by = NULL, type = "left", 
                         match = "first")

# add survey column
spotted_moray_SVC$survey <- "SVC"
spotted_moray_pred$survey <- "roving"

# rename columns
spotted_moray_SVC <- rename(spotted_moray_SVC, presence = SVC_presence)
spotted_moray_pred <- rename(spotted_moray_pred, presence = pred_presence)

# bind
spotted_moray_chi <- bind_rows(spotted_moray_SVC, spotted_moray_pred)

# replace NA values with 0
spotted_moray_chi[is.na(spotted_moray_chi)] <- 0

# convert to table
spotted_moray_chi <- table(spotted_moray_chi$presence, spotted_moray_chi$survey)

# chi-square test
spotted_moray_result <- chisq.test(spotted_moray_chi) 
# X-squared = 51.136, df = 1, p-value = 8.618e-13

# save chi-square results
saveRDS(spotted_moray_result, here("./outputs/spotted_moray_chi.rds"))


# Chi-Square Test: Sharptail Eel ===============================================

# The following performs a Chi-Square test on presence/absence recordings of 
# sharptail eel across sessions between SVC and roving surveys. 

# add species column for sharptail eel
sharptail_eel <- pred_sessions
sharptail_eel$species <- "sharptail eel"

# add presence column
SVC_fish$SVC_presence <- 1
pred_fish$pred_presence <- 1

# join sharptail eel presence values to each session
sharptail_eel_SVC <- join(sharptail_eel, SVC_fish, by = NULL, type = "left", 
                        match = "first")
sharptail_eel_pred <- join(sharptail_eel, pred_fish, by = NULL, type = "left", 
                         match = "first")

# add survey column
sharptail_eel_SVC$survey <- "SVC"
sharptail_eel_pred$survey <- "roving"

# rename columns
sharptail_eel_SVC <- rename(sharptail_eel_SVC, presence = SVC_presence)
sharptail_eel_pred <- rename(sharptail_eel_pred, presence = pred_presence)

# bind
sharptail_eel_chi <- bind_rows(sharptail_eel_SVC, sharptail_eel_pred)

# replace NA values with 0
sharptail_eel_chi[is.na(sharptail_eel_chi)] <- 0

# convert to table
sharptail_eel_chi <- table(sharptail_eel_chi$presence, sharptail_eel_chi$survey)

# chi-square test
sharptail_eel_result <- chisq.test(sharptail_eel_chi) 
# X-squared = 0.50485, df = 1, p-value = 0.4774

# save chi-square results
saveRDS(sharptail_eel_result, here("./outputs/sharptail_eel_chi.rds"))


# Chi-Square Test: Amberjack =================================================

# The following performs a Chi-Square test on presence/absence recordings of 
# amberjack across sessions between SVC and roving surveys. 

# add species column for amberjack
amberjack <- pred_sessions
amberjack$species <- "amberjack"

# add presence column
SVC_fish$SVC_presence <- 1
pred_fish$pred_presence <- 1

# join amberjack presence values to each session
amberjack_SVC <- join(amberjack, SVC_fish, by = NULL, type = "left", 
                        match = "first")
amberjack_pred <- join(amberjack, pred_fish, by = NULL, type = "left", 
                         match = "first")

# add survey column
amberjack_SVC$survey <- "SVC"
amberjack_pred$survey <- "roving"

# rename columns
amberjack_SVC <- rename(amberjack_SVC, presence = SVC_presence)
amberjack_pred <- rename(amberjack_pred, presence = pred_presence)

# bind
amberjack_chi <- bind_rows(amberjack_SVC, amberjack_pred)

# replace NA values with 0
amberjack_chi[is.na(amberjack_chi)] <- 0

# convert to table
amberjack_chi <- table(amberjack_chi$presence, amberjack_chi$survey)

# chi-square test
chisq.test(amberjack_chi) 
# X-squared = 0, df = 1, p-value = 1


# Chi-Square Test: Black Grouper ===============================================

# The following performs a Chi-Square test on presence/absence recordings of 
# black grouper across sessions between SVC and roving surveys. 

# add species column for black grouper
black_grouper <- pred_sessions
black_grouper$species <- "black grouper"

# add presence column
SVC_fish$SVC_presence <- 1
pred_fish$pred_presence <- 1

# join black grouper presence values to each session
black_grouper_SVC <- join(black_grouper, SVC_fish, by = NULL, type = "left", 
                          match = "first")
black_grouper_pred <- join(black_grouper, pred_fish, by = NULL, type = "left", 
                           match = "first")

# add survey column
black_grouper_SVC$survey <- "SVC"
black_grouper_pred$survey <- "roving"

# rename columns
black_grouper_SVC <- rename(black_grouper_SVC, presence = SVC_presence)
black_grouper_pred <- rename(black_grouper_pred, presence = pred_presence)

# bind
black_grouper_chi <- bind_rows(black_grouper_SVC, black_grouper_pred)

# replace NA values with 0
black_grouper_chi[is.na(black_grouper_chi)] <- 0

# convert to table
black_grouper_chi <- table(black_grouper_chi$presence, black_grouper_chi$survey)

# chi-square test
black_grouper_result <- chisq.test(black_grouper_chi) 
# X-squared = 18.047, df = 1, p-value = 2.155e-05

# save chi-square results
saveRDS(black_grouper_result, here("./outputs/black_grouper_chi.rds"))


# Chi-Square Test: Coney =======================================================

# The following performs a Chi-Square test on presence/absence recordings of 
# coney across sessions between SVC and roving surveys. 

# add species column for coney
coney <- pred_sessions
coney$species <- "coney"

# add presence column
SVC_fish$SVC_presence <- 1
pred_fish$pred_presence <- 1

# join coney presence values to each session
coney_SVC <- join(coney, SVC_fish, by = NULL, type = "left", 
                        match = "first")
coney_pred <- join(coney, pred_fish, by = NULL, type = "left", 
                         match = "first")

# add survey column
coney_SVC$survey <- "SVC"
coney_pred$survey <- "roving"

# rename columns
coney_SVC <- rename(coney_SVC, presence = SVC_presence)
coney_pred <- rename(coney_pred, presence = pred_presence)

# bind
coney_chi <- bind_rows(coney_SVC, coney_pred)

# replace NA values with 0
coney_chi[is.na(coney_chi)] <- 0

# convert to table
coney_chi <- table(coney_chi$presence, coney_chi$survey)

# chi-square test
coney_result <- chisq.test(coney_chi) 
# X-squared = 2.3061, df = 1, p-value = 0.1289

# save chi-square results
saveRDS(coney_result, here("./outputs/coney_chi.rds"))


# Chi-Square Test: Gag Grouper =================================================

# The following performs a Chi-Square test on presence/absence recordings of 
# gag grouper across sessions between SVC and roving surveys. 

# add species column for gag grouper
gag_grouper <- pred_sessions
gag_grouper$species <- "gag grouper"

# add presence column
SVC_fish$SVC_presence <- 1
pred_fish$pred_presence <- 1

# join gag grouper presence values to each session
gag_grouper_SVC <- join(gag_grouper, SVC_fish, by = NULL, type = "left", 
                        match = "first")
gag_grouper_pred <- join(gag_grouper, pred_fish, by = NULL, type = "left", 
                         match = "first")

# add survey column
gag_grouper_SVC$survey <- "SVC"
gag_grouper_pred$survey <- "roving"

# rename columns
gag_grouper_SVC <- rename(gag_grouper_SVC, presence = SVC_presence)
gag_grouper_pred <- rename(gag_grouper_pred, presence = pred_presence)

# bind
gag_grouper_chi <- bind_rows(gag_grouper_SVC, gag_grouper_pred)

# replace NA values with 0
gag_grouper_chi[is.na(gag_grouper_chi)] <- 0

# convert to table
gag_grouper_chi <- table(gag_grouper_chi$presence, gag_grouper_chi$survey)

# chi-square test
gag_grouper_result <- chisq.test(gag_grouper_chi) 
# X-squared = 12.703, df = 1, p-value = 0.000365

# save chi-square results
saveRDS(gag_grouper_result, here("./outputs/gag_grouper_chi.rds"))


# Chi-Square Test: Graysby =====================================================

# The following performs a Chi-Square test on presence/absence recordings of 
# graysby across sessions between SVC and roving surveys. 

# add species column for graysby
graysby <- pred_sessions
graysby$species <- "graysby"

# add presence column
SVC_fish$SVC_presence <- 1
pred_fish$pred_presence <- 1

# join graysby presence values to each session
graysby_SVC <- join(graysby, SVC_fish, by = NULL, type = "left", 
                        match = "first")
graysby_pred <- join(graysby, pred_fish, by = NULL, type = "left", 
                         match = "first")

# add survey column
graysby_SVC$survey <- "SVC"
graysby_pred$survey <- "roving"

# rename columns
graysby_SVC <- rename(graysby_SVC, presence = SVC_presence)
graysby_pred <- rename(graysby_pred, presence = pred_presence)

# bind
graysby_chi <- bind_rows(graysby_SVC, graysby_pred)

# replace NA values with 0
graysby_chi[is.na(graysby_chi)] <- 0

# convert to table
graysby_chi <- table(graysby_chi$presence, graysby_chi$survey)

# chi-square test
graysby_result <- chisq.test(graysby_chi) 
# X-squared = 1.567, df = 1, p-value = 0.2106

# save chi-square results
saveRDS(graysby_result, here("./outputs/graysby_chi.rds"))


# Chi-Square Test: Nassau Grouper ==============================================

# The following performs a Chi-Square test on presence/absence recordings of 
# nassau grouper across sessions between SVC and roving surveys. 

# add species column for nassau grouper
nassau_grouper <- pred_sessions
nassau_grouper$species <- "nassau grouper"

# add presence column
SVC_fish$SVC_presence <- 1
pred_fish$pred_presence <- 1

# join nassau grouper presence values to each session
nassau_grouper_SVC <- join(nassau_grouper, SVC_fish, by = NULL, type = "left", 
                        match = "first")
nassau_grouper_pred <- join(nassau_grouper, pred_fish, by = NULL, type = "left", 
                         match = "first")

# add survey column
nassau_grouper_SVC$survey <- "SVC"
nassau_grouper_pred$survey <- "roving"

# rename columns
nassau_grouper_SVC <- rename(nassau_grouper_SVC, presence = SVC_presence)
nassau_grouper_pred <- rename(nassau_grouper_pred, presence = pred_presence)

# bind
nassau_grouper_chi <- bind_rows(nassau_grouper_SVC, nassau_grouper_pred)

# replace NA values with 0
nassau_grouper_chi[is.na(nassau_grouper_chi)] <- 0

# convert to table
nassau_grouper_chi <- table(nassau_grouper_chi$presence, 
                            nassau_grouper_chi$survey)

# chi-square test
nassau_grouper_result <- chisq.test(nassau_grouper_chi) 
# X-squared = 19.97, df = 1, p-value = 7.866e-06

# save chi-square results
saveRDS(nassau_grouper_result, here("./outputs/nassau_grouper_chi.rds"))


# Chi-Square Test: Red Grouper =================================================

# The following performs a Chi-Square test on presence/absence recordings of 
# red grouper across sessions between SVC and roving surveys. 

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
red_grouper_chi <- bind_rows(red_grouper_SVC, red_grouper_pred)

# replace NA values with 0
red_grouper_chi[is.na(red_grouper_chi)] <- 0

# convert to table
red_grouper_chi <- table(red_grouper_chi$presence, red_grouper_chi$survey)

# chi-square test
red_grouper_result <- chisq.test(red_grouper_chi) 
# X-squared = 12.712, df = 1, p-value = 0.0003634

# save chi-square results
saveRDS(red_grouper_result, here("./outputs/red_grouper_chi.rds"))


# Chi-Square Test: Red Hind ====================================================

# The following performs a Chi-Square test on presence/absence recordings of 
# red hind across sessions between SVC and roving surveys. 

# add species column for red hind
red_hind <- pred_sessions
red_hind$species <- "red hind"

# add presence column
SVC_fish$SVC_presence <- 1
pred_fish$pred_presence <- 1

# join red hind presence values to each session
red_hind_SVC <- join(red_hind, SVC_fish, by = NULL, type = "left", 
                        match = "first")
red_hind_pred <- join(red_hind, pred_fish, by = NULL, type = "left", 
                         match = "first")

# add survey column
red_hind_SVC$survey <- "SVC"
red_hind_pred$survey <- "roving"

# rename columns
red_hind_SVC <- rename(red_hind_SVC, presence = SVC_presence)
red_hind_pred <- rename(red_hind_pred, presence = pred_presence)

# bind
red_hind_chi <- bind_rows(red_hind_SVC, red_hind_pred)

# replace NA values with 0
red_hind_chi[is.na(red_hind_chi)] <- 0

# convert to table
red_hind_chi <- table(red_hind_chi$presence, red_hind_chi$survey)

# chi-square test
red_hind_result <- chisq.test(red_hind_chi) 
# X-squared = 5.322, df = 1, p-value = 0.02106

# save chi-square results
saveRDS(red_hind_result, here("./outputs/red_hind_chi.rds"))


# Chi-Square Test: Rock Hind ===================================================

# The following performs a Chi-Square test on presence/absence recordings of 
# rock hind across sessions between SVC and roving surveys. 

# add species column for rock hind
rock_hind <- pred_sessions
rock_hind$species <- "rock hind"

# add presence column
SVC_fish$SVC_presence <- 1
pred_fish$pred_presence <- 1

# join rock hind presence values to each session
rock_hind_SVC <- join(rock_hind, SVC_fish, by = NULL, type = "left", 
                        match = "first")
rock_hind_pred <- join(rock_hind, pred_fish, by = NULL, type = "left", 
                         match = "first")

# add survey column
rock_hind_SVC$survey <- "SVC"
rock_hind_pred$survey <- "roving"

# rename columns
rock_hind_SVC <- rename(rock_hind_SVC, presence = SVC_presence)
rock_hind_pred <- rename(rock_hind_pred, presence = pred_presence)

# bind
rock_hind_chi <- bind_rows(rock_hind_SVC, rock_hind_pred)

# replace NA values with 0
rock_hind_chi[is.na(rock_hind_chi)] <- 0

# convert to table
rock_hind_chi <- table(rock_hind_chi$presence, rock_hind_chi$survey)

# chi-square test
rock_hind_result <- chisq.test(rock_hind_chi)
# X-squared = 0.8197, df = 1, p-value = 0.3653

# save chi-square results
saveRDS(rock_hind_result, here("./outputs/rock_hind_chi.rds"))


# Chi-Square Test: Greater Soapfish ============================================

# The following performs a Chi-Square test on presence/absence recordings of 
# greater soapfish across sessions between SVC and roving surveys. 

# add species column for greater soapfish
greater_soapfish <- pred_sessions
greater_soapfish$species <- "greater soapfish"

# add presence column
SVC_fish$SVC_presence <- 1
pred_fish$pred_presence <- 1

# join greater soapfish presence values to each session
greater_soapfish_SVC <- join(greater_soapfish, SVC_fish, by = NULL, 
                             type = "left", match = "first")
greater_soapfish_pred <- join(greater_soapfish, pred_fish, by = NULL, 
                              type = "left", match = "first")

# add survey column
greater_soapfish_SVC$survey <- "SVC"
greater_soapfish_pred$survey <- "roving"

# rename columns
greater_soapfish_SVC <- rename(greater_soapfish_SVC, presence = SVC_presence)
greater_soapfish_pred <- rename(greater_soapfish_pred, presence = pred_presence)

# bind
greater_soapfish_chi <- bind_rows(greater_soapfish_SVC, greater_soapfish_pred)

# replace NA values with 0
greater_soapfish_chi[is.na(greater_soapfish_chi)] <- 0

# convert to table
greater_soapfish_chi <- table(greater_soapfish_chi$presence, 
                            greater_soapfish_chi$survey)

# chi-square test
greater_soapfish_result <- chisq.test(greater_soapfish_chi) 
# X-squared = 4.2904, df = 1, p-value = 0.03833

# save chi-square results
saveRDS(greater_soapfish_result, here("./outputs/greater_soapfish_chi.rds"))


# Chi-Square Test: Scamp =======================================================

# The following performs a Chi-Square test on presence/absence recordings of 
# scamp across sessions between SVC and roving surveys. 

# add species column for scamp
scamp <- pred_sessions
scamp$species <- "scamp"

# add presence column
SVC_fish$SVC_presence <- 1
pred_fish$pred_presence <- 1

# join scamp presence values to each session
scamp_SVC <- join(scamp, SVC_fish, by = NULL, type = "left", 
                           match = "first")
scamp_pred <- join(scamp, pred_fish, by = NULL, type = "left", 
                            match = "first")

# add survey column
scamp_SVC$survey <- "SVC"
scamp_pred$survey <- "roving"

# rename columns
scamp_SVC <- rename(scamp_SVC, presence = SVC_presence)
scamp_pred <- rename(scamp_pred, presence = pred_presence)

# bind
scamp_chi <- bind_rows(scamp_SVC, scamp_pred)

# replace NA values with 0
scamp_chi[is.na(scamp_chi)] <- 0

# convert to table
scamp_chi <- table(scamp_chi$presence, 
                            scamp_chi$survey)

# chi-square test
chisq.test(scamp_chi) 
# X-squared = 0, df = 1, p-value = 1


# Chi-Square Test: Black Margate ===============================================

# The following performs a Chi-Square test on presence/absence recordings of 
# black margate across sessions between SVC and roving surveys. 

# add species column for black margate
black_margate <- pred_sessions
black_margate$species <- "black margate"

# add presence column
SVC_fish$SVC_presence <- 1
pred_fish$pred_presence <- 1

# join black margate presence values to each session
black_margate_SVC <- join(black_margate, SVC_fish, by = NULL, type = "left", 
                           match = "first")
black_margate_pred <- join(black_margate, pred_fish, by = NULL, type = "left", 
                            match = "first")

# add survey column
black_margate_SVC$survey <- "SVC"
black_margate_pred$survey <- "roving"

# rename columns
black_margate_SVC <- rename(black_margate_SVC, presence = SVC_presence)
black_margate_pred <- rename(black_margate_pred, presence = pred_presence)

# bind
black_margate_chi <- bind_rows(black_margate_SVC, black_margate_pred)

# replace NA values with 0
black_margate_chi[is.na(black_margate_chi)] <- 0

# convert to table
black_margate_chi <- table(black_margate_chi$presence, 
                            black_margate_chi$survey)

# chi-square test
chisq.test(black_margate_chi) 
# X-squared = 0, df = 1, p-value = 1


# Chi-Square Test: Cubera Snapper ==============================================

# The following performs a Chi-Square test on presence/absence recordings of 
# cubera snapper across sessions between SVC and roving surveys. 

# add species column for cubera snapper
cubera_snapper <- pred_sessions
cubera_snapper$species <- "cubera snapper"

# add presence column
SVC_fish$SVC_presence <- 1
pred_fish$pred_presence <- 1

# join cubera snapper presence values to each session
cubera_snapper_SVC <- join(cubera_snapper, SVC_fish, by = NULL, type = "left", 
                           match = "first")
cubera_snapper_pred <- join(cubera_snapper, pred_fish, by = NULL, type = "left", 
                            match = "first")

# add survey column
cubera_snapper_SVC$survey <- "SVC"
cubera_snapper_pred$survey <- "roving"

# rename columns
cubera_snapper_SVC <- rename(cubera_snapper_SVC, presence = SVC_presence)
cubera_snapper_pred <- rename(cubera_snapper_pred, presence = pred_presence)

# bind
cubera_snapper_chi <- bind_rows(cubera_snapper_SVC, cubera_snapper_pred)

# replace NA values with 0
cubera_snapper_chi[is.na(cubera_snapper_chi)] <- 0

# convert to table
cubera_snapper_chi <- table(cubera_snapper_chi$presence, 
                            cubera_snapper_chi$survey)

# chi-square test
chisq.test(cubera_snapper_chi) 
# X-squared = 0, df = 1, p-value = 1


# Chi-Square Test: Mutton Snapper ==============================================

# The following performs a Chi-Square test on presence/absence recordings of 
# mutton snapper across sessions between SVC and roving surveys. 

# add species column for mutton snapper
mutton_snapper <- pred_sessions
mutton_snapper$species <- "mutton snapper"

# add presence column
SVC_fish$SVC_presence <- 1
pred_fish$pred_presence <- 1

# join mutton snapper presence values to each session
mutton_snapper_SVC <- join(mutton_snapper, SVC_fish, by = NULL, type = "left", 
                          match = "first")
mutton_snapper_pred <- join(mutton_snapper, pred_fish, by = NULL, type = "left", 
                           match = "first")

# add survey column
mutton_snapper_SVC$survey <- "SVC"
mutton_snapper_pred$survey <- "roving"

# rename columns
mutton_snapper_SVC <- rename(mutton_snapper_SVC, presence = SVC_presence)
mutton_snapper_pred <- rename(mutton_snapper_pred, presence = pred_presence)

# bind
mutton_snapper_chi <- bind_rows(mutton_snapper_SVC, mutton_snapper_pred)

# replace NA values with 0
mutton_snapper_chi[is.na(mutton_snapper_chi)] <- 0

# convert to table
mutton_snapper_chi <- table(mutton_snapper_chi$presence, 
                            mutton_snapper_chi$survey)

# chi-square test
mutton_snapper_result <- chisq.test(mutton_snapper_chi) 
# X-squared = 0.83979, df = 1, p-value = 0.3595

# save chi-square results
saveRDS(mutton_snapper_result, here("./outputs/mutton_snapper_chi.rds"))


# Chi-Square Test: Barracuda ===================================================

# The following performs a Chi-Square test on presence/absence recordings of 
# barracuda across sessions between SVC and roving surveys. 

# add species column for barracuda
barracuda <- pred_sessions
barracuda$species <- "barracuda"

# add presence column
SVC_fish$SVC_presence <- 1
pred_fish$pred_presence <- 1

# join barracuda presence values to each session
barracuda_SVC <- join(barracuda, SVC_fish, by = NULL, type = "left", 
                     match = "first")
barracuda_pred <- join(barracuda, pred_fish, by = NULL, type = "left", 
                      match = "first")

# add survey column
barracuda_SVC$survey <- "SVC"
barracuda_pred$survey <- "roving"

# rename columns
barracuda_SVC <- rename(barracuda_SVC, presence = SVC_presence)
barracuda_pred <- rename(barracuda_pred, presence = pred_presence)

# bind
barracuda_chi <- bind_rows(barracuda_SVC, barracuda_pred)

# replace NA values with 0
barracuda_chi[is.na(barracuda_chi)] <- 0

# convert to table
barracuda_chi <- table(barracuda_chi$presence, barracuda_chi$survey)

# chi-square test
barracuda_result <- chisq.test(barracuda_chi) 
# X-squared = 0, df = 1, p-value = 1


# Chi-Square Test: Lionfish ====================================================

# The following performs a Chi-Square test on presence/absence recordings of 
# lionfish across sessions between SVC and roving surveys. 

# add species column for lionfish
lionfish <- pred_sessions
lionfish$species <- "lionfish"

# add presence column
SVC_fish$SVC_presence <- 1
pred_fish$pred_presence <- 1

# join lionfish presence values to each session
lionfish_SVC <- join(lionfish, SVC_fish, by = NULL, type = "left", 
                           match = "first")
lionfish_pred <- join(lionfish, pred_fish, by = NULL, type = "left", 
                            match = "first")

# add survey column
lionfish_SVC$survey <- "SVC"
lionfish_pred$survey <- "roving"

# rename columns
lionfish_SVC <- rename(lionfish_SVC, presence = SVC_presence)
lionfish_pred <- rename(lionfish_pred, presence = pred_presence)

# bind
lionfish_chi <- bind_rows(lionfish_SVC, lionfish_pred)

# replace NA values with 0
lionfish_chi[is.na(lionfish_chi)] <- 0

# convert to table
lionfish_chi <- table(lionfish_chi$presence, lionfish_chi$survey)

# chi-square test
lionfish_result <- chisq.test(lionfish_chi) 
# X-squared = 26.031, df = 1, p-value = 3.36e-07

# save chi-square results
saveRDS(lionfish_result, here("./outputs/lionfish_chi.rds"))


# Chi-Square Test: Spotted Scorpionfish ========================================

# The following performs a Chi-Square test on presence/absence recordings of 
# spotted scorpionfish across sessions between SVC and roving surveys. 

# add species column for spotted scorpionfish
spotted_scorpionfish <- pred_sessions
spotted_scorpionfish$species <- "spotted scorpionfish"

# add presence column
SVC_fish$SVC_presence <- 1
pred_fish$pred_presence <- 1

# join spotted scorpionfish presence values to each session
spotted_scorpionfish_SVC <- join(spotted_scorpionfish, SVC_fish, by = NULL, 
                                 type = "left", match = "first")
spotted_scorpionfish_pred <- join(spotted_scorpionfish, pred_fish, by = NULL, 
                                  type = "left", match = "first")

# add survey column
spotted_scorpionfish_SVC$survey <- "SVC"
spotted_scorpionfish_pred$survey <- "roving"

# rename columns
spotted_scorpionfish_SVC <- rename(spotted_scorpionfish_SVC, 
                                   presence = SVC_presence)
spotted_scorpionfish_pred <- rename(spotted_scorpionfish_pred, 
                                    presence = pred_presence)

# bind
spotted_scorpionfish_chi <- bind_rows(spotted_scorpionfish_SVC, 
                                      spotted_scorpionfish_pred)

# replace NA values with 0
spotted_scorpionfish_chi[is.na(spotted_scorpionfish_chi)] <- 0

# convert to table
spotted_scorpionfish_chi <- table(spotted_scorpionfish_chi$presence, 
                                  spotted_scorpionfish_chi$survey)

# chi-square test
spotted_scorpionfish_result <- chisq.test(spotted_scorpionfish_chi) 
# X-squared = 4.1809, df = 1, p-value = 0.04088

# save chi-square results
saveRDS(spotted_scorpionfish_result,
        here("./outputs/spotted_scorpionfish_chi.rds"))


# Chi-Square Test: Trumpetfish =================================================

# The following performs a Chi-Square test on presence/absence recordings of 
# trumpetfish across sessions between SVC and roving surveys. 

# add species column for trumpetfish
trumpetfish <- pred_sessions
trumpetfish$species <- "trumpetfish"

# add presence column
SVC_fish$SVC_presence <- 1
pred_fish$pred_presence <- 1

# join trumpetfish presence values to each session
trumpetfish_SVC <- join(trumpetfish, SVC_fish, by = NULL, type = "left", 
                     match = "first")
trumpetfish_pred <- join(trumpetfish, pred_fish, by = NULL, type = "left", 
                      match = "first")

# add survey column
trumpetfish_SVC$survey <- "SVC"
trumpetfish_pred$survey <- "roving"

# rename columns
trumpetfish_SVC <- rename(trumpetfish_SVC, presence = SVC_presence)
trumpetfish_pred <- rename(trumpetfish_pred, presence = pred_presence)

# bind
trumpetfish_chi <- bind_rows(trumpetfish_SVC, trumpetfish_pred)

# replace NA values with 0
trumpetfish_chi[is.na(trumpetfish_chi)] <- 0

# convert to table
trumpetfish_chi <- table(trumpetfish_chi$presence, trumpetfish_chi$survey)

# chi-square test
trumpetfish_result <- chisq.test(trumpetfish_chi) 
# X-squared = 24.516, df = 1, p-value = 7.371e-07

# save chi-square results
saveRDS(trumpetfish_result, here("./outputs/trumpetfish_chi.rds"))