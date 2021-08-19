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
SVC_fish_data <- read_csv(here("./clean_data/SVC_data.csv"))
prey_fish_data <- read_csv(here("./clean_data/prey_fish_data.csv"))


# Obtain Presence Values =======================================================

# The following compiles all presence observations across sessions for each 
# focal species.

# extract SVC observations
SVC_fish <- SVC_fish_data[,c(1,37)]

# extract transect observations 
prey_fish <- prey_fish_data[,c(1,3)]

# remove sessions with un-matched dates between surveys
SVC_fish <- SVC_fish[SVC_fish$session !=178,]
SVC_fish <- SVC_fish[SVC_fish$session !=179,]
SVC_fish <- SVC_fish[SVC_fish$session !=180,]
SVC_fish <- SVC_fish[SVC_fish$session !=268,]
prey_fish <- prey_fish[prey_fish$session !=178,]
prey_fish <- prey_fish[prey_fish$session !=179,]
prey_fish <- prey_fish[prey_fish$session !=180,]
prey_fish <- prey_fish[prey_fish$session !=268,]

# remove duplicate rows 
prey_presence <- unique(prey_fish[,1:2])

# remove session column
SVC_presence <- SVC_fish[,2]
prey_presence <- prey_presence[,2]

# add presence column
SVC_presence$presence <- 1
prey_presence$presence <- 1

# add "survey" column
SVC_presence$survey <- "SVC"
prey_presence$survey <- "transect"

# bind SVC and transect presence values together 
SVCprey_presence <- bind_rows(SVC_presence, prey_presence)


# Full Chi-Square Test =========================================================

# The following performs a Chi-Square Test on the number of sessions present
# across species between SVC and transect surveys to determine if an overall
# significant difference between survey types exists.

# convert dataframe to table
SVCprey_chi <- table(SVCprey_presence$species, 
                       SVCprey_presence$survey)

# Chi-Square Test
SVCprey_full_chi <- chisq.test(SVCprey_chi)

# save Chi-Square results
saveRDS(SVCprey_full_chi, here("./outputs/SVCprey_full_chi.rds"))


# Focal Chi-Square Test ========================================================

focal_SVCprey_pres <- filter(SVCprey_presence, species == "white grunt"|
                      species == "bluestriped grunt"|species == "hogfish"|
                      species == "mutton snapper"|
                      species == "yellowtail snapper"|species == "gray snapper"|
                      species == "red grouper"|species == "black grouper"|
                        species == "lionfish")

# convert dataframe to table
focal_SVCprey_chi <- table(focal_SVCprey_pres$species, 
                     focal_SVCprey_pres$survey)

# Chi-Square Test
SVCprey_focal_chi <- chisq.test(focal_SVCprey_chi)


# Chi-Square Test: White Grunt =================================================

# The following performs a Chi-Square test on presence/absence recordings of 
# white grunt across sessions between SVC and transect surveys. 

# extract SVC observations
SVC_fish <- SVC_fish_data[,c(1,37)]

# extract prey observations 
prey_fish <- prey_fish_data[,c(1,3)]

# extract prey sessions
prey_sessions <- prey_fish[,1]

# remove duplicate sessions 
prey_sessions <- unique(prey_sessions[,1])

# remove un-matching dates
prey_sessions <- prey_sessions[prey_sessions$session !=178,]
prey_sessions <- prey_sessions[prey_sessions$session !=179,]
prey_sessions <- prey_sessions[prey_sessions$session !=180,]
prey_sessions <- prey_sessions[prey_sessions$session !=268,]

# add species column for white grunt
white_grunt <- prey_sessions
white_grunt$species <- "white grunt"

# add presence column
SVC_fish$SVC_presence <- 1
prey_fish$prey_presence <- 1

# join white grunt presence values to each session
white_grunt_SVC <- join(white_grunt, SVC_fish, by = NULL, 
                             type = "left", match = "first")
white_grunt_prey <- join(white_grunt, prey_fish, by = NULL, 
                              type = "left", match = "first")

# add survey column
white_grunt_SVC$survey <- "SVC"
white_grunt_prey$survey <- "prey"

# rename columns
white_grunt_SVC <- rename(white_grunt_SVC, presence = SVC_presence)
white_grunt_prey <- rename(white_grunt_prey, 
                                presence = prey_presence)

# bind
white_grunt_chi <- bind_rows(white_grunt_SVC, white_grunt_prey)

# replace NA values with 0
white_grunt_chi[is.na(white_grunt_chi)] <- 0

# convert to table
white_grunt_chi <- table(white_grunt_chi$presence, 
                              white_grunt_chi$survey)

# chi-square test
white_grunt_result <- chisq.test(white_grunt_chi) 
# X-squared = 0.77471, df = 1, p-value = 0.3788

# save chi-square results
saveRDS(white_grunt_result, here("./outputs/white_grunt_chi.rds"))


# Chi-Square Test: Bluestriped Grunt ===========================================

# The following performs a Chi-Square test on presence/absence recordings of 
# bluestriped grunt across sessions between SVC and transect surveys. 

# add species column for bluestriped grunt
bluestriped_grunt <- prey_sessions
bluestriped_grunt$species <- "bluestriped grunt"

# add presence column
SVC_fish$SVC_presence <- 1
prey_fish$prey_presence <- 1

# join bluestriped grunt presence values to each session
bluestriped_grunt_SVC <- join(bluestriped_grunt, SVC_fish, by = NULL, 
                        type = "left", match = "first")
bluestriped_grunt_prey <- join(bluestriped_grunt, prey_fish, by = NULL, 
                         type = "left", match = "first")

# add survey column
bluestriped_grunt_SVC$survey <- "SVC"
bluestriped_grunt_prey$survey <- "prey"

# rename columns
bluestriped_grunt_SVC <- rename(bluestriped_grunt_SVC, presence = SVC_presence)
bluestriped_grunt_prey <- rename(bluestriped_grunt_prey, 
                           presence = prey_presence)

# bind
bluestriped_grunt_chi <- bind_rows(bluestriped_grunt_SVC, 
                                   bluestriped_grunt_prey)

# replace NA values with 0
bluestriped_grunt_chi[is.na(bluestriped_grunt_chi)] <- 0

# convert to table
bluestriped_grunt_chi <- table(bluestriped_grunt_chi$presence, 
                         bluestriped_grunt_chi$survey)

# chi-square test
bluestriped_grunt_result <- chisq.test(bluestriped_grunt_chi) 
# X-squared = 1.1622, df = 1, p-value = 0.281

# save chi-square results
saveRDS(bluestriped_grunt_result, here("./outputs/bluestriped_grunt_chi.rds"))


# Chi-Square Test: Hogfish =====================================================

# The following performs a Chi-Square test on presence/absence recordings of 
# hogfish across sessions between SVC and transect surveys. 

# add species column for hogfish
hogfish <- prey_sessions
hogfish$species <- "hogfish"

# add presence column
SVC_fish$SVC_presence <- 1
prey_fish$prey_presence <- 1

# join hogfish presence values to each session
hogfish_SVC <- join(hogfish, SVC_fish, by = NULL, 
                    type = "left", match = "first")
hogfish_prey <- join(hogfish, prey_fish, by = NULL, 
                     type = "left", match = "first")

# add survey column
hogfish_SVC$survey <- "SVC"
hogfish_prey$survey <- "prey"

# rename columns
hogfish_SVC <- rename(hogfish_SVC, presence = SVC_presence)
hogfish_prey <- rename(hogfish_prey, 
                       presence = prey_presence)

# bind
hogfish_chi <- bind_rows(hogfish_SVC, 
                         hogfish_prey)

# replace NA values with 0
hogfish_chi[is.na(hogfish_chi)] <- 0

# convert to table
hogfish_chi <- table(hogfish_chi$presence, 
                     hogfish_chi$survey)

# chi-square test
hogfish_result <- chisq.test(hogfish_chi) 
# X-squared = 1.1229, df = 1, p-value = 0.2893

# save chi-square results
saveRDS(hogfish_result, here("./outputs/hogfish_chi.rds"))


# Chi-Square Test: Mutton Snapper ==============================================

# The following performs a Chi-Square test on presence/absence recordings of 
# mutton snapper across sessions between SVC and transect surveys. 

# add species column for mutton snapper
mutton_snapper <- prey_sessions
mutton_snapper$species <- "mutton snapper"

# add presence column
SVC_fish$SVC_presence <- 1
prey_fish$prey_presence <- 1

# join mutton_snapper presence values to each session
mutton_snapper_SVC <- join(mutton_snapper, SVC_fish, by = NULL, 
                           type = "left", match = "first")
mutton_snapper_prey <- join(mutton_snapper, prey_fish, by = NULL, 
                            type = "left", match = "first")

# add survey column
mutton_snapper_SVC$survey <- "SVC"
mutton_snapper_prey$survey <- "prey"

# rename columns
mutton_snapper_SVC <- rename(mutton_snapper_SVC, presence = SVC_presence)
mutton_snapper_prey <- rename(mutton_snapper_prey, 
                              presence = prey_presence)

# bind
mutton_snapper_chi <- bind_rows(mutton_snapper_SVC, 
                                mutton_snapper_prey)

# replace NA values with 0
mutton_snapper_chi[is.na(mutton_snapper_chi)] <- 0

# convert to table
mutton_snapper_chi <- table(mutton_snapper_chi$presence, 
                            mutton_snapper_chi$survey)

# chi-square test
mutton_snapper_result <- chisq.test(mutton_snapper_chi) 
# X-squared = 2.2819, df = 1, p-value = 0.1309

# save chi-square results
saveRDS(mutton_snapper_result, here("./outputs/mutton_snapper_chi.rds"))


# Chi-Square Test: Gray Snapper ================================================

# The following performs a Chi-Square test on presence/absence recordings of 
# gray snapper across sessions between SVC and transect surveys. 

# add species column for gray snapper
gray_snapper <- prey_sessions
gray_snapper$species <- "gray snapper"

# add presence column
SVC_fish$SVC_presence <- 1
prey_fish$prey_presence <- 1

# join gray_snapper presence values to each session
gray_snapper_SVC <- join(gray_snapper, SVC_fish, by = NULL, 
                         type = "left", match = "first")
gray_snapper_prey <- join(gray_snapper, prey_fish, by = NULL, 
                          type = "left", match = "first")

# add survey column
gray_snapper_SVC$survey <- "SVC"
gray_snapper_prey$survey <- "prey"

# rename columns
gray_snapper_SVC <- rename(gray_snapper_SVC, presence = SVC_presence)
gray_snapper_prey <- rename(gray_snapper_prey, 
                            presence = prey_presence)

# bind
gray_snapper_chi <- bind_rows(gray_snapper_SVC, 
                              gray_snapper_prey)

# replace NA values with 0
gray_snapper_chi[is.na(gray_snapper_chi)] <- 0

# convert to table
gray_snapper_chi <- table(gray_snapper_chi$presence, 
                          gray_snapper_chi$survey)

# chi-square test
gray_snapper_result <- chisq.test(gray_snapper_chi) 
# X-squared = 0.33818, df = 1, p-value = 0.5609

# save chi-square results
saveRDS(gray_snapper_result, here("./outputs/gray_snapper_chi.rds"))


# Chi-Square Test: Yellowtail Snapper ==========================================

# The following performs a Chi-Square test on presence/absence recordings of 
# yellowtail snapper across sessions between SVC and transect surveys. 

# add species column for yellowtail snapper
yellowtail_snapper <- prey_sessions
yellowtail_snapper$species <- "yellowtail snapper"

# add presence column
SVC_fish$SVC_presence <- 1
prey_fish$prey_presence <- 1

# join yellowtail_snapper presence values to each session
yellowtail_snapper_SVC <- join(yellowtail_snapper, SVC_fish, by = NULL, 
                         type = "left", match = "first")
yellowtail_snapper_prey <- join(yellowtail_snapper, prey_fish, by = NULL, 
                          type = "left", match = "first")

# add survey column
yellowtail_snapper_SVC$survey <- "SVC"
yellowtail_snapper_prey$survey <- "prey"

# rename columns
yellowtail_snapper_SVC <- 
  rename(yellowtail_snapper_SVC, presence = SVC_presence)
yellowtail_snapper_prey <- rename(yellowtail_snapper_prey, 
                            presence = prey_presence)

# bind
yellowtail_snapper_chi <- bind_rows(yellowtail_snapper_SVC, 
                              yellowtail_snapper_prey)

# replace NA values with 0
yellowtail_snapper_chi[is.na(yellowtail_snapper_chi)] <- 0

# convert to table
yellowtail_snapper_chi <- table(yellowtail_snapper_chi$presence, 
                          yellowtail_snapper_chi$survey)

# chi-square test
yellowtail_snapper_result <- chisq.test(yellowtail_snapper_chi) 
# X-squared = 1.1622, df = 1, p-value = 0.281

# save chi-square results
saveRDS(yellowtail_snapper_result, here("./outputs/yellowtail_snapper_chi.rds"))


# Chi-Square Test: Red Grouper =================================================

# The following performs a Chi-Square test on presence/absence recordings of 
# red grouper across sessions between SVC and transect surveys. 

# add species column for red grouper
red_grouper <- prey_sessions
red_grouper$species <- "red grouper"

# add presence column
SVC_fish$SVC_presence <- 1
prey_fish$prey_presence <- 1

# join red_grouper presence values to each session
red_grouper_SVC <- join(red_grouper, SVC_fish, by = NULL, 
                         type = "left", match = "first")
red_grouper_prey <- join(red_grouper, prey_fish, by = NULL, 
                          type = "left", match = "first")

# add survey column
red_grouper_SVC$survey <- "SVC"
red_grouper_prey$survey <- "prey"

# rename columns
red_grouper_SVC <- rename(red_grouper_SVC, presence = SVC_presence)
red_grouper_prey <- rename(red_grouper_prey, 
                            presence = prey_presence)

# bind
red_grouper_chi <- bind_rows(red_grouper_SVC, 
                              red_grouper_prey)

# replace NA values with 0
red_grouper_chi[is.na(red_grouper_chi)] <- 0

# convert to table
red_grouper_chi <- table(red_grouper_chi$presence, 
                          red_grouper_chi$survey)

# chi-square test
red_grouper_result <- chisq.test(red_grouper_chi) 
# X-squared = 3.3753, df = 1, p-value = 0.06618

# save chi-square results
saveRDS(red_grouper_result, here("./outputs/red_grouper_chi.rds"))


# Chi-Square Test: Black Grouper ===============================================

# The following performs a Chi-Square test on presence/absence recordings of 
# black grouper across sessions between SVC and transect surveys. 

# add species column for black grouper
black_grouper <- prey_sessions
black_grouper$species <- "black grouper"

# add presence column
SVC_fish$SVC_presence <- 1
prey_fish$prey_presence <- 1

# join black_grouper presence values to each session
black_grouper_SVC <- join(black_grouper, SVC_fish, by = NULL, 
                        type = "left", match = "first")
black_grouper_prey <- join(black_grouper, prey_fish, by = NULL, 
                         type = "left", match = "first")

# add survey column
black_grouper_SVC$survey <- "SVC"
black_grouper_prey$survey <- "prey"

# rename columns
black_grouper_SVC <- rename(black_grouper_SVC, presence = SVC_presence)
black_grouper_prey <- rename(black_grouper_prey, 
                           presence = prey_presence)

# bind
black_grouper_chi <- bind_rows(black_grouper_SVC, 
                             black_grouper_prey)

# replace NA values with 0
black_grouper_chi[is.na(black_grouper_chi)] <- 0

# convert to table
black_grouper_chi <- table(black_grouper_chi$presence, 
                         black_grouper_chi$survey)

# chi-square test
black_grouper_result <- chisq.test(black_grouper_chi) 
# X-squared = 0.15803, df = 1, p-value = 0.691

# save chi-square results
saveRDS(black_grouper_result, here("./outputs/black_grouper_chi.rds"))


# Chi-Square Test: Lionfish ====================================================

# The following performs a Chi-Square test on presence/absence recordings of 
# lionfish across sessions between SVC and transect surveys. 

# add species column for lionfish
lionfish <- prey_sessions
lionfish$species <- "lionfish"

# add presence column
SVC_fish$SVC_presence <- 1
prey_fish$prey_presence <- 1

# join lionfish presence values to each session
lionfish_SVC <- join(lionfish, SVC_fish, by = NULL, 
                          type = "left", match = "first")
lionfish_prey <- join(lionfish, prey_fish, by = NULL, 
                           type = "left", match = "first")

# add survey column
lionfish_SVC$survey <- "SVC"
lionfish_prey$survey <- "prey"

# rename columns
lionfish_SVC <- rename(lionfish_SVC, presence = SVC_presence)
lionfish_prey <- rename(lionfish_prey, 
                             presence = prey_presence)

# bind
lionfish_chi <- bind_rows(lionfish_SVC, 
                               lionfish_prey)

# replace NA values with 0
lionfish_chi[is.na(lionfish_chi)] <- 0

# convert to table
lionfish_chi <- table(lionfish_chi$presence, 
                           lionfish_chi$survey)

# chi-square test
lionfish_result <- chisq.test(lionfish_chi) 
# X-squared = 1.6804, df = 1, p-value = 0.1949

# save chi-square results
saveRDS(lionfish_result, here("./outputs/lionfish_chi.rds"))


# Barplot ======================================================================

# The following creates a barplot of the number of sessions each species was 
# recorded in between SVC and transect surveys. 

# aggregate presence by sum
SVCprey_presence_bar <- aggregate(.~species+survey, focal_SVCprey_pres, sum)

# sort by species
SVCprey_presence_bar <- 
  SVCprey_presence_bar[order(SVCprey_presence_bar$species),]

# barplot
SVCprey_presabs_bar <- ggplot(SVCprey_presence_bar, aes(x = species, 
                       y = presence, fill = survey)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  theme_classic() +
  xlab("Species") + 
  ylab("Number of Sessions Present") +
  # scale_fill_manual(values = c("gray88", "gray44")) +
  scale_fill_brewer(palette = "YlGnBu") +
  theme(axis.title = element_text(size = 24)) +
  theme(axis.text = element_text(size = 22)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  theme(legend.text = element_text(size = 22)) +
  theme(legend.title = element_text(size = 24)) 
ggsave(here("./visuals/SVCprey_presabs_bar.png"), SVCprey_presabs_bar)
