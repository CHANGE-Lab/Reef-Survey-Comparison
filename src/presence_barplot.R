########## SURVEY COMPARISON PROJECT PRESENCE BARPLOT ##########
########## 
##########
# This file creates a barplot of the total number of sessions each of 8 RVC
# focal species was present between SVC, transect, and roving surveys. 
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2021-06-30
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
pred_fish_data <- read_csv(here("./clean_data/pred_fish_data.csv"))


# Obtain Presence Values =======================================================

# The following compiles all presence observations across sessions for each 
# focal species.

# extract SVC observations
SVC_fish <- SVC_fish_data[,c(1,37)]

# extract transect observations 
prey_fish <- prey_fish_data[,c(1,3)]

# extract roving observations 
pred_fish <- pred_fish_data[,c(1,3)]

# filter for SVC focal species
SVC_fish <- filter(SVC_fish, species == "white grunt"|
                     species == "bluestriped grunt"|species == "hogfish"|
                     species == "mutton snapper"|species == "gray snapper"|
                     species == "yellowtail snapper"|species == "red grouper"|
                     species == "black grouper")
prey_fish <- filter(prey_fish, species == "white grunt"|
                      species == "bluestriped grunt"|species == "hogfish"|
                      species == "mutton snapper"|species == "gray snapper"|
                      species == "yellowtail snapper"|species == "red grouper"|
                      species == "black grouper")
pred_fish <- filter(pred_fish, species == "white grunt"|
                      species == "bluestriped grunt"|species == "hogfish"|
                      species == "mutton snapper"|species == "gray snapper"|
                      species == "yellowtail snapper"|species == "red grouper"|
                      species == "black grouper")


# remove sessions with un-matched dates between surveys
SVC_fish <- SVC_fish[SVC_fish$session !=178,]
SVC_fish <- SVC_fish[SVC_fish$session !=179,]
SVC_fish <- SVC_fish[SVC_fish$session !=180,]
SVC_data <- SVC_data[SVC_data$session !=264,]
SVC_data <- SVC_data[SVC_data$session !=265,]
SVC_data <- SVC_data[SVC_data$session !=266,]
SVC_data <- SVC_data[SVC_data$session !=267,]
SVC_data <- SVC_data[SVC_data$session !=268,]
SVC_data <- SVC_data[SVC_data$session !=269,]
SVC_data <- SVC_data[SVC_data$session !=270,]
SVC_data <- SVC_data[SVC_data$session !=271,]
SVC_data <- SVC_data[SVC_data$session !=272,]
SVC_data <- SVC_data[SVC_data$session !=273,]
SVC_data <- SVC_data[SVC_data$session !=274,]
prey_fish <- prey_fish[prey_fish$session !=178,]
prey_fish <- prey_fish[prey_fish$session !=179,]
prey_fish <- prey_fish[prey_fish$session !=180,]
prey_fish <- prey_fish[prey_fish$session !=264,]
prey_fish <- prey_fish[prey_fish$session !=265,]
prey_fish <- prey_fish[prey_fish$session !=266,]
prey_fish <- prey_fish[prey_fish$session !=267,]
prey_fish <- prey_fish[prey_fish$session !=268,]
prey_fish <- prey_fish[prey_fish$session !=269,]
prey_fish <- prey_fish[prey_fish$session !=270,]
prey_fish <- prey_fish[prey_fish$session !=271,]
prey_fish <- prey_fish[prey_fish$session !=272,]
prey_fish <- prey_fish[prey_fish$session !=273,]
prey_fish <- prey_fish[prey_fish$session !=274,]
pred_fish <- pred_fish[pred_fish$session !=178,]
pred_fish <- pred_fish[pred_fish$session !=179,]
pred_fish <- pred_fish[pred_fish$session !=180,]
pred_fish <- pred_fish[pred_fish$session !=264,]
pred_fish <- pred_fish[pred_fish$session !=265,]
pred_fish <- pred_fish[pred_fish$session !=266,]
pred_fish <- pred_fish[pred_fish$session !=267,]
pred_fish <- pred_fish[pred_fish$session !=268,]
pred_fish <- pred_fish[pred_fish$session !=269,]
pred_fish <- pred_fish[pred_fish$session !=270,]
pred_fish <- pred_fish[pred_fish$session !=271,]
pred_fish <- pred_fish[pred_fish$session !=272,]
pred_fish <- pred_fish[pred_fish$session !=273,]
pred_fish <- pred_fish[pred_fish$session !=274,]

# remove duplicate rows 
prey_presence <- unique(prey_fish[,1:2])
pred_presence <- unique(pred_fish[,1:2])

# remove session column
SVC_presence <- SVC_fish[,2]
prey_presence <- prey_presence[,2]
pred_presence <- pred_presence[,2]

# add presence column
SVC_presence$presence <- 1
prey_presence$presence <- 1
pred_presence$presence <- 1

# add "survey" column
SVC_presence$survey <- "SVC"
prey_presence$survey <- "transect"
pred_presence$survey <- "roving"

# bind survey presence values together 
survey_presence <- bind_rows(SVC_presence, prey_presence, pred_presence)


# Barplot ======================================================================

# The following creates a barplot of the number of sessions each species was 
# recorded in between surveys types . 

# aggregate presence by sum
survey_presence_bar <- aggregate(.~species+survey, survey_presence, sum)

# order species
survey_presence_bar$species <- factor(survey_presence_bar$species, 
                               levels = c("white grunt", "bluestriped grunt", 
                               "hogfish", "mutton snapper", "gray snapper", 
                               "yellowtail snapper", "black grouper", 
                               "red grouper"))

# barplot
presence_barplot <- ggplot(survey_presence_bar, aes(x = species, 
                    y = presence, fill = survey)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  theme_classic() +
  xlab("Species") + 
  ylab("Number of Sessions Present") +
  scale_fill_manual(values = c("lemonchiffon1", "aquamarine3", "navyblue")) +
  theme(axis.title = element_text(size = 24)) +
  theme(axis.text = element_text(size = 22)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.text = element_text(size = 22)) +
  theme(legend.title = element_text(size = 24)) 
ggsave(here("./visuals/SVCprey_presabs_bar.png"), SVCprey_presabs_bar)