########## SURVEY COMPARISON PROJECT MODEL DATAFRAME CREATION ##########
########## 
##########
# This file combines fish observation data from three survey types (SVC, 
# transect, and roving) with associated survey metadata, habitat traits, and 
# species traits before calculating density and average depth across 
# observations and log transforming density for further analyses.
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2021-06-07
##########
##########


# Set-Up =======================================================================

# packages
library(plyr)
library(tidyverse)
library(here)

# data
fish_data <- read_csv(here("./dataframes/fish_dataframe.csv"))
SVC <- read_csv(here("./clean_data/SVC_data.csv"))
prey_meta <- read_csv(here("./clean_data/prey_metadata.csv"))
pred_meta <- read_csv(here("./clean_data/pred_metadata.csv"))
traits <- read_csv(here("./clean_data/fish_traits.csv"))
vert_relief <- read_csv(here("./clean_data/vertical_relief.csv"))


# Joining Survey Metadata ======================================================

# The following joins metadata associated with each survey type (SVC, transect 
# and roving) to the full fish dataframe. 


# Joining SVC Metadata: 

# select wanted metadata columns: session, site, date, diver, habitat, 
# cylinder_area, max_depth, octocoral, stony coral
SVC_meta <- SVC[,c(1,3,4,12,16,17,33,34)] 

# aggregate rows by session
SVC_meta <- SVC_meta %>% group_by(session, site, SVC_date, SVC_habitat) %>% 
  summarise_each(funs(mean))

# rename area column
SVC_meta <- SVC_meta %>% rename(SVC_area = SVC_cylinder_area) 

# join meta data to fish data
SVC_full <- join(fish_data, SVC_meta, by = NULL, type = "left", match = "first")


# Joining Transect Metadata:

# select wanted metadata columns: session, site, date, transect_area, depth
prey_meta <- prey_meta[,c(1,4,5,7,15)] 

# rename columns
prey_meta <- prey_meta %>% rename(prey_depth = prey_depth_m) 
prey_meta <- prey_meta %>% rename(prey_area = prey_tran_area) 

# want to aggregate depth by mean and area by sum: splitting up

# transform depth and area columns from character to numeric
prey_meta <- transform(prey_meta, prey_depth = as.numeric(prey_depth), 
                       prey_area = as.numeric(prey_area)) 

# remove area column from full transect meta
prey_depth <- prey_meta[,c(1:4)] 

# aggregate depth rows by session
prey_depth <- aggregate(.~session+site+prey_date, prey_depth, mean) 

# remove depth column from full transect meta
prey_area <- prey_meta[,c(1:3,5)]

# aggregate area rows by session
prey_area <- aggregate(.~session+site+prey_date, prey_area, sum) 

# join transect depth and area to make full transect metadata 
prey_meta <- join(prey_depth, prey_area, by = NULL, type = "full", 
                  match = "all") 

# join transect metadata to fish and SVC dataframe
SVCprey_full <- join(SVC_full, prey_meta, by = NULL, type = "left", 
                     match = "first") 


# Joining Roving Metadata:

# select wanted metadata columns: session, site, date, transect_area, depth
pred_meta <- pred_meta[,c(1,4,8,17,21)] 

# rename columns
pred_meta <- pred_meta %>% rename(pred_depth = pred_depth_ft)
pred_meta <- pred_meta %>% rename(pred_area = pred_trans_area) 

# want to aggregate depth by mean and area by sum: splitting up

# remove area column
pred_depth <- pred_meta[,c(1:4)] 

# aggregate depth rows by session
pred_depth <- aggregate(.~session+site+pred_date, pred_depth, mean) 

# remove depth column
pred_area <- pred_meta[,c(1:3,5)] 

# aggregate area rows by session
pred_area <- aggregate(.~session+site+pred_date, pred_area, sum) 

# join roving depth and area to create full roving metadata
pred_meta <- join(pred_depth, pred_area, by = NULL, type = "full", 
                  match = "all") 

# join roving metadata to fish, SVC, and transect dataframe
fish_meta <- join(SVCprey_full, pred_meta, by = NULL, type = "left", 
                  match = "first") 


# Joining Vertical Relief Data =================================================

# The following section aggregates vertical relief measures to a mean value per
# survey site, then joins these measures to the fish and survey metadata 
# dataframe. 

# select site and vert relief columns
vert_relief <- vert_relief[,c(1,7)] 

# aggregate to site mean
vert_relief <- aggregate(relief_cm~site, vert_relief, mean) 

# change KL-P30 to KL-30
vert_relief$site[vert_relief$site == "KL-P30"] <- "KL-30"

# join vertical relief measure to each site
fish_meta <- join(fish_meta, vert_relief, by = NULL, type = "left", 
                  match = "first") 


# Joining Species' Trait Data ==================================================

# The following joins adult fish traits used in further analyses to the survey
# metadata and fish dataframe.

# filter for adult lifestage
fish_traits <- filter(traits, lifestage == "adult") 

# select relevant columns: latin names, predator presence, nocturnal, 
# max_length, position, behaviour, colouration, cryptic_behaviour, shape, 
# trophic position
fish_traits <- fish_traits[,c(1:4,7,18,36,38,39,60,61,70,74)] 

# rename columns
fish_traits <- fish_traits %>% rename(colouration = colouration_cat3)
fish_traits <- fish_traits %>% rename(species = common_name)
#fish_traits <- fish_traits %>% rename(shape_ratio = final_ratio)

# join fish trait data to meta and fish dataframe
full_dataframe <- join(fish_meta, fish_traits, by = NULL, type = "full", 
                       match = "all")

# remove un-recorded species
sessions <- unique(full_dataframe$session)
length(sessions)
na_values <- which(is.na(full_dataframe), arr.ind=TRUE)

# replace roving NAs with 0s
full_dataframe$pred_date[is.na(full_dataframe$pred_date)] <- "2014-01-01"
full_dataframe$pred_depth[is.na(full_dataframe$pred_depth)] <- 0
full_dataframe$pred_area[is.na(full_dataframe$pred_area)] <- 0

# remove NA values
full_dataframe <- na.omit(full_dataframe) 


# Density Calculation ==========================================================

# The following calculates fish densities for each of the three survey types 
# (SVC, transect, and roving) for each single observation. It then calculates
# the density differences between each of the survey types for each observation.

# SVC density calculation
full_dataframe$SVC_density <- 
  full_dataframe$SVC_abundance/full_dataframe$SVC_area

# transect survey density calculation
full_dataframe$prey_density <- 
  full_dataframe$prey_abundance/full_dataframe$prey_area 

# roving survey density calculation
full_dataframe$pred_density <- 
  full_dataframe$pred_abundance/full_dataframe$pred_area 

# SVC - transect density difference calculation
full_dataframe$SVC_prey_difference <- 
  full_dataframe$SVC_density - full_dataframe$prey_density

# SVC - roving density difference calculation
full_dataframe$SVC_pred_difference <- 
  full_dataframe$SVC_density - full_dataframe$pred_density 

# transect - roving density difference calculation
full_dataframe$prey_pred_difference <- 
  full_dataframe$prey_density - full_dataframe$pred_density 


# Dataframe Edits ==============================================================

# The following provides some additional edits to the full dataframe to produce
# a final version.

# re-order columns
full_dataframe <- full_dataframe[,c(2,1,22:24,3,4,8,25,10,13,14,21,26:33,9,11,12,
                                    34,15:17,35,18:20,36:38)]

# re-name columns
full_dataframe <- full_dataframe %>% rename(habitat = SVC_habitat) 
full_dataframe <- full_dataframe %>% rename(species_order = order)


# SVC vs. Transect Survey Dataframe ============================================

# The following creates a dataframe specific to observations within SVC and 
# transect surveys.

# select SVC and transect columns
SVCprey <- full_dataframe[,c(1:29,34)] 

# calculate total density for SVC and transect surveys
SVCprey$total_density <- SVCprey$SVC_density+SVCprey$prey_density

# remove rows where total density = 0
SVCprey_data <- SVCprey[SVCprey$total_density !=0,] 

# remove trumpetfish (only species in order)
SVCprey_data <- SVCprey_data[SVCprey_data$species_order !="Syngnathiformes",] 

# remove silversides (only species in order)
SVCprey_data <- SVCprey_data[SVCprey_data$species_order !="Atheriniformes",] 

# remove eyed flounder (only depressiform species)
SVCprey_data <- SVCprey_data[SVCprey_data$species_order !="Pleuronectiformes",]

# remove mackerel scad (only pelagic species)
SVCprey_data <- SVCprey_data[SVCprey_data$species !="mackerel scad",]

# remove eels 
SVCprey_data <- SVCprey_data[SVCprey_data$species_order !="Anguilliformes",]

# remove sessions with un-matched dates between surveys
SVCprey_data <- SVCprey_data[SVCprey_data$session !=178,]
SVCprey_data <- SVCprey_data[SVCprey_data$session !=179,]
SVCprey_data <- SVCprey_data[SVCprey_data$session !=180,]
SVCprey_data <- SVCprey_data[SVCprey_data$session !=268,]


# SVC vs. Roving Survey Dataframe ==============================================

# The following creates a dataframe specific to observations within SVC and 
# roving surveys.

# select SVC and roving columns
SVCpred <- full_dataframe[,c(1:25,30:33,35)] 

# filter for species recorded on predator surveys
SVCpred <- filter(SVCpred, predator_presence == 1) 

# calculate total density for SVC and roving surveys
SVCpred$total_density <- SVCpred$SVC_density+SVCpred$pred_density

# remove rows where total density = 0
SVCpred_data <- SVCpred[SVCpred$total_density !=0,]

# remove trumpetfish (only species in order)
SVCpred_data <- SVCpred_data[SVCpred_data$species_order !="Syngnathiformes",] 

# remove gray snapper (inconsistently reported)
SVCpred_data <- SVCpred_data[SVCpred_data$species !="gray snapper",] 

# remove amberjack (only schooling species)
SVCpred_data <- SVCpred_data[SVCpred_data$species !="amberjack",]

# remove black margate (only compressiform species)
SVCpred_data <- SVCpred_data[SVCpred_data$species !="black margate",]

# remove barracuda (only elongated species)
SVCpred_data <- SVCpred_data[SVCpred_data$species !="barracuda",]

# remove sessions with un-matched dates between surveys
SVCpred_data <- SVCpred_data[SVCpred_data$session !=178,]
SVCpred_data <- SVCpred_data[SVCpred_data$session !=179,]
SVCpred_data <- SVCpred_data[SVCpred_data$session !=180,]

# remove sessions with no roving data
SVCpred_data <- SVCpred_data[SVCpred_data$session !=264,]
SVCpred_data <- SVCpred_data[SVCpred_data$session !=265,]
SVCpred_data <- SVCpred_data[SVCpred_data$session !=266,]
SVCpred_data <- SVCpred_data[SVCpred_data$session !=267,]
SVCpred_data <- SVCpred_data[SVCpred_data$session !=268,]
SVCpred_data <- SVCpred_data[SVCpred_data$session !=269,]
SVCpred_data <- SVCpred_data[SVCpred_data$session !=270,]
SVCpred_data <- SVCpred_data[SVCpred_data$session !=271,]
SVCpred_data <- SVCpred_data[SVCpred_data$session !=272,]
SVCpred_data <- SVCpred_data[SVCpred_data$session !=273,]
SVCpred_data <- SVCpred_data[SVCpred_data$session !=274,]

# remove NA values
SVCpred_data <- na.omit(SVCpred_data) 


# Density Log Transformation ===================================================

# Density differences between the survey types did not meet normality 
# assumptions required for analyses, so the following conducts a log 
# transformation of raw densities before taking the difference to improve 
# normality.

# calculate log SVC density in SVC vs. transect dataframe
log_SVCdensity <- log(SVCprey_data$SVC_density + 0.001) 

# calculate log transect density
log_preydensity <- log(SVCprey_data$prey_density + 0.001) 

# histogram of SVC vs. transect log density differences 
hist(log_SVCdensity-log_preydensity)

# calculate SVC vs. transect log density difference
SVCprey_data$log_difference <- log_SVCdensity-log_preydensity

# calculate log SVC density in SVC vs. roving dataframe
log_SVCdensity2 <- log(SVCpred_data$SVC_density + 0.001)

# calculate log roving density 
log_preddensity <- log(SVCpred_data$pred_density + 0.001)

# histogram of SVC vs. roving log density differences
hist(log_SVCdensity2-log_preddensity)

# calculate SVC vs. roving log density difference
SVCpred_data$log_difference <- log_SVCdensity2-log_preddensity


# Average Depth Calculation ====================================================

# The following calculates average depth values for each session between the two
# survey types in each dataframe (SVC vs. transect and SVC vs. roving). 

# SVC vs. transect dataframe average depth calculation
SVCprey_data$average_depth <- (SVCprey_data$SVC_max_depth + 
                                 SVCprey_data$prey_depth)/2

# SVC vs. roving dataframe average depth calculation
SVCpred_data$average_depth <- (SVCpred_data$SVC_max_depth + 
                                 SVCpred_data$pred_depth)/2

# export SVC vs. transect survey dataframe
write_csv(SVCprey_data, here("./dataframes/SVCprey_dataframe.csv"))

# export SVC vs. roving survey dataframe
write_csv(SVCpred_data, here("./dataframes/SVCpred_dataframe.csv"))
