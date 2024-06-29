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
SVCprey_fish_data <- read_csv(here("./dataframes/SVCprey_fish_dataframe.csv"))
SVCpred_fish_data <- read_csv(here("./dataframes/SVCpred_fish_dataframe.csv"))
SVC <- read_csv(here("./clean_data/SVC_data.csv"))
prey_meta <- read_csv(here("./clean_data/prey_metadata.csv"))
pred_meta <- read_csv(here("./clean_data/pred_metadata.csv"))
traits <- read_csv(here("./clean_data/fish_traits.csv"))
vert_relief <- read_csv(here("./clean_data/vertical_relief.csv"))


# Joining Survey Metadata ======================================================

# The following joins metadata associated with each survey type (SVC, transect 
# and roving) to the full fish dataframe. 


# Joining SVC Metadata: 

# create SVC survey time dataframe
SVC_time <- SVC[,c(1,10)]

# calculate average SVC time per session
SVC_time <- aggregate(.~session, SVC_time, mean)

# rename total time column
SVC_time <- SVC_time %>% rename(SVC_time = SVC_total_sample_time) 

# select wanted metadata columns: session, site, date, diver, habitat, 
# cylinder_area, max_depth, octocoral, stony coral, total time
SVC_meta <- SVC[,c(1,3,4,12,16,17,33,34)] 

# aggregate rows by session
SVC_meta <- SVC_meta %>% group_by(session, site, SVC_date, SVC_habitat) %>% 
  summarise_each(funs(mean))

# rename area column
SVC_meta <- SVC_meta %>% rename(SVC_area = SVC_cylinder_area) 

# join meta data to fish data
SVCprey_SVCfull <- join(SVCprey_fish_data, SVC_meta, by = NULL, type = "left", 
                 match = "first")
SVCpred_SVCfull <- join(SVCpred_fish_data, SVC_meta, by = NULL, type = "left", 
                 match = "first")


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
SVCprey_full <- join(SVCprey_SVCfull, prey_meta, by = NULL, type = "left", 
                     match = "first") 


# Joining Roving Metadata:

# create roving survey time dataframe
pred_time <- pred_meta[,c(1,16)]

# calculate average roving time per session
pred_time <- aggregate(.~session, pred_time, mean)

# rename total time column
pred_time <- pred_time %>% rename(pred_time = pred_total_time_min)

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
SVCpred_full <- join(SVCpred_SVCfull, pred_meta, by = NULL, type = "left", 
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
SVCprey_full <- join(SVCprey_full, vert_relief, by = NULL, type = "left", 
                  match = "first") 
SVCpred_full <- join(SVCpred_full, vert_relief, by = NULL, type = "left", 
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

# join fish trait data to meta and fish dataframes
SVCprey_dataframe <- join(SVCprey_full, fish_traits, by = NULL, type = "left", 
                       match = "all")
SVCpred_dataframe <- join(SVCpred_full, fish_traits, by = NULL, type = "left", 
                          match = "all")

# remove un-recorded species in SVC vs. belt dataframe 
SVCprey_na_values <- which(is.na(SVCprey_dataframe), arr.ind=TRUE)
SVCprey_dataframe <- na.omit(SVCprey_dataframe)

# identify SVC vs. roving NA values
SVCpred_na_values <- which(is.na(SVCpred_dataframe), arr.ind=TRUE)

# replace roving NAs with 0s
SVCpred_dataframe$pred_date[is.na(SVCpred_dataframe$pred_date)] <- "2014-01-01"
SVCpred_dataframe$pred_depth[is.na(SVCpred_dataframe$pred_depth)] <- 0
SVCpred_dataframe$pred_area[is.na(SVCpred_dataframe$pred_area)] <- 0

# remove unrecorded species from SVC vs. roving dataframe
SVCpred_dataframe <- na.omit(SVCpred_dataframe) 


# Density Calculation ==========================================================

# The following calculates fish densities for each of the three survey types 
# (SVC, transect, and roving) for each single observation. It then calculates
# the density differences between each of the survey types for each observation.

# SVC density calculation (belt dataframe)
SVCprey_dataframe$SVC_density <- 
  SVCprey_dataframe$SVC_prey_abundance/SVCprey_dataframe$SVC_area

# transect survey density calculation
SVCprey_dataframe$prey_density <- 
  SVCprey_dataframe$prey_abundance/SVCprey_dataframe$prey_area 

# SVC density calculation (roving dataframe)
SVCpred_dataframe$SVC_density <- 
  SVCpred_dataframe$SVC_pred_abundance/SVCpred_dataframe$SVC_area

# roving survey density calculation
SVCpred_dataframe$pred_density <- 
  SVCpred_dataframe$pred_abundance/SVCpred_dataframe$pred_area 

# SVC - transect density difference calculation
SVCprey_dataframe$SVC_prey_difference <- 
  SVCprey_dataframe$SVC_density - SVCprey_dataframe$prey_density

# SVC - roving density difference calculation
SVCpred_dataframe$SVC_pred_difference <- 
  SVCpred_dataframe$SVC_density - SVCpred_dataframe$pred_density

# rename dataframes 
SVCprey_full_dataframe <- SVCprey_dataframe
SVCpred_full_dataframe <- SVCpred_dataframe

# re-name SVC vs. belt columns
SVCprey_full_dataframe <- SVCprey_full_dataframe %>% 
  rename(habitat = SVC_habitat) 
SVCprey_full_dataframe <- SVCprey_full_dataframe %>% 
  rename(species_order = order)

# re-name SVC vs. roving columns
SVCpred_full_dataframe <- SVCpred_full_dataframe %>% 
  rename(habitat = SVC_habitat) 
SVCpred_full_dataframe <- SVCpred_full_dataframe %>% 
  rename(species_order = order)


# SVC vs. Transect Survey Dataframe Edits ======================================

# The following creates a dataframe specific to observations within SVC and 
# transect surveys.

# calculate total density for SVC and transect surveys
SVCprey_full_dataframe$total_density <- SVCprey_full_dataframe$SVC_density + 
  SVCprey_full_dataframe$prey_density

# remove rows where total density = 0
SVCprey_full_dataframe <- 
  SVCprey_full_dataframe[SVCprey_full_dataframe$total_density !=0,] 

# remove trumpetfish (only species in order)
SVCprey_full_dataframe <- 
  SVCprey_full_dataframe[SVCprey_full_dataframe$species_order 
                         !="Syngnathiformes",] 

# remove silversides (only species in order)
SVCprey_full_dataframe <- 
  SVCprey_full_dataframe[SVCprey_full_dataframe$species_order 
                         !="Atheriniformes",] 

# remove eyed flounder (only depressiform species)
SVCprey_full_dataframe <- 
  SVCprey_full_dataframe[SVCprey_full_dataframe$species_order 
                         !="Pleuronectiformes",]

# remove mackerel scad (only pelagic species)
SVCprey_full_dataframe <- 
  SVCprey_full_dataframe[SVCprey_full_dataframe$species 
                         !="mackerel scad",]

# remove sessions with un-matched dates between surveys
SVCprey_full_dataframe <- 
  SVCprey_full_dataframe[SVCprey_full_dataframe$session !=178,]
SVCprey_full_dataframe <- 
  SVCprey_full_dataframe[SVCprey_full_dataframe$session !=179,]
SVCprey_full_dataframe <- 
  SVCprey_full_dataframe[SVCprey_full_dataframe$session !=180,]
SVCprey_full_dataframe <- 
  SVCprey_full_dataframe[SVCprey_full_dataframe$session !=268,]

# calculate area difference between surveys 
SVCprey_full_dataframe$SVCprey_area_dif <- SVCprey_full_dataframe$SVC_area - 
  SVCprey_full_dataframe$prey_area


# SVC vs. Roving Survey Dataframe Edits ========================================

# The following creates a dataframe specific to observations within SVC and 
# roving surveys.

# filter for species recorded on predator surveys
SVCpred_full_dataframe <- filter(SVCpred_full_dataframe, predator_presence == 1) 

# calculate total density for SVC and roving surveys
SVCpred_full_dataframe$total_density <- SVCpred_full_dataframe$SVC_density + 
  SVCpred_full_dataframe$pred_density

# remove rows where total density = 0
SVCpred_full_dataframe <- 
  SVCpred_full_dataframe[SVCpred_full_dataframe$total_density !=0,]

# remove trumpetfish (only species in order)
SVCpred_full_dataframe <- 
  SVCpred_full_dataframe[SVCpred_full_dataframe$species_order 
                         !="Syngnathiformes",] 

# remove gray snapper (inconsistently reported)
SVCpred_full_dataframe <- 
  SVCpred_full_dataframe[SVCpred_full_dataframe$species !="gray snapper",] 

# remove amberjack (only schooling species)
SVCpred_full_dataframe <- 
  SVCpred_full_dataframe[SVCpred_full_dataframe$species !="amberjack",]

# remove black margate (only compressiform species)
SVCpred_full_dataframe <- 
  SVCpred_full_dataframe[SVCpred_full_dataframe$species !="black margate",]

# remove barracuda (only elongated species)
SVCpred_full_dataframe <- 
  SVCpred_full_dataframe[SVCpred_full_dataframe$species !="barracuda",]

# remove sessions with un-matched dates between surveys
SVCpred_full_dataframe <- 
  SVCpred_full_dataframe[SVCpred_full_dataframe$session !=178,]
SVCpred_full_dataframe <- 
  SVCpred_full_dataframe[SVCpred_full_dataframe$session !=179,]
SVCpred_full_dataframe <- 
  SVCpred_full_dataframe[SVCpred_full_dataframe$session !=180,]

# remove sessions with no roving data
SVCpred_full_dataframe <- 
  SVCpred_full_dataframe[SVCpred_full_dataframe$session !=264,]
SVCpred_full_dataframe <- 
  SVCpred_full_dataframe[SVCpred_full_dataframe$session !=265,]
SVCpred_full_dataframe <- 
  SVCpred_full_dataframe[SVCpred_full_dataframe$session !=266,]
SVCpred_full_dataframe <- 
  SVCpred_full_dataframe[SVCpred_full_dataframe$session !=267,]
SVCpred_full_dataframe <- 
  SVCpred_full_dataframe[SVCpred_full_dataframe$session !=268,]
SVCpred_full_dataframe <- 
  SVCpred_full_dataframe[SVCpred_full_dataframe$session !=269,]
SVCpred_full_dataframe <- 
  SVCpred_full_dataframe[SVCpred_full_dataframe$session !=270,]
SVCpred_full_dataframe <- 
  SVCpred_full_dataframe[SVCpred_full_dataframe$session !=271,]
SVCpred_full_dataframe <- 
  SVCpred_full_dataframe[SVCpred_full_dataframe$session !=272,]
SVCpred_full_dataframe <- 
  SVCpred_full_dataframe[SVCpred_full_dataframe$session !=273,]
SVCpred_full_dataframe <- 
  SVCpred_full_dataframe[SVCpred_full_dataframe$session !=274,]

# remove NA values
SVCpred_full_dataframe <- na.omit(SVCpred_full_dataframe) 

# calculate area difference
SVCpred_full_dataframe$SVCpred_area_dif <- 
  SVCpred_full_dataframe$SVC_area - SVCpred_full_dataframe$pred_area


# Density Log Transformation ===================================================

# Density differences between the survey types did not meet normality 
# assumptions required for analyses, so the following conducts a log 
# transformation of raw densities before taking the difference to improve 
# normality.

# calculate log SVC density in SVC vs. transect dataframe
log_SVCpreydensity <- log(SVCprey_full_dataframe$SVC_density + 0.001) 

# calculate log transect density
log_preydensity <- log(SVCprey_full_dataframe$prey_density + 0.001) 

# histogram of SVC vs. transect log density differences 
hist(log_SVCpreydensity-log_preydensity)

# calculate SVC vs. transect log density difference
SVCprey_full_dataframe$log_difference <- log_SVCpreydensity - log_preydensity

# calculate log SVC density in SVC vs. roving dataframe
log_SVCpreddensity <- log(SVCpred_full_dataframe$SVC_density + 0.001)

# calculate log roving density 
log_preddensity <- log(SVCpred_full_dataframe$pred_density + 0.001)

# histogram of SVC vs. roving log density differences
hist(log_SVCpreddensity-log_preddensity)

# calculate SVC vs. roving log density difference
SVCpred_full_dataframe$log_difference <- log_SVCpreddensity - log_preddensity


# Average Depth Calculation ====================================================

# The following calculates average depth values for each session between the two
# survey types in each dataframe (SVC vs. transect and SVC vs. roving). 

# SVC vs. transect dataframe average depth calculation
SVCprey_full_dataframe$average_depth <- (SVCprey_full_dataframe$SVC_max_depth + 
                                 SVCprey_full_dataframe$prey_depth)/2

# SVC vs. roving dataframe average depth calculation
SVCpred_full_dataframe$average_depth <- (SVCpred_full_dataframe$SVC_max_depth + 
                                 SVCpred_full_dataframe$pred_depth)/2


# Re-Order Dataframe Columns ===================================================

# The following re-orders the columns of each dataframe.

# re-order SVC vs. belt columns
SVCprey_full_dataframe <- SVCprey_full_dataframe[,c(2,1,18:20,3,4,7,9,12,13,17,
                                                    22:29,8,10,11,30,14:16,31,
                                                    32,33,34,35,36,5,6)]

# re-order SVC vs. roving columns 
SVCpred_full_dataframe <- SVCpred_full_dataframe[,c(2,1,18:20,3,4,7,21,9,12,13,
                                                    17,22:29,8,10,11,30,14:16,
                                                    31,32,33,34,35,36,5,6)]


# Export Full Dataframes =======================================================

# export SVC vs. transect survey dataframe with anguilliform species included 
write_csv(SVCprey_full_dataframe, 
          here("./dataframes/SVCprey_dataframe_anguilliformes.csv"))

# export SVC vs. roving survey dataframe
write_csv(SVCpred_full_dataframe, here("./dataframes/SVCpred_dataframe.csv"))

# remove anguilliform species from SVC vs. transect survey dataframe
SVCprey_data_na <- 
  SVCprey_full_dataframe[SVCprey_full_dataframe$species_order 
                         !="Anguilliformes",]

# export SVC vs. transect survey dataframe with anguilliform species removed 
write_csv(SVCprey_data_na, here("./dataframes/SVCprey_dataframe.csv"))

# join SVC and roving survey times to SVC vs. roving dataframe 
SVCpred_full_dataframe_ta <- join(SVCpred_full_dataframe, SVC_time, by = NULL, 
                                  type = "left", match = "first")
SVCpred_full_dataframe_ta <- join(SVCpred_full_dataframe_ta, pred_time, 
                                  by = NULL, type = "left", match = "first")

# calculate time difference
SVCpred_full_dataframe_ta$SVCpred_time_dif <- 
  SVCpred_full_dataframe_ta$SVC_time - SVCpred_full_dataframe_ta$pred_time

# calculate survey speeds
SVCpred_full_dataframe_ta$SVC_speed <- 
  SVCpred_full_dataframe_ta$SVC_area / SVCpred_full_dataframe_ta$SVC_time
SVCpred_full_dataframe_ta$pred_speed <- 
  SVCpred_full_dataframe_ta$pred_area / SVCpred_full_dataframe_ta$pred_time

# calculate speed differences 
SVCpred_full_dataframe_ta$SVCpred_speed_dif <- 
  SVCpred_full_dataframe_ta$SVC_speed - SVCpred_full_dataframe_ta$pred_speed

# remove rows from SVCpred dataframe where survey time was not recorded 
# (remove NAs)
SVCpred_full_dataframe_ta <- na.omit(SVCpred_full_dataframe_ta)

# export SVC vs. roving survey dataframe for survey area & duration comparison
write_csv(SVCpred_full_dataframe_ta, 
          here("./dataframes/SVCpred_dataframe_time_area.csv"))