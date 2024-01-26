# Set-Up =======================================================================

# packages
library(plyr)
library(tidyverse)
library(here)

# data
sites <- read_csv(here("./site_data.csv"))
SVC <- read_csv(here("./clean_data/SVC_data.csv"))
prey <- read_csv(here("./clean_data/prey_metadata.csv"))

# edits
sites <- sites %>% rename(session = Session)
sites <- na.omit(sites)


# SVC Data =====================================================================

# SVC date
SVC_date <- data.frame(SVC$session, SVC$SVC_date)
SVC_date <- unique(SVC_date[,1:2])
SVC_date <- SVC_date %>% rename (session = SVC.session)
sites <- join(sites, SVC_date, by = "session", type = "left")
sites <- sites %>% rename(SVC_date = SVC.SVC_date)

# SVC time
SVC_time <- data.frame(SVC$session, SVC$SVC_sample_start)
SVC_time <- unique(SVC_time[,1:2])
SVC_time <- SVC_time %>% rename (session = SVC.session)
sites <- join(sites, SVC_time, by = "session", type = "left")
sites <- sites %>% rename(SVC_time = SVC.SVC_sample_start)

# SVC depth
SVC_depth <- data.frame(SVC$session, SVC$SVC_max_depth)
SVC_depth <- unique(SVC_depth[,1:2])
SVC_depth <- SVC_depth %>% rename (session = SVC.session)
SVC_depth <- SVC_depth[-c(),]
sites <- join(sites, SVC_depth, by = "session", type = "left")
sites <- sites[-c(86,93,95,97,99,101,116,118),]
sites <- sites %>% rename(SVC_depth = SVC.SVC_max_depth)


# Belt Transect Data ===========================================================

# transect dates
prey_date <- data.frame(prey$session, prey$prey_date)
prey_date <- unique(prey_date[,1:2])
prey_date <- prey_date %>% rename (session = prey.session)
sites <- join(sites, prey_date, by = "session", type = "left")
sites <- sites %>% rename(prey_date = prey.prey_date)

# transect times
prey_time <- data.frame(prey$session, prey$prey_time)
prey_time <- unique(prey_time[,1:2])
prey_time <- prey_time %>% rename (session = prey.session)
sites <- join(sites, prey_time, by = "session", type = "left")
sites <- sites %>% rename(prey_time = prey.prey_sample_start)

# transect depth 



