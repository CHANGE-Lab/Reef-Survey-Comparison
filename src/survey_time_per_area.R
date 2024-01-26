# packages
library(plyr)
library(tidyverse)
library(here)

# data
SVC_metadata <- SVC_data
pred_metadata <- pred_metadata

# new dataframes
SVC_area_time <- data.frame(SVC_metadata$session, SVC_metadata$SVC_habitat, SVC_metadata$SVC_cylinder_area, SVC_metadata$SVC_total_sample_time)
pred_area <- data.frame(pred_metadata$session, pred_metadata$pred_site_area_m2)
pred_time <- data.frame(pred_metadata$session, pred_metadata$pred_total_time_min)
pred_habitat <- data.frame(pred_metadata$session, pred_metadata$pred_habitat)

# select unique sessions (aggregate by mean survey time for pred data)
SVC_area_time_unq <- distinct(SVC_area_time)
pred_area_unq <- distinct(pred_area)
pred_habitat_unq <- distinct(pred_habitat)
pred_time_unq <- aggregate(pred_metadata.pred_total_time_min~pred_metadata.session, pred_time, mean)
pred_area_time_unq <- merge(pred_area_unq, pred_time_unq, by = "pred_metadata.session", all = TRUE)
pred_area_time_full <- merge(pred_area_time_unq, pred_habitat_unq, by = "pred_metadata.session", all = TRUE)

# remove NAs
SVC_area_time_noNA <- na.omit(SVC_area_time_unq)
pred_area_time_noNA <- na.omit(pred_area_time_full)

# divide area by time
SVC_area_time_noNA$m2_per_min <- SVC_area_time_noNA$SVC_metadata.SVC_cylinder_area/SVC_area_time_noNA$SVC_metadata.SVC_total_sample_time
pred_area_time_noNA$m2_per_min <- pred_area_time_noNA$pred_metadata.pred_site_area_m2/pred_area_time_noNA$pred_metadata.pred_total_time_min

# SVC: removed sessions absent in analyses 
SVC_area_time_noNA <- SVC_area_time_noNA[SVC_area_time_noNA$SVC_metadata.session !=178,]
SVC_area_time_noNA <- SVC_area_time_noNA[SVC_area_time_noNA$SVC_metadata.session !=179,]
SVC_area_time_noNA <- SVC_area_time_noNA[SVC_area_time_noNA$SVC_metadata.session !=180,]
SVC_area_time_noNA <- SVC_area_time_noNA[SVC_area_time_noNA$SVC_metadata.session !=268,]

# pred: removed sessions absent in analyses 
pred_area_time_noNA <- pred_area_time_noNA[pred_area_time_noNA$pred_metadata.session !=178,]
pred_area_time_noNA <- pred_area_time_noNA[pred_area_time_noNA$pred_metadata.session !=179,]
pred_area_time_noNA <- pred_area_time_noNA[pred_area_time_noNA$pred_metadata.session !=180,]

# SVC: average and range (full)
mean(SVC_area_time_noNA$m2_per_min)
min(SVC_area_time_noNA$m2_per_min)
max(SVC_area_time_noNA$m2_per_min)

# SVC: average and range (patch reefs)
mean(SVC_area_time_noNA[SVC_area_time_noNA$SVC_metadata.SVC_habitat == "Patch", "m2_per_min"])
min(SVC_area_time_noNA[SVC_area_time_noNA$SVC_metadata.SVC_habitat == "Patch", "m2_per_min"])
max(SVC_area_time_noNA[SVC_area_time_noNA$SVC_metadata.SVC_habitat == "Patch", "m2_per_min"])

# SVC: average and range (continuous reefs)
mean(SVC_area_time_noNA[SVC_area_time_noNA$SVC_metadata.SVC_habitat == "Continuous", "m2_per_min"])
min(SVC_area_time_noNA[SVC_area_time_noNA$SVC_metadata.SVC_habitat == "Continuous", "m2_per_min"])
max(SVC_area_time_noNA[SVC_area_time_noNA$SVC_metadata.SVC_habitat == "Continuous", "m2_per_min"])

# pred: average and range (full)
mean(pred_area_time_noNA$m2_per_min)
min(pred_area_time_noNA$m2_per_min)
max(pred_area_time_noNA$m2_per_min)

# pred: average and range (patch reefs)
mean(pred_area_time_noNA[pred_area_time_noNA$pred_metadata.pred_habitat == "patch", "m2_per_min"])
min(pred_area_time_noNA[pred_area_time_noNA$pred_metadata.pred_habitat == "patch", "m2_per_min"])
max(pred_area_time_noNA[pred_area_time_noNA$pred_metadata.pred_habitat == "patch", "m2_per_min"])

# pred: average and range (continuous reefs)
mean(pred_area_time_noNA[pred_area_time_noNA$pred_metadata.pred_habitat == "continuous", "m2_per_min"])
min(pred_area_time_noNA[pred_area_time_noNA$pred_metadata.pred_habitat == "continuous", "m2_per_min"])
max(pred_area_time_noNA[pred_area_time_noNA$pred_metadata.pred_habitat == "continuous", "m2_per_min"])

# SVC time calculations
mean(SVC_area_time_noNA$SVC_metadata.SVC_total_sample_time)
min(SVC_area_time_noNA$SVC_metadata.SVC_total_sample_time)
max(SVC_area_time_noNA$SVC_metadata.SVC_total_sample_time)

# SVC area calculations
mean(SVC_area_time_noNA$SVC_metadata.SVC_cylinder_area)
min(SVC_area_time_noNA$SVC_metadata.SVC_cylinder_area)
max(SVC_area_time_noNA$SVC_metadata.SVC_cylinder_area)

# pred time calculations
mean(pred_area_time_noNA$pred_metadata.pred_total_time_min)
min(pred_area_time_noNA$pred_metadata.pred_total_time_min)
max(pred_area_time_noNA$pred_metadata.pred_total_time_min)

# pred area calculations 
mean(pred_area_time_noNA$pred_metadata.pred_site_area_m2)
min(pred_area_time_noNA$pred_metadata.pred_site_area_m2)
max(pred_area_time_noNA$pred_metadata.pred_site_area_m2)
