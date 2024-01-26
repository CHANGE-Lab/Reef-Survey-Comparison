########## 
##########
# This file calculates effect sizes for the differences in density observed 
# between SVC, transect, and roving surveys. 
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2023-05-16
##########
##########


# Set-Up =======================================================================

# packages
library(tidyverse)
library(here)

# data
SVCprey <- read_csv(here("./dataframes/SVCprey_dataframe.csv"))
SVCpred <- read_csv(here("./dataframes/SVCpred_dataframe.csv"))


# Log Density Calculations =====================================================

SVCprey$log_SVCdensity <- log(SVCprey$SVC_density + 0.001) 
hist(SVCprey$log_SVCdensity)
hist(SVCprey$SVC_density) # untransformed densities 
SVCprey$log_preydensity <- log(SVCprey$prey_density + 0.001) 
hist(SVCprey$log_preydensity)
hist(SVCprey$prey_density) # untransformed densities 
SVCpred$log_SVCdensity <- log(SVCpred$SVC_density + 0.001) 
hist(SVCpred$log_SVCdensity)
SVCpred$log_preddensity <- log(SVCpred$pred_density + 0.001) 
hist(SVCpred$log_preddensity)


# Effect Size Calculations Test ================================================

SVCprey %>% filter(shape == "elongated", size_bin < 5) %>% summarize(Avg = mean(SVC_density)) %>% as.data.frame()
SVCprey %>% filter(shape == "elongated", size_bin < 5) %>% summarize(Avg = mean(prey_density)) %>% as.data.frame()
(0.007240227-0.1256763)/0.007240227

# SOLITARY:

# % Difference
SVC_sol <- SVCprey %>% filter(behavior == "solitary") %>% summarize(Avg = mean(log_SVCdensity)) %>% as.data.frame()
prey_sol <- SVCprey %>% filter(behavior == "solitary") %>% summarize(Avg = mean(log_preydensity)) %>% as.data.frame()
((SVC_sol)-(prey_sol))/(SVC_sol)
# or calculate as median
SVC_sol_m <- SVCprey %>% filter(behavior == "solitary") %>% summarize(Avg = median(log_SVCdensity)) %>% as.matrix()
prey_sol_m <- SVCprey %>% filter(behavior == "solitary") %>% summarize(Avg = median(log_preydensity)) %>% as.matrix()
((SVC_sol_m)-(prey_sol_m))/(SVC_sol_m)

# Uncertainty
SVC_sol_sd <- SVCprey %>% filter(behavior == "solitary") %>% summarize(Sd = sd(log_SVCdensity)) %>% as.data.frame()
SVC_sol_plussd <- SVC_sol + SVC_sol_sd
SVC_sol_minussd <- SVC_sol - SVC_sol_sd
prey_sol_sd <- SVCprey %>% filter(behavior == "solitary") %>% summarize(Sd = sd(log_preydensity)) %>% as.data.frame()
prey_sol_plussd <- prey_sol + prey_sol_sd
prey_sol_minussd <- prey_sol - prey_sol_sd
# greatest difference:
((SVC_sol_minussd )-(prey_sol_plussd))/(SVC_sol_minussd)
# smallest difference:
((SVC_sol_plussd )-(prey_sol_minussd))/(SVC_sol_plussd)
# or as quartiles 
SVCprey_sol <- SVCprey %>% filter(behavior == "solitary")
quantile(SVCprey_sol$log_SVCdensity)
quantile(SVCprey_sol$log_preydensity)
# greatest difference:
((-6.90775528)-(-3.6496587))/(-6.90775528)
# smallest difference:
((-4.89336971)-(-5.1159958))/(-4.89336971)

# SVC CI
SVC_sol_sd <- SVCprey %>% filter(behavior == "solitary") %>% summarize(Sd = sd(SVC_density)) %>% as.data.frame()
SVC_sol_se <- SVC_sol_sd/sqrt(7458)
SVC_sol_margin <- qt(0.975, df=7457)*SVC_sol_se
SVC_sol-SVC_sol_margin 
SVC_sol+SVC_sol_margin 

# Transect CI 
prey_sol_sd <- SVCprey %>% filter(behavior == "solitary") %>% summarize(Sd = sd(prey_density)) %>% as.data.frame()
prey_sol_se <- prey_sol_sd/sqrt(7458)
prey_sol_margin <- qt(0.975, df=7457)*prey_sol_se
prey_sol-prey_sol_margin 
prey_sol+prey_sol_margin 

# Uncertainty 
(0.01238211-0.03184159)/0.01238211 
(0.01032765-0.03576315)/0.01032765 
-2.462855-(-1.976902)
-1.976902-(-1.57158)

# UNTRANSFORMED: 

# % Difference (untransformed)
SVC_sol_ut <- SVCprey %>% filter(behavior == "solitary") %>% summarize(Avg = mean(SVC_density)) %>% as.data.frame()
prey_sol_ut <- SVCprey %>% filter(behavior == "solitary") %>% summarize(Avg = mean(prey_density)) %>% as.data.frame()
((SVC_sol_ut)-(prey_sol_ut))/(SVC_sol_ut)
# or calculate as median
SVC_sol_m_ut <- SVCprey %>% filter(behavior == "solitary") %>% summarize(Avg = median(SVC_density)) %>% as.matrix()
prey_sol_m_ut <- SVCprey %>% filter(behavior == "solitary") %>% summarize(Avg = median(prey_density)) %>% as.matrix()
((SVC_sol_m_ut)-(prey_sol_m_ut))/(SVC_sol_m_ut)

# Uncertainty (untransformed)
SVC_sol_sd_ut <- SVCprey %>% filter(behavior == "solitary") %>% summarize(Sd = sd(SVC_density)) %>% as.data.frame()
SVC_sol_plussd_ut <- SVC_sol_ut + SVC_sol_sd_ut
SVC_sol_minussd_ut <- SVC_sol_ut - SVC_sol_sd_ut
prey_sol_sd_ut <- SVCprey %>% filter(behavior == "solitary") %>% summarize(Sd = sd(prey_density)) %>% as.data.frame()
prey_sol_plussd_ut <- prey_sol_ut + prey_sol_sd_ut
prey_sol_minussd_ut <- prey_sol_ut - prey_sol_sd_ut
# greatest difference:
((SVC_sol_minussd_ut)-(prey_sol_plussd_ut))/(SVC_sol_minussd_ut)
# smallest difference:
((SVC_sol_plussd_ut)-(prey_sol_minussd_ut))/(SVC_sol_plussd_ut)
# or as quartiles 
SVCprey_sol <- SVCprey %>% filter(behavior == "solitary")
quantile(SVCprey_sol$SVC_density)
quantile(SVCprey_sol$prey_density)
# greatest difference:
((-6.90775528)-(-3.6496587))/(-6.90775528)
# smallest difference:
((-4.89336971)-(-5.1159958))/(-4.89336971)

# Mean +/- SD:
# Transformed: 20.60% (-41.46% to 58.11%)
# Untransformed: 197.69% (192.88% to 454.53%); unsure if this range is right as SVC range is contained within prey range 

# Mean +/- Quartiles:
# Transformed: 20.60% (-4.55% to 47.17%)
# Untransformed: 197.69%; lower quartile for SVC is 0. How to calculate the range with that? Would lower bound just be 0? 

# Median +/- Quartiles:
# Transformed: 34.71% (-4.55% to 47.17%)
# Untransformed: not possible as median SVC density is 0


# SCHOOLING: 

# % Difference
SVC_school <- SVCprey %>% filter(behavior == "schooling") %>% summarize(Avg = mean(log_SVCdensity)) %>% as.data.frame()
prey_school <- SVCprey %>% filter(behavior == "schooling") %>% summarize(Avg = mean(log_preydensity)) %>% as.data.frame()
((SVC_school)-(prey_school))/(SVC_school)

# Uncertainty
SVC_school_sd <- SVCprey %>% filter(behavior == "schooling") %>% summarize(Sd = sd(log_SVCdensity)) %>% as.data.frame()
SVC_school_plussd <- SVC_school + SVC_school_sd
SVC_school_minussd <- SVC_school - SVC_school_sd
prey_school_sd <- SVCprey %>% filter(behavior == "schooling") %>% summarize(Sd = sd(log_preydensity)) %>% as.data.frame()
prey_school_plussd <- prey_school + prey_school_sd
prey_school_minussd <- prey_school - prey_school_sd
# greatest difference:
((SVC_school_minussd )-(prey_school_plussd))/(SVC_school_minussd)
# smallest difference:
((SVC_school_plussd )-(prey_school_minussd))/(SVC_school_plussd)


# CAMOUFLAGE:

# % Difference
SVC_cam <- SVCprey %>% filter(colouration == "camouflage") %>% summarize(Avg = mean(log_SVCdensity)) %>% as.data.frame()
prey_cam <- SVCprey %>% filter(colouration == "camouflage") %>% summarize(Avg = mean(log_preydensity)) %>% as.data.frame()
((SVC_cam)-(prey_cam))/(SVC_cam)

# Uncertainty
SVC_cam_sd <- SVCprey %>% filter(colouration == "camouflage") %>% summarize(Sd = sd(log_SVCdensity)) %>% as.data.frame()
SVC_cam_plussd <- SVC_cam + SVC_cam_sd
SVC_cam_minussd <- SVC_cam - SVC_cam_sd
prey_cam_sd <- SVCprey %>% filter(colouration == "camouflage") %>% summarize(Sd = sd(log_preydensity)) %>% as.data.frame()
prey_cam_plussd <- prey_cam + prey_cam_sd
prey_cam_minussd <- prey_cam - prey_cam_sd
# greatest difference:
((SVC_cam_minussd )-(prey_cam_plussd))/(SVC_cam_minussd)
# smallest difference:
((SVC_cam_plussd )-(prey_cam_minussd))/(SVC_cam_plussd)


# NEUTRAL:

# % Difference
SVC_neut <- SVCprey %>% filter(colouration == "neutral") %>% summarize(Avg = mean(log_SVCdensity)) %>% as.data.frame()
prey_neut <- SVCprey %>% filter(colouration == "neutral") %>% summarize(Avg = mean(log_preydensity)) %>% as.data.frame()
((SVC_neut)-(prey_neut))/(SVC_neut)

# Uncertainty
SVC_neut_sd <- SVCprey %>% filter(colouration == "neutral") %>% summarize(Sd = sd(log_SVCdensity)) %>% as.data.frame()
SVC_neut_plussd <- SVC_neut + SVC_neut_sd
SVC_neut_minussd <- SVC_neut - SVC_neut_sd
prey_neut_sd <- SVCprey %>% filter(colouration == "neutral") %>% summarize(Sd = sd(log_preydensity)) %>% as.data.frame()
prey_neut_plussd <- prey_neut + prey_neut_sd
prey_neut_minussd <- prey_neut - prey_neut_sd
# greatest difference:
((SVC_neut_minussd )-(prey_neut_plussd))/(SVC_neut_minussd)
# smallest difference:
((SVC_neut_plussd )-(prey_neut_minussd))/(SVC_neut_plussd)


# SILVERING:

# % Difference
SVC_silver <- SVCprey %>% filter(colouration == "silvering") %>% summarize(Avg = mean(log_SVCdensity)) %>% as.data.frame()
prey_silver <- SVCprey %>% filter(colouration == "silvering") %>% summarize(Avg = mean(log_preydensity)) %>% as.data.frame()
((SVC_silver)-(prey_silver))/(SVC_silver)

# Uncertainty
SVC_silver_sd <- SVCprey %>% filter(colouration == "silvering") %>% summarize(Sd = sd(log_SVCdensity)) %>% as.data.frame()
SVC_silver_plussd <- SVC_silver + SVC_silver_sd
SVC_silver_minussd <- SVC_silver - SVC_silver_sd
prey_silver_sd <- SVCprey %>% filter(colouration == "silvering") %>% summarize(Sd = sd(log_preydensity)) %>% as.data.frame()
prey_silver_plussd <- prey_silver + prey_silver_sd
prey_silver_minussd <- prey_silver - prey_silver_sd
# greatest difference:
((SVC_silver_minussd )-(prey_silver_plussd))/(SVC_silver_minussd)
# smallest difference:
((SVC_silver_plussd )-(prey_silver_minussd))/(SVC_silver_plussd)


# COLOURFUL:

# % Difference
SVC_color <- SVCprey %>% filter(colouration == "colourful") %>% summarize(Avg = mean(log_SVCdensity)) %>% as.data.frame()
prey_color <- SVCprey %>% filter(colouration == "colourful") %>% summarize(Avg = mean(log_preydensity)) %>% as.data.frame()
((SVC_color)-(prey_color))/(SVC_color)

# Uncertainty
SVC_color_sd <- SVCprey %>% filter(colouration == "colourful") %>% summarize(Sd = sd(log_SVCdensity)) %>% as.data.frame()
SVC_color_plussd <- SVC_color + SVC_color_sd
SVC_color_minussd <- SVC_color - SVC_color_sd
prey_color_sd <- SVCprey %>% filter(colouration == "colourful") %>% summarize(Sd = sd(log_preydensity)) %>% as.data.frame()
prey_color_plussd <- prey_color + prey_color_sd
prey_color_minussd <- prey_color - prey_color_sd
# greatest difference:
((SVC_color_minussd )-(prey_color_plussd))/(SVC_color_minussd)
# smallest difference:
((SVC_color_plussd )-(prey_color_minussd))/(SVC_color_plussd)


# COMPRESSIFORM:

# % Difference
SVC_comp <- SVCprey %>% filter(shape == "compressiform") %>% summarize(Avg = mean(log_SVCdensity)) %>% as.data.frame()
prey_comp <- SVCprey %>% filter(shape == "compressiform") %>% summarize(Avg = mean(log_preydensity)) %>% as.data.frame()
((SVC_comp)-(prey_comp))/(SVC_comp)

# Uncertainty
SVC_comp_sd <- SVCprey %>% filter(shape == "compressiform") %>% summarize(Sd = sd(log_SVCdensity)) %>% as.data.frame()
SVC_comp_plussd <- SVC_comp + SVC_comp_sd
SVC_comp_minussd <- SVC_comp - SVC_comp_sd
prey_comp_sd <- SVCprey %>% filter(shape == "compressiform") %>% summarize(Sd = sd(log_preydensity)) %>% as.data.frame()
prey_comp_plussd <- prey_comp + prey_comp_sd
prey_comp_minussd <- prey_comp - prey_comp_sd
# greatest difference:
((SVC_comp_minussd )-(prey_comp_plussd))/(SVC_comp_minussd)
# smallest difference:
((SVC_comp_plussd )-(prey_comp_minussd))/(SVC_comp_plussd)


# ELONGATED:

# % Difference
SVC_elong <- SVCprey %>% filter(shape == "elongated") %>% summarize(Avg = mean(log_SVCdensity)) %>% as.data.frame()
prey_elong <- SVCprey %>% filter(shape == "elongated") %>% summarize(Avg = mean(log_preydensity)) %>% as.data.frame()
((SVC_elong)-(prey_elong))/(SVC_elong)

# Uncertainty
SVC_elong_sd <- SVCprey %>% filter(shape == "elongated") %>% summarize(Sd = sd(log_SVCdensity)) %>% as.data.frame()
SVC_elong_plussd <- SVC_elong + SVC_elong_sd
SVC_elong_minussd <- SVC_elong - SVC_elong_sd
prey_elong_sd <- SVCprey %>% filter(shape == "elongated") %>% summarize(Sd = sd(log_preydensity)) %>% as.data.frame()
prey_elong_plussd <- prey_elong + prey_elong_sd
prey_elong_minussd <- prey_elong - prey_elong_sd
# greatest difference:
((SVC_elong_minussd )-(prey_elong_plussd))/(SVC_elong_minussd)
# smallest difference:
((SVC_elong_plussd )-(prey_elong_minussd))/(SVC_elong_plussd)


# GLOBIFORM:

# % Difference
SVC_glob <- SVCprey %>% filter(shape == "globiform") %>% summarize(Avg = mean(log_SVCdensity)) %>% as.data.frame()
prey_glob <- SVCprey %>% filter(shape == "globiform") %>% summarize(Avg = mean(log_preydensity)) %>% as.data.frame()
((SVC_glob)-(prey_glob))/(SVC_glob)

# Uncertainty
SVC_glob_sd <- SVCprey %>% filter(shape == "globiform") %>% summarize(Sd = sd(log_SVCdensity)) %>% as.data.frame()
SVC_glob_plussd <- SVC_glob + SVC_glob_sd
SVC_glob_minussd <- SVC_glob - SVC_glob_sd
prey_glob_sd <- SVCprey %>% filter(shape == "globiform") %>% summarize(Sd = sd(log_preydensity)) %>% as.data.frame()
prey_glob_plussd <- prey_glob + prey_glob_sd
prey_glob_minussd <- prey_glob - prey_glob_sd
# greatest difference:
((SVC_glob_minussd)-(prey_glob_plussd))/(SVC_glob_minussd)
# smallest difference:
((SVC_glob_plussd)-(prey_glob_minussd))/(SVC_glob_plussd)


# FUSIFORM:

# % Difference
SVC_fus <- SVCprey %>% filter(shape == "fusiform") %>% summarize(Avg = mean(log_SVCdensity)) %>% as.data.frame()
prey_fus <- SVCprey %>% filter(shape == "fusiform") %>% summarize(Avg = mean(log_preydensity)) %>% as.data.frame()
((SVC_fus)-(prey_fus))/(SVC_fus)

# Uncertainty
SVC_fus_sd <- SVCprey %>% filter(shape == "fusiform") %>% summarize(Sd = sd(log_SVCdensity)) %>% as.data.frame()
SVC_fus_plussd <- SVC_fus + SVC_fus_sd
SVC_fus_minussd <- SVC_fus - SVC_fus_sd
prey_fus_sd <- SVCprey %>% filter(shape == "fusiform") %>% summarize(Sd = sd(log_preydensity)) %>% as.data.frame()
prey_fus_plussd <- prey_fus + prey_fus_sd
prey_fus_minussd <- prey_fus - prey_fus_sd
# greatest difference:
((SVC_fus_minussd)-(prey_fus_plussd))/(SVC_fus_minussd)
# smallest difference:
((SVC_fus_plussd)-(prey_fus_minussd))/(SVC_fus_plussd)


# LARGE, FUSIFORM, SILVER, PELAGIC:

# % Difference
SVC_lfsp <- SVCprey %>% filter(colouration == "silvering", position == "pelagic", shape == "fusiform", size_bin > 3) %>% summarize(Avg = mean(log_SVCdensity)) %>% as.data.frame()
prey_lfsp <- SVCprey %>% filter(shape == "elongated", size_bin < 5) %>% summarize(Avg = mean(log_preydensity)) %>% as.data.frame()
((SVC_lfsp)-(prey_lfsp))/(SVC_lfsp)

# Uncertainty
SVC_lfsp_sd <- SVCprey %>% filter(colouration == "silvering", position == "pelagic", shape == "fusiform", size_bin > 3) %>% summarize(Sd = sd(log_SVCdensity)) %>% as.data.frame()
SVC_lfsp_plussd <- SVC_lfsp + SVC_lfsp_sd
SVC_lfsp_minussd <- SVC_lfsp - SVC_lfsp_sd
prey_lfsp_sd <- SVCprey %>% filter(colouration == "silvering", position == "pelagic", shape == "fusiform", size_bin > 3) %>% summarize(Sd = sd(log_preydensity)) %>% as.data.frame()
prey_lfsp_plussd <- prey_lfsp + prey_lfsp_sd
prey_lfsp_minussd <- prey_lfsp - prey_lfsp_sd
# greatest difference:
((SVC_lfsp_minussd)-(prey_lfsp_plussd))/(SVC_lfsp_minussd)
# smallest difference:
((SVC_lfsp_plussd)-(prey_lfsp_minussd))/(SVC_lfsp_plussd)

# large, fusiform, camouflage or neutral, demersal, 
itary
SVCprey %>% filter(colouration == "camouflage" | colouration == "neutral", position == "demersal", shape == "fusiform", behavior == "solitary", size_bin > 3) %>% summarize(Avg = mean(SVC_density)) %>% as.data.frame()
SVCprey %>% filter(colouration == "camouflage" | colouration == "neutral", position == "demersal", shape == "fusiform", behavior == "solitary", size_bin > 3) %>% summarize(Avg = mean(prey_density)) %>% as.data.frame()
(0.005143097-0.009840079)/0.005143097

# large, compressiform or fusiform, colorful, shoaling or caming
SVCprey %>% filter(colouration == "colourful", shape == "fusiform" | shape == "compressiform", behavior == "shoaling" | behavior == "caming", size_bin > 3) %>% summarize(Avg = mean(SVC_density)) %>% as.data.frame()
SVCprey %>% filter(colouration == "colourful", shape == "fusiform" | shape == "compressiform", behavior == "shoaling" | behavior == "caming", size_bin > 3) %>% summarize(Avg = mean(prey_density)) %>% as.data.frame()
(0.03383452-0.03498727)/0.03383452

# small, compressiform, colorful
SVCprey %>% filter(colouration == "colourful", shape == "compressiform", size_bin < 4) %>% summarize(Avg = mean(SVC_density)) %>% as.data.frame()
SVCprey %>% filter(colouration == "colourful", shape == "compressiform", size_bin < 4) %>% summarize(Avg = mean(prey_density)) %>% as.data.frame()
(0.03177051-0.0557155)/0.03177051

# small, compressiform or fusiform, colorful, solitary or shoaling
SVCprey %>% filter(colouration == "colourful", shape == "fusiform" | shape == "compressiform", behavior == "shoaling" | behavior == "solitary", size_bin < 4) %>% summarize(Avg = mean(SVC_density)) %>% as.data.frame()
SVCprey %>% filter(colouration == "colourful", shape == "fusiform" | shape == "compressiform", behavior == "shoaling" | behavior == "solitary", size_bin < 4) %>% summarize(Avg = mean(prey_density)) %>% as.data.frame()
(0.03184459-0.07068214)/0.03184459

# small, compressiform, colorful, solitary
SVCprey %>% filter(colouration == "colourful", shape == "compressiform", behavior == "solitary", size_bin < 4) %>% summarize(Avg = mean(SVC_density)) %>% as.data.frame()
SVCprey %>% filter(colouration == "colourful", shape == "compressiform", behavior == "solitary", size_bin < 4) %>% summarize(Avg = mean(prey_density)) %>% as.data.frame()
(0.03048319-0.0613942)/0.03048319

# small, elongated, solitary, cryptic
SVCprey %>% filter(shape == "elongated", behavior == "solitary", cryptic_behaviour == 1, size_bin < 4) %>% summarize(Avg = mean(SVC_density)) %>% as.data.frame()
SVCprey %>% filter(shape == "elongated", behavior == "solitary", cryptic_behaviour == 1, size_bin < 4) %>% summarize(Avg = mean(prey_density)) %>% as.data.frame()
(0.000768871-0.05426671)/0.000768871

# small, cryptic, demersal or benthic, solitary, fusiform or elongated, camouflage
SVCprey %>% filter(colouration == "camouflage", shape == "elongated" | shape == "fusiform", behavior == "solitary", cryptic_behaviour == 1, position == "demersal" | position == "benthic", size_bin < 4) %>% summarize(Avg = mean(SVC_density)) %>% as.data.frame()
SVCprey %>% filter(colouration == "camouflage", shape == "elongated" | shape == "fusiform", behavior == "solitary", cryptic_behaviour == 1, position == "demersal" | position == "benthic", size_bin < 4) %>% summarize(Avg = mean(prey_density)) %>% as.data.frame()
(0.001297218-0.04919154)/0.001297218



# Effect Size in SVC vs. Roving Results ========================================


# SILVERING:

# % Difference
SVC_silv <- SVCpred %>% filter(colouration == "silvering") %>% summarize(Avg = mean(log_SVCdensity)) %>% as.data.frame()
pred_silv <- SVCpred %>% filter(colouration == "silvering") %>% summarize(Avg = mean(log_preddensity)) %>% as.data.frame()
((SVC_silv)-(pred_silv))/(SVC_silv)

# Uncertainty
SVC_silv_sd <- SVCpred %>% filter(shape == "silviform") %>% summarize(Sd = sd(log_SVCdensity)) %>% as.data.frame()
SVC_silv_plussd <- SVC_silv + SVC_silv_sd
SVC_silv_minussd <- SVC_silv - SVC_silv_sd
pred_silv_sd <- SVCpred %>% filter(shape == "silviform") %>% summarize(Sd = sd(log_preddensity)) %>% as.data.frame()
pred_silv_plussd <- pred_silv + pred_silv_sd
pred_silv_minussd <- pred_silv - pred_silv_sd
# greatest difference:
((SVC_silv_minussd)-(pred_silv_plussd))/(SVC_silv_minussd)
# smallest difference:
((SVC_silv_plussd)-(pred_silv_minussd))/(SVC_silv_plussd)

# neutral
SVCpred %>% filter(colouration == "neutral") %>% summarize(Avg = mean(SVC_density)) %>% as.data.frame()
SVCpred %>% filter(colouration == "neutral") %>% summarize(Avg = mean(pred_density)) %>% as.data.frame()
(0.002263545-0.001221478)/0.002263545

# camouflage
SVCpred %>% filter(colouration == "camouflage") %>% summarize(Avg = mean(SVC_density)) %>% as.data.frame()
SVCpred %>% filter(colouration == "camouflage") %>% summarize(Avg = mean(pred_density)) %>% as.data.frame()
(0.000728824-0.001369977)/0.000728824

# fusiform
SVCpred %>% filter(shape == "fusiform") %>% summarize(Avg = mean(SVC_density)) %>% as.data.frame()
SVCpred %>% filter(shape == "fusiform") %>% summarize(Avg = mean(pred_density)) %>% as.data.frame()
(0.002464931-0.001189571)/0.002464931

# anguiliform
SVCpred %>% filter(shape == "anguilliform") %>% summarize(Avg = mean(SVC_density)) %>% as.data.frame()
SVCpred %>% filter(shape == "anguilliform") %>% summarize(Avg = mean(pred_density)) %>% as.data.frame()
(0.0005196896-0.00129334)/0.0005196896

# large, fusiform, camouflage or neutral, demersal, solitary
SVCpred %>% filter(colouration == "camouflage" | colouration == "neutral", position == "demersal", shape == "fusiform", behavior == "solitary", size_bin > 3) %>% summarize(Avg = mean(SVC_density)) %>% as.data.frame()
SVCpred %>% filter(colouration == "camouflage" | colouration == "neutral", position == "demersal", shape == "fusiform", behavior == "solitary", size_bin > 3) %>% summarize(Avg = mean(pred_density)) %>% as.data.frame()
(0.002153992-0.001260955)/0.002153992

# small, cryptic, demersal or benthic, solitary, fusiform or elongated, camouflage
SVCpred %>% filter(colouration == "camouflage", shape == "fusiform", behavior == "solitary", cryptic_behaviour == 1, position == "demersal" | position == "benthic", size_bin < 4) %>% summarize(Avg = mean(SVC_density)) %>% as.data.frame()
SVCpred %>% filter(colouration == "camouflage", shape == "fusiform", behavior == "solitary", cryptic_behaviour == 1, position == "demersal" | position == "benthic", size_bin < 4) %>% summarize(Avg = mean(pred_density)) %>% as.data.frame()
(0.001297218-0.04919154)/0.001297218

