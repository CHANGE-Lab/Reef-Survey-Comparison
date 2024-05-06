########## SURVEY COMPARISON PROJECT HEAT PLOT CREATION ##########
########## 
##########
# This file creates heat plots comparing the difference in fish densities 
# recorded between SVC and belt transect surveys across different levels of 
# species colouration, shape, size, and site stony coral cover. 
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2023-09-11
##########
##########


# Set-Up =======================================================================

# packages
library(plyr)
library(tidyverse)
library(lme4)
library(MuMIn)
library(lattice)
library(fields)
library(akima)
library(grDevices)
library(MASS)
library(here)

# data
SVCprey_model_data <- read_csv(here("./dataframes/SVCprey_dataframe.csv"))


# Standardize Continuous Variables =============================================

# The following centres all continuous variables by subtracting the mean. 

# octocoral
SVCprey_model_data$octocoral_c <- SVCprey_model_data$octocoral - 
  mean(SVCprey_model_data$octocoral)

# stony coral
SVCprey_model_data$stony_c <- SVCprey_model_data$stony - 
  mean(SVCprey_model_data$stony)

# vertical relief
SVCprey_model_data$relief_c <- SVCprey_model_data$relief_cm - 
  mean(SVCprey_model_data$relief_cm)

# size bins
SVCprey_model_data$size_bin_c <- SVCprey_model_data$size_bin_lengths - 
  mean(SVCprey_model_data$size_bin_lengths)

# maximum length
SVCprey_model_data$max_length_c <- SVCprey_model_data$max_length - 
  mean(SVCprey_model_data$max_length)

# average depth 
SVCprey_model_data$depth_c <- SVCprey_model_data$average_depth - 
  mean(SVCprey_model_data$average_depth)


# Create Global Model ==========================================================

# The following runs a global linear mixed effects model with the same structure
# as that utilized in main analyses. 

#specify global model
SVCprey_mod <- lmer(log_difference ~ habitat+octocoral_c+stony_c+relief_c+
                      size_bin_c*colouration+nocturnal+position+
                      max_length_c+behavior+cryptic_behaviour+depth_c+
                      size_bin_c*shape + 
                      (1|site) + (1|species_order), 
                    data = SVCprey_model_data, 
                    na.action = "na.fail")

# summary
summary(SVCprey_mod)


# Compare Models with Dredge ===================================================

# The following reads in the dredge that was previously performed on the global
# SVC vs. belt transect model. 

# read in saved dredge results 
SVCprey_dredge <- read_rds(here("./outputs/SVCprey_global_dredge_centred.rds"))

# subset the top models (i.e. delta AIC is <4)
subset(SVCprey_dredge, delta < 4)
subset(SVCprey_dredge,cumsum(weight) <= 0.95)

# visualize the model selection table
if(require(graphics))
plot(SVCprey_dredge)


# Conduct Model Averaging ======================================================

# The following performs model averaging for the dredge run on the SVC vs. belt
# transect global model. 

# model avg for only models with delta AIC <4 
SVCprey_avgmod.delta4CEI <- model.avg(SVCprey_dredge, subset = delta<4)
summary(SVCprey_avgmod.delta4CEI)
confint(SVCprey_avgmod.delta4CEI)

# model avg as a 95% confidence set
SVCprey_avgmod.95p <- model.avg(SVCprey_dredge, cumsum(weight) <= 0.95)
summary(SVCprey_avgmod.95p)


# Trait Coefficients ===========================================================

# The following obtains model coefficients for each trait level in the SVC vs. 
# belt transect global model. 

# get negative log likelihood values
SVCprey_MOD <- summary(SVCprey_avgmod.95p)

# isolate coefficients of trait levels 
intCEI <- coef(summary(SVCprey_MOD))[1]
shoalCEI <- coef(summary(SVCprey_MOD))[2]
solitaryCEI <- coef(summary(SVCprey_MOD))[3]
colourfulCEI <- coef(summary(SVCprey_MOD))[4]
neutralCEI <- coef(summary(SVCprey_MOD))[5]
silverCEI <- coef(summary(SVCprey_MOD))[6]
patchCEI <- coef(summary(SVCprey_MOD))[7]
maxlengthCEI <- coef(summary(SVCprey_MOD))[8]
octocoralCEI <- coef(summary(SVCprey_MOD))[9]
elongatedCEI <- coef(summary(SVCprey_MOD))[10]
fusiformCEI <- coef(summary(SVCprey_MOD))[11]
globiformCEI <- coef(summary(SVCprey_MOD))[12]
sizebinCEI <- coef(summary(SVCprey_MOD))[13]
stonyCEI <- coef(summary(SVCprey_MOD))[14]
elongated.sizeCEI <- coef(summary(SVCprey_MOD))[15]
fusiform.sizeCEI <- coef(summary(SVCprey_MOD))[16]
globiform.sizeCEI <- coef(summary(SVCprey_MOD))[17]
colourful.sizeCEI <- coef(summary(SVCprey_MOD))[18]
neutral.sizeCEI <- coef(summary(SVCprey_MOD))[19]
silvering.sizeCEI <- coef(summary(SVCprey_MOD))[20]
crypsisCEI <- coef(summary(SVCprey_MOD))[21]
demersalCEI <- coef(summary(SVCprey_MOD))[22]
nocturnalCEI <- coef(summary(SVCprey_MOD))[23]


# Heat Plot: Colourful & Fusiform ==============================================

# The following creates a heat plot for colourful and fusiform species over
# the full range of sizes and stony coral cover.  

# get estimates of density differences
SVCprey_p.difference <- intCEI + stonyCEI*SVCprey_model_data$stony_c + 
  sizebinCEI*SVCprey_model_data$size_bin_c + colourfulCEI + fusiformCEI

# get data for variables to plot
StonyCoral <- SVCprey_model_data$stony_c
SizeBin <- SVCprey_model_data$size_bin_c
PredictData <- cbind(StonyCoral, SizeBin, SVCprey_p.difference)
PredictData1 <- as.data.frame(PredictData)
PredictData1
str(PredictData1)

# uncentre
PredictData1$StonyCoral <- PredictData1$StonyCoral + 
  mean(SVCprey_model_data$stony)
PredictData1$SizeBin <- PredictData1$SizeBin + 
  mean(SVCprey_model_data$size_bin_lengths)

# interpolate data frame to matrix for plots

# mean
SVCprey_plotdata <- interp(PredictData1$SizeBin, PredictData1$StonyCoral, 
                           PredictData1$SVCprey_p.difference, 
                           duplicate = "strip")

# contour plots for mean and CIs of detection probability

# mean probabilities

par(mar = c(5,6,2,2), mgp = c(3.5,1,0))

image.plot(SVCprey_plotdata, 
           xlim =c(3,48), 
           ylim=c(0,65), 
           cex.lab = 1.5, cex.axis = 1.5,
           axis.args = list(cex.axis = 1), 
           zlim=c(-2,1.5))

contour(SVCprey_plotdata, vfont = c("sans serif", "bold"), nlevels = 12, 
        add = TRUE, labcex = 1.5, labtype = "bold")


# Heat Plot: Colourful & Elongated =============================================

# The following creates a heat plot for colourful and elongated species over
# the full range of sizes and stony coral cover.  

# get estimates of density differences
SVCprey_p.difference_CoEl <- intCEI + stonyCEI*SVCprey_model_data$stony_c + 
  sizebinCEI*SVCprey_model_data$size_bin_c + colourfulCEI + elongatedCEI

# get data for variables to plot
StonyCoral <- SVCprey_model_data$stony_c
SizeBin <- SVCprey_model_data$size_bin_c
PredictData_CoEl <- cbind(StonyCoral, SizeBin, SVCprey_p.difference_CoEl)
PredictData1_CoEl <- as.data.frame(PredictData_CoEl)
PredictData1_CoEl
str(PredictData1_CoEl)

# uncentre
PredictData1_CoEl$StonyCoral <- PredictData1_CoEl$StonyCoral + 
  mean(SVCprey_model_data$stony)
PredictData1_CoEl$SizeBin <- PredictData1_CoEl$SizeBin + 
  mean(SVCprey_model_data$size_bin_lengths)

# interpolate data frame to matrix for plots

# mean
SVCprey_plotdata_CoEl <- interp(PredictData1_CoEl$SizeBin, 
                                PredictData1_CoEl$StonyCoral, 
                                PredictData1_CoEl$SVCprey_p.difference, 
                                duplicate = "strip")

# plot 
par(mar = c(5,6,2,2), mgp = c(3.5,1,0))

image.plot(SVCprey_plotdata_CoEl, 
           xlim =c(3,48), 
           ylim=c(0,65), 
           cex.lab = 1.5, cex.axis = 1.5,
           axis.args = list(cex.axis = 1),
           zlim=c(-2,1.5))

contour(SVCprey_plotdata_CoEl, vfont = c("sans serif", "bold"), nlevels = 12, 
        add = TRUE, labcex = 1.5, labtype = "bold")


# Heat Plot: Colourful & Globiform =============================================

# The following creates a heat plot for colourful and globiform species over
# the full range of sizes and stony coral cover.  

### next get estimates of density differences
SVCprey_p.difference_CoGl <- intCEI + stonyCEI*SVCprey_model_data$stony_c + 
  sizebinCEI*SVCprey_model_data$size_bin_c + colourfulCEI + globiformCEI

### get data for variables to plot
StonyCoral <- SVCprey_model_data$stony_c
SizeBin <- SVCprey_model_data$size_bin_c
PredictData_CoGl <- cbind(StonyCoral, SizeBin, SVCprey_p.difference_CoGl)
PredictData1_CoGl <- as.data.frame(PredictData_CoGl)
PredictData1_CoGl
str(PredictData1_CoGl)

# uncentre
PredictData1_CoGl$StonyCoral <- PredictData1_CoGl$StonyCoral + 
  mean(SVCprey_model_data$stony)
PredictData1_CoGl$SizeBin <- PredictData1_CoGl$SizeBin + 
  mean(SVCprey_model_data$size_bin_lengths)

# Interpolate data frame to matrix for plots

# mean
SVCprey_plotdata_CoGl <- interp(PredictData1_CoGl$SizeBin,
                                PredictData1_CoGl$StonyCoral, 
                                PredictData1_CoGl$SVCprey_p.difference, 
                                duplicate = "strip")

# plot 
par(mar = c(5,6,2,2), mgp = c(3.5,1,0))

image.plot(SVCprey_plotdata_CoGl, 
           xlim =c(3,48), 
           ylim=c(0,65), 
           cex.lab = 1.5, cex.axis = 1.5,
           axis.args = list(cex.axis = 1), 
           zlim=c(-2,1.5))

contour(SVCprey_plotdata_CoGl, vfont = c("sans serif", "bold"), nlevels = 12, 
        add = TRUE, labcex = 1.5, labtype = "bold")


# Heat Plot: Neutral & Fusiform ================================================

# The following creates a heat plot for neutral and fusiform species over
# the full range of sizes and stony coral cover.  

### next get estimates of density differences
SVCprey_p.difference_NeFu <- intCEI + stonyCEI*SVCprey_model_data$stony_c + 
  sizebinCEI*SVCprey_model_data$size_bin_c + neutralCEI + fusiformCEI

### get data for variables to plot
StonyCoral <- SVCprey_model_data$stony_c
SizeBin <- SVCprey_model_data$size_bin_c
PredictData_NeFu <- cbind(StonyCoral, SizeBin, SVCprey_p.difference_NeFu)
PredictData1_NeFu <- as.data.frame(PredictData_NeFu)
PredictData1_NeFu
str(PredictData1_NeFu)

# uncentre
PredictData1_NeFu$StonyCoral <- PredictData1_NeFu$StonyCoral + 
  mean(SVCprey_model_data$stony)
PredictData1_NeFu$SizeBin <- PredictData1_NeFu$SizeBin + 
  mean(SVCprey_model_data$size_bin_lengths)

# Interpolate data frame to matrix for plots

# mean
SVCprey_plotdata_NeFu <- interp(PredictData1_NeFu$SizeBin,
                                PredictData1_CoGl$StonyCoral, 
                                PredictData1_NeFu$SVCprey_p.difference, 
                                duplicate = "strip")

# plot 
par(mar = c(5,6,2,2), mgp = c(3.5,1,0))

image.plot(SVCprey_plotdata_NeFu, 
           xlim =c(3,48), 
           ylim=c(0,65), 
           cex.lab = 1.5, cex.axis = 1.5,
           axis.args = list(cex.axis = 1), 
           zlim=c(-2,1.5))

contour(SVCprey_plotdata_NeFu, vfont = c("sans serif", "bold"), nlevels = 12, 
        add = TRUE, labcex = 1.5, labtype = "bold")


# Heat Plot: Neutral & Elongated ===============================================

# The following creates a heat plot for neutral and elongated species over
# the full range of sizes and stony coral cover.  

### next get estimates of density differences
SVCprey_p.difference_NeEl <- intCEI + stonyCEI*SVCprey_model_data$stony_c + 
  sizebinCEI*SVCprey_model_data$size_bin_c + neutralCEI + elongatedCEI

### get data for variables to plot
StonyCoral <- SVCprey_model_data$stony_c
SizeBin <- SVCprey_model_data$size_bin_c
PredictData_NeEl <- cbind(StonyCoral, SizeBin, SVCprey_p.difference_NeEl)
PredictData1_NeEl <- as.data.frame(PredictData_NeEl)
str(PredictData1_NeEl)

# uncentre
PredictData1_NeEl$StonyCoral <- PredictData1_NeEl$StonyCoral + 
  mean(SVCprey_model_data$stony)
PredictData1_NeEl$SizeBin <- PredictData1_NeEl$SizeBin + 
  mean(SVCprey_model_data$size_bin_lengths)

# interpolate data frame to matrix for plots

# mean
SVCprey_plotdata_NeEl <- interp(PredictData1_NeEl$SizeBin,
                                PredictData1_CoEl$StonyCoral, 
                                PredictData1_NeEl$SVCprey_p.difference, 
                                duplicate = "strip")

# plot 
par(mar = c(5,6,2,2), mgp = c(3.5,1,0))

image.plot(SVCprey_plotdata_NeEl, 
           xlim =c(3,48), 
           ylim=c(0,65), 
           cex.lab = 1.5, cex.axis = 1.5,
           axis.args = list(cex.axis = 1), 
           zlim=c(-2,1.5))

contour(SVCprey_plotdata_NeEl, vfont = c("sans serif", "bold"), 
        nlevels = 12, add = TRUE, labcex = 1.5, labtype = "bold")


# Heat Plot: Neutral & Globiform ===============================================

# The following creates a heat plot for neutral and globiform species over
# the full range of sizes and stony coral cover.  

### next get estimates of density differences
SVCprey_p.difference_NeGl <- intCEI + stonyCEI*SVCprey_model_data$stony_c + 
  sizebinCEI*SVCprey_model_data$size_bin_c + neutralCEI + globiformCEI

### get data for variables to plot
StonyCoral <- SVCprey_model_data$stony_c
SizeBin <- SVCprey_model_data$size_bin_c
PredictData_NeGl <- cbind(StonyCoral, SizeBin, SVCprey_p.difference_NeGl)
PredictData1_NeGl <- as.data.frame(PredictData_NeGl)
str(PredictData1_NeGl)

# uncentre
PredictData1_NeGl$StonyCoral <- PredictData1_NeGl$StonyCoral + 
  mean(SVCprey_model_data$stony)
PredictData1_NeGl$SizeBin <- PredictData1_NeGl$SizeBin + 
  mean(SVCprey_model_data$size_bin_lengths)

# interpolate data frame to matrix for plots

# mean
SVCprey_plotdata_NeGl <- interp(PredictData1_NeGl$SizeBin,
                                PredictData1_CoGl$StonyCoral, 
                                PredictData1_NeGl$SVCprey_p.difference, 
                                duplicate = "strip")

# plot 
par(mar = c(5,6,2,2), mgp = c(3.5,1,0))

image.plot(SVCprey_plotdata_NeGl, 
           xlim =c(3,48), 
           ylim=c(0,65), 
           cex.lab = 1.5, cex.axis = 1.5,
           axis.args = list(cex.axis = 1), 
           zlim=c(-2,1.5))

contour(SVCprey_plotdata_NeGl, vfont = c("sans serif", "bold"), nlevels = 12, 
        add = TRUE, labcex = 1.5, labtype = "bold")


# Heat Plot: Silver & Fusiform =================================================

# The following creates a heat plot for silver and fusiform species over
# the full range of sizes and stony coral cover.  

### next get estimates of density differences
SVCprey_p.difference_SiFu <- intCEI + stonyCEI*SVCprey_model_data$stony_c + 
  sizebinCEI*SVCprey_model_data$size_bin_c + silverCEI + fusiformCEI

### get data for variables to plot
StonyCoral <- SVCprey_model_data$stony_c
SizeBin <- SVCprey_model_data$size_bin_c
PredictData_SiFu <- cbind(StonyCoral, SizeBin, SVCprey_p.difference_SiFu)
PredictData1_SiFu <- as.data.frame(PredictData_SiFu)
str(PredictData1_SiFu)

# uncentre
PredictData1_SiFu$StonyCoral <- PredictData1_SiFu$StonyCoral + 
  mean(SVCprey_model_data$stony)
PredictData1_SiFu$SizeBin <- PredictData1_SiFu$SizeBin + 
  mean(SVCprey_model_data$size_bin_lengths)

# Interpolate data frame to matrix for plots

# mean
SVCprey_plotdata_SiFu <- interp(PredictData1_SiFu$SizeBin,
                                PredictData1_CoGl$StonyCoral, 
                                PredictData1_SiFu$SVCprey_p.difference, 
                                duplicate = "strip")

# plot 
par(mar = c(5,6,2,2), mgp = c(3.5,1,0))

image.plot(SVCprey_plotdata_SiFu, 
           xlim =c(3,48), 
           ylim=c(0,65), 
           cex.lab = 1.5, cex.axis = 1.5,
           axis.args = list(cex.axis = 1), 
           zlim=c(-2,1.5))

contour(SVCprey_plotdata_SiFu, vfont = c("sans serif", "bold"), nlevels = 12, 
        add = TRUE, labcex = 1.5, labtype = "bold")


# Heat Plot: Silver & Elongated ================================================

# The following creates a heat plot for silver and elongated species over
# the full range of sizes and stony coral cover.  

### next get estimates of density differences
SVCprey_p.difference_SiEl <- intCEI + stonyCEI*SVCprey_model_data$stony_c + 
  sizebinCEI*SVCprey_model_data$size_bin_c + silverCEI + elongatedCEI

### get data for variables to plot
StonyCoral <- SVCprey_model_data$stony_c
SizeBin <- SVCprey_model_data$size_bin_c
PredictData_SiEl <- cbind(StonyCoral, SizeBin, SVCprey_p.difference_SiEl)
PredictData1_SiEl <- as.data.frame(PredictData_SiEl)
str(PredictData1_SiEl)

# uncentre
PredictData1_SiEl$StonyCoral <- PredictData1_SiEl$StonyCoral + 
  mean(SVCprey_model_data$stony)
PredictData1_SiEl$SizeBin <- PredictData1_SiEl$SizeBin + 
  mean(SVCprey_model_data$size_bin_lengths)

# Interpolate data frame to matrix for plots

# mean
SVCprey_plotdata_SiEl <- interp(PredictData1_SiEl$SizeBin,
                                PredictData1_CoEl$StonyCoral, 
                                PredictData1_SiEl$SVCprey_p.difference, 
                                duplicate = "strip")

# plot 
par(mar = c(5,6,2,2), mgp = c(3.5,1,0))

image.plot(SVCprey_plotdata_SiEl, 
           xlim =c(3,48), 
           ylim=c(0,65), 
           cex.lab = 1.5, cex.axis = 1.5,
           axis.args = list(cex.axis = 1), 
           zlim=c(-2,1.5))

contour(SVCprey_plotdata_SiEl, vfont = c("sans serif", "bold"), nlevels = 12, 
        add = TRUE, labcex = 1.5, labtype = "bold")


# Heat Plot: Silver & Globiform ================================================

# The following creates a heat plot for silver and globiform species over
# the full range of sizes and stony coral cover.  

### next get estimates of density differences
SVCprey_p.difference_SiGl <- intCEI + stonyCEI*SVCprey_model_data$stony_c + 
  sizebinCEI*SVCprey_model_data$size_bin_c + silverCEI + globiformCEI

### get data for variables to plot
StonyCoral <- SVCprey_model_data$stony_c
SizeBin <- SVCprey_model_data$size_bin_c
PredictData_SiGl <- cbind(StonyCoral, SizeBin, SVCprey_p.difference_SiGl)
PredictData1_SiGl <- as.data.frame(PredictData_SiGl)
str(PredictData1_SiGl)

# uncentre
PredictData1_SiGl$StonyCoral <- PredictData1_SiGl$StonyCoral + 
  mean(SVCprey_model_data$stony)
PredictData1_SiGl$SizeBin <- PredictData1_SiGl$SizeBin + 
  mean(SVCprey_model_data$size_bin_lengths)

# interpolate data frame to matrix for plots

# mean
SVCprey_plotdata_SiGl <- interp(PredictData1_SiGl$SizeBin,
                                PredictData1_CoGl$StonyCoral, 
                                PredictData1_SiGl$SVCprey_p.difference, 
                                duplicate = "strip")

# plot 
par(mar = c(5,6,2,2), mgp = c(3.5,1,0))

image.plot(SVCprey_plotdata_SiGl, 
           xlim =c(3,48), 
           ylim=c(0,65), 
           cex.lab = 1.5, cex.axis = 1.5,
           axis.args = list(cex.axis = 1), 
           zlim=c(-2,1.5))

contour(SVCprey_plotdata_SiGl, vfont = c("sans serif", "bold"), 
        nlevels = 12, add = TRUE, labcex = 1.5, labtype = "bold")