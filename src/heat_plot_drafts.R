########## SURVEY COMPARISON PROJECT HEAT PLOT CREATION ##########
########## 
##########
# This file...
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

# Code for SVC vs Roving is still in this file but I've blocked it from running
#SVCpred_model_data <- read_csv(here("./dataframes/SVCpred_dataframe.csv"))


# Standardize Continuous Variables =============================================

# the following centres all continuous variables by subtracting the mean 

# SVC vs. Belt Transect

# octocoral
SVCprey_model_data$octocoral_c <- SVCprey_model_data$octocoral - mean(SVCprey_model_data$octocoral)

# stony coral
SVCprey_model_data$stony_c <- SVCprey_model_data$stony - mean(SVCprey_model_data$stony)

# vertical relief
SVCprey_model_data$relief_c <- SVCprey_model_data$relief_cm - mean(SVCprey_model_data$relief_cm)

# size bins
SVCprey_model_data$size_bin_c <- SVCprey_model_data$size_bin_lengths - mean(SVCprey_model_data$size_bin_lengths)

# maximum length
SVCprey_model_data$max_length_c <- SVCprey_model_data$max_length - mean(SVCprey_model_data$max_length)

# average depth 
SVCprey_model_data$depth_c <- SVCprey_model_data$average_depth - mean(SVCprey_model_data$average_depth)

# SVC vs. Roving

# octocoral
#SVCpred_model_data$octocoral_c <- SVCpred_model_data$octocoral - mean(SVCpred_model_data$octocoral)

# stony coral
#SVCpred_model_data$stony_c <- SVCpred_model_data$stony - mean(SVCpred_model_data$stony)

# vertical relief
#SVCpred_model_data$relief_c <- SVCpred_model_data$relief_cm - mean(SVCpred_model_data$relief_cm)

# size bins
#SVCpred_model_data$size_bin_c <- SVCpred_model_data$size_bin_lengths - mean(SVCpred_model_data$size_bin_lengths)


# SVC vs. Roving: Create Global Model ==========================================

#specify global model
#SVCpred_mod <- lmer(log_difference ~ habitat+octocoral_c+stony_c+relief_c+nocturnal+
                       #cryptic_behaviour+size_bin_c+colouration+shape + 
                       #(1|site) + (1|species_order), data = SVCpred_model_data, 
                    #na.action = "na.fail")

# summary
#summary(SVCpred_mod)
# GIVES DIFFERENT ESTIMATES THAN PREVIOUS MODELS 


# SVC vs. Roving: Compare Models with Dredge ===================================

#SVCpred_dredge <- dredge(SVCpred_mod)

#Subset the top models (i.e. delta AIC is <4)
#subset(SVCpred_dredge, delta <4)
#subset(SVCpred_dredge,cumsum(weight) <= 0.95)

#visualize the model selection table
#if (require(graphics))
  #plot(SVCpred_dredge)

# 'Best' model
#summary(get.models(SVCpred_dredge, 1))[[1]]


# SVC vs. Belt Transect: Create Global Model ===================================

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
# GIVES DIFFERENT ESTIMATES THAN PREVIOUS MODELS (lmer vs lme) 


# SVC vs. Belt Transect: Compare Models with Dredge ============================

# read in saved dredge results 
SVCprey_dredge <- read_rds(here("./outputs/SVCprey_global_dredge_centred.rds"))

#Subset the top models (i.e. delta AIC is <4)
subset(SVCprey_dredge, delta <4)
subset(SVCprey_dredge,cumsum(weight) <= 0.95)

#visualize the model selection table
if (require(graphics))
  plot(SVCprey_dredge)


# SVC vs. Belt Transect: Conduct Model Averaging ===============================

# and obtain model-averaged parameter estimates for each fixed effect
# model avg for only models with delta AIC <4 
SVCprey_avgmod.delta4CEI <- model.avg(SVCprey_dredge, subset = delta<4)
summary(SVCprey_avgmod.delta4CEI)
confint(SVCprey_avgmod.delta4CEI)

# model avg for all models
# summary(model.avg(ddCEI))
# or as a 95% confidence set:
SVCprey_avgmod.95p <- model.avg(SVCprey_dredge, cumsum(weight) <= 0.95)

#confint(avgmod.95p)
summary(SVCprey_avgmod.95p)

# The same result as above, but re-fitting the models via 'get.models'
#confset.95p <- get.models(ddCEI, cumsum(weight) <= .95)
#model.avg(confset.95p)


# Steph's Heat Plot Code =======================================================

# SVC vs. Belt Transect: Prediction Plots ======================================

##### a) first get negative log likelihood values
SVCprey_MOD <- summary(SVCprey_avgmod.95p)

# NO SCALE plot fishes:
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

### next get estimates of density differences
SVCprey_p.difference <- intCEI + stonyCEI*SVCprey_model_data$stony_c + sizebinCEI*SVCprey_model_data$size_bin_c + colourfulCEI + fusiformCEI

### next covert to probabilities 
# average value for fixed effects 
SVCprey_p.difavg <- (exp(SVCprey_p.difference)/(1 + exp(SVCprey_p.difference)))
# CIs including random effects 
SVCprey_p.difup <- (exp(SVCprey_p.difference + 1.96)/(1 + exp(SVCprey_p.difference + 1.96)))
SVCprey_p.diflow <- (exp(SVCprey_p.difference - 1.96)/(1 + exp(SVCprey_p.difference - 1.96)))

### get data for variables to plot
StonyCoral <- SVCprey_model_data$stony_c
SizeBin <- SVCprey_model_data$size_bin_c
PredictData <- cbind(StonyCoral, SizeBin, SVCprey_p.difavg, SVCprey_p.difup, SVCprey_p.diflow)
PredictData1 <- as.data.frame(PredictData)

PredictData1

str(PredictData1)

# uncentre

PredictData1$StonyCoral <- PredictData1$StonyCoral + mean(SVCprey_model_data$stony)
PredictData1$SizeBin <- PredictData1$SizeBin + mean(SVCprey_model_data$size_bin_lengths)


#write.table(PredictData1,file="C:\\Users\\Steph\\Desktop\\PredictData1.csv",sep=",",row.names=F)


# SVC vs. Belt Transect: Plots of Predicted Model ==============================

# Interpolate data frame to matrix for plots 
# mean
SVCprey_interavg <- interp(PredictData1$StonyCoral, PredictData1$SizeBin, PredictData1$SVCprey_p.difavg, duplicate = "strip")

# Contour plots for mean and CIs of detection probability
# mean probabilities

par(mar = c(5,6,2,2), mgp = c(3.5,1,0))

image.plot(SVCprey_interavg, xlab = "Prey Size (cm)", xlim =c(0,48), ylab = "Stony Coral Cover", ylim=c(0,65), cex.lab = 3, cex.axis = 2.4,
           axis.args = list(cex.axis = 1.4))

contour(SVCprey_interavg, vfont = c("sans serif", "bold"), nlevels = 12, add = TRUE, labcex = 2.4, labtype = "bold")


# Alex's Heat Plot Code ========================================================

n=100 #number of points in matrix row this will give us a matrix of 10,000

#creating new value objects with the min and max of the variable we want 
#as our axis--- should be continuous, and then merge together
minsize=min(SVCprey_model_data$size_bin_c)
maxsize=max(SVCprey_model_data$size_bin_c)
sizeT=seq(minsize,maxsize,length.out=n)

minstony=min(SVCprey_model_data$stony_c)
maxstony=max(SVCprey_model_data$stony_c)
stonyT=seq(minstony,maxstony,length.out=n)

#creating new value objects with the average of the variable, to hold static
summary(SVCprey_avgmod.delta4CEI)
maxlengthT=rep(mean(SVCprey_model_data$max_length_c),n^2)
octocoralT=rep(mean(SVCprey_model_data$octocoral_c),n^2)

# creating new value objects with all the levels of the categorical variables
# DO I DO ALL LEVELS OF EACH PREDICTOR? EVEN THE BASE ONES USED FOR COMPARISON 
# IN MODEL OUTPUT?
behaviorTsho=rep("Shoaling",n^2)
behaviorTsol=rep("Solitary",n^2)
behaviorTsch=rep("Schooling",n^2)

colorTcol=rep("Colorful",n^2)
colorTneu=rep("Neutral",n^2)
colorTsil=rep("Silvering",n^2)
colorTcam=rep("Camouflage",n^2)

habitatTpat=rep("Patch",n^2)
habitatTcon=rep("Continuous",n^2)

shapeTelo=rep("Elongated",n^2)
shapeTfus=rep("Fusiform",n^2)
shapeTglo=rep("Globiform",n^2)
shapeTcom=rep("Compressiform",n^2)

crypticT1=rep("1",n^2)
crypticT0=rep("0",n^2)

positionTdem=rep("Demersal",n^2)
positionTpel=rep("Pelagic",n^2)


#Create the new matrices with only the variable that were significant in the models
# column name of what you want the axis to be ( coral, size) rep (value object created, each n)
#column name from original data frames = value object just created
# Must repeat for every shape/color combo for 16 total panels
# ONLY SIGNIFICANT VARIABLES, NOT ALL THAT WERE PRESENT IN TOP MODELS IN DREDGE 
# OUTPUT? 

# shape/color combos: 
# colourful*fusiform, colorful*elongated, colorful*globiform, colorful*compressiform
# neutral*fusiform, neutral*elongated, neutral*globiform, neutral*compressiform
# silvering*fusiform, silvering*elongated, silvering*globiform, silvering*compressiform
# camouflage*fusiform, camouflage*elongated, camouflage*globiform, camouflage*compressiform

# HOW TO ADD OTHER CATEGORICAL VARIABLES INTO THESE?

# colorful
new.dataTColFus=data.frame(stony_coral_cover=rep(stonyT,each=n), size=rep(sizeT,n),
                           max_length_c=maxlengthT, octocoral_c=octocoralT, 
                           colouration=colorTcol, shape=shapeTfus)

new.dataTColElo=data.frame(stony_coral_cover=rep(stonyT,each=n), size=rep(sizeT,n),
                           max_length_c=maxlengthT, octocoral_c=octocoralT, 
                           colouration=colorTcol, shape=shapeTelo)

new.dataTColGlo=data.frame(stony_coral_cover=rep(stonyT,each=n), size=rep(sizeT,n),
                           max_length_c=maxlengthT, octocoral_c=octocoralT, 
                           colouration=colorTcol, shape=shapeTglo)

new.dataTColCom=data.frame(stony_coral_cover=rep(stonyT,each=n), size=rep(sizeT,n),
                           max_length_c=maxlengthT, octocoral_c=octocoralT, 
                           colouration=colorTcol, shape=shapeTcom)

# neutral

# silvering

# camouflage 


##this is where you bring in your model again you will have to
#do this for every combination (16 total), 
#my model is newtime.true, 
#new.dataTCexp is my new dataframe
#newdata is a created column with predictions based on your model output
newpredictDENSITYDIFcolfus=predict(SVCprey_avgmod.delta4CEI, 
                                   newdata=new.dataTColFus, type='response',
                                   backtransform=FALSE, re.form=NA)

newpredictTIMEcH=predict(newtime.true, newdata = new.dataTCexp, type='response', 
                         backtransform =FALSE, re.form=NA) #check this 

#interpolate using the predictions just created, and referencing the 
#continous axis variables from you created dataframe
TimeInterpCH <-interp(new.dataTCexp$Site_area_m2,new.dataTCexp$SiteDens1000,
                      newpredictTIMEcH)


##midday
newpredictTIMEm=predict(newtime.true, newdata = new.dataTMexp, type='response', 
                        backtransform =FALSE, re.form=NA) #check this 

TimeInterpM <-interp(new.dataTMexp$Site_area_m2,new.dataTMexp$SiteDens1000,newpredictTIMEm)


image.plot(TimeInterpC)


library(lattice)
library(fields)
library(akima)
library(grDevices)
library(MASS)
library(viridis)
#This is just making the color pallete you dont need to use PNW colors
library(PNWColors)
names(pnw_palettes)
pal=pnw_palette("Sunset2",100)

###Crepuscular###
set.panel() # reset plotting device
image.plot(TimeInterpCH, col= pal,
           main = "Crepuscular Hours",
           cex.main =2,
           xlab = "Site Area (m2)", xlim =c(0,8000), 
           ylab = "Density (#/1000)", ylim = c(0,11.1),
           cex.lab = 2, cex.axis = 1.4,
           axis.args = list(cex.axis = 1.4),
           legend.width = 1.5,
           legend.mar = 3,
           legend.args = list( text = "Time (min)",
                               cex = 1.4,
                               side = 2,
                               line = .3),
           zlim=c(0.4,3.3))

contour(TimeInterpCH, vfont = c("sans serif", "bold"), nlevels = 12, add = TRUE, 
        labcex = 2.4, labtype = "bold", col= "white")
#if you want to plot a specific point on the plot
points(2080,1.7,pch="*", cex=3)
points(4250,2.3,pch="+", cex=3)


###midday###
set.panel() # reset plotting device
image.plot(TimeInterpM, col= pal,
           main = "Midday Hours",
           cex.main =2,
           xlab = "Site Area", xlim =c(0,8000), 
           ylab = "Density (#/1000)", ylim = c(0,11.1),
           cex.lab = 2, cex.axis = 1.4,
           axis.args = list(cex.axis = 1.4),
           legend.width = 1.5,
           legend.mar = 3,
           legend.args = list( text = "Time (min)",
                               cex = 1.4,
                               side = 2,
                               line = .3),
           zlim=c(0.4,3.3)) ##make sure you set this uppper limit to the highest of time

contour(TimeInterpM, vfont = c("sans serif", "bold"), nlevels = 12, add = TRUE, 
        labcex = 2.4, labtype = "bold", col= "white")
