######NEW ANALYSIS April 23 2014#######
# account for phylogenetic relatedness
# AIC model selection and multi-model averaging
# uses non-scaled parameters (so directly comparable)
# uses prey body mass instead of length



#########  1) load data frames

#Selection data:
SelectionCEI <- read.csv("C:\\Users\\Steph\\Documents\\selection paper\\data frames\\CEI data frame revised_May 30_Phylo_countspared_Jan 20.csv")

#Prey trait information:
Prey <- read.csv("C:\\Users\\Steph\\Documents\\selection paper\\data frames\\SpeciesData_cluster_July 6_Phylo_ab.csv")

#Phylogenetic information:
Phylo <- read.csv("C:\\Users\\Steph\\Documents\\selection paper\\data frames\\FishTaxonomy_Phylo.csv")



########## 2) join data frames together:

# 1/2 join selection and prey group data frames by species name
PreySelectionCEI <- merge(SelectionCEI, Prey, by = "SpeciesName", all.x = "TRUE")

# 2/2 join PreySelectionNP with phylogenetic information
PreyPhyloCEI <- merge(PreySelectionCEI, Phylo,  by = "SpeciesName", all.x = "True")

#write.table(PreyPhyloCEI,file="C:\\Users\\Steph\\Desktop\\PreyPhyloCEI.csv",sep=",",row.names=F)
#str(PreyPhyloCEI)





######## 3) standardize continuous variables (ratio and length) in data frame to mean=0, SD=1

PreyPhyloCEI$AvgRatioScale <- scale(PreyPhyloCEI$AvgRatio)
PreyPhyloCEI$PreyLengthScale <- scale(PreyPhyloCEI$PreyLength)

# view resulting data frame
#str(PreyPhyloCEI)

PreyPhyloCEICaptures <- subset(PreyPhyloCEI, CountCapture == 2)

#create new variable for prey mass (g) by converting prey lengh (cm) using a and b constants
PreyPhyloCEICaptures$BodyMass <- PreyPhyloCEICaptures$a*PreyPhyloCEICaptures$PreyLength^PreyPhyloCEICaptures$b

# scale variable for body mass
PreyPhyloCEICaptures$BodyMassScale <- scale(PreyPhyloCEI$BodyMass)

# export the dataframe as a csv file for inspection
#write.table(PreyPhyloCEICaptures,file="C:\\Users\\Steph\\Documents\\PreySelectionCaptures.csv",sep=",",row.names=F)

str(PreyPhyloCEICaptures)






######### 4) create glmm for binary response data with glmer function in lme4 package

library(lme4) ### for lmer/glmer

# Use non-scaled values because they are comparable across data sets (SD varies between two data set
# so scaling skews the comparison)

#specify global model
selectCEIglmer <- glmer(Proportion ~ AvgRatio + BodyMass
                                + Defense 
                                + Position 
                                + Cleaner1
                                + Group 
                                + Nocturnal
                                + (1|LionLength) + (1|Lion) + (1|CountNum) 
                                + (1|Family.x)
                                , family = binomial(link = "cloglog") 
                                , weights = Abund
                                , data = PreyPhyloCEICaptures)                         

#summary
summary(selectCEIglmer)







########## 5) compare models using MuMIn package 

#load package for multi-model analaysis
library(MuMIn)

#global model for this analysis from above 
ddCEI <- dredge(selectCEIglmer)

#Subset the top models (i.e. delta AIC is <4)
subset(ddCEI, delta <4)
subset(ddCEI,cumsum(weight) <= 0.95)

#visualize the model selection table
if (require(graphics))
  plot(ddCEI)

# 'Best' model
summary(get.models(ddCEI, 1))[[1]]






######### 6A) conduct model avergaging (non-scale)

# and obtain model-averaged parameter estimates for each fixed effect
# model avg for only models with delta AIC <4 
avgmod.delta4CEI <- model.avg(ddCEI, subset = delta <4)
summary(avgmod.delta4CEI)
confint(avgmod.delta4CEI)

# model avg for all models
# summary(model.avg(ddCEI))
# or as a 95% confidence set:
avgmod.95p <- model.avg(ddCEI, cumsum(weight) <= 0.95)

#confint(avgmod.95p)
summary(avgmod.95p)

# The same result as above, but re-fitting the models via 'get.models'
#confset.95p <- get.models(ddCEI, cumsum(weight) <= .95)
#model.avg(confset.95p)







######### 7) prediction plots

##### a) first get negative log likelihood values
#Most likely to be eaten: nocturnal, shoaling/solitay (no coeff added)
#MOD <- avgmod.delta4NP
MOD <- summary(avgmod.95p)

# NO SCALE plot fishes:
intCEI <- coef(summary(MOD))[1]
depthCEI <- coef(summary(MOD))[2]
massCEI <- coef(summary(MOD))[3]
schoolCEI <- coef(summary(MOD))[4]
solitaryCEI <- coef(summary(MOD))[5]
nocturnalCEI <- coef(summary(MOD))[6]
defenseCEI <- coef(summary(MOD))[7]
demersalCEI <- coef(summary(MOD))[8]
pelagicCEI <- coef(summary(MOD))[9]
cleanCEI <- coef(summary(MOD))[10]


### b) next get probabilty of being eaten
#for most vulnerable: demersal, diurnal, solitary, non-cleaning
p.most <- intCEI + depthCEI*PreyPhyloCEICaptures$AvgRatio +  massCEI*PreyPhyloCEICaptures$BodyMass +
  demersalCEI + solitaryCEI  + nocturnalCEI #+cleanCEI 

### least vulnerable
#(pelagic, schooling, cleaning)
#p.least <- intCEI + depthCEI*PreyPhyloCEICaptures$AvgRatio  + lengthCEI*PreyPhyloCEICaptures$PreyLength +
  #+ schoolCEI + cleanCEI + nocturnalCEI + pelagicCEI

### c) next covert to probabilities for:
###most likely to be eaten
#average value for fixed effects
p.mostavg <- (exp(p.most)/ (1 + exp(p.most)))
## CIs including random effects; 95% values are between -1.96*stdev random effect and 1.96*stdev random effect ()
p.mostup<- (exp(p.most + 1.96)/(1 + exp(p.most + 1.96)))
p.mostlow <- (exp(p.most - 1.96)/(1 + exp(p.most - 1.96)))

### d) get data for variables to plot
PreyMass <- PreyPhyloCEICaptures$BodyMass
AvgRatio <- PreyPhyloCEICaptures$AvgRatio
PredictData <- cbind(PreyMass, AvgRatio, p.mostavg, p.mostup, p.mostlow, p.leastavg, p.leastup, p.leastlow)
PredictData1 <-as.data.frame(PredictData)

PredictData1

str(PredictData1)


#write.table(PredictData1,file="C:\\Users\\Steph\\Desktop\\PredictData1.csv",sep=",",row.names=F)








########## 8) plots of predicted model

### a)  Load libraries
library(lattice)
library(fields)
library(akima)
library(grDevices)
library(MASS)

### b)  interpolate data frame to matrix for plots
##### most likely (yes)
#mean 
Intermostavg <- interp(PredictData1$PreyMass,PredictData1$AvgRatio, PredictData1$p.mostavg, duplicate = "strip")

 
### c) contour plots for mean and CIs of detection probability
### mean probabilities
# to change plot for each prey type, go to step 7b) and change variables included in pro statement

#windows()

par(mar = c(5,6,2,2), mgp = c(3.5,1,0))

image.plot(Intermostavg, xlab = "Prey total length (cm)", xlim =c(0,40), ylab = "Body depth ratio", cex.lab = 3, cex.axis = 2.4,
           axis.args = list(cex.axis = 1.4))

contour(Intermostavg, vfont = c("sans serif", "bold"), nlevels = 12, add = TRUE, labcex = 2.4, labtype = "bold")


















##### extra stuff##

#model for scaled parameters
selectCEIglmerscale <- glmer(Proportion ~ AvgRatioScale + BodyMass
                             + Defense 
                             + Position 
                             + Cleaner1
                             + Group 
                             + Nocturnal
                             + (1|LionLength) + (1|Lion) + (1|CountNum) 
                             + (1|Family.x)
                             , family = binomial(link = "cloglog") 
                             , weights = Abund
                             , data = PreyPhyloCEICaptures)                         

summary(selectCEIglmerscale)


#load package for multi-model analaysis
library(MuMIn)

#global model for this analysis from above 
ddCEIscale <- dredge(selectCEIglmerscale)


######## 6B) conduct model avergaging (scaled parameters model)

# and obtain model-averaged parameter estimates for each fixed effect
# model avg for only models with delta AIC <4 
avgmod.delta4CEIscale <- model.avg(ddCEIscale, subset = delta <4)
summary(avgmod.delta4CEIscale)
confint(avgmod.delta4CEIscale)

# model avg for all models
# summary(model.avg(ddCEI))
# or as a 95% confidence set:
avgmod.95pscale <- model.avg(ddCEIscale, cumsum(weight) <= 0.95)

#confint(avgmod.95p)
summary(avgmod.95pscale)


#### least likely to be eaten
#average value for fixed effects
p.leastavg <- (exp(p.least)/ (1 + exp(p.least)))
## CIs including random effects; 95% values are between -1.96*stdev random effect and 1.96*stdev random effect (here = 2.01)
p.leastup<- (exp(p.least + 1.96)/(1 + exp(p.least + 1.96)))
p.leastlow <- (exp(p.least - 1.96)/(1 + exp(p.least - 1.96)))


#upper CI
Intermostup <- interp(PredictData1$PreyMass,PredictData1$AvgRatio, PredictData1$p.mostup, duplicate = "strip")
#lower CI
Intermostlow <- interp(PredictData1$PreyMass,PredictData1$AvgRatio, PredictData1$p.mostlow, duplicate = "strip")

##### least likely (no)
#mean 
Interleastavg <- interp(PredictData1$PreyMass,PredictData1$AvgRatio, PredictData1$p.leastavg, duplicate = "strip")
#upper CI
Interleastup <- interp(PredictData1$PreyMass,PredictData1$AvgRatio, PredictData1$p.leastup, duplicate = "strip")
#lower CI
Interleastlow <- interp(PredictData1$PreyMass,PredictData1$AvgRatio, PredictData1$p.leastlow, duplicate = "strip")










#windows()

#par(mfrow = c(2,1))

#up CI
#par(mar = c(4,4,1,1))
par(mgp = c(2.5,1,0))
image.plot(Intermostup, xlab = "Prey total length (cm)", ylab = "Body depth ratio", cex.lab = 2, cex.axis = 1.8,
           axis.args = list(cex.axis = 1.2))
contour(Intermostup, vfont = c("sans serif", "bold"), nlevels = 10, add = TRUE, labcex = 2)

#low CI
#par(mar = c(4,4,1,1))
par(mgp = c(2.5,1,0))
image.plot(Intermostlow, xlab = "Prey total length (cm)", ylab = "body depth ratio", cex.lab = 2, cex.axis = 1.8,
           axis.args = list(cex.axis = 1.2))
contour(Intermostlow, vfont = c("sans serif", "bold"), nlevels = 10, add = TRUE, labcex = 2)



###### for least likely to be eaten
#windows()
#mean
par( mar = c(5,6,4,2), mgp = c(3,1,0))
image.plot(Interleastavg,  xlab = "Prey total length (cm)", ylab = "Body depth ratio", cex.lab = 3, cex.axis = 2.4,
           axis.args = list(cex.axis = 1.4))
contour(Interleastavg, vfont = c("sans serif", "bold"), nlevels = 12, add = TRUE, labcex = 2.4, labtype = "bold")

#windows()

#par(mfrow = c(2,1))

#up CI
#par(mar = c(4,4,1,1))
par(mgp = c(2.5,1,0))
image.plot(Interleastup, xlab = "Prey total length (cm)", ylab = "Body depth ratio", cex.lab = 2, cex.axis = 1.8,
           axis.args = list(cex.axis = 1.2))
contour(Interleastup, vfont = c("sans serif", "bold"), nlevels = 10, add = TRUE, labcex = 2)

#low CI
#par(mar = c(4,4,1,1))
par(mgp = c(2.5,1,0))
image.plot(Interleastlow, xlab = "Prey total length (cm)", ylab = "body depth ratio", cex.lab = 2, cex.axis = 1.8,
           axis.args = list(cex.axis = 1.2))
contour(Interleastlow, vfont = c("sans serif", "bold"), nlevels = 10, add = TRUE, labcex = 2)



