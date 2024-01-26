###sample code for iris


################peter is a fool###################
n=100 #number of points in matrix row this will give us a matrix of 10,000
#creating new value objects with the min and max of the variable we want 
#as our axis--- should be continuous, and then merge together
mindensityT=min(time_per_fish$SiteDens1000)
maxdensityT=max(time_per_fish$SiteDens1000)
densityT=seq(mindensityT,maxdensityT,length.out=n)

minareaT=min(time_per_fish$Site_area_m2)
maxareaT=max(time_per_fish$Site_area_m2)
areaT=seq(minareaT,maxareaT,length.out=n)


#creating new value objects with the average of the variable, to hold static
lfsizeT= rep(mean(time_per_fish$Lionfish_size_TL),n^2)
depthT=rep(mean(time_per_fish$Depth_ft),n^2)
gorgT=rep(mean(time_per_fish$AvgGorg),n^2)

# creating new value objects with all the levels of the categorical variables
timeofdayTC=rep("Crepuscular",n^2)
timeofdayTM=rep("midday",n^2)

experienceTH=rep("High",n^2)
experienceTM=rep("Medium",n^2)
experienceTL=rep("Low",n^2)
experienceTN=rep("None",n^2)

#Create the new matrices with only the variable that were significant in the models
# column name of what you want the axis to be ( coral, size) rep (value object created, each n)
#column name from original data frames = value object just created
# Must repeat for every shape/color combo for 16 total panels

#silvering
#make one new data frame for each shape
new.dataTCexp=data.frame(Site_area_m2=rep(areaT,each=n),  SiteDens1000=rep(densityT, n),
                         Depth_ft=depthT,  AvgGorg=gorgT, Lionfish_size_TL = lfsizeT,  
                         Cap_exp_new=experienceTH, TOD=timeofdayTC)

new.dataTMexp=data.frame(Site_area_m2=rep(areaT,each=n),  SiteDens1000=rep(densityT, n),
                         Depth_ft=depthT,  AvgGorg=gorgT, Lionfish_size_TL = lfsizeT,  
                         Cap_exp_new=experienceTH, TOD=timeofdayTM)


#Medium
new.dataTCmed=data.frame(Site_area_m2=rep(areaT,each=n),  SiteDens1000=rep(densityT, n),
                         Depth_ft=depthT,  AvgGorg=gorgT, Lionfish_size_TL = lfsizeT,  
                         Cap_exp_new=experienceTM, TOD=timeofdayTC)

new.dataTMemed=data.frame(Site_area_m2=rep(areaT,each=n),  SiteDens1000=rep(densityT, n),
                          Depth_ft=depthT,  AvgGorg=gorgT, Lionfish_size_TL = lfsizeT,  
                          Cap_exp_new=experienceTM, TOD=timeofdayTM)


#Low
new.dataTClow=data.frame(Site_area_m2=rep(areaT,each=n),  SiteDens1000=rep(densityT, n),
                         Depth_ft=depthT,  AvgGorg=gorgT, Lionfish_size_TL = lfsizeT,  
                         Cap_exp_new=experienceTL, TOD=timeofdayTC)

new.dataTMlow=data.frame(Site_area_m2=rep(areaT,each=n),  SiteDens1000=rep(densityT, n),
                         Depth_ft=depthT,  AvgGorg=gorgT, Lionfish_size_TL = lfsizeT,  
                         Cap_exp_new=experienceTL, TOD=timeofdayTM)

#Novice
new.dataTCnov=data.frame(Site_area_m2=rep(areaT,each=n),  SiteDens1000=rep(densityT, n),
                         Depth_ft=depthT,  AvgGorg=gorgT, Lionfish_size_TL = lfsizeT,  
                         Cap_exp_new=experienceTN, TOD=timeofdayTC)

new.dataTMnov=data.frame(Site_area_m2=rep(areaT,each=n),  SiteDens1000=rep(densityT, n),
                         Depth_ft=depthT,  AvgGorg=gorgT, Lionfish_size_TL = lfsizeT, 
                         Cap_exp_new=experienceTN, TOD=timeofdayTM)


##cexperience high, this is where you bring in your model again you will have to
#do this for every combination (16 total), 
#my model is newtime.true, 
#new.dataTCexp is my new dataframe
#newdata is a created column with predictions based on your model output
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