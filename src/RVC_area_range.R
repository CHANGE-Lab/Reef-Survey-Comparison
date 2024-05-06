########## SURVEY COMPARISON PROJECT RVC AREA RANGE ##########
########## 
##########
# This file extracts RVC (i.e. SVC) survey data from a publicly available 
# dataset and determines the range in total survey area to compare to the 
# range in area observed in our data. 
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2024-05-06
##########
##########


# rvc -> install and load the South Florida Reef Visual Census package from
# https://github.com/jeremiaheb/rvc

install.packages('devtools')
library(devtools)
devtools::install_github('jeremiaheb/rvc')
library(rvc)

# import all rvc data
rvc <- getRvcData(years = c(2014, 2015), regions = "FLA KEYS")

# import only rvc occurrence data, specifying your desired year(s), region(s), etc.
rvc_occurence <- getSampleData(years = c(2014, 2015), regions = "FLA KEYS")

# calculate survey radius 
rvc_occurence$radius <- ifelse(rvc_occurence$UNDERWATER_VISIBILITY < 7.5, 
                               rvc_occurence$UNDERWATER_VISIBILITY, 7.5)