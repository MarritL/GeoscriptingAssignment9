# Authors: Anne-Juul Welsink and Marrit Leenstra
# 18th January 2019
# Exercise 9, Geoscripting, Wageningen University 

# This project analyses how well landsat band reflectance can predict VCF tree cover.

# load libraries
if (!require("biglm")) install.packages("sp") 
if (!require("raster")) install.packages("raster")
library(biglm)
library(raster)

# source functions
source("R/....R")

# download and unzip data 
retrieveData("...", "data")

# load data to global environment
load("data/GewataB1.rda")
load("data/GewataB5.rda")
load("data/GewataB7.rda")
## Band 6 (thermal infra-red) will be excluded from this exercise
## Build a brick containing all data
alldata <- brick(GewataB1, GewataB2, GewataB3, GewataB4, GewataB5, GewataB7, vcfGewata)
names(alldata) <- c("band1", "band2", "band3", "band4", "band5", "band7", "VCF")

## Find borders of Gewate region
eth <- getData('GADM', country='ETH', level=3)
gwt <- eth[eth$NAME_3=="Getawa",]
gwt <- spTransform(gwt, CRSobj = crs(vcfGewata))

#apply the mask to all data
alldata <- mask(x = alldata, mask=gwt)
plotRGB(alldata, 3, 2, 1, stretch="lin", main = "RGB image of the Gewata region")

## Extract all data to a data.frame
df <- as.data.frame(getValues(alldata))

## select random sample points (choose a size what you think is proper)
## and use as training data
sRandomGWT <- sampleRandom(alldata, na.rm=TRUE, sp=TRUE, size = 1000)
sRandomData <- data.frame(VCF = sRandomGWT@data[[7]], 
                          band1 = sRandomGWT@data[[1]],
                          band2 = sRandomGWT@data[[2]],
                          band3 = sRandomGWT@data[[3]],
                          band4 = sRandomGWT@data[[4]],
                          band5 = sRandomGWT@data[[5]],
                          band7 = sRandomGWT@data[[6]],
                          latitude = sRandomGWT@coords[,'y'],
                          longitude = sRandomGWT@coords[,'x'])
plot(gwt, main="The randomly selected sampling points")
plot(sRandomGWT, add = TRUE, col = "red")
