# Authors: Anne-Juul Welsink and Marrit Leenstra
# 18th January 2019
# Exercise 9, Geoscripting, Wageningen University 

# This project analyses how well landsat band reflectance can predict VCF tree cover.

# load libraries
if (!require("raster")) install.packages("raster")
library(raster)

# source functions
source("R/retrieveData.R")

# download and unzip data 
retrieveData("https://github.com/GeoScripting-WUR/AdvancedRasterAnalysis/raw/gh-pages/data/GewataBands.zip", "data")

# load data to global environment
fileList <- list.files(path = "data/", pattern = glob2rx('*.rda'), full.names = TRUE)
for (file in fileList){
  load(file)
}

# Build a brick containing all data
vcfGewata[vcfGewata > 100] <- NA # Remove water, cloud or cloud shadow pixels
gewata <- brick(GewataB1, GewataB2, GewataB3, GewataB4, GewataB5, GewataB7, vcfGewata)
names(gewata) <- c("band1", "band2", "band3", "band4", "band5", "band7", "VCF")

# Find borders of Gewate region
eth <- getData('GADM', country='ETH', level=3)
gwt <- eth[eth$NAME_3=="Getawa",]
gwt <- spTransform(gwt, CRSobj = crs(vcfGewata))

# apply the mask to all data
gewata <- mask(x = gewata, mask=gwt)
plotRGB(gewata, 3, 2, 1, stretch="lin", main = "RGB image of the Gewata region")

# extract all data to a data.frame
df <- as.data.frame(getValues(gewata))

# select random sample points and use as training data
sRandomGWT <- sampleRandom(gewata, na.rm=TRUE, sp=TRUE, size = 1000)
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

# plot relationship Landsat bands - VCF cover
opar <- par(mfrow=c(2, 3))
plot(VCF ~ band1, data = sRandomData, pch = ".")
plot(VCF ~ band2, data = sRandomData, pch = ".")
plot(VCF ~ band3, data = sRandomData, pch = ".")
mtext(side = 3, line = 2, "Relationship between respective landsat bands and VCF cover", adj = 1, cex = 1.1)
plot(VCF ~ band4, data = sRandomData, pch = ".")
plot(VCF ~ band5, data = sRandomData, pch = ".")
plot(VCF ~ band7, data = sRandomData, pch = ".")

# check correlation and select bands
correlation <- cor(sRandomData)
useBands <- names(correlation[1,(correlation[1,] > 0.6 & correlation[1,] < 1) | correlation[1,] < -0.6])

# create model
############################################## To do
model <-  lm(VCF ~ band2 + band3 + band5 + band7, data = sRandomData)

# predict tree cover using the linear regression model
predTC <- predict(gewata, model = model, na.rm=TRUE)
predTC[predTC < 0] <- NA # remove values smaller than 0
opar <- par(mfrow=c(1, 2))
plot(predTC)
plot(gewata$VCF)
par(mfrow = 1,1)

# difference raster
diff <- plot(gewata$VCF - predTC, main = "Difference (%) in estimated tree cover between VCF and linear model")

# calculate RMSE
dfpredTC <- as.data.frame(getValues(predTC))
dfVCF <- as.data.frame(getValues(gewata$VCF))
sqrt( mean( (dfpredTC-dfVCF)^2 , na.rm = TRUE ) )


