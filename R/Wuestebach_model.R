library(lubridate)
library(plyr)
library(openair)
library(ggplot2)
library(plyr)
library(ggplot2)
library(raster)
library(sp)
library(rgdal)
library(graphics)
library(base)
library(colorspace)

rm(list=ls())

setwd("D:/Droesen/Master thesis/Research/Downscaling_soilmoisture")

#select some dates in preprocessing



#documentation about getting terrain attributes
weightingraster <- raster("data/kwuest.map")

# exclude all zeroes
prj_string <- "+proj=tmerc +lat_0=0 +lon_0=6 +k=1 +x_0=2500000 +y_0=0 +ellps=bessel +datum=potsdam +units=m +no_defs +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7"
weightingraster[weightingraster==0] <- NA

#plot soil moisture weighting map
colfunc1 <- colorRampPalette(c("red","orange", "yellow", "green", "blue", "darkblue","purple"))
plot(weightingraster, col=colfunc1(255), main="Soil moisture content")

#wet condition testing, subset for certain condition
moderate <- subset(SMWuest_xy, timestampto == "2009-11-01", select=c("Id", "timestampto", "daily_mean_SM", "X.coord", "Y.coord"))
write.csv(moderate, file="data/SMwuest20cm_mod")
head(moderate)

#xy columns for spatial points
xy <- moderate[4:5]
#get soil moisture data
df <- moderate[3]

#dataframe to spatial points
points <- SpatialPointsDataFrame(xy, df, proj4string = CRS(prj_string))
points1 <- SpatialPoints(points)

#plot points
colfunc2 <- colorRampPalette(c("white","darkblue"))
points$col <- colfunc2(255)[as.numeric(cut(points$daily_mean_SM, breaks = 255))]
points(points, pch=23, col=points$col)

plot(points)
library(leaflet)
?leaflet
leaflet(data = points)

m <- leaflet(points)
m <- addMarkers(points)
m
?leaflet
weightings <- extract(weightingraster, points1, sp=FALSE, df=TRUE)
df$ID<-seq.int(nrow(df))
merged<- merge(df, weightings, by.df = ID, by.weightings = ID)
head(merged)
plot(merged[,("daily_mean_SM")], merged[,("kwuest")], main="Weighting versus measured soil moisture", xlab="Measured soil moisture", ylab="Weighting")
lm(merged[,("daily_mean_SM")]~merged[,("kwuest")]) -> corWuestmod #Slope and SM, 27 september
summary(corWuestmod)$r.squared #R-squared T6 wetness    

