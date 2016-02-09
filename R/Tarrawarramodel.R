library(plyr)
library(ggplot2)
library(raster)
library(sp)
library(rgdal)
library(graphics)
library(base)
library(dplyr)

rm(list=ls())
setwd("D:/Droesen/Master thesis/Research/Downscaling_soilmoisture")

#Read soil moisture table with terrain attributes Aspect, Slope, (3x) Curvature, Wetness index, Contributing area, relative elevation
read.csv("data/SM_Tarra_all.csv") -> SM_TAR_all

#Average per time
subset(SM_TAR_all, FIELD_1==11, select=c("FIELD_1", "FIELD_2",  "FIELD_3",	"FIELD_4"))  -> subT11TARSM

listofmeasures <- split(SM_TAR_all, SM_TAR_all$FIELD_1)
lapply(listofmeasures$FIELD_4, mean)
listofmeasures$FIELD_4[[1]]
library(data.table)

agg <- aggregate(SM_TAR_all, FUN=mean, data = SM_TAR_all$FIELD_4)
i <- 1
for (day in listofmeasures) {
  SM_TAR_all$sm_avg <- mean(day$FIELD_4)
  i <- i + 1
}

unsplit(lapply(split(SM_TAR_all, SM_TAR_all$FIELD_1), mean))

listofmeasures[[1]]
head(listofmeasures)

mean(listofmeasures[[1]]$FIELD_4)

#documentation about getting terrain attributes
weightingraster <- raster("data/ktar.map")

# exclude all zeroes
prj_string <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

weightingraster[weightingraster==0] <- NA

#plot soil moisture weighting map
colfunc1 <- colorRampPalette(c("red","orange", "yellow", "green", "blue", "darkblue","purple"))
plot(weightingraster, col=colfunc1(255), main="Soil moisture content")

#wet condition testing
subset(SM_TAR_all, FIELD_1==11, select=c("FIELD_1", "FIELD_2",  "FIELD_3",	"FIELD_4"))  -> subT11TARSM
write.csv(subT11TARSM, "T11subset.csv")
xy <- subT11TARSM[2:3]
df <- subT11TARSM[4]

points <- SpatialPointsDataFrame(xy, df, proj4string = CRS(prj_string))
points1 <- SpatialPoints(points)

plot(points)

weightings <- extract(weightingraster, points1, sp=FALSE, df=TRUE)
df$ID<-seq.int(nrow(df))
merged<- merge(df, weightings, by.df = ID, by.weightings = ID)

plot(merged[,("FIELD_4")], merged[,("ktar")], main="Weighting versus measured soil moisture", xlab="Measured soil moisture", ylab="Weighting")
lm(merged[,("FIELD_4")]~merged[,("ktar")]) -> corTarT11 #Slope and SM, 27 september
summary(corTarT11)$r.squared #R-squared T6 wetness    
