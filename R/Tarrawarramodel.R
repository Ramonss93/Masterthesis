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

#stack all terrain attributes
list.files(pattern = glob2rx('LC8*.grd'), full.names = TRUE)
terrainatts <- list.files(path = 'D:/Droesen/Master thesis/Research/Resultaat/Wuestebach', pattern = glob2rx('*.tif'), full.names = TRUE) 
?list.files
#Average per time
subT11TARSM <- subset(SM_TAR_all, FIELD_1==11, select=c("FIELD_1", "FIELD_2",  "FIELD_3",	"FIELD_4"))


## preprocessing Tarra

#script for selecting the dates to measure 
listofmeasures <- split(SM_TAR_all, SM_TAR_all$FIELD_1)

i <- 1
for (day in listofmeasures) {
  listofmeasures[[i]]$sm_avg <- mean(listofmeasures[[i]]$FIELD_4, na.rm = TRUE)
  listofmeasures[[i]]$sm_var <- var(listofmeasures[[i]]$FIELD_4, na.rm = TRUE)
  listofmeasures[[i]]$sm_sd <- sd(listofmeasures[[i]]$FIELD_4, na.rm = TRUE)
  i <- i + 1
}

oneframe <- do.call(rbind.data.frame, listofmeasures)

maximumvar <- subset(oneframe, oneframe$sm_var == max(oneframe$sm_var))
minumumavg <- subset(oneframe, oneframe$sm_avg == min(oneframe$sm_avg))
maximumavg <- subset(oneframe, oneframe$sm_avg == max(oneframe$sm_avg))


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
