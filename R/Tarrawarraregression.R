library(plyr)
library(ggplot2)
library(raster)
library(sp)
library(rgdal)
library(graphics)
library(base)
rm(list=ls())
setwd("D:/Droesen/Master thesis/Rscripts/Downscaling_soilmoisture")

#Read soil moisture table with terrain attributes Aspect, Slope, (3x) Curvature, Wetness index, Contributing area, relative elevation
read.csv("data/SM_Tarra_all.csv") -> SM_TAR_all

#Abstracting the measuring dates 
write.csv(subT11TARSM, file="TarT1csv1")
subset(SM_TAR_all, FIELD_1==2, select=c("FIELD_1", "FIELD_2",	"FIELD_3",	"FIELD_4", "TARWETNESS",  "TARLNCATCH",  "SLOPE",	"CURVATURE"	, "PLANCURVAT",	"PROFILECUR",	"ASPECT"))  -> subT2TARSM
subset(SM_TAR_all, FIELD_1==3, select=c("FIELD_1", "FIELD_2",  "FIELD_3",	"FIELD_4", "TARWETNESS",  "TARLNCATCH",  "SLOPE",	"CURVATURE"	, "PLANCURVAT",	"PROFILECUR",	"ASPECT"))  -> subT3TARSM
subset(SM_TAR_all, FIELD_1==4, select=c("FIELD_1", "FIELD_2",  "FIELD_3",	"FIELD_4", "TARWETNESS",  "TARLNCATCH","SLOPE",	"CURVATURE"	, "PLANCURVAT",	"PROFILECUR",	"ASPECT"))  -> subT4TARSM
subset(SM_TAR_all, FIELD_1==5, select=c("FIELD_1", "FIELD_2",  "FIELD_3",	"FIELD_4", "TARWETNESS",  "TARLNCATCH","SLOPE",	"CURVATURE"	, "PLANCURVAT",	"PROFILECUR",	"ASPECT"))  -> subT5TARSM
subset(SM_TAR_all, FIELD_1==6, select=c("FIELD_1", "FIELD_2",  "FIELD_3",	"FIELD_4", "TARWETNESS",  "TARLNCATCH","SLOPE",	"CURVATURE"	, "PLANCURVAT",	"PROFILECUR",	"ASPECT"))  -> subT6TARSM
subset(SM_TAR_all, FIELD_1==7, select=c("FIELD_1", "FIELD_2",  "FIELD_3",	"FIELD_4", "TARWETNESS",  "TARLNCATCH","SLOPE",	"CURVATURE"	, "PLANCURVAT",	"PROFILECUR",	"ASPECT"))  -> subT7TARSM
subset(SM_TAR_all, FIELD_1==8, select=c("FIELD_1", "FIELD_2",  "FIELD_3",	"FIELD_4", "TARWETNESS",  "TARLNCATCH","SLOPE",	"CURVATURE"	, "PLANCURVAT",	"PROFILECUR",	"ASPECT"))  -> subT8TARSM
subset(SM_TAR_all, FIELD_1==9, select=c("FIELD_1", "FIELD_2",  "FIELD_3",	"FIELD_4", "TARWETNESS",  "TARLNCATCH","SLOPE",	"CURVATURE"	, "PLANCURVAT",	"PROFILECUR",	"ASPECT"))  -> subT9TARSM
subset(SM_TAR_all, FIELD_1==10, select=c("FIELD_1", "FIELD_2",  "FIELD_3",	"FIELD_4", "TARWETNESS", "TARLNCATCH", "SLOPE",	"CURVATURE"	, "PLANCURVAT",	"PROFILECUR",	"ASPECT"))  -> subT10TARSM
subset(SM_TAR_all, FIELD_1==11, select=c("FIELD_1", "FIELD_2",  "FIELD_3",	"FIELD_4"))  -> subT11TARSM
subset(SM_TAR_all, FIELD_1==12, select=c("FIELD_1", "FIELD_2",  "FIELD_3",	"FIELD_4", "TARWETNESS",  "TARLNCATCH","SLOPE",	"CURVATURE"	, "PLANCURVAT",	"PROFILECUR",	"ASPECT"))  -> subT12TARSM
subset(SM_TAR_all, FIELD_1==13, select=c("FIELD_1", "FIELD_2",  "FIELD_3",	"FIELD_4", "TARWETNESS", "TARLNCATCH", "SLOPE",	"CURVATURE"	, "PLANCURVAT",	"PROFILECUR",	"ASPECT"))  -> subT13TARSM

#Scatterplot of soil moisture versus wetness index (high and low spatial organisation)
plot(subT11TARSM[,("FIELD_4")]~subT11TARSM[,("TARWETNESS")], main="Soil moisture versus wetness index (wet)", xlab="Wetness index", ylab="Soil moisture") #wetness
plot(subT6TARSM[,("FIELD_4")]~subT6TARSM[,("TARWETNESS")], main="Soil moisture versus wetness index (dry)", xlab="Wetness index", ylab="Soil moisture") #wetness
data.frame(VAR1=subT11TARSM[,("TARLNCATCH")], VAR2=subT11TARSM[,("FIELD_4")]) -> twovectors
abline(lm(subT11TARSM[,("TARLNCATCH")]~subT11TARSM[,("FIELD_4")]) -> comUpslopeTARmod, data=twovectors, col='red')
legend("topright", bty="n", legend=paste("R2 is", format(summary(fit)$adj.r.squared, digits=4)))

data.frame(Upslope_area = subT11TARSM[,("TARLNCATCH")], Soil_moisture=subT11TARSM[,("FIELD_4")]) -> testforggplot
ds <- ddply(testforggplot, .(Upslope_area), summarise, mean = mean(Soil_moisture), sd = sd(Soil_moisture))
ggplot(testforggplot, aes(x = Upslope_area, y = Soil_moisture)) +  geom_point() + geom_smooth(method=lm, se=TRU) + geom_point(data = ds, aes(y = mean),
             colour = "black", size = 1) +  ggtitle("Soil moisture versus upslope area")+ theme(plot.title=element_text(size=15, vjust=3, face="bold")) + xlab("Upslope area")+ylab("Soil moisture")

ggplot(subT11TARSM, aes(x=xvar, y=yvar)) +
  geom_point(shape=1)
#Scatterplot of soil moisture versus catchment area (high and low spatial organisation)
plot(subT11TARSM[,("FIELD_4")]~subT11TARSM[,("TARLNCATCH")], main="Soil moisture versus (Ln) contributing area (wet)", xlab="contributing area", ylab="Soil moisture") #wetness
plot(subT6TARSM[,("FIELD_4")]~subT6TARSM[,("TARLNCATCH")], main="Soil moisture versus (Ln) contributing area (dry)", xlab="contributing area", ylab="Soil moisture") #wetness

#Scatterplot of soil moisture versus plan curvature (high and low spatial organisation)
plot(subT11TARSM[,("FIELD_4")]~subT11TARSM[,("PLANCURVAT")], main="Soil moisture versus plan curvature (wet)", xlab="plan curvature", ylab="Soil moisture") #wetness
plot(subT6TARSM[,("FIELD_4")]~subT6TARSM[,("PLANCURVAT")], main="Soil moisture versus plan curvature (dry)", xlab="plan curvature", ylab="Soil moisture") #wetness

#regression model of soil moisture and terrain attribute (high connectivity)
lm(subT11TARSM[,("SLOPE")]~subT11TARSM[,("FIELD_4")]) -> comSlopeTARhigh #Slope and SM, 27 september
lm(subT11TARSM[,("CURVATURE")]~subT11TARSM[,("FIELD_4")]) -> comCurvTARhigh #Slope and SM, 27 september
lm(subT11TARSM[,("PLANCURVAT")]~subT11TARSM[,("FIELD_4")]) -> comPlancurvTARhigh #Slope and SM, 27 september
lm(subT11TARSM[,("PROFILECUR")]~subT11TARSM[,("FIELD_4")]) -> comProfcurvTARhigh #Slope and SM, 27 september
lm(subT11TARSM[,("ASPECT")]~subT11TARSM[,("FIELD_4")]) -> comAspectTARhigh #Slope and SM, 27 september
lm(subT11TARSM[,("TARWETNESS")]~subT11TARSM[,("FIELD_4")]) -> comWITARhigh #Slope and SM, 27 september
lm(subT11TARSM[,("TARLNCATCH")]~subT11TARSM[,("FIELD_4")]) -> comUpslopeTARhigh #Slope and SM, 27 september

#regression model of soil moisture and terrain attribute (moderate connectivity)
lm(subT7TARSM[,("SLOPE")]~subT7TARSM[,("FIELD_4")]) -> comSlopeTARmod #Slope and SM, 27 september
lm(subT7TARSM[,("CURVATURE")]~subT7TARSM[,("FIELD_4")]) -> comCurvTARmod #Slope and SM, 27 september
lm(subT7TARSM[,("PLANCURVAT")]~subT7TARSM[,("FIELD_4")]) -> comPlancurvTARmod #Slope and SM, 27 september
lm(subT7TARSM[,("PROFILECUR")]~subT7TARSM[,("FIELD_4")]) -> comProfcurvTARmod #Slope and SM, 27 september
lm(subT7TARSM[,("ASPECT")]~subT7TARSM[,("FIELD_4")]) -> comAspectTARmod #Slope and SM, 27 september
lm(subT7TARSM[,("TARWETNESS")]~subT7TARSM[,("FIELD_4")]) -> comWITARmod #Slope and SM, 27 september
lm(subT7TARSM[,("TARLNCATCH")]~subT7TARSM[,("FIELD_4")]) -> comUpslopeTARmod #Slope and SM, 27 september

#regression model of soil moisture and terrain attribute (low connectivity)
lm(subT6TARSM[,("SLOPE")]~subT6TARSM[,("FIELD_4")]) -> comSlopeTARlow #Slope and SM, 27 september
lm(subT6TARSM[,("CURVATURE")]~subT6TARSM[,("FIELD_4")]) -> comCurvTARlow #Slope and SM, 27 september
lm(subT6TARSM[,("PLANCURVAT")]~subT6TARSM[,("FIELD_4")]) -> comPlancurvTARlow #Slope and SM, 27 september
lm(subT6TARSM[,("PROFILECUR")]~subT6TARSM[,("FIELD_4")]) -> comProfcurvTARlow #Slope and SM, 27 september
lm(subT6TARSM[,("ASPECT")]~subT6TARSM[,("FIELD_4")]) -> comAspectTARlow #Slope and SM, 27 september
lm(subT6TARSM[,("TARWETNESS")]~subT6TARSM[,("FIELD_4")]) -> comWITARlow #Slope and SM, 27 september
lm(subT6TARSM[,("TARLNCATCH")]~subT6TARSM[,("FIELD_4")]) -> comUpslopeTARlow #Slope and SM, 27 september

                                    

#regression model comparison
SMcomparewet[,("FIELD_4")]~subTARSMcomparewet[,("X1448285209")]



#plot R squared 27/09/1995 - high connectivity
summary(comSlopeTARhigh)$r.squared #R-squared T6 wetness                                      
summary(comCurvTARhigh)$r.squared #R-squared T7 wetness
summary(comPlancurvTARhigh)$r.squared #R-squared T7 wetness
summary(comProfcurvTARhigh)$r.squared #R-squared T7 wetness
summary(comAspectTARhigh)$r.squared #R-squared T7 wetness
summary(comWITARhigh)$r.squared #R-squared T7 wetness
summary(comUpslopeTARhigh)$r.squared #R-squared T7 wetness
                                       
#plot R squared 20/09/1995 - moderate connectivity
summary(comSlopeTARmod)$r.squared #R-squared T6 wetness                                      
summary(comCurvTARmod)$r.squared #R-squared T7 wetness
summary(comPlancurvTARmod)$r.squared #R-squared T7 wetness
summary(comProfcurvTARmod)$r.squared #R-squared T7 wetness
summary(comAspectTARmod)$r.squared #R-squared T7 wetness
summary(comWITARmod)$r.squared #R-squared T7 wetness
summary(comUpslopeTARmod)$r.squared #R-squared T7 wetness
                                                
#plot R squared 14/02/1995 - low connectivity
summary(comSlopeTARlow)$r.squared #R-squared T6 wetness                                      
summary(comCurvTARlow)$r.squared #R-squared T7 wetness
summary(comPlancurvTARlow)$r.squared #R-squared T7 wetness
summary(comProfcurvTARlow)$r.squared #R-squared T7 wetness
summary(comAspectTARlow)$r.squared #R-squared T7 wetness
summary(comWITARlow)$r.squared #R-squared T7 wetness
summary(comUpslopeTARlow)$r.squared #R-squared T7 wetness

#Scatterplot of soil moisture versus terrain attribute comparison scales
plot(SM_TAR_scales[,("FIELD_4_1")]~SM_TAR_scales[,("X1448549405")], main="Soil moisture versus contributing area (5)", xlab="Contributing area", ylab="Soil moisture", xlim=c(3,12), ylim=c(0,60)) #wetness
plot(SM_TAR_scales[,("FIELD_4_1")]~SM_TAR_scales[,("X1448549405_1")], main="Soil moisture versus contributing area (10)", xlab="Contributing area", ylab="Soil moisture", xlim=c(3,12), ylim=c(0,60)) #wetness
plot(SM_TAR_scales[,("FIELD_4_1")]~SM_TAR_scales[,("X1448549405_2")], main="Soil moisture versus contributing area (25)", xlab="Contributing area", ylab="Soil moisture", xlim=c(3,12), ylim=c(0,60)) #wetness
#regression model for soil moisture and catchment area 5, 10 , 25 meters
lm(SM_TAR_scales[,("X1448549405")]~SM_TAR_scales[,("FIELD_4_1")]) -> comUpslope5m #Slope and SM, 27 september
lm(SM_TAR_scales[,("X1448549405_1")]~SM_TAR_scales[,("FIELD_4_1")]) -> comUpslope10m #Slope and SM, 27 september
lm(SM_TAR_scales[,("X1448549405_2")]~SM_TAR_scales[,("FIELD_4_1")]) -> comUpslope25m #Slope and SM, 27 september
#r squared soil moisture and catchment area 5,10,25m
summary(comUpslope5m)$r.squared #R-squared T6 wetness                                      
summary(comUpslope10m)$r.squared #R-squared T6 wetness                                      
summary(comUpslope25m)$r.squared #R-squared T6 wetness 

#plot correlations
cor(subT7TARSM[,("TARLNCATCH")],subT7TARSM[,("FIELD_4")])
lm(SM_TAR[,("FIELD_4")]~SM_TAR[,("TARRASLOPE")]) -> corSlope
                                    
#selecting wet versus dry conditions/connectivity
subT6TARSM versus subT7TARSM
                                     
#multiple regression model (wetness index and curvature/aspect)

lm(subT11TARSM[,("FIELD_4")]~subT11TARSM[,("TARWETNESS")]+subT11TARSM[,("ASPECT")],data=SM_TAR_all) -> corMultING
summary(corMultING)$r.squared #R-squared T7 wetness
                                                