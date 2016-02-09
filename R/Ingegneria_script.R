library(ggplot2)

rm(list=ls())

read.csv("E:/Master thesis/Resultaat_Dsultaat/Terrain_interpolated/Ingeg_EAS.csv") -> SM_ING

#soil moisture data with wetness index
read.csv("E:/Master thesis/Resultaat_D/Ingegneria/Ingeg_SM_wetness.csv") -> SM_ing_wet

#Soil moisture data with terrain attributes
read.csv("D:/Droesen/Resultaat/Ingegneria/Ingeg_SM_all.csv") -> all_terrain

#average soil moisture values of days

colMeans(SM_ING[,(7:20)]) -> Daily_avg
View(Daily_avg)


#plot soil moisture versus terrain attribute
plot(SM_ING[,("INGEGSLOPE")],SM_ING[,("X20_02_04")], main="Soil moisture versus slope at 13-02-2004", xlab="Slope", ylab="Soil moisture")
plot(SM_ING[,("DEMING")],SM_ING[,("X13_02_04")], main="Soil moisture versus elevation", xlab="Elevation", ylab="Soil moisture")
plot(SM_ING[,("INGEGASPEC")],SM_ING[,("X13_02_04")], main="Soil moisture versus aspect", xlab="Aspect", ylab="Soil moisture")
plot(all_terrain[,("INGEG_PROF")],all_terrain[,("X20_02_04")], main="Soil moisture versus wetness index (dry)", xlab="Wetness index", ylab="Soil moisture") -> driest

#plot soil moisture versus wetness index driest and wettest measuring date
plot(SM_ing_wet[,("INGEG_WETN")],SM_ing_wet[,("X20_02_04")], main="Ingegneria - Soil moisture versus wetness index (wet)", xlab="Wetness index", ylab="Soil moisture") -> wettest
plot(SM_ing_wet[,("INGEG_WETN")],SM_ing_wet[,("X31_05_04")], main="Ingegneria - Soil moisture versus wetness index (dry)", xlab="Wetness index", ylab="Soil moisture") -> driest



#lineair regression model
lm(SM_ING[,("INGEGASPEC")]~SM_ING[,("X13_02_04")]) -> corAspectING
lm(SM_ING[,("INGEGSLOPE")]~SM_ING[,("X13_02_04")]) -> corSlopeING
lm(SM_ING[,("X10_05_04")]~SM_ING[,("INGEGASPEC")]+SM_ING[,("INGEGSLOPE")]+SM_ING[,("DEMING")],data=SM_ING) -> corMultING

#lineair regression model wetness versus SM
lm(SM_ing_wet[,("INGEG_WETN")]~SM_ing_wet[,("X20_02_04")]) -> corWIingwet
lm(SM_ing_wet[,("INGEG_WETN")]~SM_ing_wet[,("X31_05_04")]) -> corWIingdry

#R squared
summary(corWIingwet)$r.squared
summary(corWIingdry)$r.squared

lm(all_terrain[,("INGEG_MCUR")]~all_terrain[,("X31_05_04")]) -> corMcurvING
lm(all_terrain[,("INGEG_PROF")]~all_terrain[,("X31_05_04")]) -> corPRcurvING
lm(all_terrain[,("INGEG_PLAN")]~all_terrain[,("X31_05_04")]) -> corPLcurvING



summary(corMcurvING)$r.squared
summary(corPRcurvING)$r.squared
summary(corPLcurvING)$r.squared

cor(SM_ing_wet[,("INGEG_WETN")],SM_ing_wet[,("X20_02_04")])


