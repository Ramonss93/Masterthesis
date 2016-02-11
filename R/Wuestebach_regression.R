library(lubridate)
library(plyr)
library(openair)
library(ggplot2)
library(xts)
library(raster)

rm(list=ls())

setwd("D:/Droesen/Master thesis/Research/Downscaling_soilmoisture")

SMWuest_xy <- read.csv("data/Wüstebach/SMwuest_50cm.csv")

#script for selecting the dates to measure 
listofmeasures <- split(SMWuest_xy, SMWuest_xy$timestampto)




i <- 1
for (day in listofmeasures) {
  listofmeasures[[i]]$sm_avg <- mean(listofmeasures[[i]]$daily_mean_SM, na.rm = TRUE)
  listofmeasures[[i]]$sm_var <- var(listofmeasures[[i]]$daily_mean_SM, na.rm = TRUE)
  listofmeasures[[i]]$sm_sd <- sd(listofmeasures[[i]]$daily_mean_SM, na.rm = TRUE)
  i <- i + 1
}
head(listofmeasures[[1]])
oneframe <- do.call(rbind.data.frame, listofmeasures)

countdays <- ddply(oneframe,~timestampto,summarise,distinct_orders=length(unique(Id)))

head(countdays)

datescorrect <- countdays[countdays$distinct_orders > 130,]



merged <- merge(oneframe, datescorrect, by="timestampto")

#select year 2011
n = length(merged$sm_var)
sort(merged$sm_var,partial=n-450)[n-450]

#select driest and wettest
maximumvar <- subset(merged, merged$sm_var == max(merged$sm_var))
minumumavg <- subset(merged, merged$sm_avg == min(merged$sm_avg))
maximumavg <- subset(merged, merged$sm_avg == max(merged$sm_avg))

head(maximumavg)

write.csv(maximumvar, file = "temp/WuestMaxVar50.csv")

write.csv(minumumavg, file = "temp/WuestMinAvg50.csv")

write.csv(maximumavg, file = "temp/WuestMaxAvg50.csv")


plot(oneframe$timestampto, oneframe$sm_avg)

# terrain attributes (tif files in data folder)
terrainatts <- list.files(path = 'D:/Droesen/Master thesis/Research/Resultaat/Wuestebach', pattern = glob2rx('*.tif'), full.names = TRUE) 
terrainstack <- stack(terrainatts[2:10])



#unnecessary to plot dates
#SMWuest_xy$timestampto <- as.Date( as.character(SMWuest_xy$timestampto), "%Y-%m-%d")
prj_string <- "+proj=tmerc +lat_0=0 +lon_0=6 +k=1 +x_0=2500000 +y_0=0 +ellps=bessel +datum=potsdam +units=m +no_defs +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7"
subset(SMWuest_xy, timestampto == "2009-09-01", select=c("Id", "timestampto", "daily_mean_SM", "X.coord", "Y.coord")) -> SMWUEST_1min

xy <- maximumvar[6:7]
df <- maximumvar[5]

points <- SpatialPointsDataFrame(xy, df, proj4string = CRS(prj_string))
points1 <- SpatialPoints(points)
terrainpoints <- extract(terrainstack, points, sp=TRUE)

mergedpoints <- merge(points, terrainpoints)
frame <- data.frame(mergedpoints)
lm(frame[,("Wuest_carea")]~frame[,("daily_mean_SM")]) -> comWIwuesthigh #Slope and SM, 27 september

plot(frame[1], frame[3])

SM_wuest_mod$residModProfileC <- resid(WmodPprofileCurv)

write.csv(SM_wuest_mod, file="D:/Droesen/Resultaat/Wuestebach/SM_Wuest_residuals")

v1 <- variog(coords = ozone[,3:4], data = ozone[,2], breaks = breaks)


subset(SMWuest_xy, timestampto == "2009-11-01", select=c("Id", "timestampto", "daily_mean_SM", "X.coord", "Y.coord")) -> SMWUEST_1max
subset(SMWuest_xy, timestampto == "2011-01-03", select=c("Id", "timestampto", "daily_mean_SM", "X.coord", "Y.coord")) -> SMWUEST_1mod
subset(SMWuest_xy, timestampto == "2011-05-06", select=c("Id", "timestampto", "daily_mean_SM", "X.coord", "Y.coord")) -> SMWUEST_1min
subset(SMWuest_xy, timestampto == "2009-10-04", select=c("Id", "timestampto", "daily_mean_SM", "X.coord", "Y.coord")) -> SMWUEST_dry
subset(SMWuest_xy, timestampto == "2009-10-13", select=c("Id", "timestampto", "daily_mean_SM", "X.coord", "Y.coord")) -> SMWUEST_wet2
subset(SMWuest_xy, timestampto == "2009-10-15", select=c("Id", "timestampto", "daily_mean_SM", "X.coord", "Y.coord")) -> SMWUEST_wet1
subset(SMWuest_xy, timestampto == "2009-12-02", select=c("Id", "timestampto", "daily_mean_SM", "X.coord", "Y.coord")) -> SMWUEST_moderate
subset(SMWuest_xy, timestampto == "2011-05-28", select=c("Id", "timestampto", "daily_mean_SM", "X.coord", "Y.coord")) -> SMWUEST_dry28052011
subset(SMWuest_xy, timestampto == "2012-01-01", select=c("Id", "timestampto", "daily_mean_SM", "X.coord", "Y.coord")) -> SMWUEST_wet01012012

subset(SMWuest_xy, timestampto == "2010-01-20", select=c("Id", "timestampto", "daily_mean_SM", "X.coord", "Y.coord")) -> SMWUEST20_wet20012010

write.csv(SMWUEST20_wet20012010, 'SMWUEST_wet20012012')

#Wet condition
01-01-2010
#for each timestamp

write.csv(SMWUEST_dry, 'data/SMWUEST_moderate')
write.csv(SMWUEST_wet2, 'SMWUEST_wet2')
write.csv(SMWUEST_wet1, 'SMWUEST_wet1')
write.csv(SMWUEST_moderate, 'SMWUEST_moderate')
write.csv(SMWUEST_dry28052011, 'SMWUEST_dry28052011')
write.csv(SMWUEST_wet01012012, 'SMWUEST_wet01012012')




write.csv(SMWuest_xy, 'testfloat')

read.csv("D:/Droesen/Resultaat/Wuestebach/Wuest_all_max.csv", header=TRUE) -> SM_wuest_max
read.csv("D:/Droesen/Resultaat/Wuestebach/Wuest_all_mod.csv", header=TRUE) -> SM_wuest_mod
read.csv("D:/Droesen/Resultaat/Wuestebach/Wuest_all_min.csv", header=TRUE) -> SM_wuest_min

read.csv("D:/Droesen/Resultaat/Wuestebach/Wuest_lnCA.csv", header=TRUE) -> SM_wuest_lnCA

read.csv("D:/Droesen/Resultaat/Wuestebach/Wuest_WI_Bicubic_max.csv", header=TRUE) -> SM_wuest_Bicubic
read.csv("D:/Droesen/Resultaat/Wuestebach/Wuest_WI_bilinear_max.csv", header=TRUE) -> SM_wuest_Bilinear
read.csv("D:/Droesen/Resultaat/Wuestebach/Wuest_WI_Bspline_max.csv", header=TRUE) -> SM_wuest_Bspline
read.csv("D:/Droesen/Resultaat/Wuestebach/Wuest_WI_IDW_max.csv", header=TRUE) -> SM_wuest_IDW

read.csv("D:/Droesen/Resultaat/Wuestebach/Wuest_all_02122009.csv", header=TRUE) -> SM_wuest_02122009
read.csv("D:/Droesen/Resultaat/Wuestebach/Wuest_wet_04102009.csv", header=TRUE) ->  SM_wuest_wet_04102009
read.csv("D:/Droesen/Resultaat/Wuestebach/Wuest_mod_02122009.csv", header=TRUE) -> SM_wuest_mod_02122009

read.csv("D:/Droesen/Resultaat/Wuestebach/Wuest_wet_28052011.csv", header=TRUE) -> SM_wuest_dry_28052011

read.csv("D:/Droesen/Resultaat/Wuestebach/Wuest_all_wet2.csv", header=TRUE) -> SM_wuest_wet2

read.csv("D:/Droesen/Resultaat/Wuestebach/Wuest_wet_20012010.csv", header=TRUE) -> SM_wuest_high

read.csv("D:/Droesen/Resultaat/Wuestebach/Wuest_moderate.csv", header=TRUE) -> SM_wuest_moderate_wet
read.csv("D:/Droesen/Resultaat/Wuestebach/Wuest_wet_01012012.csv", header=TRUE) -> SM_wuest_01012012

lm(SM_wuest_moderate_wet[,("DAILY_MEAN")]~SM_wuest_moderate_wet[,("WETNESSIND")]) -> comWIwuesthigh #Slope and SM, 27 september
summary(comWIwuesthigh)$r.squared #R-squared T7 wetness                                      

plot(SM_wuest_01012012[,("WETNESSIND")],SM_wuest_01012012[,("DAILY_MEAN")])

#Scatterplots
data.frame(WI = SM_wuest_max[,("WUEST_MOD.")], Soil_moisture=SM_wuest_max[,("DAILY_MEAN")]) -> plotWImax
ds <- ddply(plotWImax, .(WI), summarise, mean = mean(Soil_moisture), sd = sd(Soil_moisture))
ggplot(plotWImax, aes(x = WI, y = Soil_moisture)) +  geom_point() + geom_smooth(method=lm, se=TRUE) + geom_point(data = ds, aes(y = mean),
                                                                                                                              colour = "black", size = 1) +  ggtitle("Soil moisture versus WI")+ theme(plot.title=element_text(size=15, vjust=3, face="bold")) + xlab("WI")+ylab("Soil moisture")
CATCHMENTS  ASPECT	LNWUESTCAR	WETNESSIND	SLOPE	CURVATURE	CATCHMENTA	PLANCURVAT	PROFILECUR	MODIFIEDCA
#Scatterplot
plot(SM_wuest_high[,("DAILY_MEAN")]~SM_wuest_high[,("CATCHMENTS")], main="Soil moisture versus plan curvature (dry)", xlab="modified catchment area", ylab="Soil moisture") #wetness
plot(SM_wuest_high[,("DAILY_MEAN")]~SM_wuest_high[,("ASPECT")], main="Soil moisture versus plan curvature (dry)", xlab="modified catchment area", ylab="Soil moisture") #wetness
plot(SM_wuest_high[,("DAILY_MEAN")]~SM_wuest_high[,("LNWUESTCAR")], main="Soil moisture versus plan curvature (wet)", xlab="catchment area", ylab="Soil moisture") #wetness
plot(SM_wuest_high[,("DAILY_MEAN")]~SM_wuest_high[,("WETNESSIND")], main="Soil moisture versus plan curvature (dry)", xlab="wetness index", ylab="Soil moisture") #wetness
plot(SM_wuest_high[,("DAILY_MEAN")]~SM_wuest_high[,("SLOPE")], main="Soil moisture versus plan curvature (dry)", xlab="modified catchment area", ylab="Soil moisture") #wetness
plot(SM_wuest_high[,("DAILY_MEAN")]~SM_wuest_high[,("CURVATURE")], main="Soil moisture versus plan curvature (dry)", xlab="modified catchment area", ylab="Soil moisture") #wetness
plot(SM_wuest_high[,("DAILY_MEAN")]~SM_wuest_high[,("PLANCURVAT")], main="Soil moisture versus plan curvature (dry)", xlab="modified catchment area", ylab="Soil moisture") #wetness
plot(SM_wuest_high[,("DAILY_MEAN")]~SM_wuest_high[,("PROFILECUR")], main="Soil moisture versus plan curvature (dry)", xlab="modified catchment area", ylab="Soil moisture") #wetness

plot(SM_wuest_mod[,("DAILY_MEAN")]~SM_wuest_mod[,("WUEST_ASPE")], main="Soil moisture versus plan curvature (dry)", xlab="modified catchment area", ylab="Soil moisture") #wetness
plot(SM_wuest_mod[,("DAILY_MEAN")]~SM_wuest_mod[,("WUEST_MCUR")], main="Soil moisture versus plan curvature (dry)", xlab="modified catchment area", ylab="Soil moisture") #wetness
plot(SM_wuest_mod[,("DAILY_MEAN")]~SM_wuest_mod[,("WUEST_WI.T")], main="Soil moisture versus plan curvature (dry)", xlab="modified catchment area", ylab="Soil moisture") #wetness
plot(SM_wuest_mod[,("DAILY_MEAN")]~SM_wuest_mod[,("WUEST_CARE")], main="Soil moisture versus plan curvature (dry)", xlab="modified catchment area", ylab="Soil moisture") #wetness
plot(SM_wuest_mod[,("DAILY_MEAN")]~SM_wuest_mod[,("WUEST_MOD.")], main="Soil moisture versus plan curvature (dry)", xlab="modified catchment area", ylab="Soil moisture") #wetness
plot(SM_wuest_mod[,("DAILY_MEAN")]~SM_wuest_mod[,("WUEST_SLOP")], main="Soil moisture versus plan curvature (dry)", xlab="modified catchment area", ylab="Soil moisture") #wetness
plot(SM_wuest_mod[,("DAILY_MEAN")]~SM_wuest_mod[,("WUEST_PRCU")], main="Soil moisture versus plan curvature (dry)", xlab="modified catchment area", ylab="Soil moisture") #wetness
plot(SM_wuest_mod[,("DAILY_MEAN")]~SM_wuest_mod[,("WUEST_PLCU")], main="Soil moisture versus plan curvature (dry)", xlab="modified catchment area", ylab="Soil moisture") #wetness

plot(SM_wuest_min[,("DAILY_MEAN")]~SM_wuest_min[,("timestampto")])
plot(SM_wuest_lnCA[,("DAILY_MEAN")]~SM_wuest_lnCA[,("LNWUESTCAR")], main="Soil moisture versus plan curvature (dry)", xlab="modified catchment area", ylab="Soil moisture") #wetness


#graphs for regression Wuestebach with R squared
plot(SM_wuest_high[,("DAILY_MEAN")]~SM_wuest_high[,("WUEST_ASPE")], main="Soil moisture versus plan curvature (dry)", xlab="modified catchment area", ylab="Soil moisture") #wetness
plot(SM_wuest_high[,("DAILY_MEAN")]~SM_wuest_high[,("WUEST_MCUR")], main="Soil moisture versus plan curvature (wet)", xlab="catchment area", ylab="Soil moisture") #wetness
plot(SM_wuest_high[,("DAILY_MEAN")]~SM_wuest_high[,("WETNESSIND")], main="Soil moisture versus plan curvature (dry)", xlab="wetness index", ylab="Soil moisture") #wetness
plot(SM_wuest_high[,("DAILY_MEAN")]~SM_wuest_high[,("SLOPE")], main="Soil moisture versus plan curvature (dry)", xlab="modified catchment area", ylab="Soil moisture") #wetness
plot(SM_wuest_high[,("DAILY_MEAN")]~SM_wuest_high[,("CURVATURE")], main="Soil moisture versus plan curvature (dry)", xlab="modified catchment area", ylab="Soil moisture") #wetness
plot(SM_wuest_high[,("DAILY_MEAN")]~SM_wuest_high[,("PLANCURVAT")], main="Soil moisture versus plan curvature (dry)", xlab="modified catchment area", ylab="Soil moisture") #wetness
plot(SM_wuest_high[,("DAILY_MEAN")]~SM_wuest_high[,("PROFILECUR")], main="Soil moisture versus plan curvature (dry)", xlab="modified catchment area", ylab="Soil moisture") #wetness


plot(SM_wuest_min[,("DAILY_MEAN")]~SM_wuest_min[,("WUEST_ASPE")], main="Soil moisture versus plan curvature (dry)", xlab="modified catchment area", ylab="Soil moisture") #wetness
plot(SM_wuest_min[,("DAILY_MEAN")]~SM_wuest_min[,("WUEST_MCUR")], main="Soil moisture versus plan curvature (dry)", xlab="modified catchment area", ylab="Soil moisture") #wetness
plot(SM_wuest_min[,("DAILY_MEAN")]~SM_wuest_min[,("WUEST_WI.T")], main="Soil moisture versus plan curvature (dry)", xlab="modified catchment area", ylab="Soil moisture") #wetness
plot(SM_wuest_min[,("DAILY_MEAN")]~SM_wuest_min[,("WUEST_CARE")], main="Soil moisture versus plan curvature (dry)", xlab="modified catchment area", ylab="Soil moisture", xlim=(c(0,5000))) #wetness
plot(SM_wuest_min[,("DAILY_MEAN")]~SM_wuest_min[,("WUEST_MOD.")], main="Soil moisture versus plan curvature (dry)", xlab="modified catchment area", ylab="Soil moisture", xlim=(c(0,5000))) #wetness
plot(SM_wuest_min[,("DAILY_MEAN")]~SM_wuest_min[,("WUEST_SLOP")], main="Soil moisture versus plan curvature (dry)", xlab="modified catchment area", ylab="Soil moisture") #wetness
plot(SM_wuest_min[,("DAILY_MEAN")]~SM_wuest_min[,("WUEST_PRCU")], main="Soil moisture versus plan curvature (dry)", xlab="modified catchment area", ylab="Soil moisture") #wetness
plot(SM_wuest_min[,("DAILY_MEAN")]~SM_wuest_min[,("WUEST_PLCU")], main="Soil moisture versus plan curvature (dry)", xlab="modified catchment area", ylab="Soil moisture") #wetness

read.csv("D:/Droesen/Resultaat/Wuestebach/Wuest_mod_terrain.csv", header=TRUE) -> SM_wuest_mod
read.csv("D:/Droesen/Resultaat/Wuestebach/Wuest_wet2_terrain.csv", header=TRUE) -> SM_wuest_wet2



data.frame(Upslope_area = SM_wuest_max[,("WUEST_WI.T")], Soil_moisture=SM_wuest_max[,("DAILY_MEAN")]) -> testforggplot
ds <- ddply(testforggplot, .(Upslope_area), summarise, mean = mean(Soil_moisture), sd = sd(Soil_moisture))
p <- ggplot(testforggplot, aes(x = Upslope_area, y = Soil_moisture)) +  geom_point() + geom_smooth(method=lm, se=FALSE) + geom_point(data = ds, aes(y = mean), colour = "black", size = 1) +  ggtitle("Relation soil moisture and wetness index, high organisation (Wuestebach)")+ theme(plot.title=element_text(size=15, vjust=3, face="bold")) + xlab("Wetness index")+ylab("Soil moisture") + ylim(c(15,80))

lm_eqn = function(df){
  m = lm(SM_wuest_max[,("DAILY_MEAN")] ~SM_wuest_max[,("WUEST_WI.T")], testforggplot);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}
p + geom_text(x = 12, y = 20, label = lm_eqn(testforggplot), parse = TRUE)


plot(SM_wuest_max[,("DAILY_MEAN")]~SM_wuest_max[,("WUEST_WI.T")], main="Soil moisture versus plan curvature (NN)", xlab="catchment area", ylab="Soil moisture", xlim=c(7,14)) #wetness
plot(SM_wuest_Bicubic[,("DAILY_MEAN")]~SM_wuest_Bicubic[,("WETNESSIND")], main="Soil moisture versus plan curvature (Bicubic)", xlab="modified catchment area", ylab="Soil moisture", xlim=c(7,14)) #wetness
plot(SM_wuest_Bilinear[,("DAILY_MEAN")]~SM_wuest_Bilinear[,("WETNESSIND")], main="Soil moisture versus plan curvature (Bilinear)", xlab="modified catchment area", ylab="Soil moisture", xlim=c(7,14)) #wetness
plot(SM_wuest_IDW[,("DAILY_MEAN")]~SM_wuest_IDW[,("WETNESSIND")], main="Soil moisture versus plan curvature (IDW)", xlab="modified catchment area", ylab="Soil moisture", xlim=c(7,14)) #wetness
plot(SM_wuest_Bspline[,("DAILY_MEAN")]~SM_wuest_Bspline[,("WETNESSIND")], main="Soil moisture versus plan curvature (Bspline)", xlab="modified catchment area", ylab="Soil moisture", xlim=c(7,14)) #wetness

lm(SM_wuest_Bicubic[,("DAILY_MEAN")]~SM_wuest_Bicubic[,("WETNESSIND")]) -> WcorBicub
lm(SM_wuest_Bilinear[,("DAILY_MEAN")]~SM_wuest_Bilinear[,("WETNESSIND")]) -> WcorBiline
lm(SM_wuest_IDW[,("DAILY_MEAN")]~SM_wuest_IDW[,("WETNESSIND")]) -> WcorBiIDW
lm(SM_wuest_Bspline[,("DAILY_MEAN")]~SM_wuest_Bspline[,("WETNESSIND")]) -> WcorBispline
lm(SM_wuest_max[,("DAILY_MEAN")]~SM_wuest_max[,("WUEST_WI.T")]) -> WcorNN

SMWUEST_wet01012012

summary(WcorBicub)$r.squared #R-squared T6 wetness                                      
summary(WcorBiline)$r.squared #R-squared T6 wetness                                      
summary(WcorBiIDW)$r.squared #R-squared T6 wetness                                      
summary(WcorBispline)$r.squared #R-squared T6 wetness                                      
summary(WcorNN)$r.squared #R-squared T6 wetness                                      



SM_wuest_wet2

#linear model
lm(SM_wuest_wet2[,("DAILY_MEAN")]~SM_wuest_wet2[,("MODIFIEDCA")]) -> Wwetmod
lm(SM_wuest_wet2[,("DAILY_MEAN")]~SM_wuest_wet2[,("CATCHMENTS")]) -> Wwetcatchslope
lm(SM_wuest_wet2[,("DAILY_MEAN")]~SM_wuest_wet2[,("ASPECT")]) -> Wwetaspect
lm(SM_wuest_wet2[,("DAILY_MEAN")]~SM_wuest_wet2[,("LNWUESTCAR")]) -> Wwetupslop
lm(SM_wuest_wet2[,("DAILY_MEAN")]~SM_wuest_wet2[,("WETNESSIND")]) -> WwetWI
lm(SM_wuest_wet2[,("DAILY_MEAN")]~SM_wuest_wet2[,("SLOPE")]) -> Wslope
lm(SM_wuest_wet2[,("DAILY_MEAN")]~SM_wuest_wet2[,("CURVATURE")]) -> Wcurvature
lm(SM_wuest_wet2[,("DAILY_MEAN")]~SM_wuest_wet2[,("CATCHMENTA")]) -> Wcatcment
lm(SM_wuest_wet2[,("DAILY_MEAN")]~SM_wuest_wet2[,("PLANCURVAT")]) -> WmaxPlanC
lm(SM_wuest_wet2[,("DAILY_MEAN")]~SM_wuest_wet2[,("PROFILECUR")]) -> WmaxPprofileCurv

lm(SM_wuest_mod[,("DAILY_MEAN")]~SM_wuest_mod[,("MODIFIEDCA")]) -> Wmodmod
lm(SM_wuest_mod[,("DAILY_MEAN")]~SM_wuest_mod[,("CATCHMENTS")]) -> Wmodcatchslope
lm(SM_wuest_mod[,("DAILY_MEAN")]~SM_wuest_mod[,("ASPECT")]) -> Wmodaspect
lm(SM_wuest_mod[,("DAILY_MEAN")]~SM_wuest_mod[,("LNWUESTCAR")]) -> Wwmodupslop
lm(SM_wuest_mod[,("DAILY_MEAN")]~SM_wuest_mod[,("WETNESSIND")]) -> WmodWI
lm(SM_wuest_mod[,("DAILY_MEAN")]~SM_wuest_mod[,("SLOPE")]) -> Wmodslope
lm(SM_wuest_mod[,("DAILY_MEAN")]~SM_wuest_mod[,("CURVATURE")]) -> Wmodcurvature
lm(SM_wuest_mod[,("DAILY_MEAN")]~SM_wuest_mod[,("CATCHMENTA")]) -> Wmodcatcment
lm(SM_wuest_mod[,("DAILY_MEAN")]~SM_wuest_mod[,("PLANCURVAT")]) -> WmodPlanC
lm(SM_wuest_mod[,("DAILY_MEAN")]~SM_wuest_mod[,("PROFILECUR")]) -> WmodPprofileCurv

SM_wuest_mod$residMod <- resid(Wmodmod)
SM_wuest_mod$residCatch<- resid(Wmodcatchslope)
SM_wuest_mod$residAsp <- resid(Wmodaspect)
SM_wuest_mod$residUpslope <- resid(Wwmodupslop)
SM_wuest_mod$residWetness<- resid(WmodWI)
SM_wuest_mod$residSlope <- resid(Wmodslope)
SM_wuest_mod$residCurv <- resid(Wmodcurvature)
SM_wuest_mod$residModifCatch <- resid(Wmodcatcment)
SM_wuest_mod$residModPlanC <- resid(WmodPlanC)
SM_wuest_mod$residModProfileC <- resid(WmodPprofileCurv)

write.csv(SM_wuest_mod, file="D:/Droesen/Resultaat/Wuestebach/SM_Wuest_residuals")

v1 <- variog(coords = ozone[,3:4], data = ozone[,2], breaks = breaks)


#R squared
summary(Wmodmod)$r.squared #R-squared T6 wetness                                      
summary(Wmodcatchslope)$r.squared #R-squared T6 wetness                                      
summary(Wmodaspect)$r.squared #R-squared T6 wetness                                      
summary(Wwmodupslop)$r.squared #R-squared T6 wetness                                      
summary(WmodWI)$r.squared #R-squared T6 wetness                                      
summary(Wmodcurvature)$r.squared #R-squared T6 wetness                                      
summary(Wmodcatcment)$r.squared #R-squared T6 wetness                                      
summary(WmodPlanC)$r.squared #R-squared T6 wetness
summary(WmodPprofileCurv)$r.squared #R-squared T6 wetness

summary(WminAspect)$r.squared #R-squared T6 wetness                                      
summary(WminMcurv)$r.squared #R-squared T6 wetness                                      
summary(WminWetness)$r.squared #R-squared T6 wetness                                      
summary(WminUpslope)$r.squared #R-squared T6 wetness                                      
summary(WminModUp)$r.squared #R-squared T6 wetness                                      
summary(WminSlope)$r.squared #R-squared T6 wetness                                      
summary(WminProfileC)$r.squared #R-squared T6 wetness                                      
summary(WminPlanC)$r.squared #R-squared T6 wetness

summary(WlnCA)$r.squared #R-squared T6 wetness

