library(ggplot2)
library(lubridate)
library(plyr)
library(openair)

rm(list=ls())

setwd("D:/Droesen/Datasets/Wüstebach/SM_SOILNet_WUE_2009_2013/Test50/")
list.files(path="D:/Droesen/Datasets/Wüstebach/SM_SOILNet_WUE_2009_2013/SM_50cm/", pattern=".txt") -> fileName50

#change filenames

#read files
testfolder <- "D:/Droesen/Datasets/Wüstebach/SM_SOILNet_WUE_2009_2013/Test50/"
i <- 1
for (fileName in fileName50) {
  sample <- read.table(fileName, header = TRUE, sep = ",")
  #add column id?
  subset(sample, qualifierid==2 | qualifierid==20 | qualifierid==21 | qualifierid==11, select=c("timestampto", "datavalue", "qualifierid") )  -> sample_ok
  strftime(sample_ok$timestampto, format="%Y-%m-%d") -> sample_ok$timestampto
  ddply(sample_ok, .(timestampto), summarize, daily_mean_SM = mean(datavalue)) -> averaged_over_day
  #add columns x, y
  averaged_over_day[["Id"]] = i
  write.csv(averaged_over_day, paste(testfolder, "GemID", "20", formatC(i, width=3, flag="0"), '.csv', sep="_") )
  i <- i + 1
}


list.files(path="D:/Droesen/Datasets/Wüstebach/SM_SOILNet_WUE_2009_2013/Test50/", pattern=".csv")-> files


for (file in files){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataWuest")){
    dataWuest <- read.csv(file, header=TRUE, sep="")
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataWuest")){
    temp_dataset <-read.csv(file, header=TRUE, sep="")
    dataWuest <- rbind(dataWuest, temp_dataset)
    rm(temp_dataset)
  }
  
}

onedataframe50 <- do.call(rbind,lapply(files,read.csv))


Pointcoordinates <- read.table("D:/Droesen/Datasets/Wüstebach/Soilnet_coordinates.txt", header=TRUE, sep=",")

merge(onedataframe50, Pointcoordinates) -> SMWuest_xy_50

#unnecessary to plot dates
#SMWuest_xy$timestampto <- as.Date( as.character(SMWuest_xy$timestampto), "%Y-%m-%d")

subset(SMWuest_xy, timestampto == "2009-09-01", select=c("Id", "timestampto", "daily_mean_SM", "X.coord", "Y.coord")) -> SMWUEST_1min
subset(SMWuest_xy, timestampto == "2009-11-01", select=c("Id", "timestampto", "daily_mean_SM", "X.coord", "Y.coord")) -> SMWUEST_1max
subset(SMWuest_xy, timestampto == "2011-01-03", select=c("Id", "timestampto", "daily_mean_SM", "X.coord", "Y.coord")) -> SMWUEST_1mod
subset(SMWuest_xy, timestampto == "2011-05-06", select=c("Id", "timestampto", "daily_mean_SM", "X.coord", "Y.coord")) -> SMWUEST_1min
subset(SMWuest_xy, timestampto == "2009-10-04", select=c("Id", "timestampto", "daily_mean_SM", "X.coord", "Y.coord")) -> SMWUEST_dry
subset(SMWuest_xy, timestampto == "2009-10-13", select=c("Id", "timestampto", "daily_mean_SM", "X.coord", "Y.coord")) -> SMWUEST_wet2
subset(SMWuest_xy, timestampto == "2009-10-15", select=c("Id", "timestampto", "daily_mean_SM", "X.coord", "Y.coord")) -> SMWUEST_wet1
subset(SMWuest_xy_50, timestampto == "2009-12-02", select=c("Id", "timestampto", "daily_mean_SM", "X.coord", "Y.coord")) -> SMWUEST50_moderate
subset(SMWuest_xy_50, timestampto == "2011-05-28", select=c("Id", "timestampto", "daily_mean_SM", "X.coord", "Y.coord")) -> SMWUEST50_dry28052011
subset(SMWuest_xy_50, timestampto == "2012-01-01", select=c("Id", "timestampto", "daily_mean_SM", "X.coord", "Y.coord")) -> SMWUEST50_wet01012012

write.csv(SMWUEST50_moderate, 'SMWUEST50_moderate')
write.csv(SMWUEST50_dry28052011, 'SMWUEST50_dry28052011')
write.csv(SMWUEST50_wet01012012, 'SMWUEST50_wet01012012')
