library(lubridate)
library(plyr)
library(openair)
library(ggplot2)

rm(list=ls())

setwd("D:/Droesen/Master thesis/2Datasets/Wüstebach/SM_SOILNet_WUE_2009_2013/SM20_gem")
list.files(path="D:/Droesen/Datasets/W?stebach/SM_SOILNet_WUE_2009_2013/SM_20cm/", pattern=".txt") -> fileNames1
fileNames <- Sys.glob("*14.txt")

read.table("D:/Droesen/Datasets/W?stebach/SM_SOILNet_WUE_2009_2013/SM_20cm/SM201_SE001.txt", header=TRUE, sep=",")  -> test1

#change filenames

#read files
testfolder <- "D:/Droesen/Master thesis/2Datasets/Wüstebach/SM_SOILNet_WUE_2009_2013/SM20_gem"
i <- 1
for (fileName in fileNames1) {
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


list.files(path=testfolder, pattern=".csv")-> files

for (file in files){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataWuest")){
    dataWuest <- read.csv(file,header=TRUE, sep="")
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataWuest")){
    temp_dataset <-read.csv(file,header=TRUE, sep="")
    dataWuest <- rbind(dataWuest, temp_dataset)
    rm(temp_dataset)
  }
  
}


onedataframe <- do.call(rbind,lapply(files,read.csv))


#Coordinates for points
Pointcoordinates <- read.table("D:/Droesen/Master thesis/Datasets/Wüstebach/Soilnet_coordinates.txt", header=TRUE, sep=",")

merge(onedataframe, Pointcoordinates) -> SMWuest_xy

write.csv(SMWuest_xy, file="D:/Droesen/Master thesis/Datasets/Wüstebach/SMwuest_20cm.csv")
