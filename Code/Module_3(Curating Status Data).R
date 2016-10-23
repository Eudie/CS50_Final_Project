##Author: Eudie------
##This is my CS50 final project. Here I am attempting to
##I am trying to analyze the effeciency of indian railways
##The metric I chose is the average train delay in departure in past 30 days
##I am fetching the data from unofficial APIs "railwayapi.com"
##This Module is to curate daily status data for ml algorithms.

##library used and WD----
#setwd("D:/Study/CS_basics/CS50_Final_Project")
#setwd("~/PythonProjects/CS50_Final_Project")  
library(chron)
library(lubridate)
library(plyr)

##Reading data from local----
List_of_Trains <- read.csv("Data/List_of_Trains.csv" ,stringsAsFactors=FALSE, colClasses=c("number"="character"))
List_of_Stations <- read.csv("Data/List_of_Station.csv",stringsAsFactors=FALSE)
Route_of_Trains <- read.csv("Data/Route_of_Trains.csv",stringsAsFactors=FALSE, colClasses=c("train_number"="character"))
Detail_of_Stations <- read.csv("Data/Detail_of_Stations.csv",stringsAsFactors=FALSE )
Train_summary <- read.csv("Data/Train_summary.csv",stringsAsFactors=FALSE, colClasses=c("number"="character"))

##Reading all status file

file_names <- dir("Data/Daily_status/TestDir/") 

your_data_frame <- do.call(rbind,lapply(paste0("Data/Daily_status/TestDir/",file_names),read.csv,stringsAsFactors = F, colClasses=c("train_no"="character")))
your_data_frame$uniqueKey <- paste(your_data_frame$train_no, your_data_frame$station, your_data_frame$scharr_datetime, sep = "|")
length(unique(your_data_frame$uniqueKey))
uniquedf <- your_data_frame[!(duplicated(your_data_frame$uniqueKey)),]
uniquedf$scharr_datetime <- as.POSIXct(uniquedf$scharr_datetime, "%Y-%m-%d %H:%M:%S")
uniquedf$schdep_datetime <- as.POSIXct(uniquedf$schdep_datetime, "%Y-%m-%d %H:%M:%S")
uniquedf$actarr_datetime <- as.POSIXct(uniquedf$actarr_datetime, "%Y-%m-%d %H:%M:%S")
uniquedf$actdep_datetime <- as.POSIXct(uniquedf$actdep_datetime, "%Y-%m-%d %H:%M:%S")

onlyReachedDF <- uniquedf[uniquedf$is_reached == TRUE,]
summary(onlyReachedDF$scharr_datetime)
crowd <- integer()
for(x in onlyReachedDF$uniqueKey){
  fil <- onlyReachedDF[onlyReachedDF$uniqueKey == x,]
  uniquedf$crowd[uniquedf$uniqueKey == x] <- nrow(onlyReachedDF[(onlyReachedDF$station == fil$station &
                                               onlyReachedDF$actarr_datetime >= (fil$actarr_datetime - hours(1)) &
                                               onlyReachedDF$actarr_datetime <= (fil$actarr_datetime + hours(1))),])
}
uniquedf$crowd[is.na(uniquedf$crowd)] <- 0
