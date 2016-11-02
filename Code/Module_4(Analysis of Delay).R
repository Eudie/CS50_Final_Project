##Author: Eudie------
##This is my CS50 final project. Here I am attempting to
##I am trying to analyze the effeciency of indian railways
##The metric I chose is the average train delay in departure in past 30 days
##I am fetching the data from unofficial APIs "railwayapi.com"
##This Module is to find the cause of delay using machine learning algos

##library used and WD----
#setwd("D:/Study/CS_basics/CS50_Final_Project")
#setwd("~/PythonProjects/CS50_Final_Project")  
library(chron)
library(lubridate)
library(plyr)
library(biglm)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)

##Reading data from local----
List_of_Trains <- read.csv("Data/List_of_Trains.csv" ,stringsAsFactors=FALSE, colClasses=c("number"="character"))
List_of_Stations <- read.csv("Data/List_of_Station.csv",stringsAsFactors=FALSE)
Route_of_Trains <- read.csv("Data/Route_of_Trains.csv",stringsAsFactors=FALSE, colClasses=c("train_number"="character"))
Detail_of_Stations <- read.csv("Data/Detail_of_Stations.csv",stringsAsFactors=FALSE )
Train_summary <- read.csv("Data/Train_summary.csv",stringsAsFactors=FALSE, colClasses=c("number"="character"))
LiveStatus_1 <- read.csv("Data/CleanStatus/LiveStatus_1.csv", stringsAsFactors = FALSE, colClasses=c("train_no"="character", 
                                                                                                     "scharr_datetime"="POSIXct",
                                                                                                     "actarr_datetime"="POSIXct",
                                                                                                     "schdep_datetime"="POSIXct",
                                                                                                     "actdep_datetime"="POSIXct"))
LiveStatus_2 <- read.csv("Data/CleanStatus/LiveStatus_2.csv", stringsAsFactors = FALSE, colClasses=c("train_no"="character", 
                                                                                                     "scharr_datetime"="POSIXct",
                                                                                                     "actarr_datetime"="POSIXct",
                                                                                                     "schdep_datetime"="POSIXct",
                                                                                                     "actdep_datetime"="POSIXct"))
LiveStatus_3 <- read.csv("Data/CleanStatus/LiveStatus_3.csv", stringsAsFactors = FALSE, colClasses=c("train_no"="character", 
                                                                                                     "scharr_datetime"="POSIXct",
                                                                                                     "actarr_datetime"="POSIXct",
                                                                                                     "schdep_datetime"="POSIXct",
                                                                                                     "actdep_datetime"="POSIXct"))
LiveStatus_4 <- read.csv("Data/CleanStatus/LiveStatus_4.csv", stringsAsFactors = FALSE, colClasses=c("train_no"="character", 
                                                                                                     "scharr_datetime"="POSIXct",
                                                                                                     "actarr_datetime"="POSIXct",
                                                                                                     "schdep_datetime"="POSIXct",
                                                                                                     "actdep_datetime"="POSIXct"))


LiveStatus <- rbind(LiveStatus_1, LiveStatus_2, LiveStatus_3, LiveStatus_4)
rm(LiveStatus_1, LiveStatus_2, LiveStatus_3, LiveStatus_4)

##Merging all important factors of train and station in Livestatus table----
drops <- c("uniqueKey","is_reached","oneRoute")
LiveStatus <- LiveStatus[,!(names(LiveStatus) %in% drops)]

dropStation <- c("fullname","DelayOverall","DelayDiscreate", "Pin_code")
DetailStations <- Detail_of_Stations[,!(names(Detail_of_Stations) %in% dropStation)]

dropTrain <- c("start_time","end_time","journey_days", "DelayOverall","DelayDiscreate")
TrainSummary <- Train_summary[,!(names(Train_summary) %in% dropTrain)]


LiveStatus <- merge(LiveStatus, DetailStations, by.x = "station", by.y = "code", all.x = T)
LiveStatus <- merge(LiveStatus, TrainSummary, by.x = "train_no", by.y = "number", all.x = T)

rm(TrainSummary, Train_summary, DetailStations, Detail_of_Stations, Route_of_Trains, List_of_Stations, List_of_Trains, dropTrain, dropStation, drops)

## Converting character as factor variable----
LiveStatus$station <- as.factor(LiveStatus$station)
LiveStatus$train_no <- as.factor(LiveStatus$train_no)
LiveStatus$state <- as.factor(LiveStatus$state)
LiveStatus$train_type <- as.factor(LiveStatus$train_type)

##Formulae for iterations----
formula0 <- formula("lateDiscreate ~ train_no+station+distance+day+no+scharr_datetime+actarr_datetime+
                      schdep_datetime+actdep_datetime+crowd+lat+lng+state+stops+total_stations+
                      distance_covered+journey_time+train_type+is_superfast+average_speed")
formula1 <- formula("latemin ~ distance+scharr_datetime+actarr_datetime+
                      schdep_datetime+actdep_datetime+crowd+lat+lng+state+stops+total_stations+
                    distance_covered+journey_time+train_type+is_superfast+average_speed")
formula2 <- formula("lateDiscreate ~ . - latemin")
formula3 <- formula("lateDiscreate ~ . - latemin")
formula4 <- formula("lateDiscreate ~ . - latemin")

##Tree Model----
set.seed(2)
fold <- trainControl(method = "cv", number = 10)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
train(formula1, data = LiveStatus, method = "rpart", trControl = fold, tuneGrid = cartGrid, na)


cart_Model <- rpart(formula1, data = LiveStatus, minbucket = 1000)
prp(cart_Model)

summary(cart_Model)
