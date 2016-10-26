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

LiveStatus <- rbind(LiveStatus_1, LiveStatus_2)
