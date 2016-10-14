##Author: Eudie------
##This is my CS50 final project. Here I am attempting to
##I am trying to analyze the effeciency of indian railways
##The metric I chose is the average train delay in departure in past 30 days
##I am fetching the data from unofficial APIs "railwayapi.com"
##This Module is just for fetching the data of all trains on daily basis

##library used and WD----
  #setwd("D:/Study/CS_basics/CS50_Final_Project")
  #setwd("~/PythonProjects/CS50_Final_Project")  
  library(jsonlite)
  library(chron)
  library(lubridate)

##Reading data from local----
  List_of_Trains <- read.csv("Data/List_of_Trains.csv" ,stringsAsFactors=FALSE, colClasses=c("number"="character"))
  List_of_Stations <- read.csv("Data/List_of_Station.csv",stringsAsFactors=FALSE)
  Route_of_Trains <- read.csv("Data/Route_of_Trains.csv",stringsAsFactors=FALSE, colClasses=c("train_number"="character"))
  Detail_of_Stations <- read.csv("Data/Detail_of_Stations.csv",stringsAsFactors=FALSE )
  Train_summary <- read.csv("Data/Train_summary.csv",stringsAsFactors=FALSE, colClasses=c("number"="character"))

##Function used ----
  ## This fuction is to clean the raw data of Train_Status table
  clean_train_status <- function(json_data){
    df <- as.data.frame(json_data$route)
    df$station_ <- NULL
    df$status <- NULL
    
    df$scharr[1] <- df$schdep[1]
    df$actarr[1] <- df$actdep[1]
    df$schdep[length(df$schdep)] <- df$scharr[length(df$scharr)]
    df$actdep[length(df$actdep)] <- df$actarr[length(df$actarr)]
    
    df$scharr_datetime <- strptime(paste(df$scharr_date,df$scharr), "%d %b %Y %H:%M")
    df$actarr_datetime <- strptime(paste(df$actarr_date,df$actarr), "%d %b %Y %H:%M")
    df$schdep_datetime <- strptime(paste(df$scharr_date,df$schdep), "%d %b %Y %H:%M")
    df$actdep_datetime <- strptime(paste(df$actarr_date,df$actdep), "%d %b %Y %H:%M")
    
    df$schdep_datetime[df$schdep_datetime < df$scharr_datetime] <- df$schdep_datetime + days(1)
    df$actdep_datetime[df$actdep_datetime < df$actarr_datetime] <- df$actdep_datetime + days(1)
    
    
    df$train_no <- json_data$train_number
    lastStationNo <- max(df$no[df$has_arrived])
    df$is_reached <- ifelse(df$no <= lastStationNo, TRUE, FALSE)
    drops <- c("scharr","actarr","schdep", "actdep", "scharr_date", "actarr_date", "has_arrived", "has_departed")
    df <- df[,!(names(df) %in% drops)]
    return(df)
  }

##Getting daily train status data----
  raw_data <- fromJSON("http://api.railwayapi.com/live/train/12721/doj/20161012/apikey/klbec7664/")
  testing <- clean_train_status(raw_data)
  PracticeTrainSummary = Train_summary[Train_summary$number %in% c("12721", "12624", "12725", "56661", "15119", "11045", "11047", "11058", "11072"),]

##main----
  main <- function(TrainSummary, api_key){
    dataFrame = data.frame(
                         latemin=integer(), 
                         no=integer(), 
                         station=character(),
                         distance=integer(), 
                         day=integer(), 
                         scharr_datetime=character(), 
                         actarr_datetime=character(), 
                         schdep_datetime=character(), 
                         actdep_datetime=character(), 
                         train_no=character(),  
                         is_reached=logical(), 
                         stringsAsFactors=FALSE)
    for (i in TrainSummary$number) {
      date <- format((today() - days(max(TrainSummary$journey_days[TrainSummary$number == i], 1))), "%Y%m%d")
      tryCatch({
        raw_status <- fromJSON(paste0("http://api.railwayapi.com/live/train/",i,"/doj/",date,"/apikey/",api_key,"/"))
        fdaToAppend <- clean_train_status(raw_status)
        dataFrame <- rbind(dataFrame, fdaToAppend)
        
      }, error=function(e){})
    }
  write.table(dataFrame, paste0("Data/Daily_status/", today(),".csv"), row.names=F,  sep = ",")
  }
  
main(Train_summary, "klbec7664")


