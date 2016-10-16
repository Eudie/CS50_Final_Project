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

##main functions, whose job is to fetch data of current train status
  main_morning <- function(TrainSummary, api_key){
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
  write.table(dataFrame, paste0("Data/Daily_status/", today(),"_Morning.csv"), row.names=F,  sep = ",")
  }
  
  main_evening <- function(TrainSummary, api_key){
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
      date <- format(today() - days(TrainSummary$journey_days[TrainSummary$number == i]), "%Y%m%d")
      tryCatch({
        raw_status <- fromJSON(paste0("http://api.railwayapi.com/live/train/",i,"/doj/",date,"/apikey/",api_key,"/"))
        fdaToAppend <- clean_train_status(raw_status)
        dataFrame <- rbind(dataFrame, fdaToAppend)
        
      }, error=function(e){})
    }
    write.table(dataFrame, paste0("Data/Daily_status/", today(),"_Evening.csv"), row.names=F,  sep = ",")
  }

##Reading data from local----
  List_of_Trains <- read.csv("Data/List_of_Trains.csv" ,stringsAsFactors=FALSE, colClasses=c("number"="character"))
  List_of_Stations <- read.csv("Data/List_of_Station.csv",stringsAsFactors=FALSE)
  Route_of_Trains <- read.csv("Data/Route_of_Trains.csv",stringsAsFactors=FALSE, colClasses=c("train_number"="character"))
  Detail_of_Stations <- read.csv("Data/Detail_of_Stations.csv",stringsAsFactors=FALSE )
  Train_summary <- read.csv("Data/Train_summary.csv",stringsAsFactors=FALSE, colClasses=c("number"="character"))

##Getting daily train status data Morning----
  ForMorning <- Train_summary[Train_summary$journey_days == 0,]
  main_morning(ForMorning, "klbec7664")

##Getting daily train status data Evening----  
  TodayMorning <- read.csv(paste0("Data/Daily_status/", today(),"_Morning.csv"), stringsAsFactors = FALSE, colClasses = c("train_no"="character"))
  ForEvening <- Train_summary[!(Train_summary$number %in% unique(TodayMorning$train_no)),]
  main_evening(ForEvening, "klbec7664")
  TodayEvening <- read.csv(paste0("Data/Daily_status/", today(),"_Evening.csv"), stringsAsFactors = FALSE, colClasses = c("train_no"="character"))
  length(unique(TodayEvening$train_no))
