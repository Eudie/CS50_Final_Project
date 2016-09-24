##Author: Eudie
##This is my CS50 final project. Here I am attempting to
##view the efficiency of indian railway by regions.
##I metric I chose is the average train delay in departure in past 30 days
##I am taking the data from unofficial APIs "railwayapi.com"
##I will look at heat map of India at state, distict and station level
##Cleaning up----
rm(list = ls())
setwd("D:/Study/CS basics/CS50_Final_Project")

##Libraries used----
  library(jsonlite)
  library(chron)
  library(lubridate)

##Functions used----
    #This function gives the list of all unique stations in India
    AllStations <- function(){
      df <- data.frame(fullname=character(), code=character(), stringsAsFactors=FALSE)
      for(x in letters){
        fromWeb <- fromJSON(paste0("http://api.railwayapi.com/suggest_station/name/",x,"/apikey/klbec7664/"))
        
        if(fromWeb[[3]] == 200){
          df <- rbind(df, as.data.frame(fromWeb[[1]]))
        }
      }
      df <- unique(df)
      return(df)
    }
    
    #This function gives the list of all trains in India
    AllTrains <- function(){
      df <- data.frame(name=character(), number=character(), stringsAsFactors=FALSE)
      for(x in c(0:9)){
        fromWeb <- fromJSON(paste0("http://api.railwayapi.com/suggest_train/trains/",x,"/apikey/klbec7664/"))
        
        if(fromWeb[[4]] == 200){
          df <- rbind(df, as.data.frame(fromWeb[[3]]))
        }
      }
      return(df)
    }
    
    ##This function gives the location detail fo the station in India
    Station_detail <- function(station_code){
      df <- data.frame(fullname=character(), lat=double(), state=character(), lng=double(),code=character(), stringsAsFactors=FALSE)
      for(x in station_code){
        fromWeb <- fromJSON(paste0("http://api.railwayapi.com/code_to_name/code/",tolower(x),"/apikey/klbec7664/"))
        
        if(fromWeb[[2]] == 200){
          df <- rbind(df, as.data.frame(fromWeb[[1]]))
        }
      }
      df <- df[!duplicated(df$code), ]
      return(df)
    }
    
    ##This function gives the detail of train
    Train_detail <- function(train_no){
      #TO DO
    }
    
    ## This fuction is to clean the raw data of Train_Status table
    clean_train_status <- function(json_data){
      df <- as.data.frame(raw_data[[2]])
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
      
      df[,c("scharr","actarr","schdep", "actdep", "scharr_date", "actarr_date", "day", "no")] <- NULL
      df$train_no <- raw_data[7]
      return(df)
    }

##Getting Data (One time extraction)---- 
    List_of_Trains <- AllTrains()
    List_of_Stations <- AllStations()
    Detail_of_Trains <- Train_detail(List_of_Trains$number)
    Detail_of_Stations <- Station_detail(List_of_Stations$code)

##Getting Data (Daily Extraction)----
    raw_data <- fromJSON("http://api.railwayapi.com/live/train/12721/doj/20160922/apikey/klbec7664/")
    testing <- clean_dataframe(raw_data)

