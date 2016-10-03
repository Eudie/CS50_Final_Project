##Author: Eudie
##This is my CS50 final project. Here I am attempting to
##view the efficiency of indian railway by regions.
##I metric I chose is the average train delay in departure in past 30 days
##I am taking the data from unofficial APIs "railwayapi.com"
##I will look at heat map of India at state, distict and station level
##Cleaning up----
rm(list = ls())
setwd("D:/Study/CS_basics/CS50_Final_Project")

##Libraries used----
  library(jsonlite)
  library(chron)
  library(lubridate)

##Functions used----
    #This function gives the list of all unique stations in India
    AllStations <- function(api_key){
      df <- data.frame(fullname=character(), code=character(), stringsAsFactors=FALSE)
      for(x in letters){
        fromWeb <- fromJSON(paste0("http://api.railwayapi.com/suggest_station/name/",x,"/apikey/",api_key,"/"))
        
        if(fromWeb$response_code == 200){
          df <- rbind(df, as.data.frame(fromWeb$station))
        }
      }
      df <- unique(df)
      return(df)
    }
    
    #This function gives the list of all trains in India
    AllTrains <- function(api_key){
      df <- data.frame(name=character(), number=character(), stringsAsFactors=FALSE)
      for(x in c(0:9)){
        fromWeb <- fromJSON(paste0("http://api.railwayapi.com/suggest_train/trains/",x,"/apikey/",api_key,"/"))
        
        if(fromWeb$response_code == 200){
          df <- rbind(df, as.data.frame(fromWeb$trains))
        }
      }
      return(df)
    }
    
    ##This function gives the location detail fo the station in India
    Station_detail <- function(station_code, api_key){
      df <- data.frame(fullname=character(), lat=double(), state=character(), lng=double(),code=character(), stringsAsFactors=FALSE)
      for(x in station_code){
        fromWeb <- fromJSON(paste0("http://api.railwayapi.com/code_to_name/code/",tolower(x),"/apikey/",api_key,"/"))
        
        if(fromWeb$response_code == 200){
          df <- rbind(df, as.data.frame(fromWeb$stations))
        }
      }
      df <- df[!duplicated(df$code), ]
      return(df)
    }
    
    ##This function gives the detail of train
    Train_route <- function(train_no, api_key){
      intermediate_df <- data.frame(no = integer(), distance = integer(), day = integer(),halt = integer(), route = integer(), code = character(), fullname=character(), lat=double(), state=character(), lng=double(), scharr = character(), schdep = character(),stringsAsFactors=FALSE)
      df <- data.frame(Train_number = character(),no = integer(), distance = integer(), day = integer(),halt = integer(), route = integer(), code = character(), fullname=character(), lat=double(), state=character(), lng=double(), scharr = character(), schdep = character(),stringsAsFactors=FALSE)
      for (x in train_no){
        fromWeb <- fromJSON(paste0("http://api.railwayapi.com/route/train/",x,"/apikey/",api_key,"/"))
        if(fromWeb$response_code == 200){
          intermediate_df <- as.data.frame(fromWeb$route)
          intermediate_df$scharr[1] = intermediate_df$schdep[1]
          intermediate_df$schdep[length(intermediate_df$schdep)] = intermediate_df$scharr[length(intermediate_df$scharr)]
          intermediate_df$train_number = x
          df <- rbind(df, intermediate_df)
        }
      }
      return(df)
    }
    
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
      
      df[,c("scharr","actarr","schdep", "actdep", "scharr_date", "actarr_date", "day", "no")] <- NULL
      df$train_no <- json_data$train_number
      return(df)
    }
    
    ##Function is to find length of train
    Detail_of_Train <- function(Train_no, Route){
      fdf <- data.frame(number=character(), total_stations=integer(), distance_covered = integer(),travel_time = numeric() ,stringsAsFactors=FALSE)
      for (x in Train_no){
        a <- Route[Route$train_number == x,]
        df <- data.frame(number=character(), total_stations=integer(), distance_covered = integer(),travel_time = numeric() ,stringsAsFactors=FALSE)
        total_stations <- max(a$no)
        distance_covered <- max(a$distance)
        travel_time <- 1#Route$schdep[as.numeric(Route$no) == df$total_station] +
         #                 hour(24*(Route$day[as.numeric(Route$no) == df$total_station]-1)) -
          #                Route$scharr[as.numeric(Route$no) == 1]
        number <- as.character(x)
        df <- list(number, total_stations,distance_covered, travel_time)
        
        
        fdf <- rbind(fdf, df)
      }
      # colnames(fdf) <- c()
      # fdf$number <- as.character(fdf$number)
      # fdf$total_stations <- as.numeric(fdf$total_stations)
      # fdf$distance_covered <- as.numeric(fdf$distance_covered)
      # fdf$travel_time <- as.numeric(fdf$travel_time)
      return(fdf)
    }
    Sab <- Detail_of_Train(c(12721, 12722), Route_of_Trains)

##Getting Data (One time extraction)---- 
    #List_of_Trains <- AllTrains("klbec7664")
    #List_of_Stations <- AllStations("klbec7664")
    #Route_of_Trains <- Train_route(List_of_Trains$number, "klbec7664")
    #Detail_of_Stations <- Station_detail("BHS", "klbec7664")
    
    #write.csv(List_of_Stations , "List_of_Station.csv", row.names = FALSE)
    #write.csv(List_of_Trains , "List_of_Trains.csv", row.names = FALSE)
    #write.csv(Route_of_Trains , "Route_of_Trains.csv", row.names = FALSE)
    List_of_Trains <- read.csv("List_of_Trains.csv" ,stringsAsFactors=FALSE)
    List_of_Stations <- read.csv("List_of_Station.csv",stringsAsFactors=FALSE)
    Route_of_Trains <- read.csv("Route_of_Trains.csv",stringsAsFactors=FALSE)
    
    List_of_Trains$total_stations <- max(Route_of_Trains$no[Route_of_Trains$train_number == List_of_Trains$number])
##Getting Data (Daily Extraction)----
    raw_data <- fromJSON("http://api.railwayapi.com/live/train/12721/doj/20160922/apikey/klbec7664/")
    testing <- clean_dataframe(raw_data)


    fromWeb3 <- fromJSON("http://api.railwayapi.com/live/train/12721/doj/20160926/apikey/klbec7664/")
    