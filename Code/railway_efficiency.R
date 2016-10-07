##Author: Eudie------
##This is my CS50 final project. Here I am attempting to
##view the efficiency of indian railway by regions.
##I metric I chose is the average train delay in departure in past 30 days
##I am taking the data from unofficial APIs "railwayapi.com"
##I will look at heat map of India at state, distict and station level
##Cleaning up----
rm(list = ls())
#setwd("D:/Study/CS_basics/CS50_Final_Project")
#setwd("~/PythonProjects/CS50_Final_Project")

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
          intermediate_df$schdep[intermediate_df$schdep=="Destination"] = intermediate_df$scharr[intermediate_df$schdep=="Destination"]
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
      
      #df[,c("scharr","actarr","schdep", "actdep", "scharr_date", "actarr_date", "day", "no")] <- NULL
      df$train_no <- json_data$train_number
      return(df)
    }
    
    ##Function is to find length of train
    Detail_of_Train <- function(Train_no, Route){
      fdf <- data.frame(number=character(), total_stations=integer(), distance_covered = integer(),travel_time = numeric() ,stringsAsFactors=FALSE)
      for (x in Train_no){
        if(any(x == Route$train_number)){
        a <- Route[Route$train_number == x,]
        t_type_table <- data.frame(first_num = as.character(c(0,1,2,3,4,5,6,7,8,9)), 
                                   type = c("Special", "Long Distance", "Long Distance", "Suburban Kol", "Suburban Ch De SC", "Passenger", "MEMU", "DMU", "reserved", "Suburban MU"),
                                   stringsAsFactors = F)
        total_stations <- max(a$no)
        distance_covered <- max(a$distance)
        end_time <-  as.character(a$schdep[a$no == total_stations])
        start_time <- as.character(a$scharr[a$no == 1])
        journey_days <- a$day[as.numeric(a$no) == total_stations]-1
        journey_time <- as.numeric(difftime(strptime(end_time, "%H:%M") + days(journey_days), strptime(start_time, "%H:%M"), units = c("hours"))) #+ as.numeric(hours(24*journey_days))
        number <- as.character(x)
        train_type <- t_type_table$type[t_type_table$first_num == substr(number, 1, 1)]
        is_superfast <- all(any(substr(number, 1, 1) == c("0", "1", "2")), substr(number,2, 2) == "2")
        df <- data.frame(number, total_stations,distance_covered,  start_time, end_time,  journey_time, train_type, is_superfast,stringsAsFactors = F)
        fdf <- rbind(fdf, df)
      }}
      
      fdf$average_speed <- fdf$distance_covered/fdf$journey_time
      return(fdf)
    }


##Getting Data (One time extraction)---- 
    #List_of_Trains <- AllTrains("klbec7664")
    #List_of_Stations <- AllStations("klbec7664")
    #Route_of_Trains <- Train_route(List_of_Trains$number, "klbec7664")
    Detail_of_Stations <- Station_detail(List_of_Stations$code, "klbec7664")
    #Train_summary <- Detail_of_Train(List_of_Trains$number, Route_of_Trains)
    
    #write.csv(List_of_Stations , "List_of_Station.csv", row.names = FALSE)
    #write.csv(List_of_Trains , "List_of_Trains.csv", row.names = FALSE)
    #write.csv(Route_of_Trains , "Route_of_Trains.csv", row.names = FALSE)
    #write.csv(Train_summary , "Train_summary.csv", row.names = FALSE)
    List_of_Trains <- read.csv("List_of_Trains.csv" ,stringsAsFactors=FALSE, colClasses=c("number"="character"))
    List_of_Stations <- read.csv("List_of_Station.csv",stringsAsFactors=FALSE)
    Route_of_Trains <- read.csv("Route_of_Trains.csv",stringsAsFactors=FALSE, colClasses=c("train_number"="character"))
    Train_summary <- read.csv("Train_summary.csv",stringsAsFactors=FALSE, colClasses=c("number"="character"))
    
##Getting Data (Daily Extraction)----
    raw_data <- fromJSON("http://api.railwayapi.com/live/train/12722/doj/20161002/apikey/klbec7664/")
    testing <- clean_train_status(raw_data)


    fromWeb3 <- fromJSON("http://api.railwayapi.com/live/train/12721/doj/20160926/apikey/klbec7664/")
    
    
    Route_of_Trains12721 <- Train_route(12721, "klbec7664")
    Route_of_Trains12722 <- Train_route(12722, "klbec7664")
    travel_time <- as.numeric(difftime(strptime(Route_of_Trains12722$schdep[Route_of_Trains12722$no == 65], "%H:%M"), strptime(Route_of_Trains12722$scharr[Route_of_Trains12722$no == 1], "%H:%M"), units = c("hours")))
    travel_time
    