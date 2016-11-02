##Author: Eudie------
##This is my CS50 final project. Here I am attempting to
##I am trying to analyze the effeciency of indian railways
##The metric I chose is the average train delay in departure in past 30 days
##I am fetching the data from unofficial APIs "railwayapi.com"
##This Module is just for fetching static(one time) data

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
    
    ##Function is to find length of train
    Detail_of_Train <- function(Train_no, Route){
      fdf <- data.frame(number=character(), total_stations=integer(), distance_covered = integer(),travel_time = numeric() ,stringsAsFactors=FALSE)
      for (x in Train_no){
        if(any(x == Route$train_number)){
        a <- Route[Route$train_number == x,]
        t_type_table <- data.frame(first_num = as.character(c(0,1,2,3,4,5,6,7,8,9)), 
                                   type = c("Special", "Long Distance", "Long Distance", "Suburban Kol", "Suburban Ch De SC", "Passenger", "MEMU", "DMU", "Reserved", "Suburban MU"),
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
        df <- data.frame(number, total_stations,distance_covered,  start_time, end_time, journey_days, journey_time, train_type, is_superfast,stringsAsFactors = F)
        fdf <- rbind(fdf, df)
      }}
      
      fdf$average_speed <- fdf$distance_covered/fdf$journey_time
      return(fdf)
    }

##Getting Data (One time extraction)---- 
    ##I have commented the code used to fetch data using api and writing that to csv. For future iterations i will read csvs.
    
    List_of_Trains <- AllTrains("klbec7664")
    List_of_Stations <- AllStations("klbec7664")
    Route_of_Trains <- Train_route(List_of_Trains$number, "klbec7664")
    Detail_of_Stations <- Station_detail(List_of_Stations$code, "klbec7664")

##Correcting data issues----
    #There are 4 trains whose day is showing one where it should be 2
    Route_of_Trains$day[Route_of_Trains$train_number == "52074" & Route_of_Trains$no == 15] = 2
    Route_of_Trains$day[Route_of_Trains$train_number == "59718" & Route_of_Trains$no == 28] = 2
    Route_of_Trains$day[Route_of_Trains$train_number == "64466" & Route_of_Trains$no == 19] = 2
    Route_of_Trains$day[Route_of_Trains$train_number == "66310" & Route_of_Trains$no == 16] = 2
    
    #Splitting pincode and States in 'Detail of stations'
    Detail_of_Stations$Pin_code <- ifelse(gsub('[^0-9]+','',Detail_of_Stations$state) == "",NA, gsub('[^0-9]+','',Detail_of_Stations$state))
    Detail_of_Stations$state <- gsub(' [0-9]+','',Detail_of_Stations$state)
    Detail_of_Stations$state[Detail_of_Stations$state == "Bhubaneswar"] <- "Odisha"
    Detail_of_Stations$state[Detail_of_Stations$state == "Odisha."] <- "Odisha"
    Detail_of_Stations$state[Detail_of_Stations$state == "West Bengal Station"] <- "West Bengal"
    Detail_of_Stations$state[Detail_of_Stations$state == "Chattisgarh"] <- "Chhattisgarh"
    Detail_of_Stations$state[Detail_of_Stations$state == "Thakrahan - Tamkuhi Station Rd"] <- "Bihar"
    Detail_of_Stations$state[substr(Detail_of_Stations$state, 1, 2) == "18"] <- "Jammu and Kashmir"
    Detail_of_Stations$state[substr(Detail_of_Stations$state, 1, 2) == "19"] <- "Jammu and Kashmir"
    Detail_of_Stations$state[substr(Detail_of_Stations$state, 1, 2) == "78"] <- "Assam"
    Detail_of_Stations$state[substr(Detail_of_Stations$state, 1, 2) == "79"] <- "Arunachal Pradesh"
    Detail_of_Stations$state[Detail_of_Stations$state == "Kaliyani"] <- "Tamil Nadu"
    Detail_of_Stations$state[Detail_of_Stations$state == "Station Rd"] <- "Maharashtra"
    Train_summary <- Detail_of_Train(List_of_Trains$number, Route_of_Trains)
    
##Writing on local----
    write.csv(List_of_Stations , "Data/List_of_Station.csv", row.names = FALSE)
    write.csv(List_of_Trains , "Data/List_of_Trains.csv", row.names = FALSE)
    write.csv(Route_of_Trains , "Data/Route_of_Trains.csv", row.names = FALSE)
    write.csv(Train_summary , "Data/Train_summary.csv", row.names = FALSE)
    write.csv(Detail_of_Stations , "Data/Detail_of_Stations.csv", row.names = FALSE)

##End of the Module----