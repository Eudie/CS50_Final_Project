##Author: Eudie------
##This is my CS50 final project. Here I am attempting to
##I am trying to analyze the effeciency of indian railways
##The metric I chose is the average train delay in departure in past 30 days
##I am fetching the data from unofficial APIs "railwayapi.com"
##This Module is just for understanding the data

##library used and WD----
#setwd("D:/Study/CS_basics/CS50_Final_Project")
#setwd("~/PythonProjects/CS50_Final_Project")  
  library(ggplot2)
  library(chron)
  library(lubridate)
  library(plyr)
  library(ggmap)

##Reading data from local----
  List_of_Trains <- read.csv("Data/List_of_Trains.csv" ,stringsAsFactors=FALSE, colClasses=c("number"="character"))
  List_of_Stations <- read.csv("Data/List_of_Station.csv",stringsAsFactors=FALSE)
  Route_of_Trains <- read.csv("Data/Route_of_Trains.csv",stringsAsFactors=FALSE, colClasses=c("train_number"="character"))
  Detail_of_Stations <- read.csv("Data/Detail_of_Stations.csv",stringsAsFactors=FALSE )
  Train_summary <- read.csv("Data/Train_summary.csv",stringsAsFactors=FALSE, colClasses=c("number"="character"))
  

##Summary of trains----
  summary(Train_summary)
  Train_summary$train_type <- as.factor(Train_summary$train_type)
  Train_summary$train_type <- factor(Train_summary$train_type, levels = c( "Long Distance","Passenger","Special","MEMU","DMU","Suburban Kol","Reserved"))
  
##Graphs of Train
  ggplot(data=Train_summary, aes(x = train_type, y =  journey_time)) + 
    geom_boxplot(color = "blue", fill = "blue", alpha =0.2, width = 0.5) + theme_bw() +
    labs(title = "Journey time (Hours) by type of train", x = "Type of trains", y = "Journey Time (Hour)")
  ggsave("Charts/Train/JourneyTime_by_TrainType.jpeg", width = 11.25, height = 7.5)
  
  ggplot(data=Train_summary, aes(train_type)) + geom_bar(color = "blue", fill = "blue", alpha =0.2, width = 0.5) + theme_bw() + labs(title = "Number of trains by type", x = "Type of trains", y = "Number of trains")
  ggsave("Charts/Train/NoOfTrains_by_Type.jpeg", width = 11.25, height = 7.5)
  
  ggplot(data=Train_summary, aes(x = train_type, y =  distance_covered)) + 
    geom_boxplot(color = "blue", fill = "blue", alpha =0.2, width = 0.5) + theme_bw() +
    labs(title = "Distance covered (kms) by type of train", x = "Type of trains", y = "Distance (kms)")
  ggsave("Charts/Train/Distance_by_TrainType.jpeg", width = 11.25, height = 7.5)
  
  ggplot(data=Train_summary, aes(x = train_type, y =  total_stations)) + 
    geom_boxplot(color = "blue", fill = "blue", alpha =0.2, width = 0.5) + theme_bw() +
    labs(title = "Number of stops by type of train", x = "Type of trains", y = "Number of stops")
  ggsave("Charts/Train/NoOFStops_by_TrainType.jpeg", width = 11.25, height = 7.5)
  
  
  ggplot(data=Train_summary, aes(x = train_type, y =  average_speed)) + 
    geom_jitter(aes(color = is_superfast),   alpha =0.2, width = 0.4) + theme_bw() + ylim(0,100) +scale_color_discrete(name = "Is Superfast")+
    labs(title = "Average speed (kmph) by type of train", x = "Type of trains", y = "Average speed (kmph)")
  ggsave("Charts/Train/AverageSpeed_by_TrainType.jpeg", width = 11.25, height = 7.5)
  
  ggplot(data=Train_summary, aes(x = total_stations, y =  distance_covered)) + 
    geom_point(aes(color = train_type, size = average_speed)  ,alpha =0.3) + theme_bw() +scale_color_discrete(name = "Train Type") + scale_size(name = "Avg. Speed (kmph)") +
    labs(title = "Relation between 'Stops' and 'Distance'" , x = "No. of Stops", y = "Distance (kms)")
  ggsave("Charts/Train/Overall.jpeg", width = 11.25, height = 7.5)
  
  ggplot(data=Train_summary, aes(x = total_stations, y =  average_speed)) + 
    geom_point(aes(color = train_type, size = distance_covered)  ,alpha =0.3) + theme_bw() + ylim(0,100) +
    geom_smooth(method = "lm")  + scale_color_discrete(name = "Train Type") + scale_size(name = "Distance (kms)") +
    labs(title = "Relation between 'No. of Stops' and 'Average Speed'", x = "No. of Stops", y = "Average Speed (kmph)")
  ggsave("Charts/Train/speedByStops.jpeg", width = 11.25, height = 7.5)
  
  ggplot(data=Train_summary[Train_summary$train_type == "Long Distance", ], aes(x = total_stations, y =  average_speed)) + 
    geom_point(aes(color = is_superfast, size = distance_covered)  ,alpha =0.3) + theme_bw() + ylim(0,100) +
    geom_smooth()  + scale_color_discrete(name = "Is Superfast") + scale_size(name = "Distance (kms)") +
    labs(title = "Relation between 'No. of Stops' and 'Average Speed' of 'Long Distance' trains" , x = "No. of Stops", y = "Average Speed (kmph)")
  ggsave("Charts/Train/speedByStopsOfLongTrains.jpeg", width = 11.25, height = 7.5)
  
  ggplot(data=Train_summary, aes(x = total_stations, y =  average_speed)) + 
    geom_point(aes(color = is_superfast, size = distance_covered)  ,alpha =0.3) + theme_bw() + ylim(0,100) +
    scale_color_discrete(name = "Is Superfast") + scale_size(name = "Distance (kms)") +
    labs(title = "Relation between 'No. of Stops' and 'Average Speed'" , x = "No. of Stops", y = "Average Speed (kmph)")
  ggsave("Charts/Train/speedByStopsForSuperfast.jpeg", width = 11.25, height = 7.5)
  

##Summary of Stations----
  TrainStops <- ddply(Route_of_Trains, .(code), "nrow")
  names(TrainStops)[2] <- "stops"
  
  Station_with_stops <- merge(Detail_of_Stations, TrainStops, by.x = "code", by.y = "code", all.x = T)
  
  map = get_map(location = 'INDIA')
  ggmap(map) + geom_point(aes(x = lng, y = lat, size = stops), data = Station_with_stops, alpha = 0.5)
  