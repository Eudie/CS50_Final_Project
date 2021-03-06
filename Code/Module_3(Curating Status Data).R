##Author: Eudie------
##This is my CS50 final project. Here I am attempting to
##I am trying to analyze the effeciency of indian railways
##The metric I chose is the average train delay in departure in past 30 days
##I am fetching the data from unofficial APIs "railwayapi.com"
##This Module is to curate daily status data for ml algorithms.

##library used and WD----
#setwd("D:/Study/CS_basics/CS50_Final_Project")
#setwd("~/PythonProjects/CS50_Final_Project")  
  library(ggplot2)
  library(chron)
  library(lubridate)
  library(plyr)
  library(ggmap)
  library(scales)
##Reading data from local----
List_of_Trains <- read.csv("Data/List_of_Trains.csv" ,stringsAsFactors=FALSE, colClasses=c("number"="character"))
List_of_Stations <- read.csv("Data/List_of_Station.csv",stringsAsFactors=FALSE)
Route_of_Trains <- read.csv("Data/Route_of_Trains.csv",stringsAsFactors=FALSE, colClasses=c("train_number"="character"))
Detail_of_Stations <- read.csv("Data/Detail_of_Stations.csv",stringsAsFactors=FALSE )
Train_summary <- read.csv("Data/Train_summary.csv",stringsAsFactors=FALSE, colClasses=c("number"="character"))

##Reading all status file in data frame and filtering unwanted data----
  file_names <- dir("Data/Daily_status/") 
  
  LiveStatus <- do.call(rbind,lapply(paste0("Data/Daily_status/",file_names),read.csv,stringsAsFactors = F, colClasses=c("train_no"="character")))
  LiveStatus$uniqueKey <- paste(LiveStatus$train_no, LiveStatus$station, LiveStatus$scharr_datetime, sep = "|")
  
  LiveStatus <- LiveStatus[!(duplicated(LiveStatus$uniqueKey)),]
  LiveStatus$scharr_datetime <- as.POSIXct(LiveStatus$scharr_datetime,tz= "India", "%Y-%m-%d %H:%M:%S")
  LiveStatus$schdep_datetime <- as.POSIXct(LiveStatus$schdep_datetime,tz= "India", "%Y-%m-%d %H:%M:%S")
  LiveStatus$actarr_datetime <- as.POSIXct(LiveStatus$actarr_datetime,tz= "India", "%Y-%m-%d %H:%M:%S")
  LiveStatus$actdep_datetime <- as.POSIXct(LiveStatus$actdep_datetime,tz= "India", "%Y-%m-%d %H:%M:%S")
  
  

##Finding the number of trains arrived on each station 1 hour before and after the arrival of train----
  for(x in c(1:length(LiveStatus$uniqueKey))){
    fil <- LiveStatus[x,]
    LiveStatus$crowd[x] <- nrow(LiveStatus[(LiveStatus$station == fil$station &
                                                 LiveStatus$actarr_datetime >= (fil$actarr_datetime - hours(1)) &
                                                 LiveStatus$actarr_datetime <= (fil$actarr_datetime + hours(1))),])
  }
  LiveStatus <- LiveStatus[LiveStatus$is_reached == TRUE,]

##Filtering the data with only main route of train----
  for(i in c(1:length(LiveStatus$scharr_datetime))){
    if(LiveStatus$no[i]==1){
      LiveStatus$oneRoute[i] = TRUE
    }else if((LiveStatus$scharr_datetime[i]- LiveStatus$scharr_datetime[i-1])>0 & LiveStatus$oneRoute[i-1]==TRUE){
      LiveStatus$oneRoute[i] = TRUE
    }else{
      LiveStatus$oneRoute[i] = FALSE
    }
  }
  LiveStatus <- LiveStatus[LiveStatus$oneRoute == T,]

##Making new variable "last_delay" which gives incremental delay occured on each station of each train----
 for(j in c(1:length(LiveStatus$scharr_datetime))){
   if(LiveStatus$no[j]==1){
     LiveStatus$lateDiscreate[j] = LiveStatus$latemin[j]
   }else{
     LiveStatus$lateDiscreate[j] = LiveStatus$latemin[j] - LiveStatus$latemin[j-1]
   }
 }
LiveStatus_1 <- LiveStatus[1:200000,]
LiveStatus_2 <- LiveStatus[200001:400000,]
LiveStatus_3 <- LiveStatus[400001:600000,]
LiveStatus_4 <- LiveStatus[600001:length(LiveStatus$distance),]
write.csv(LiveStatus_1, "Data/CleanStatus/LiveStatus_1.csv", row.names = FALSE)
write.csv(LiveStatus_2, "Data/CleanStatus/LiveStatus_2.csv", row.names = FALSE)
write.csv(LiveStatus_3, "Data/CleanStatus/LiveStatus_3.csv", row.names = FALSE)
write.csv(LiveStatus_4, "Data/CleanStatus/LiveStatus_4.csv", row.names = FALSE)
##Train Status by Stations----
  TrainStops <- ddply(Route_of_Trains, .(code), "nrow")
  names(TrainStops)[2] <- "stops"
  
  Detail_of_Stations <- merge(Detail_of_Stations, TrainStops, by.x = "code", by.y = "code", all.x = T)
  
  for(i in c(1:length(Detail_of_Stations$code))){
    Detail_of_Stations$DelayOverall[i] <- mean(LiveStatus$latemin[LiveStatus$station == Detail_of_Stations$code[i]])
    Detail_of_Stations$DelayDiscreate[i] <- mean(LiveStatus$lateDiscreate[LiveStatus$station == Detail_of_Stations$code[i]])
  }
  write.csv(Detail_of_Stations, "Data/Detail_of_Stations.csv", row.names = FALSE)
  ##Plotting Chart
  map = get_map(location = 'INDIA', zoom = 4)
  ggmap(map) + geom_point(aes(x = lng, y = lat, size = stops, color = DelayOverall), data = Detail_of_Stations, alpha = 0.5)+
    scale_x_continuous(limits = c(68, 98), expand = c(0, 0)) +scale_y_continuous(limits = c(7, 37), expand = c(0, 0)) +
    scale_size_continuous(name = "No. of train stops",range = c(1,8))  +scale_color_gradient(name = "Overall Delay (mins)",space ="Lab",low = "turquoise", high = "red", limits = c(0,120), oob=squish) +
    theme(text=element_text(size=15),axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(), axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position="top", legend.direction = "horizontal") +
    labs(title = "Overall Delay by location" )
  ggsave("Charts/Station/stationDelayOverall.jpeg", width = 10.5, height = 13)
  
  ggmap(map) + geom_point(aes(x = lng, y = lat, size = stops, color = DelayDiscreate), data = Detail_of_Stations, alpha = 0.5)+
    scale_x_continuous(limits = c(68, 98), expand = c(0, 0)) +scale_y_continuous(limits = c(7, 37), expand = c(0, 0)) +
    scale_size_continuous(name = "No. of train stops",range = c(1,8))  +scale_color_gradient(name = "Discrete Delay (mins)",space ="Lab",low = "turquoise", high = "red", limits = c(0,10), oob=squish) +
    theme(text=element_text(size=15),axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(), axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position="top", legend.direction = "horizontal") +
    labs(title = "Discrete Delay by location" )
  ggsave("Charts/Station/stationDelayDiscrete.jpeg", width = 10.5, height = 13)

##Train Status by train----
  for(i in c(1:length(Train_summary$number))){
    Train_summary$DelayOverall[i] <- mean(LiveStatus$latemin[LiveStatus$train_no == Train_summary$number[i]])
    Train_summary$DelayDiscreate[i] <- mean(LiveStatus$lateDiscreate[LiveStatus$train_no == Train_summary$number[i]])
  }
  write.csv(Train_summary , "Data/Train_summary.csv", row.names = FALSE)
  ##Plotting Chart
  Train_summary$train_type <- factor(Train_summary$train_type, levels = c( "Long Distance","Passenger","Special","MEMU","DMU","Suburban Kol","Reserved"))
  ggplot(data=Train_summary, aes(x = train_type, y =  DelayOverall)) + 
    geom_boxplot( color = "blue", fill = "blue",alpha =0.2, width = 0.4) + theme_bw() + ylim(0,120) +
    labs(title = "Overall delay by type of train", x = "Type of trains", y = "Overall Delay (mins)")
  ggsave("Charts/Train/Delay_by_TrainType.jpeg", width = 11.25, height = 7.5)
  
  ggplot(data=Train_summary, aes(x = train_type, y =  DelayDiscreate)) + 
    geom_boxplot( color = "blue", fill = "blue",alpha =0.2, width = 0.4) + theme_bw() + ylim(0,15) + 
    labs(title = "Discrete delay by type of train", x = "Type of trains", y = "Discrete Delay (mins)")
  ggsave("Charts/Train/DiscreteDelay_by_TrainType.jpeg", width = 11.25, height = 7.5)
  
  