##Author: Eudie------
##This is my CS50 final project. Here I am attempting to
##I am trying to analyze the effeciency of indian railways
##The metric I chose is the average train delay in departure in past 30 days
##I am fetching the data from unofficial APIs "railwayapi.com"
##This Module is just for fetching the data of all trains on daily basis

##library used----
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
    
    #df[,c("scharr","actarr","schdep", "actdep", "scharr_date", "actarr_date", "day", "no")] <- NULL
    df$train_no <- json_data$train_number
    return(df)
  }

##Getting daily train status data----
  raw_data <- fromJSON("http://api.railwayapi.com/live/train/15119/doj/20161012/apikey/klbec7664/")
  testing <- clean_train_status(raw_data)
  PracticeTrainSummary = Train_summary[Train_summary$number %in% c("12721", "12624", "12725", "56661", "15119", "11045", "11047", "11058", "11072"),]

##main----
  
  main <- function(TrainSummary, api_key){
    dataFrame = data.frame(scharr_date=character(), 
                         actdep=character(), 
                         latemin=integer(), 
                         no=integer(), 
                         station=character(), 
                         scharr=character(), 
                         has_departed=logical(), 
                         has_arrived=logical(), 
                         actarr=character(), 
                         actarr_date=character(), 
                         schdep=character(), 
                         distance=integer(), 
                         day=integer(), 
                         scharr_datetime=character(), 
                         actarr_datetime=character(), 
                         schdep_datetime=character(), 
                         actdep_datetime=character(), 
                         train_no=character(), 
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
