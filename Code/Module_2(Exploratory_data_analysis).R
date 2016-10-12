##Author: Eudie------
##This is my CS50 final project. Here I am attempting to
##I am trying to analyze the effeciency of indian railways
##The metric I chose is the average train delay in departure in past 30 days
##I am fetching the data from unofficial APIs "railwayapi.com"
##This Module is just for understanding the data

##libraries----
  library(ggplot2)

##Summary of trains----
  summary(Train_summary)
  Train_summary$number <- as.factor(Train_summary$number)
  Train_summary$train_type <- as.factor(Train_summary$train_type)
  
  ggplot(data=Train_summary, aes(Train_summary$distance_covered)) + geom_density( aes(group = train_type),col="red", fill="green", alpha = .2)
  ggplot(data=Train_summary, aes(Train_summary$total_stations)) + geom_histogram( aes(group = train_type,colour=train_type, fill=train_type),binwidth=1,col="red", fill="green", alpha = .2)
  ggplot(data=Train_summary, aes(Train_summary$journey_time)) + geom_density( aes(group = is_superfast,colour=is_superfast, fill=is_superfast))
  
  verylong <- Train_summary[Train_summary$distance_covered >3000,]
  