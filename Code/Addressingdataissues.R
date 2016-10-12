##Author: Eudie------
##This is my CS50 final project. Here I am attempting to
##I am trying to analyze the effeciency of indian railways
##The metric I chose is the average train delay in departure in past 30 days
##I am fetching the data from unofficial APIs "railwayapi.com"
##This Module is TO address data issues on the raw data

##Train summary----
  #There are 4 trains whose day is showing one where it should be 2
  Route_of_Trains$day[Route_of_Trains$train_number == "52074" & Route_of_Trains$no == 15] = 2
  Route_of_Trains$day[Route_of_Trains$train_number == "59718" & Route_of_Trains$no == 28] = 2
  Route_of_Trains$day[Route_of_Trains$train_number == "64466" & Route_of_Trains$no == 19] = 2
  Route_of_Trains$day[Route_of_Trains$train_number == "66310" & Route_of_Trains$no == 16] = 2
  
  Train_summary <- Detail_of_Train(List_of_Trains$number, Route_of_Trains)
  
  write.csv(Route_of_Trains , "Data/Route_of_Trains.csv", row.names = FALSE)
  write.csv(Train_summary , "Data/Train_summary.csv", row.names = FALSE)
