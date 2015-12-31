## need to have all appropiate variables in workspace (source utility_functions.R properly)
## NOTE: make sure that you have valid_time column in your csv file. This is an advanced feature generated out of minutes_past column. You need to run 'add_valid_time_to_train_test.R' script to add it to train_*.csv files.

source("utility_functions.R")

trainFileNo <- c(1,2,3,4,5,6,7,8,9,10) #fullTrain
fullTest <- "../../data_CSV/test.csv"
trainFiles <- c(length(trainFileNo))
for (i in 1:length(trainFileNo))
  trainFiles[i] <- paste("../../data_CSV/splits_10/train_new_split", trainFileNo[i], ".csv", sep="")

trData <- rbindlist(lapply(trainFiles, fread, sep=","))
fullTeData <- rbindlist(lapply(fullTest, fread, sep=","))

xgbPrediction <- getXgboostPredictions(trData, fullTeData)
Id <- 1:length(xgbPrediction)
Expected <- xgbPrediction
predictions <- data.frame(Id)
predictions$Expected <- xgbPrediction
dtTime <- gsub(":", "-", Sys.time())
write.csv(predictions,paste("sample_solution_",dtTime,".csv"),row.names=F)