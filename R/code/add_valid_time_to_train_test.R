# add valid_time column to train and test .csv so don't have to do that heavy computation again
# make sure after adding the files you move them to the correct directory (under data_csv folder)
#
# INPUT: this scripts needs 'train_new_split*.csv' files as their input. These can be generated from the SQL script - data_preprocess\sql_server_scripts\create_10_train_splits.sql
# OUTPUT: this script will output in the current directory new split files with valid_time added. You need to move these files into the /data_CSV/splits_10/ directory before starting the 'generate_final_solution.R' file

source("utility_functions.R")
# new.hashtable <- function() {
#   e <- new.env()
#   list(set = function(key, value) assign(as.character(key), value, e),
#        get = function(key) get(as.character(key), e),
#        rm = function(key) rm(as.character(key), e))
# }
# ht <- new.hashtable()
# ht$set(245, 3)
# ht$get(245)

trainFileNo <- c(1,2,3,4,5,6,7,8,9,10) #all train files
fullTest <- "../../data_CSV/test.csv"
trainFiles <- c(length(trainFileNo))
for (i in 1:length(trainFileNo)){
  #trainFiles[i]
  trf <- paste("../../data_CSV/splits_10/old_without_valid_time/train_new_split", trainFileNo[i], ".csv", sep="")
  trData <- rbindlist(lapply(trf, fread, sep=","))
  temp<- trData %>% group_by(Id) %>% summarize(valid_time=get_valid_time_equal_weightage(minutes_past))
  trData$valid_time<- temp$valid_time
  write.csv(trData,paste("../train_new_split",trainFileNo[i],".csv",sep = ''),row.names=F,na = '')
}

fullTeData <- rbindlist(lapply(fullTest, fread, sep=","))
temp<- fullTeData %>% group_by(Id) %>% summarize(valid_time=get_valid_time_equal_weightage(minutes_past))
fullTeData$valid_time<- temp$valid_time
write.csv(fullTeData,paste("../test_new.csv",sep = ''),row.names=F,na = '')
