source("utility_functions.R")

# on test 1,4,7,10 - local best(xgb only) = 23.08662, Public best = 23.76626

# should choose by random
trainFileNo <- c(1,2,4,5,6,8,9)
testFileNo <- c(3,7,10)
trainFiles <- c(length(trainFileNo))
testFiles <- c(length(testFileNo))
for (i in 1:length(trainFileNo))
  trainFiles[i] <- paste("../../data_CSV/splits_10/train_new_split", trainFileNo[i], ".csv", sep="")
for (i in 1:length(testFileNo))
  testFiles[i] <- paste("../../data_CSV/splits_10/train_new_split", testFileNo[i], ".csv", sep="")
trData <- rbindlist(lapply(trainFiles, fread, sep=","))
teData <- rbindlist(lapply(testFiles, fread, sep=","))

# do Marshall-Palmer before scaling
predictionsMP<-output_marshall_palmer(teData)
xgbPrediction <- predictions_xgb(trData, teData)

## TEMP
#xgbAllFeatures <- rbindlist(lapply("7030/xgbAllFeatures.ensemble.7030.csv", fread, sep=","))
#write.csv(xgbTill50InBestMinMax,paste("xgbTill50InBestMinMax.ensemble.7030.csv"),row.names=F)
xgbCurrentBest <- xgbPrediction
xgbAllFeatures <- xgbPrediction
xgbTill22 <- xgbPrediction
xgbTill22NoRd <- xgbPrediction
xgbTill50InBest <- xgbPrediction
xgbTill50InBestMinMax <- xgbPrediction
xgbTill50InBestMinMaxInfToNa <- xgbPrediction
##
#predictionsh2o<-predictions_h2o_rf(trData, teData)

#predictionsh2ogbm<-predictions_h2ogbm(trData, teData)

 
teExp <- teData[, .(Expected = mean(Expected)),Id]$Expected
# Iterate over various values
resultMat <- matrix(NA, 1, 4)
for (hi in seq(0,10)/10) {
  for (mi in seq(0,10)/10) {
    for (xi in seq(0,10)/10) {
      if (hi + mi + xi == 1){
        
        #predictions <- predictionsh2ogbm*hi + predictionsMP$Expected*mi + xgbPrediction*xi
        predictions <- xgbCurrentBest$x*hi + xgbAllFeatures$x*mi + xgbTill50InBestMinMaxInfToNa*xi
        #xgbPrediction <- predictions_xgb(trData, teData)
        #predictions <- xgbPrediction #
        #predictions <- predictionsh2ogbm
        predTable <- as.data.table(predictions) 
        minusV <- predTable$predictions - teExp
        p <- sum(abs(minusV))/length(minusV)
        p
        print(paste('p=',p))
        pVal <- c(hi,mi,xi,p)
        resultMat <- rbind(resultMat, pVal)
      }
    }
  }
}
summary(resultMat)
resultMatDT <- data.table(resultMat, key="V4")
#resultMatDT <- resultMatDT[order(resultMatDT$V2),]
print(resultMatDT)
write.table(resultMatDT, append = T, "xgb_new_start.txt")
