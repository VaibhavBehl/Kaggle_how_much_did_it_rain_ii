## need to have all appropiate variables in workspace
source("utility_functions.R")

trainFileNo <- c(1,2,3,4,5,6,7,8,9,10) #fullTrain
fullTest <- "../../data_CSV/test.csv"
trainFiles <- c(length(trainFileNo))
for (i in 1:length(trainFileNo))
  trainFiles[i] <- paste("../../data_CSV/splits_10/train_new_split", trainFileNo[i], ".csv", sep="")

trData <- rbindlist(lapply(trainFiles, fread, sep=","))
fullTeData <- rbindlist(lapply(fullTest, fread, sep=","))

predictionsMP <-output_marshall_palmer_new_with_valid_time(fullTeData,mpAlpha,mpBeta)
predictionsMP_Z_Zdr <-output_z_zdr_valid_time(fullTeData,z_zdr_aa,z_zdr_bb,z_zdr_cc)

xgbPrediction <- predictions_xgb(trData, fullTeData)
xgbPrediction_temp <- xgbPrediction
xgbPrediction[xgbPrediction>30] <- 30
bestData[bestData$Expected>50]$Expected <- 3.713
#predictionsh2o <-predictions_h2o_rf(trData, fullTeData)
predictionsh2ogbm<-predictions_h2ogbm(trData, fullTeData)

predh2ogbm <- predictionsh2ogbm$predict

## for ALL data
predTemp <- predictionsMP$Expected*0 + xgbPrediction*1

Id <- 1:length(xgbPrediction)
Expected <- xgbPrediction
predictions <- data.frame(Id)
predictions$Expected <- xgbPrediction
dtTime <- gsub(":", "-", Sys.time())
write.csv(predictions,paste("sample_solution_",dtTime,".csv"),row.names=F)


write.table(predictionsh2ogbm, "LAST_RUN_PREDICTIONS_BACKUP(all_data)/predictionsh2ogbm.txt")
h2ogbmRead <- read.table("LAST_RUN_PREDICTIONS_BACKUP(all_data)/predictionsh2ogbm.txt")


write.csv(tr,paste("tr_ALL_many_feat.csv"),row.names=F)

# combining 0.80*(ps23.73851) + 0.20*(ps23.75655)
ps23.73851 <- rbindlist(lapply("ps23.73851.csv", fread, sep=","))
ps23.75655 <- rbindlist(lapply("ps23.75655.csv", fread, sep=","))

ps23.73851$Expected <- 0.70*ps23.73851$Expected + 0.30*ps23.75655$Expected
write.csv(ps23.73851,paste("ps23.73851.new.7030.csv"),row.names=F)

#temp
ps23.73851 <- rbindlist(lapply("ps23.73851.csv", fread, sep=","))
ps23.73851$Expected <- xgbPrediction
write.csv(ps23.73851,paste("latest.new.csv"),row.names=F)

#%%% temp code
predClassh2o=='x' #use this to modify the excel results

bestSample <- "best_sample.csv"
bestData <- rbindlist(lapply(bestSample, fread, sep=","))
fullTeData$target <- log1p(bestData)


bsExp <- bestData$Expected
#modify bsExp based on 'x' in predClassh2o.. (make = 73)
newBsExp <- bsExp
newBsExp[predClassh2o=='x'] <- 2.306

np <- bestSampleData
np$Expected <- newBsExp
dtTime <- gsub(":", "-", Sys.time())
write.csv(np,paste("sample_solution_np_",dtTime,".csv"),row.names=F)
#%%%

#%% temp code
trTemp <- trData[which(log1p(trData$Expected)<=5.303304908)]

xgbPredictionL4 <- predictions_xgb(trTemp, fullTeData)
xgbPrediction4to6 <- predictions_xgb(trTemp, fullTeData)
xgbPrediction6to7.5 <- predictions_xgb(trTemp, fullTeData)
xgbPrediction7.5to8.5 <- predictions_xgb(trTemp, fullTeData)
#%%
### reading best XGB submission
bestSample <- "./best_sample.csv"
bestData <- rbindlist(lapply(bestSample, fread, sep=","))
bExp <- bestData$Expected
### from combineClassificationAndRegression.R
### NEW CODE ### using multiclass prediction outputs(ordered by % precision score)
#predMPExp <- xgbPrediction

for (i in seq(1,length(bExp))) {
  if(i %% 10000 == 0){
    print(i)
  }
  if(predictions4less[i] == 'x'){
    bExp[i] <- mean(c(xgbPredictionL4[i],bExp[i]))
  }
  else if(predictions7.5to8.5[i] == 'x') {
    bExp[i] <- xgbPrediction7.5to8.5[i]
  }
  else if(predictions6to7.5[i] == 'x') {
    bExp[i] <- xgbPrediction6to7.5[i]
  }
  else if(predictions4to6[i] == 'x') {
    bExp[i] <- xgbPrediction4to6[i]
  }
  
  if(predictions4less[i] == 'x' && predictions7.5to8.5[i] == 'x') {
    bExp[i] <- mean(c(bExp[i], xgbPrediction7.5to8.5[i]))
  } else if(predictions4less[i] != 'x' && predictions7.5to8.5[i] != 'x' 
            && predictions6to7.5[i] == 'x' && predictions4to6[i] == 'x') {
    bExp[i] <- mean(c(bExp[i], xgbPrediction4to6[i]))
  }
}
bestData$Expected <- bExp
dtTime <- gsub(":", "-", Sys.time())
write.csv(bestData,paste("sample_solution_",dtTime,".csv"),row.names=F)
###






# plots for all data
# plot of h20 and mp
ph2o <- predictionsh2o*1+ predictionsMP$Expected*0
plot(predictionsMP$Id, expm1(ph2o$predict))
lines(predictionsMP$Id,predictionsMP$Expected,col="green")
# combined plot
summary(predictionsMP$Expected)
newPredictionsh2o <- predictionsh2o[predictionsh2o$predict<log1p(164),]
newPredictionsMP <- predictionsMP[predictionsh2o$predict<log1p(164),]
ph2o <- newPredictionsh2o*1+ newPredictionsMP$Expected*0
plot(newPredictionsMP$Id, expm1(ph2o), ylim=range(c(0,164)), cex=0.4)
par(new = TRUE)
plot(newPredictionsMP$Id, newPredictionsMP$Expected, ylim=range(c(0,164)), cex=0.4)
lines(newPredictionsMP$Id,newPredictionsMP$Expected,col="green")
