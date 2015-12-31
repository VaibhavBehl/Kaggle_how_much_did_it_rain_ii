# ON HOLD - local CV and public don't match at -all!

# must have predictionsMP & xgbPrediction initialised from run_ensemble.R,
# and the following values properly initialised from classification.R:
#predictions4less     97-98%
#predictions4to6      75-76%
#predictions6to7.5    78-80%
#predictions7.5to8.5  95-96%

# constant mean values for these ranges taken from ALL training data(with help of hist plot and excel)
mean4less = 3.107855228
mean4to6 = 151.8075933
mean6to7.5 = 931.6206641
mean7.5to8.5 = 2520.865962



predictionsMP<-output_marshall_palmer(teData)
predMPExp <- predictionsMP$Expected

for (i in seq(1,length(predMPExp))) {
  if(predictions4less[i] == 'x')
    predMPExp[i] <- mean4less
  else if(predictions7.5to8.5[i] == 'x')
    predMPExp[i] <- mean7.5to8.5
  else if(predictions6to7.5[i] == 'x')
    predMPExp[i] <- mean6to7.5
  else if(predictions4to6[i] == 'x')
    predMPExp[i] <- mean4to6
}





teExp <- teData[, .(Expected = mean(Expected)),Id]$Expected

minusV <- predMPExp - teExp
p <- sum(abs(minusV))/length(minusV)
p #