# h2o does very good on local CV but not on public leaderboard
source("marshall_palmer.R")

trData2 <- trData1[,.(
  exp = mean(Expected, na.rm = T)
),Id]
a <- trData2$exp
b <- teData2$exp
cc <- c(a, b)
hist(log1p(cc), labels = T)

classifyExpected <- function(ei) {
  ei <- log1p(ei)
  if (ei < 4) { # USE!
    # VG - precision =  97.4211613150265 recall =  99.9926309066792, OrigIsB =  284974 , classifiedAsB =  292496 , correctlyClassifiedAsB =  284953
    # VG with individual(1 vs all) - precision =  98.9125624989112  recall =  99.6175089657302 OrigIsB =  284974 , classifiedAsB =  287005 , correctlyClassifiedAsB =  283884
    ret <- 'a'
  } else if (ei >= 4 && ei < 6) { # USE!
    # (deprecate-- this result was for 4 to 5.5) precision =  77.319587628866 recall =  1.75192711983181, OrigIsB =  4281 , classifiedAsB =  97 , correctlyClassifiedAsB =  75 
    # AVG with individual(1 vs all) - precision =  76.3480392156863  recall =  50.3231017770598 OrigIsB =  4952 , classifiedAsB =  3264 , correctlyClassifiedAsB =  2492
    ret <- 'b'
  } else if (ei >= 6 && ei < 7.5) { # USE!!
    # (deprecate-- this result was for 5.5 to 7.5) precision =  100 recall =  1.38751238850347, OrigIsB =  2018 , classifiedAsB =  28 , correctlyClassifiedAsB =  28
    # G with individual(1 vs all) - precision =  80.4780876494024  recall =  59.985152190052 OrigIsB =  1347 , classifiedAsB =  1004 , correctlyClassifiedAsB =  808
    ret <- 'c'
  } else if (ei >= 7.5 && ei < 8.5) { # USE!!!
    # USELESS when using multi-class mode - precision =  100  recall =  0.222882615156018 OrigIsB =  1346 , classifiedAsB =  3 , correctlyClassifiedAsB =  3
    # VG with individual(1 vs all) - precision =  96.5384615384615  recall =  74.591381872214 OrigIsB =  1346 , classifiedAsB =  1040 , correctlyClassifiedAsB =  1004
    ret <- 'd'
  } else if (ei >= 8.5) { # <NOT USE>
    # USELESS with multi-class mode     -  precision =  NaN  recall =  0 OrigIsB =  5 , classifiedAsB =  0 , correctlyClassifiedAsB =  0
    # USELESS with individual(1 vs all) - precision =  0  recall =  0 OrigIsB =  5 , classifiedAsB =  1 , correctlyClassifiedAsB =  0
    ret <- 'e'
  }
  return(ret)
}

######################## new ranges
teData <- fullTeData

classifyExpected <- function(ei) {
  ei <- log1p(ei)
  if (ei >= 4.3) {
    ret <- 'x'
  } else {
    ret <- 'o'
  }
  return(ret)
}

trData1 <- trData
trData1$modRef <- get_mod_ref(trData1$Ref)

thData <- trData1[,.(
  targetClass = classifyExpected(mean(Expected)),
  
  rd = mean(radardist_km, na.rm = T),
  meanRef = mean(get_valid_time(minutes_past) * Ref, na.rm = T),
  meanRef50p   = mean(Ref_5x5_50th, na.rm = T),
  meanRef90p  = mean(Ref_5x5_90th, na.rm = T),
  meanRefcomp = mean(RefComposite,na.rm=T),
  meanRefcomp50p = mean(RefComposite_5x5_50th,na.rm=T),
  meanRefcomp90p = mean(RefComposite_5x5_90th,na.rm=T),
  meanZdr = mean(Zdr, na.rm = T),
  meanZdr50p = mean(Zdr_5x5_50th, na.rm = T),
  meanZdr90p = mean(Zdr_5x5_90th, na.rm = T),
  
  mp = sum(get_valid_time(minutes_past) * modRef, na.rm = T),
  records = .N,
  naCounts = sum(is.na(Ref))
),Id]

thData$newT <- as.factor(thData$targetClass)
thData$targetClass <- NULL

trainHex<-as.h2o(thData,destination_frame="train.hex")
cols <- colnames(thData)
xFeatures <- cols[2:(length(cols)-1)] # removing "id" and "target"

rfHex<-h2o.randomForest(x=xFeatures, y="newT",training_frame=trainHex, ntrees=200, max_depth= 25, 
                        balance_classes=T)
print("Training finished...")

teData1 <- teData
teData1$modRef <- get_mod_ref(teData1$Ref)
testHex<-as.h2o(teData1[,.(
  rd = mean(radardist_km, na.rm = T),
  meanRef = mean(get_valid_time(minutes_past) * Ref, na.rm = T),
  meanRef50p   = mean(Ref_5x5_50th, na.rm = T),
  meanRef90p  = mean(Ref_5x5_90th, na.rm = T),
  meanRefcomp = mean(RefComposite,na.rm=T),
  meanRefcomp50p = mean(RefComposite_5x5_50th,na.rm=T),
  meanRefcomp90p = mean(RefComposite_5x5_90th,na.rm=T),
  meanZdr = mean(Zdr, na.rm = T),
  meanZdr50p = mean(Zdr_5x5_50th, na.rm = T),
  meanZdr90p = mean(Zdr_5x5_90th, na.rm = T),
  
  mp = sum(get_valid_time(minutes_past) * modRef, na.rm = T),
  records = .N,
  naCounts = sum(is.na(Ref))
),Id])

classificationh2o<-as.data.frame(h2o.predict(rfHex,testHex))
predClassh2o <- classificationh2o[,1]

# store for later use
#predictions4less <- predicted
#predictions4to6 <- predicted  
#predictions6to7.5 <- predicted
#predictions7.5to8.5 <- predicted  







teData3 <- teData1[,.(
  records = .N,
  naCounts = sum(is.na(Ref))
),Id][records>naCounts]


### RESULT EVALUATION ###
# extract true values and predicted values
teData2 <- teData1[,.(
  exp = mean(Expected, na.rm = T)
),Id]
trueValues <- lapply(teData2$exp, classifyExpected)
#evaluate results
OrigIsB = 0; # CLASS2 in orig
classifiedAsB = 0; # CLASS2 in prediction
correctlyClassifiedAsB = 0; # match found
for (i in seq(1,length(trueValues))) {
  trueVal <- trueValues[i]
  pred <- predicted[i]
  if (trueVal == pred && pred=='x') {
    correctlyClassifiedAsB <- correctlyClassifiedAsB + 1
  }
  if (trueVal == 'x') {
    OrigIsB <- OrigIsB + 1
  }
  if (pred == 'x') {
    classifiedAsB <- classifiedAsB + 1
  }
}
print(paste('precision = ', correctlyClassifiedAsB*100/classifiedAsB, 
            ' recall = ', correctlyClassifiedAsB*100/OrigIsB,
            'OrigIsB = ', OrigIsB, 
            ', classifiedAsB = ', classifiedAsB, 
            ', correctlyClassifiedAsB = ', correctlyClassifiedAsB))




