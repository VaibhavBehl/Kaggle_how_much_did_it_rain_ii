library(h2o)
library(data.table)
library(Metrics)

## use data table to only read the Estimated, Ref, and Id fields
#print(paste("reading training file:",Sys.time()))
#train<-fread("../input/train.csv",select=c(1,4,24))

predictions_h2ogbm <- function(trData, teData) {

  #Cut off outliers of Expected >= 70
  tr_raw <- subset(trData, Expected < 69)
  tr_raw$modRef <- get_mod_ref(tr_raw$Ref)
  thData <- tr_raw[,.(
    target = log1p(mean(Expected)),
    
    rd = mean(radardist_km, na.rm = T),
    meanRef = mean(get_valid_time(minutes_past) * Ref, na.rm = T),
    meanRef50p   = mean(Ref_5x5_50th, na.rm = T),
    meanRef90p  = mean(Ref_5x5_90th, na.rm = T),
    naRefCount = sum(is.na(Ref)),
    naRefPercCount = sum(is.na(Ref))/.N,
    naRef10pCount = sum(is.na(Ref_5x5_10th)),
    naRef10pPercCount = sum(is.na(Ref_5x5_10th))/.N,
    naRef50pCount = sum(is.na(Ref_5x5_50th)),
    naRef50pPercCount = sum(is.na(Ref_5x5_50th))/.N,
    naRef90pCount = sum(is.na(Ref_5x5_90th)),
    naRef90pPercCount = sum(is.na(Ref_5x5_90th))/.N,
    
    meanRefComp = mean(RefComposite,na.rm=T),
    meanRefComp50p = mean(RefComposite_5x5_50th,na.rm=T),
    meanRefComp90p = mean(RefComposite_5x5_90th,na.rm=T),
    naRefCompCount = sum(is.na(RefComposite)),
    naRefCompPercCount = sum(is.na(RefComposite))/.N,
    naRefComp10pCount = sum(is.na(RefComposite_5x5_10th)),
    naRefComp10pPercCount = sum(is.na(RefComposite_5x5_10th))/.N,
    naRefComp50pCount = sum(is.na(RefComposite_5x5_50th)),
    naRefComp50pPercCount = sum(is.na(RefComposite_5x5_50th))/.N,
    naRefComp90pCount = sum(is.na(RefComposite_5x5_90th)),
    naRefComp90pPercCount = sum(is.na(RefComposite_5x5_90th))/.N,
    
    naRhoHVCount = sum(is.na(RhoHV)),
    naRhoHVPercCount = sum(is.na(RhoHV))/.N,
    naRhoHV10pCount = sum(is.na(RhoHV_5x5_10th)),
    naRhoHV10pPercCount = sum(is.na(RhoHV_5x5_10th))/.N,
    
    meanZdr = mean(Zdr, na.rm = T),
    meanZdr50p = mean(Zdr_5x5_50th, na.rm = T),
    meanZdr90p = mean(Zdr_5x5_90th, na.rm = T),
    naZdrCount = sum(is.na(Zdr)),
    naZdrPercCount = sum(is.na(Zdr))/.N,
    naZdr10pCount = sum(is.na(Zdr_5x5_10th)),
    naZdr10pPercCount = sum(is.na(Zdr_5x5_10th))/.N,
    naZdr50pCount = sum(is.na(Zdr_5x5_50th)),
    naZdr50pPercCount = sum(is.na(Zdr_5x5_50th))/.N,
    naZdr90pCount = sum(is.na(Zdr_5x5_90th)),
    naZdr90pPercCount = sum(is.na(Zdr_5x5_90th))/.N,
    
    naKdpCount = sum(is.na(Kdp)),
    naKdpPercCount = sum(is.na(Kdp))/.N,
    naKdp10pCount = sum(is.na(Kdp_5x5_10th)),
    naKdp10pPercCount = sum(is.na(Kdp_5x5_10th))/.N,
    
    mp = sum(get_valid_time(minutes_past) * modRef, na.rm = T),
    records = .N
  ),Id]
  
  trainHex<-as.h2o(thData, destination_frame="train.hex")
  
  cols <- colnames(thData)
  xFeatures <- cols[3:length(cols)]
  
  gbmHex<-h2o.gbm(x=xFeatures,
                  y="target",training_frame=trainHex,model_id="gbmStarter.hex",
                  distribution="AUTO",
                  #nfolds = 0,
                  seed = 666,
                  ntrees = 1500,
                  max_depth = 21,
                  min_rows = 10,
                  learn_rate = 0.01)
  
  #rm(train)
  gbmHex
  
  
  
  #test<-fread("../input/test.csv",select=c(1,4))
  te_raw <- teData
  te_raw$modRef <- get_mod_ref(te_raw$Ref)
  testHex<-as.h2o(te_raw[,.(
    rd = mean(radardist_km, na.rm = T),
    meanRef = mean(get_valid_time(minutes_past) * Ref, na.rm = T),
    meanRef50p   = mean(Ref_5x5_50th, na.rm = T),
    meanRef90p  = mean(Ref_5x5_90th, na.rm = T),
    naRefCount = sum(is.na(Ref)),
    naRefPercCount = sum(is.na(Ref))/.N,
    naRef10pCount = sum(is.na(Ref_5x5_10th)),
    naRef10pPercCount = sum(is.na(Ref_5x5_10th))/.N,
    naRef50pCount = sum(is.na(Ref_5x5_50th)),
    naRef50pPercCount = sum(is.na(Ref_5x5_50th))/.N,
    naRef90pCount = sum(is.na(Ref_5x5_90th)),
    naRef90pPercCount = sum(is.na(Ref_5x5_90th))/.N,
    
    meanRefComp = mean(RefComposite,na.rm=T),
    meanRefComp50p = mean(RefComposite_5x5_50th,na.rm=T),
    meanRefComp90p = mean(RefComposite_5x5_90th,na.rm=T),
    naRefCompCount = sum(is.na(RefComposite)),
    naRefCompPercCount = sum(is.na(RefComposite))/.N,
    naRefComp10pCount = sum(is.na(RefComposite_5x5_10th)),
    naRefComp10pPercCount = sum(is.na(RefComposite_5x5_10th))/.N,
    naRefComp50pCount = sum(is.na(RefComposite_5x5_50th)),
    naRefComp50pPercCount = sum(is.na(RefComposite_5x5_50th))/.N,
    naRefComp90pCount = sum(is.na(RefComposite_5x5_90th)),
    naRefComp90pPercCount = sum(is.na(RefComposite_5x5_90th))/.N,
    
    naRhoHVCount = sum(is.na(RhoHV)),
    naRhoHVPercCount = sum(is.na(RhoHV))/.N,
    naRhoHV10pCount = sum(is.na(RhoHV_5x5_10th)),
    naRhoHV10pPercCount = sum(is.na(RhoHV_5x5_10th))/.N,
    
    meanZdr = mean(Zdr, na.rm = T),
    meanZdr50p = mean(Zdr_5x5_50th, na.rm = T),
    meanZdr90p = mean(Zdr_5x5_90th, na.rm = T),
    naZdrCount = sum(is.na(Zdr)),
    naZdrPercCount = sum(is.na(Zdr))/.N,
    naZdr10pCount = sum(is.na(Zdr_5x5_10th)),
    naZdr10pPercCount = sum(is.na(Zdr_5x5_10th))/.N,
    naZdr50pCount = sum(is.na(Zdr_5x5_50th)),
    naZdr50pPercCount = sum(is.na(Zdr_5x5_50th))/.N,
    naZdr90pCount = sum(is.na(Zdr_5x5_90th)),
    naZdr90pPercCount = sum(is.na(Zdr_5x5_90th))/.N,
    
    naKdpCount = sum(is.na(Kdp)),
    naKdpPercCount = sum(is.na(Kdp))/.N,
    naKdp10pCount = sum(is.na(Kdp_5x5_10th)),
    naKdp10pPercCount = sum(is.na(Kdp_5x5_10th))/.N,
    
    mp = sum(get_valid_time(minutes_past) * modRef, na.rm = T),
    records = .N
  ),Id],destination_frame="test.hex")
  
  #submission <-fread("../input/sample_solution.csv")
  predictionsh2ogbm<-as.data.frame(h2o.predict(gbmHex,testHex))
  #submission$Expected <- expm1(predictions$predict)*0.7 + submission$Expected*0.3
  
  #summary(submission)
  #write.csv(submission,"R_H2O_gbm.lr15c_6.csv",row.names=F)

}
