getXgboostPredictions<-function(trData,teData) {
  
  set.seed(292)
  
  print("--processing train data--")
  trSel<-select(trData,Id,minutes_past,radardist_km,
      Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th,
      RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th,
      RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th,
      Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th,
      Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th,valid_time,Expected)
  
  ##Valid values based on 0.01in measurements
  ul=expm1(5.5) #upper cut-off limit
  trSel<-filterTrainOnExpectedValues(trSel,ul)
  trSel$modRef<-get_mod_ref(trSel$Ref,mpAlpha,mpBeta)
  trSel$modRefAll<-get_mod_ref(rowMeans(cbind (trSel$Ref,trSel$Ref_5x5_10th,trSel$Ref_5x5_50th,trSel$Ref_5x5_90th),na.rm=T),mpAlpha,mpBeta)
  trSel$modZZdrAll<-get_mod_z_zdr(rowMeans(cbind (trSel$Ref,trSel$Ref_5x5_10th,trSel$Ref_5x5_50th,trSel$Ref_5x5_90th),na.rm=T),rowMeans(cbind (trSel$Zdr,trSel$Zdr_5x5_10th,trSel$Zdr_5x5_50th,trSel$Zdr_5x5_90th),na.rm=T),z_zdr_aa,z_zdr_bb,z_zdr_cc)
  #collapsing records by ID (one record for each hourly reading)
  train<-trSel[,.(logExpected = log1p(mean(Expected)), #taking log so that large values can be handles(this improves accuracy)
      radarDist = mean(radardist_km,na.rm=T),
      meanRef = mean(valid_time*Ref,na.rm=T),
      meanRef50p   = mean(valid_time*Ref_5x5_50th,na.rm=T),
      meanRef90p  = mean(valid_time*Ref_5x5_90th,na.rm=T),
      meanRefAll = mean(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th),na.rm=T),na.rm = T),
      minRefAll = min(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th),na.rm=T),na.rm = T),
      maxRefAll = max(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th),na.rm=T),na.rm = T),
      madRefAll = mad(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th),na.rm=T),na.rm = T),
      iqrRefAll = IQR(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th),na.rm=T),na.rm = T),
      varRefAll = var(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th),na.rm=T),na.rm = T),
      skewRefAll = skewness(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th),na.rm=T),na.rm = T),
      kurtRefAll = kurtosis(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th),na.rm=T),na.rm = T),
      naRef10pPercCount = sum(is.na(Ref_5x5_10th))/.N,
      
      #orig better,meanRefComp = mean(valid_time*RefComposite,na.rm=T),
      meanRefCompOrig = mean(RefComposite,na.rm=T),
      meanRefComp50p = mean(valid_time*RefComposite_5x5_50th,na.rm=T),
      meanRefComp90p = mean(valid_time*RefComposite_5x5_90th,na.rm=T),
      #orig better,meanRefCompAll = mean(valid_time*rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th),na.rm=T),na.rm = T),
      meanRefCompAllOrig = mean(rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th),na.rm=T),na.rm = T),
      minRefCompAllOrig = min(rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th),na.rm=T),na.rm = T),
      maxRefCompAllOrig = max(rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th),na.rm=T),na.rm = T),
      varRefCompAll = var(valid_time*rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th),na.rm=T),na.rm = T),
      skewRefCompAll = skewness(valid_time*rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th),na.rm=T),na.rm = T),
      naRefComp10pPercCount = sum(is.na(RefComposite_5x5_10th))/.N,
      
      meanRhoHVAll = mean(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th),na.rm=T),na.rm = T),
      minRhoHVAll = min(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th),na.rm=T),na.rm = T),
      maxRhoHVAll = max(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th),na.rm=T),na.rm = T),
      skewRhoHVAll = skewness(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th),na.rm=T),na.rm = T),
      
      meanZdr = mean(valid_time*Zdr,na.rm=T),
      #other better,meanZdrOrig = mean(Zdr,na.rm=T),
      meanZdr50p = mean(valid_time*Zdr_5x5_50th,na.rm=T),
      meanZdr90p = mean(valid_time*Zdr_5x5_90th,na.rm=T),
      meanZdrAll = mean(valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th),na.rm=T),na.rm = T),
      minZdrAll = min(valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th),na.rm=T),na.rm = T),
      maxZdrAll = max(valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th),na.rm=T),na.rm = T),
      medianZdrAll = median(valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th),na.rm=T),na.rm = T),
      
      meanKdpAll = mean(valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th),na.rm=T),na.rm = T),
      minKdpAll = min(valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th),na.rm=T),na.rm = T),
      maxKdpAll = max(valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th),na.rm=T),na.rm = T),
      #other better,meanKdpAllOrig = mean(rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th),na.rm=T),na.rm = T),
      
      recordsPerId=.N,
      mpOrig = sum(valid_time*modRef,na.rm=T),
      mpAll = sum(valid_time*modRefAll,na.rm=T),
      zzdrAll = sum(valid_time*modZZdrAll,na.rm=T)),Id]
  
  train[train==Inf]<-NA
  train[train==-Inf]<-NA
  
  #side load
  #train<-rbindlist(lapply("tr_ensemble7030_many_feat.csv",fread,sep=","))
  #write.csv(te[,xFeatures ],paste("te_ensemble7030_selected_feat.csv"),row.names=F)
  
  cols<-colnames(train)
  xFeatures<-cols[3:length(cols)] #not selecting Id and logExpected
  train<-as.data.frame(train)
  print("--building train xgb.DMatrix--")
  xgbDMTrain=xgb.DMatrix(as.matrix(train[,xFeatures]),label=train$logExpected,missing=NA)
  
  print("--xgb.train started-- ")
  xgbModel<-xgb.train(params=list("max_depth"=9,"eta"=0.01,"subsample"=0.8,"nthread"=4),
                      data=xgbDMTrain,nrounds =5000,verbose=2)
  print("--xgb.train finished--")
  #uncomment below to dump/load model
  #dtTime<-gsub(":|\\s","-",Sys.time())
  ##xgb.dump(xgbModel,paste('xgbModel.dump',dtTime),with.stats=TRUE)
  #xgb.save(xgbModel,'xgbModel.xgb.save')
  #xgbModel.load=xgb.load('xgbModel.xgb.save')
  
  print("--processing test data--")
  teSel<-select(teData,Id,minutes_past,radardist_km,
      Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th,
      RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th,
      RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th,
      Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th,
      Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th,valid_time)
  
  teSel$modRef<-get_mod_ref(teSel$Ref,mpAlpha,mpBeta)
  teSel$modRefAll<-get_mod_ref(rowMeans(cbind (teSel$Ref,teSel$Ref_5x5_10th,teSel$Ref_5x5_50th,teSel$Ref_5x5_90th),na.rm=T),mpAlpha,mpBeta)
  teSel$modZZdrAll<-get_mod_z_zdr(rowMeans(cbind (teSel$Ref,teSel$Ref_5x5_10th,teSel$Ref_5x5_50th,teSel$Ref_5x5_90th),na.rm=T),rowMeans(cbind (teSel$Zdr,teSel$Zdr_5x5_10th,teSel$Zdr_5x5_50th,teSel$Zdr_5x5_90th),na.rm=T),z_zdr_aa,z_zdr_bb,z_zdr_cc)
  test<-teSel[,.(radarDist = mean(radardist_km,na.rm=T),
      meanRef = mean(valid_time*Ref,na.rm=T),
      meanRef50p   = mean(valid_time*Ref_5x5_50th,na.rm=T),
      meanRef90p  = mean(valid_time*Ref_5x5_90th,na.rm=T),
      meanRefAll = mean(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th),na.rm=T),na.rm = T),
      minRefAll = min(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th),na.rm=T),na.rm = T),
      maxRefAll = max(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th),na.rm=T),na.rm = T),
      madRefAll = mad(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th),na.rm=T),na.rm = T),
      iqrRefAll = IQR(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th),na.rm=T),na.rm = T),
      varRefAll = var(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th),na.rm=T),na.rm = T),
      skewRefAll = skewness(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th),na.rm=T),na.rm = T),
      kurtRefAll = kurtosis(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th),na.rm=T),na.rm = T),
      naRef10pPercCount = sum(is.na(Ref_5x5_10th))/.N,
      
      #orig better,meanRefComp = mean(valid_time*RefComposite,na.rm=T),
      meanRefCompOrig = mean(RefComposite,na.rm=T),
      meanRefComp50p = mean(valid_time*RefComposite_5x5_50th,na.rm=T),
      meanRefComp90p = mean(valid_time*RefComposite_5x5_90th,na.rm=T),
      #orig better,meanRefCompAll = mean(valid_time*rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th),na.rm=T),na.rm = T),
      meanRefCompAllOrig = mean(rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th),na.rm=T),na.rm = T),
      minRefCompAllOrig = min(rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th),na.rm=T),na.rm = T),
      maxRefCompAllOrig = max(rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th),na.rm=T),na.rm = T),
      varRefCompAll = var(valid_time*rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th),na.rm=T),na.rm = T),
      skewRefCompAll = skewness(valid_time*rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th),na.rm=T),na.rm = T),
      naRefComp10pPercCount = sum(is.na(RefComposite_5x5_10th))/.N,
      
      meanRhoHVAll = mean(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th),na.rm=T),na.rm = T),
      minRhoHVAll = min(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th),na.rm=T),na.rm = T),
      maxRhoHVAll = max(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th),na.rm=T),na.rm = T),
      skewRhoHVAll = skewness(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th),na.rm=T),na.rm = T),
      
      meanZdr = mean(valid_time*Zdr,na.rm=T),
      #other better,meanZdrOrig = mean(Zdr,na.rm=T),
      meanZdr50p = mean(valid_time*Zdr_5x5_50th,na.rm=T),
      meanZdr90p = mean(valid_time*Zdr_5x5_90th,na.rm=T),
      meanZdrAll = mean(valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th),na.rm=T),na.rm = T),
      minZdrAll = min(valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th),na.rm=T),na.rm = T),
      maxZdrAll = max(valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th),na.rm=T),na.rm = T),
      medianZdrAll = median(valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th),na.rm=T),na.rm = T),
      
      meanKdpAll = mean(valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th),na.rm=T),na.rm = T),
      minKdpAll = min(valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th),na.rm=T),na.rm = T),
      maxKdpAll = max(valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th),na.rm=T),na.rm = T),
      #other better,meanKdpAllOrig = mean(rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th),na.rm=T),na.rm = T),
      
      recordsPerId = .N,
      mpOrig = sum(valid_time*modRef,na.rm=T),
      mpAll = sum(valid_time*modRefAll,na.rm=T),
      zzdrAll = sum(valid_time*modZZdrAll,na.rm=T)),Id]
  
  test[test==Inf]<-NA
  test[test==-Inf]<-NA
  test<-as.data.frame(test)
  
  #side load
  #test<-rbindlist(lapply("test_ensemble7030_many_feat.csv",fread,sep=","))
  #write.csv(test,paste("test_ensemble7030_many_feat.csv"),row.names=F)
  
  rm(teSel)

  print("--Building test xgb.DMatrix--")
  xgbDMTest=xgb.DMatrix(as.matrix(test[,xFeatures]),missing=NA)
  
  print("--Predicting--")
  xgbPrediction<-expm1(predict(xgbModel,xgbDMTest)) # taking exponent because we used log initially
  xgbPrediction[xgbPrediction<0]<-0
  xgbPrediction<-round(xgbPrediction/inchToMM001)*inchToMM001 # rounding to reduce the mae error as much as possible
  print("--Prediction DONE--")
  
  #xgb.importance(feature_names=xFeatures,model=xgbModel) # displays the feature gain info
  rm(xgbModel)
  rm(test)
  
  return(xgbPrediction)
}