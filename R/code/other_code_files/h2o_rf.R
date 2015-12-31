
predictions_h2o_rf <- function(trData, teData) {
  
  #   # # add this condition when refnull is not checked already-> [records>naCounts,]
  #   ccc <-trData[,.(
  #     target = log1p(mean(Expected)),
  #     meanRef = mean(Ref,na.rm=T),
  #     sumRef = sum(Ref,na.rm=T),
  #     records = .N,
  #     naCounts = sum(is.na(Ref))
  #   ),Id]#[target<log1p(700000)]
  #   # rm(ccc)
  #   
  #   qplot(ccc$target, geom="histogram") + 
  #     scale_y_continuous(breaks=setTicks(6)) +
  #     labs(title = "histogram",
  #          x = "log(expected)",
  #          y = "histogram count")
  
  print("Training started...")
  # add this condition when refnull is not checked already-> [records>naCounts,]
  # adding this gives good accuracy [target<log1p(36)]
  trData1 <- trData
  trData1$modRef <- get_mod_ref(trData1$Ref)
  trData1$radardist_km <- rescale(trData1$radardist_km)
  thData <- trData1[,.(
    target = log1p(mean(Expected)),
    rd = mean(radardist_km),
    
    meanRef = mean(get_valid_time(minutes_past) * Ref, na.rm = T) * mean(radardist_km),
    #varRef = var(get_valid_time(minutes_past) * Ref, na.rm = T) * mean(radardist_km),
    #meanRef50p = mean(get_valid_time(minutes_past) * Ref_5x5_50th, na.rm = T) * mean(radardist_km),
    #varRef50p = var(get_valid_time(minutes_past) * Ref_5x5_50th, na.rm = T) * mean(radardist_km),
    meanRef90p = mean(Ref_5x5_90th, na.rm = T) * mean(radardist_km),
    varRef90p = mean(Ref_5x5_90th, na.rm = T) * mean(radardist_km),
    
    #meanRefcomp = mean(get_valid_time(minutes_past) * RefComposite,na.rm=T) * mean(radardist_km),
    #varRefcomp = var(get_valid_time(minutes_past) * RefComposite,na.rm=T) * mean(radardist_km),
    #meanRefcomp50p = mean(get_valid_time(minutes_past) * RefComposite_5x5_50th,na.rm=T) * mean(radardist_km),
    #varRefcomp50p = var(get_valid_time(minutes_past) * RefComposite_5x5_50th,na.rm=T) * mean(radardist_km),
    meanRefcomp90p = mean(RefComposite_5x5_90th,na.rm=T) * mean(radardist_km),
    varRefcomp90p = var(RefComposite_5x5_90th,na.rm=T) * mean(radardist_km),
    
    #meanRhoHV = mean(RhoHV, na.rm = T) * mean(radardist_km),
    #varRhoHV = var(RhoHV, na.rm = T) * mean(radardist_km),
    #meanRhoHV10p = mean(RhoHV_5x5_10th, na.rm = T) * mean(radardist_km),
    #varRhoHV10p = var(RhoHV_5x5_10th, na.rm = T) * mean(radardist_km),
    meanRhoHV50p = mean(RhoHV_5x5_50th, na.rm = T) * mean(radardist_km),
    #varRhoHV50p = var(RhoHV_5x5_50th, na.rm = T) * mean(radardist_km),
    #meanRhoHV90p = mean(RhoHV_5x5_90th, na.rm = T) * mean(radardist_km),
    varRhoHV90p = var(RhoHV_5x5_90th, na.rm = T) * mean(radardist_km),
    
    #meanZdr = mean(get_valid_time(minutes_past) * Zdr, na.rm = T) * mean(radardist_km),
    #varZdr = var(get_valid_time(minutes_past) * Zdr, na.rm = T) * mean(radardist_km),
    meanZdr50p = mean(get_valid_time(minutes_past) * Zdr_5x5_50th, na.rm = T) * mean(radardist_km),
    varZdr50p = var(get_valid_time(minutes_past) * Zdr_5x5_50th, na.rm = T) * mean(radardist_km),
    #meanZdr90p = mean(Zdr_5x5_90th, na.rm = T) * mean(radardist_km),
    #varZdr90p = var(Zdr_5x5_90th, na.rm = T) * mean(radardist_km),
    
    #meanKdp10p = mean(Kdp_5x5_10th, na.rm = T) * mean(radardist_km),
    #varKdp10p = var(Kdp_5x5_10th, na.rm = T) * mean(radardist_km),
    meanKdp50p = mean(Kdp_5x5_50th, na.rm = T) * mean(radardist_km),
    varKdp50p = var(Kdp_5x5_50th, na.rm = T) * mean(radardist_km),
    #meanKdp90p = mean(Kdp_5x5_90th, na.rm = T) * mean(radardist_km),
    #varKdp90p = var(Kdp_5x5_90th, na.rm = T) * mean(radardist_km),
    
    mp = sum(get_valid_time(minutes_past) * modRef, na.rm = T),
    records = .N,
    naCounts = sum(is.na(Ref))
  ),Id][target<log1p(70)]
  
  trainHex<-as.h2o(thData,destination_frame="train.hex")
  cols <- colnames(thData)
  xFeatures <- cols[3:length(cols)] # removing "id" and "target"
  
  rfHex<-h2o.randomForest(x=xFeatures,
                          y="target",training_frame=trainHex, ntrees=200, max_depth= 25) #doesn't change optimal ensemble, makes h2o only bad, makes good when seen with other sub-optimal ensemble
  
  print("Training finished...")
  
  teData1 <- teData
  teData1$modRef <- get_mod_ref(teData1$Ref)
  teData1$radardist_km <- rescale(teData1$radardist_km)
  testHex<-as.h2o(teData1[,.(
    rd = mean(radardist_km),
    
    meanRef = mean(get_valid_time(minutes_past) * Ref, na.rm = T) * mean(radardist_km),
    #varRef = var(get_valid_time(minutes_past) * Ref, na.rm = T) * mean(radardist_km),
    #meanRef50p = mean(get_valid_time(minutes_past) * Ref_5x5_50th, na.rm = T) * mean(radardist_km),
    #varRef50p = var(get_valid_time(minutes_past) * Ref_5x5_50th, na.rm = T) * mean(radardist_km),
    meanRef90p = mean(Ref_5x5_90th, na.rm = T) * mean(radardist_km),
    varRef90p = mean(Ref_5x5_90th, na.rm = T) * mean(radardist_km),
    
    #meanRefcomp = mean(get_valid_time(minutes_past) * RefComposite,na.rm=T) * mean(radardist_km),
    #varRefcomp = var(get_valid_time(minutes_past) * RefComposite,na.rm=T) * mean(radardist_km),
    #meanRefcomp50p = mean(get_valid_time(minutes_past) * RefComposite_5x5_50th,na.rm=T) * mean(radardist_km),
    #varRefcomp50p = var(get_valid_time(minutes_past) * RefComposite_5x5_50th,na.rm=T) * mean(radardist_km),
    meanRefcomp90p = mean(RefComposite_5x5_90th,na.rm=T) * mean(radardist_km),
    varRefcomp90p = var(RefComposite_5x5_90th,na.rm=T) * mean(radardist_km),
    
    #meanRhoHV = mean(RhoHV, na.rm = T) * mean(radardist_km),
    #varRhoHV = var(RhoHV, na.rm = T) * mean(radardist_km),
    #meanRhoHV10p = mean(RhoHV_5x5_10th, na.rm = T) * mean(radardist_km),
    #varRhoHV10p = var(RhoHV_5x5_10th, na.rm = T) * mean(radardist_km),
    meanRhoHV50p = mean(RhoHV_5x5_50th, na.rm = T) * mean(radardist_km),
    #varRhoHV50p = var(RhoHV_5x5_50th, na.rm = T) * mean(radardist_km),
    #meanRhoHV90p = mean(RhoHV_5x5_90th, na.rm = T) * mean(radardist_km),
    varRhoHV90p = var(RhoHV_5x5_90th, na.rm = T) * mean(radardist_km),
    
    #meanZdr = mean(get_valid_time(minutes_past) * Zdr, na.rm = T) * mean(radardist_km),
    #varZdr = var(get_valid_time(minutes_past) * Zdr, na.rm = T) * mean(radardist_km),
    meanZdr50p = mean(get_valid_time(minutes_past) * Zdr_5x5_50th, na.rm = T) * mean(radardist_km),
    varZdr50p = var(get_valid_time(minutes_past) * Zdr_5x5_50th, na.rm = T) * mean(radardist_km),
    #meanZdr90p = mean(Zdr_5x5_90th, na.rm = T) * mean(radardist_km),
    #varZdr90p = var(Zdr_5x5_90th, na.rm = T) * mean(radardist_km),
    
    #meanKdp10p = mean(Kdp_5x5_10th, na.rm = T) * mean(radardist_km),
    #varKdp10p = var(Kdp_5x5_10th, na.rm = T) * mean(radardist_km),
    meanKdp50p = mean(Kdp_5x5_50th, na.rm = T) * mean(radardist_km),
    varKdp50p = var(Kdp_5x5_50th, na.rm = T) * mean(radardist_km),
    #meanKdp90p = mean(Kdp_5x5_90th, na.rm = T) * mean(radardist_km),
    #varKdp90p = var(Kdp_5x5_90th, na.rm = T) * mean(radardist_km),
    
    mp = sum(get_valid_time(minutes_past) * modRef, na.rm = T),
    records = .N,
    naCounts = sum(is.na(Ref))
  ),Id])
  
  predictionsh2o<-as.data.frame(h2o.predict(rfHex,testHex))
  return(expm1(predictionsh2o))
}