library(data.table)
library(ggplot2)
library(fastmatch)
library(xgboost)
library(e1071)
source("marshall_palmer.R")
source("utility_functions.R")


# evalerror <- function(preds, dtrain) {
#   labels <- getinfo(dtrain, "label")
#   err <- as.numeric(mean(abs(labels - preds)))
#   print(err)
#   return(list(metric = "error", value = err))
# }

predictions_xgb <- function(trData, teData) {

  set.seed(292)
  
  ##Valid values based on 0.01in measurements
  ul = expm1(8.5)
  valid_vals <- 0.254 * 0:ceiling(ul/0.254) #(changed from 300) reverted back.. this restricts values in between 0.254 and 76.2 
  valid_vals1 <- 2.54 * 0:ceiling(ul/2.54)
  
  tr_raw <- select(trData, Id, minutes_past, radardist_km, 
                   Ref, Ref_5x5_10th, Ref_5x5_50th, Ref_5x5_90th,
                   RefComposite, RefComposite_5x5_10th, RefComposite_5x5_50th, RefComposite_5x5_90th,
                   RhoHV, RhoHV_5x5_10th, RhoHV_5x5_50th, RhoHV_5x5_90th,
                   Zdr, Zdr_5x5_10th, Zdr_5x5_50th, Zdr_5x5_90th,
                   Kdp, Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th,valid_time, Expected) # added
  
  #rm(trData)
  #gc()
  #restrictValues with OR condition, either 0.01 inch of 0.1 inch(some gauges are 0.1inch also)
  tr_raw <- tr_raw[restrictValues(round(Expected, 3),valid_vals) | restrictValues(round(Expected, 2),valid_vals1)] # removing this decreased accuracy
  #tr_raw <- tr_raw[which(tr_raw$Expected<expm1(5.5))]
  
  tr_raw$modRef <- get_mod_ref(tr_raw$Ref,mpAlpha,mpBeta)
  tr_raw$modRefAll <- get_mod_ref(rowMeans(cbind (tr_raw$Ref,tr_raw$Ref_5x5_10th,tr_raw$Ref_5x5_50th,tr_raw$Ref_5x5_90th), na.rm=T),mpAlpha,mpBeta)
  tr_raw$modZZdrAll <- get_mod_z_zdr(rowMeans(cbind (tr_raw$Ref,tr_raw$Ref_5x5_10th,tr_raw$Ref_5x5_50th,tr_raw$Ref_5x5_90th), na.rm=T),rowMeans(cbind (tr_raw$Zdr,tr_raw$Zdr_5x5_10th,tr_raw$Zdr_5x5_50th,tr_raw$Zdr_5x5_90th), na.rm=T),z_zdr_aa,z_zdr_bb,z_zdr_cc)
  #group records by ID
  tr <- tr_raw[, .(
    target = log1p(mean(Expected)),
    
    rd = mean(radardist_km, na.rm = T),
    meanRef = mean(valid_time * Ref, na.rm = T),
    meanRef50p   = mean(Ref_5x5_50th, na.rm = T),
    meanRef90p  = mean(Ref_5x5_90th, na.rm = T),
    meanRefAll = mean(rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=T),na.rm = T),
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
    meanRefCompAll = mean(rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),na.rm = T),
    naRefCompCount = sum(is.na(RefComposite)),
    naRefCompPercCount = sum(is.na(RefComposite))/.N,
    naRefComp10pCount = sum(is.na(RefComposite_5x5_10th)),
    naRefComp10pPercCount = sum(is.na(RefComposite_5x5_10th))/.N,
    naRefComp50pCount = sum(is.na(RefComposite_5x5_50th)),
    naRefComp50pPercCount = sum(is.na(RefComposite_5x5_50th))/.N,
    naRefComp90pCount = sum(is.na(RefComposite_5x5_90th)),
    naRefComp90pPercCount = sum(is.na(RefComposite_5x5_90th))/.N,
    
    meanRhoHVAll = mean(rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th), na.rm=T),na.rm = T),
    naRhoHVCount = sum(is.na(RhoHV)),
    naRhoHVPercCount = sum(is.na(RhoHV))/.N,
    naRhoHV10pCount = sum(is.na(RhoHV_5x5_10th)),
    naRhoHV10pPercCount = sum(is.na(RhoHV_5x5_10th))/.N,
    
    meanZdr = mean(Zdr, na.rm = T),
    meanZdr50p = mean(Zdr_5x5_50th, na.rm = T),
    meanZdr90p = mean(Zdr_5x5_90th, na.rm = T),
    meanZdrAll = mean(rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th), na.rm=T),na.rm = T),
    naZdrCount = sum(is.na(Zdr)),
    naZdrPercCount = sum(is.na(Zdr))/.N,
    naZdr10pCount = sum(is.na(Zdr_5x5_10th)),
    naZdr10pPercCount = sum(is.na(Zdr_5x5_10th))/.N,
    naZdr50pCount = sum(is.na(Zdr_5x5_50th)),
    naZdr50pPercCount = sum(is.na(Zdr_5x5_50th))/.N,
    naZdr90pCount = sum(is.na(Zdr_5x5_90th)),
    naZdr90pPercCount = sum(is.na(Zdr_5x5_90th))/.N,
    
    meanKdpAll = mean(rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),na.rm = T),
    naKdpCount = sum(is.na(Kdp)),
    naKdpPercCount = sum(is.na(Kdp))/.N,
    naKdp10pCount = sum(is.na(Kdp_5x5_10th)),
    naKdp10pPercCount = sum(is.na(Kdp_5x5_10th))/.N,
    
    mp = sum(valid_time * modRef, na.rm = T),
    mpAll = sum(valid_time * modRefAll, na.rm = T),
    zzdrAll = sum(valid_time * modZZdrAll, na.rm = T),
    records = .N
  ), Id]
  rm(tr_raw)
  
  cols <- colnames(tr)
  xFeatures <- cols[3:length(cols)]
  y<-tr$target
  tr<-as.data.frame(tr)
  tr<-tr[,xFeatures]
  param0 <- list("objective"  = "reg:linear" 
                 , "eval_metric" = "rmse"
                 , "eta" = 0.01 # 0.01 better than 0.07
                 , "subsample" = 0.8 # (0.5,1,0.1) worse at 0.5,0.1,1, orig = 0.8
                 , "min_child_weight" = 1 # this was tested to be optimal[orig=10]
                 , "max_depth" = 9
                 , "nthreads" = 4
                 #, "eval_metric"=evalerror
                 #, "maximize"=FALSE
                 #, "feval"=evalerror
  )

  print("building xgb.DMatrix ...")
  
  xgtrain = xgb.DMatrix(as.matrix(tr), label = y, missing = NA)
  
  rm(tr)
  gc()
  
  print("xgb.train started... ")
  xgbModel  <- xgb.train(params = param0, data = xgtrain , nrounds =300, verbose = 2)
  #dtTime <- gsub(":|\\s", "-", Sys.time())
  ##xgb.dump(xgbModel,paste('xgbModel.dump',dtTime), with.stats = TRUE)
  #xgb.save(xgbModel,'xgbModel.xgb.save')
  #xgbModel.load = xgb.load('xgbModel.xgb.save')
  print("xgb.train finished, Processing test data...")
  
  # te_raw<-fread("test.csv", select=c(    
  #   "Id", 
  #   "minutes_past", 
  #   "radardist_km",
  #   "Ref", 
  #   "RefComposite", 
  #   "Expected")
  # )
  
  te_raw <- select(teData, Id, minutes_past, radardist_km, 
                   Ref, Ref_5x5_10th, Ref_5x5_50th, Ref_5x5_90th,
                   RefComposite, RefComposite_5x5_10th, RefComposite_5x5_50th, RefComposite_5x5_90th,
                   RhoHV, RhoHV_5x5_10th, RhoHV_5x5_50th, RhoHV_5x5_90th,
                   Zdr, Zdr_5x5_10th, Zdr_5x5_50th, Zdr_5x5_90th,
                   Kdp, Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th,valid_time) # added
  #rm(teData)
  
  te_raw$modRef <- get_mod_ref(te_raw$Ref,mpAlpha,mpBeta)
  te_raw$modRefAll <- get_mod_ref(rowMeans(cbind (te_raw$Ref,te_raw$Ref_5x5_10th,te_raw$Ref_5x5_50th,te_raw$Ref_5x5_90th), na.rm=T),mpAlpha,mpBeta)
  te_raw$modZZdrAll <- get_mod_z_zdr(rowMeans(cbind (te_raw$Ref,te_raw$Ref_5x5_10th,te_raw$Ref_5x5_50th,te_raw$Ref_5x5_90th), na.rm=T),rowMeans(cbind (te_raw$Zdr,te_raw$Zdr_5x5_10th,te_raw$Zdr_5x5_50th,te_raw$Zdr_5x5_90th), na.rm=T),z_zdr_aa,z_zdr_bb,z_zdr_cc)
  
  te <- te_raw[, .(
    rd = mean(radardist_km, na.rm = T),
    meanRef = mean(valid_time * Ref, na.rm = T),
    meanRef50p   = mean(Ref_5x5_50th, na.rm = T),
    meanRef90p  = mean(Ref_5x5_90th, na.rm = T),
    meanRefAll = mean(rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=T),na.rm = T),
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
    meanRefCompAll = mean(rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),na.rm = T),
    naRefCompCount = sum(is.na(RefComposite)),
    naRefCompPercCount = sum(is.na(RefComposite))/.N,
    naRefComp10pCount = sum(is.na(RefComposite_5x5_10th)),
    naRefComp10pPercCount = sum(is.na(RefComposite_5x5_10th))/.N,
    naRefComp50pCount = sum(is.na(RefComposite_5x5_50th)),
    naRefComp50pPercCount = sum(is.na(RefComposite_5x5_50th))/.N,
    naRefComp90pCount = sum(is.na(RefComposite_5x5_90th)),
    naRefComp90pPercCount = sum(is.na(RefComposite_5x5_90th))/.N,
    
    meanRhoHVAll = mean(rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th), na.rm=T),na.rm = T),
    naRhoHVCount = sum(is.na(RhoHV)),
    naRhoHVPercCount = sum(is.na(RhoHV))/.N,
    naRhoHV10pCount = sum(is.na(RhoHV_5x5_10th)),
    naRhoHV10pPercCount = sum(is.na(RhoHV_5x5_10th))/.N,
    
    meanZdr = mean(Zdr, na.rm = T),
    meanZdr50p = mean(Zdr_5x5_50th, na.rm = T),
    meanZdr90p = mean(Zdr_5x5_90th, na.rm = T),
    meanZdrAll = mean(rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th), na.rm=T),na.rm = T),
    naZdrCount = sum(is.na(Zdr)),
    naZdrPercCount = sum(is.na(Zdr))/.N,
    naZdr10pCount = sum(is.na(Zdr_5x5_10th)),
    naZdr10pPercCount = sum(is.na(Zdr_5x5_10th))/.N,
    naZdr50pCount = sum(is.na(Zdr_5x5_50th)),
    naZdr50pPercCount = sum(is.na(Zdr_5x5_50th))/.N,
    naZdr90pCount = sum(is.na(Zdr_5x5_90th)),
    naZdr90pPercCount = sum(is.na(Zdr_5x5_90th))/.N,
    
    meanKdpAll = mean(rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),na.rm = T),
    naKdpCount = sum(is.na(Kdp)),
    naKdpPercCount = sum(is.na(Kdp))/.N,
    naKdp10pCount = sum(is.na(Kdp_5x5_10th)),
    naKdp10pPercCount = sum(is.na(Kdp_5x5_10th))/.N,
    
    mp = sum(valid_time * modRef, na.rm = T),
    mpAll = sum(valid_time * modRefAll, na.rm = T),
    zzdrAll = sum(valid_time * modZZdrAll, na.rm = T),
    records = .N
  ),Id]
  te<-as.data.frame(te)
  
  rm(te_raw)
  gc()
  ## <- temp , exec till here for test
  print("Building test xgb.DMatrix...")
  xgtest = xgb.DMatrix(as.matrix(te[,xFeatures ]), missing = NA)
  
  print("Predicting...")
  pr  <- predict(xgbModel,xgtest)
  
  xgbPrediction <- expm1(pr)
  
  xgbPrediction[xgbPrediction<0] <-0
  xgbPrediction <- round(xgbPrediction / 0.254) * 0.254
  print("Prediction DONE...")
  
  #xgb.importance(feature_names = xFeatures, model = xgbModel)
  rm(xgbModel)
  rm(te)
  gc()
  
  return(xgbPrediction)
}