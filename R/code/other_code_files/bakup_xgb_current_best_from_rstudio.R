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
  ul = expm1(4.5)
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
  ### remove this increased local CV accuracy!!, tr_raw <- tr_raw[restrictValues(round(Expected, 3),valid_vals) | restrictValues(round(Expected, 2),valid_vals1)] # removing this decreased accuracy
  tr_raw <- tr_raw[tr_raw$Expected<ul]
  
  tr_raw$modRef <- get_mod_ref(tr_raw$Ref,mpAlpha,mpBeta)
  tr_raw$modRefAll <- get_mod_ref(rowMeans(cbind (tr_raw$Ref,tr_raw$Ref_5x5_10th,tr_raw$Ref_5x5_50th,tr_raw$Ref_5x5_90th), na.rm=T),mpAlpha,mpBeta)
  tr_raw$modZZdrAll <- get_mod_z_zdr(rowMeans(cbind (tr_raw$Ref,tr_raw$Ref_5x5_10th,tr_raw$Ref_5x5_50th,tr_raw$Ref_5x5_90th), na.rm=T),rowMeans(cbind (tr_raw$Zdr,tr_raw$Zdr_5x5_10th,tr_raw$Zdr_5x5_50th,tr_raw$Zdr_5x5_90th), na.rm=T),z_zdr_aa,z_zdr_bb,z_zdr_cc)
  #group records by ID
  tr <- tr_raw[, .(
    target = log1p(mean(Expected)),
    
    rd = mean(radardist_km, na.rm = T),
    meanRef = mean(valid_time*Ref, na.rm = T),
    meanRef50p   = mean(valid_time*Ref_5x5_50th, na.rm = T),
    meanRef90p  = mean(valid_time*Ref_5x5_90th, na.rm = T),
    meanRefAll = mean(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=T),na.rm = T),
    minRefAll = min(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=T),na.rm = T),
    maxRefAll = max(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=T),na.rm = T),
#     medianRefAll = median(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=T),na.rm = T),
    madRefAll = mad(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=T),na.rm = T),
    iqrRefAll = IQR(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=T),na.rm = T),
    varRefAll = var(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=T),na.rm = T),
#     sdRefAll = sd(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=T),na.rm = T),
    skewRefAll = skewness(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=T),na.rm = T),
    kurtRefAll = kurtosis(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=T),na.rm = T),
#     naRefCount = sum(is.na(Ref)),
#     naRefPercCount = sum(is.na(Ref))/.N,
#     naRef10pCount = sum(is.na(Ref_5x5_10th)),
    naRef10pPercCount = sum(is.na(Ref_5x5_10th))/.N,
#     naRef50pCount = sum(is.na(Ref_5x5_50th)),
#     naRef50pPercCount = sum(is.na(Ref_5x5_50th))/.N,
#     naRef90pCount = sum(is.na(Ref_5x5_90th)),
#     naRef90pPercCount = sum(is.na(Ref_5x5_90th))/.N,
#     
    #orig better, meanRefComp = mean(valid_time*RefComposite,na.rm=T),
    meanRefCompOrig = mean(RefComposite,na.rm=T),
    meanRefComp50p = mean(valid_time*RefComposite_5x5_50th,na.rm=T),
    meanRefComp90p = mean(valid_time*RefComposite_5x5_90th,na.rm=T),
    #orig better, meanRefCompAll = mean(valid_time*rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),na.rm = T),
    meanRefCompAllOrig = mean(rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),na.rm = T),
    minRefCompAllOrig = min(rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),na.rm = T),
    maxRefCompAllOrig = max(rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),na.rm = T),
#     medianRefCompAll = median(valid_time*rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),na.rm = T),
#     madRefCompAll = mad(valid_time*rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),na.rm = T),
#     iqrRefCompAll = IQR(valid_time*rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),na.rm = T),
    varRefCompAll = var(valid_time*rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),na.rm = T),
#     sdRefCompAll = sd(valid_time*rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),na.rm = T),
    skewRefCompAll = skewness(valid_time*rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),na.rm = T),
#     kurtRefCompAll = kurtosis(valid_time*rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),na.rm = T),
#     naRefCompCount = sum(is.na(RefComposite)),
#     naRefCompPercCount = sum(is.na(RefComposite))/.N,
#     naRefComp10pCount = sum(is.na(RefComposite_5x5_10th)),
    naRefComp10pPercCount = sum(is.na(RefComposite_5x5_10th))/.N,
#     naRefComp50pCount = sum(is.na(RefComposite_5x5_50th)),
#     naRefComp50pPercCount = sum(is.na(RefComposite_5x5_50th))/.N,
#     naRefComp90pCount = sum(is.na(RefComposite_5x5_90th)),
#     naRefComp90pPercCount = sum(is.na(RefComposite_5x5_90th))/.N,
#     
    meanRhoHVAll = mean(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th), na.rm=T),na.rm = T),
    minRhoHVAll = min(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th), na.rm=T),na.rm = T),
    maxRhoHVAll = max(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th), na.rm=T),na.rm = T),
#     medianRhoHVAll = median(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th), na.rm=T),na.rm = T),
#     madRhoHVAll = mad(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th), na.rm=T),na.rm = T),
#     iqrRhoHVAll = IQR(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th), na.rm=T),na.rm = T),
#     varRhoHVAll = var(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th), na.rm=T),na.rm = T),
#     sdRhoHVAll = sd(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th), na.rm=T),na.rm = T),
    skewRhoHVAll = skewness(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th), na.rm=T),na.rm = T),
#     kurtRhoHVAll = kurtosis(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th), na.rm=T),na.rm = T),
#     naRhoHVCount = sum(is.na(RhoHV)),
#     naRhoHVPercCount = sum(is.na(RhoHV))/.N,
#     naRhoHV10pCount = sum(is.na(RhoHV_5x5_10th)),
#     naRhoHV10pPercCount = sum(is.na(RhoHV_5x5_10th))/.N,
#     
    meanZdr = mean(valid_time*Zdr, na.rm = T),
    #other better, meanZdrOrig = mean(Zdr, na.rm = T),
    meanZdr50p = mean(valid_time*Zdr_5x5_50th, na.rm = T),
    meanZdr90p = mean(valid_time*Zdr_5x5_90th, na.rm = T),
    meanZdrAll = mean(valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th), na.rm=T),na.rm = T),
    minZdrAll = min(valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th), na.rm=T),na.rm = T),
    maxZdrAll = max(valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th), na.rm=T),na.rm = T),
    medianZdrAll = median(valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th), na.rm=T),na.rm = T),
#     madZdrAll = mad(valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th), na.rm=T),na.rm = T),
#     iqrZdrAll = IQR(valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th), na.rm=T),na.rm = T),
#     varZdrAll = var(valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th), na.rm=T),na.rm = T),
#     sdZdrAll = sd(valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th), na.rm=T),na.rm = T),
#     skewZdrAll = skewness(valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th), na.rm=T),na.rm = T),
#     kurtZdrAll = kurtosis(valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th), na.rm=T),na.rm = T),
#     naZdrCount = sum(is.na(Zdr)),
#     naZdrPercCount = sum(is.na(Zdr))/.N,
#     naZdr10pCount = sum(is.na(Zdr_5x5_10th)),
#     naZdr10pPercCount = sum(is.na(Zdr_5x5_10th))/.N,
#     naZdr50pCount = sum(is.na(Zdr_5x5_50th)),
#     naZdr50pPercCount = sum(is.na(Zdr_5x5_50th))/.N,
#     naZdr90pCount = sum(is.na(Zdr_5x5_90th)),
#     naZdr90pPercCount = sum(is.na(Zdr_5x5_90th))/.N,
#     
    meanKdpAll = mean(valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),na.rm = T),
    minKdpAll = min(valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),na.rm = T),
    maxKdpAll = max(valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),na.rm = T),
    #other better, meanKdpAllOrig = mean(rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),na.rm = T),
#     medianKdpAll = median(valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),na.rm = T),
#     madKdpAll = mad(valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),na.rm = T),
#     iqrKdpAll = IQR(valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),na.rm = T),
#     varKdpAll = var(valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),na.rm = T),
#     sdKdpAll = sd(valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),na.rm = T),
#     skewKdpAll = skewness(valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),na.rm = T),
#     kurtKdpAll = kurtosis(valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),na.rm = T),
#     naKdpCount = sum(is.na(Kdp)),
#     naKdpPercCount = sum(is.na(Kdp))/.N,
#     naKdp10pCount = sum(is.na(Kdp_5x5_10th)),
#     naKdp10pPercCount = sum(is.na(Kdp_5x5_10th))/.N,
#     
#     #Correlation Coefficient(total 10 pairs)
#     covRefRefComp = cov(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=T),
#                         valid_time*rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),
#                         use = 'na.or.complete'),
#     covRefRhoHV = cov(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=T),
#                       valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th), na.rm=T),
#                       use = 'na.or.complete'),
#     covRefZdr = cov(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=T),
#                     valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th), na.rm=T),
#                     use = 'na.or.complete'),
#     covRefKdp = cov(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=T),
#                     valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),
#                     use = 'na.or.complete'),
#     covRefCompRhoHV = cov(valid_time*rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),
#                           valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th), na.rm=T),
#                           use = 'na.or.complete'),
#     covRefCompZdr = cov(valid_time*rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),
#                         valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th), na.rm=T),
#                           use = 'na.or.complete'),
#     covRefCompKdp = cov(valid_time*rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),
#                         valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),
#                         use = 'na.or.complete'),
#     covRhoHVZdr = cov(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th), na.rm=T),
#                       valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th), na.rm=T),
#                       use = 'na.or.complete'),
#     covRhoHVKdp = cov(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th), na.rm=T),
#                       valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),
#                       use = 'na.or.complete'),
#     covZdrKdp = cov(valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th), na.rm=T),
#                       valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),
#                       use = 'na.or.complete'),
#     
#     
    mp = sum(valid_time * modRef, na.rm = T),
    mpAll = sum(valid_time * modRefAll, na.rm = T),
    zzdrAll = sum(valid_time * modZZdrAll, na.rm = T),
    records = .N
  ), Id]
  tr[tr==Inf] <- NA
  tr[tr==-Inf] <- NA
  tr[cbindIsNan(tr)] <- NA
  rm(tr_raw)
  
  #tr <- rbindlist(lapply("tr_ensemble7030_many_feat.csv", fread, sep=","))
  #write.csv(tr,paste("tr_ensemble7030_many_feat.csv"),row.names=F)
  
  cols <- colnames(tr)
  xFeatures <- cols[3:length(cols)]
  y<-tr$target
  tr<-as.data.frame(tr)
  param0 <- list("objective"  = "reg:linear" 
                 , "eval_metric" = "rmse"
                 , "eta" = 0.01 # 0.01 better than 0.07
                 , "subsample" = 0.8 # (0.5,1,0.1) worse at 0.5,0.1,1, orig = 0.8
                 , "min_child_weight" = 1 # this was tested to be optimal[orig=10]
                 , "max_depth" = 9
                 , "nthread" = 18
                 #, "eval_metric"=evalerror
                 #, "maximize"=FALSE
                 #, "feval"=evalerror
  )
  #trScale <- as.data.frame(lapply(tr[,xFeatures], doit)) 

  print("building xgb.DMatrix ...")
  xgtrain = xgb.DMatrix(as.matrix(tr[,xFeatures]), label = y, missing = NA)
  print("xgb.train started... ")
  xgbModel  <- xgb.train(params = param0, data = xgtrain , nrounds =300, verbose = 2)
  #dtTime <- gsub(":|\\s", "-", Sys.time())
  ##xgb.dump(xgbModel,paste('xgbModel.dump',dtTime), with.stats = TRUE)
  #xgb.save(xgbModel,'xgbModel.xgb.save')
  #xgbModel.load = xgb.load('xgbModel.xgb.save')
  print("xgb.train finished, Processing test data...")
  
  #rm(tr,xgtrain)
  #gc()
  
  te_raw <- select(teData, Id, minutes_past, radardist_km, 
                   Ref, Ref_5x5_10th, Ref_5x5_50th, Ref_5x5_90th,
                   RefComposite, RefComposite_5x5_10th, RefComposite_5x5_50th, RefComposite_5x5_90th,
                   RhoHV, RhoHV_5x5_10th, RhoHV_5x5_50th, RhoHV_5x5_90th,
                   Zdr, Zdr_5x5_10th, Zdr_5x5_50th, Zdr_5x5_90th,
                   Kdp, Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th,valid_time) # added
  
  te_raw$modRef <- get_mod_ref(te_raw$Ref,mpAlpha,mpBeta)
  te_raw$modRefAll <- get_mod_ref(rowMeans(cbind (te_raw$Ref,te_raw$Ref_5x5_10th,te_raw$Ref_5x5_50th,te_raw$Ref_5x5_90th), na.rm=T),mpAlpha,mpBeta)
  te_raw$modZZdrAll <- get_mod_z_zdr(rowMeans(cbind (te_raw$Ref,te_raw$Ref_5x5_10th,te_raw$Ref_5x5_50th,te_raw$Ref_5x5_90th), na.rm=T),rowMeans(cbind (te_raw$Zdr,te_raw$Zdr_5x5_10th,te_raw$Zdr_5x5_50th,te_raw$Zdr_5x5_90th), na.rm=T),z_zdr_aa,z_zdr_bb,z_zdr_cc)
  
  te <- te_raw[, .(
    rd = mean(radardist_km, na.rm = T),
    meanRef = mean(valid_time*Ref, na.rm = T),
    meanRef50p   = mean(valid_time*Ref_5x5_50th, na.rm = T),
    meanRef90p  = mean(valid_time*Ref_5x5_90th, na.rm = T),
    meanRefAll = mean(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=T),na.rm = T),
    minRefAll = min(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=T),na.rm = T),
    maxRefAll = max(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=T),na.rm = T),
    #     medianRefAll = median(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=T),na.rm = T),
    madRefAll = mad(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=T),na.rm = T),
    iqrRefAll = IQR(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=T),na.rm = T),
    varRefAll = var(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=T),na.rm = T),
    #     sdRefAll = sd(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=T),na.rm = T),
    skewRefAll = skewness(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=T),na.rm = T),
    kurtRefAll = kurtosis(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=T),na.rm = T),
    #     naRefCount = sum(is.na(Ref)),
    #     naRefPercCount = sum(is.na(Ref))/.N,
    #     naRef10pCount = sum(is.na(Ref_5x5_10th)),
    naRef10pPercCount = sum(is.na(Ref_5x5_10th))/.N,
    #     naRef50pCount = sum(is.na(Ref_5x5_50th)),
    #     naRef50pPercCount = sum(is.na(Ref_5x5_50th))/.N,
    #     naRef90pCount = sum(is.na(Ref_5x5_90th)),
    #     naRef90pPercCount = sum(is.na(Ref_5x5_90th))/.N,
    #     
    #orig better, meanRefComp = mean(valid_time*RefComposite,na.rm=T),
    meanRefCompOrig = mean(RefComposite,na.rm=T),
    meanRefComp50p = mean(valid_time*RefComposite_5x5_50th,na.rm=T),
    meanRefComp90p = mean(valid_time*RefComposite_5x5_90th,na.rm=T),
    #orig better, meanRefCompAll = mean(valid_time*rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),na.rm = T),
    meanRefCompAllOrig = mean(rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),na.rm = T),
    minRefCompAllOrig = min(rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),na.rm = T),
    maxRefCompAllOrig = max(rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),na.rm = T),
    #     medianRefCompAll = median(valid_time*rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),na.rm = T),
    #     madRefCompAll = mad(valid_time*rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),na.rm = T),
    #     iqrRefCompAll = IQR(valid_time*rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),na.rm = T),
    varRefCompAll = var(valid_time*rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),na.rm = T),
    #     sdRefCompAll = sd(valid_time*rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),na.rm = T),
    skewRefCompAll = skewness(valid_time*rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),na.rm = T),
    #     kurtRefCompAll = kurtosis(valid_time*rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),na.rm = T),
    #     naRefCompCount = sum(is.na(RefComposite)),
    #     naRefCompPercCount = sum(is.na(RefComposite))/.N,
    #     naRefComp10pCount = sum(is.na(RefComposite_5x5_10th)),
    naRefComp10pPercCount = sum(is.na(RefComposite_5x5_10th))/.N,
    #     naRefComp50pCount = sum(is.na(RefComposite_5x5_50th)),
    #     naRefComp50pPercCount = sum(is.na(RefComposite_5x5_50th))/.N,
    #     naRefComp90pCount = sum(is.na(RefComposite_5x5_90th)),
    #     naRefComp90pPercCount = sum(is.na(RefComposite_5x5_90th))/.N,
    #     
    meanRhoHVAll = mean(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th), na.rm=T),na.rm = T),
    minRhoHVAll = min(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th), na.rm=T),na.rm = T),
    maxRhoHVAll = max(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th), na.rm=T),na.rm = T),
    #     medianRhoHVAll = median(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th), na.rm=T),na.rm = T),
    #     madRhoHVAll = mad(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th), na.rm=T),na.rm = T),
    #     iqrRhoHVAll = IQR(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th), na.rm=T),na.rm = T),
    #     varRhoHVAll = var(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th), na.rm=T),na.rm = T),
    #     sdRhoHVAll = sd(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th), na.rm=T),na.rm = T),
    skewRhoHVAll = skewness(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th), na.rm=T),na.rm = T),
    #     kurtRhoHVAll = kurtosis(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th), na.rm=T),na.rm = T),
    #     naRhoHVCount = sum(is.na(RhoHV)),
    #     naRhoHVPercCount = sum(is.na(RhoHV))/.N,
    #     naRhoHV10pCount = sum(is.na(RhoHV_5x5_10th)),
    #     naRhoHV10pPercCount = sum(is.na(RhoHV_5x5_10th))/.N,
    #     
    meanZdr = mean(valid_time*Zdr, na.rm = T),
    #other better, meanZdrOrig = mean(Zdr, na.rm = T),
    meanZdr50p = mean(valid_time*Zdr_5x5_50th, na.rm = T),
    meanZdr90p = mean(valid_time*Zdr_5x5_90th, na.rm = T),
    meanZdrAll = mean(valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th), na.rm=T),na.rm = T),
    minZdrAll = min(valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th), na.rm=T),na.rm = T),
    maxZdrAll = max(valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th), na.rm=T),na.rm = T),
    medianZdrAll = median(valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th), na.rm=T),na.rm = T),
    #     madZdrAll = mad(valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th), na.rm=T),na.rm = T),
    #     iqrZdrAll = IQR(valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th), na.rm=T),na.rm = T),
    #     varZdrAll = var(valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th), na.rm=T),na.rm = T),
    #     sdZdrAll = sd(valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th), na.rm=T),na.rm = T),
    #     skewZdrAll = skewness(valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th), na.rm=T),na.rm = T),
    #     kurtZdrAll = kurtosis(valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th), na.rm=T),na.rm = T),
    #     naZdrCount = sum(is.na(Zdr)),
    #     naZdrPercCount = sum(is.na(Zdr))/.N,
    #     naZdr10pCount = sum(is.na(Zdr_5x5_10th)),
    #     naZdr10pPercCount = sum(is.na(Zdr_5x5_10th))/.N,
    #     naZdr50pCount = sum(is.na(Zdr_5x5_50th)),
    #     naZdr50pPercCount = sum(is.na(Zdr_5x5_50th))/.N,
    #     naZdr90pCount = sum(is.na(Zdr_5x5_90th)),
    #     naZdr90pPercCount = sum(is.na(Zdr_5x5_90th))/.N,
    #     
    meanKdpAll = mean(valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),na.rm = T),
    minKdpAll = min(valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),na.rm = T),
    maxKdpAll = max(valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),na.rm = T),
    #other better, meanKdpAllOrig = mean(rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),na.rm = T),
    #     medianKdpAll = median(valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),na.rm = T),
    #     madKdpAll = mad(valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),na.rm = T),
    #     iqrKdpAll = IQR(valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),na.rm = T),
    #     varKdpAll = var(valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),na.rm = T),
    #     sdKdpAll = sd(valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),na.rm = T),
    #     skewKdpAll = skewness(valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),na.rm = T),
    #     kurtKdpAll = kurtosis(valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),na.rm = T),
    #     naKdpCount = sum(is.na(Kdp)),
    #     naKdpPercCount = sum(is.na(Kdp))/.N,
    #     naKdp10pCount = sum(is.na(Kdp_5x5_10th)),
    #     naKdp10pPercCount = sum(is.na(Kdp_5x5_10th))/.N,
    #     
    #     #Correlation Coefficient(total 10 pairs)
    #     covRefRefComp = cov(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=T),
    #                         valid_time*rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),
    #                         use = 'na.or.complete'),
    #     covRefRhoHV = cov(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=T),
    #                       valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th), na.rm=T),
    #                       use = 'na.or.complete'),
    #     covRefZdr = cov(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=T),
    #                     valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th), na.rm=T),
    #                     use = 'na.or.complete'),
    #     covRefKdp = cov(valid_time*rowMeans(cbind(Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=T),
    #                     valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),
    #                     use = 'na.or.complete'),
    #     covRefCompRhoHV = cov(valid_time*rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),
    #                           valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th), na.rm=T),
    #                           use = 'na.or.complete'),
    #     covRefCompZdr = cov(valid_time*rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),
    #                         valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th), na.rm=T),
    #                           use = 'na.or.complete'),
    #     covRefCompKdp = cov(valid_time*rowMeans(cbind(RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th), na.rm=T),
    #                         valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),
    #                         use = 'na.or.complete'),
    #     covRhoHVZdr = cov(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th), na.rm=T),
    #                       valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th), na.rm=T),
    #                       use = 'na.or.complete'),
    #     covRhoHVKdp = cov(valid_time*rowMeans(cbind(RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th), na.rm=T),
    #                       valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),
    #                       use = 'na.or.complete'),
    #     covZdrKdp = cov(valid_time*rowMeans(cbind(Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th), na.rm=T),
    #                       valid_time*rowMeans(cbind(Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=T),
    #                       use = 'na.or.complete'),
    #     
    #     
    mp = sum(valid_time * modRef, na.rm = T),
    mpAll = sum(valid_time * modRefAll, na.rm = T),
    zzdrAll = sum(valid_time * modZZdrAll, na.rm = T),
    records = .N
  ),Id]
  te[te==Inf] <- NA
  te[te==-Inf] <- NA
  te[cbindIsNan(te)] <- NA
  te<-as.data.frame(te) #<<< ran till here _temp_msg
  #teScale <- as.data.frame(lapply(te[,xFeatures], doit))
  
  #te <- rbindlist(lapply("te_ensemble7030_many_feat.csv", fread, sep=","))
  #write.csv(te,paste("te_ensemble7030_many_feat.csv"),row.names=F)
  
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
  #rm(xgbModel)
  #rm(te)
  #gc()
  
  return(xgbPrediction)
}