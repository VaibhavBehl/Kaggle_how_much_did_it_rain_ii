# Source this file/Run the below lines, before executing any other file
#NOTE: need to have all necessary R packages installed, refer R docs for more information
setwd(dirname(parent.frame(2)$ofile))
library(dplyr)
library(data.table)
library(xgboost)
library(ggplot2)
#library(h2o)
#h2o.init(nthreads=-1)
library(e1071,fastmatch,scales)
source("marshall_palmer.R")
source("xgb.R")

# common utility functions here

# will return a boolean array indicating indices at which 'matchAgainst' values are present in origValues
restrictValues <- function(origValues, matchAgainst) {
  fmatch(origValues,matchAgainst,nomatch=0)>0
}

inchToMM001<-0.254 # conversion value,0.01inch=0.245mm,because rain gauges originally report in inches,but the train Expected values are in mm
inchToMM01<-2.54 # 0.1inch=2.54mm
filterTrainOnExpectedValues <- function(trSel,ul) {
  validValuesSet<-inchToMM001*0:ceiling(ul/inchToMM001) #(changed from 300) reverted back.. this restricts values in between 0.254 and 76.2 
  validValuesSet1<-inchToMM01*0:ceiling(ul/inchToMM01)
  #restrictValues with OR condition,either 0.01 inch of 0.1 inch(as some gauges are 0.1inch also)
  trSel<-trSel[restrictValues(round(Expected,3),validValuesSet) | restrictValues(round(Expected,2),validValuesSet1)] # removing this decreased accuracy
  #trSel<-trSel[which(trSel$Expected<ul)] #blanket restricting
}



# Data Domain specific common functions here

# Ref in the dataset is 10*log(Z) where the log is to the base 10
# therefore, z or zh = 10^(ref/10)
#
# RATE_Z_ZDR = zzdr_aa * (ZH ^ zzdr_bb) * (ZDR ^ zzdr_cc)
#
# **current optimal** MP alpha beta
mpAlpha <- 0.488
mpBeta <- 123

# ** current optimal** z_zdr values
z_zdr_aa <- 0.006
z_zdr_bb <- 0.8
z_zdr_cc <- -0.12


get_mod_z_zdr <- function(ref,zdr,aa,bb,cc) {
  zh <- 10^(ref/10)
  z_zdr <- aa*(zh^bb)*(zdr^cc)
  z_zdr[which(is.infinite(z_zdr))] <- NA
  return(z_zdr)
}

output_z_zdr_valid_time <- function(test,aa,bb,cc) {
  results <- test %>% group_by(Id) %>% summarize(Expected=sum(valid_time*get_mod_z_zdr(rowMeans(cbind (Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=TRUE),rowMeans(cbind (Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th), na.rm=TRUE),aa,bb,cc), na.rm = T))
  return(results)
}

get_mod_kdp <- function(kdp,aa,bb) {
  ret_kdp <- aa*(abs(kdp)^bb)
  return(ret_kdp)
}

output_kdp_valid_time <- function(test,aa,bb,cc) {
  results <- test %>% group_by(Id) %>% summarize(Expected=sum(valid_time*get_mod_kdp(rowMeans(cbind (Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th), na.rm=TRUE),aa,bb), na.rm = T))
  return(results)
}