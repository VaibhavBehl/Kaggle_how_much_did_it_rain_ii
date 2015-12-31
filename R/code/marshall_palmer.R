
# this func will give equal weightage, so no zero entries in begining
get_valid_time_equal_weightage <- function(minutes_past) {
  sort_min_index = order(minutes_past)
  minutes_past <- minutes_past[sort_min_index]
  lmpast <- length(minutes_past)
  
  # calculate the length of time for which each reflectivity value is valid
  valid_time <- rep(0, lmpast)
  valid_time[1] <- minutes_past[1]
  if (lmpast > 1) {
    # for first entry
    leftPart <- 0
    rightPart <- (minutes_past[1] + minutes_past[2])/2
    valid_time[1] <- rightPart-leftPart
    
    if(lmpast>2) { # if more than 2 entries are there
      for (i in seq(2, lmpast-1)) { # all entries in middle
        leftPart <- (minutes_past[i] + minutes_past[i-1])/2
        rightPart <- (minutes_past[i] + minutes_past[i+1])/2
        valid_time[i] <- rightPart-leftPart
      }
    }
    
    # for last entry
    lastIndex = lmpast
    leftPart <- (minutes_past[lastIndex] + minutes_past[lastIndex-1])/2
    rightPart <- 60
    valid_time[lastIndex] <- rightPart-leftPart
  } else {
    valid_time <- 60
  }
  # check and remove zeros
  lvt <- length(valid_time)
  if(lvt>1){
    for (i in seq(1,lvt)){
      if(valid_time[i]==0){
        j = which(valid_time[i:lvt]!=0)[1]; #firstNonZeroIndex
        if(!is.na(j))
          valid_time[i:j] = valid_time[j]/length(i:j)
        else { #no non zero after 'i', search first non-zero before 'i'
          j = which(valid_time[0:i]!=0);
          if(!any(is.na(j)) && length(j)>0){
            jh = j[length(j)]
            valid_time[jh:i] = valid_time[jh]/length(jh:i)
          }
        }
      }
    }
  }
  if(sum(valid_time)!=60)
    stop("sum of valid_time is not 60")
  valid_time = valid_time / 60
  return(valid_time)
}

get_valid_time <- function(minutes_past) {
  sort_min_index = order(minutes_past)
  minutes_past <- minutes_past[sort_min_index]
  
  # calculate the length of time for which each reflectivity value is valid
  valid_time <- rep(0, length(minutes_past))
  valid_time[1] <- minutes_past[1]
  if (length(valid_time) > 1) {
    for (i in seq(2, length(minutes_past))) {
      valid_time[i] <- minutes_past[i] - minutes_past[i-1]
    }
    valid_time[length(valid_time)] = valid_time[length(valid_time)] + 60 - sum(valid_time)
  } else {
    valid_time <- 60
  }
  
  valid_time = valid_time / 60
}

get_mod_ref <- function(ref,a,b) {
  ((10^(ref/10))/b) ^ a
}

get_marshall_palmer <- function(ref,minutes_past,a,b) {
  
  # order reflectivity values and minutes_past
  sort_min_index = order(minutes_past)
  minutes_past <- minutes_past[sort_min_index]
  ref <- ref[sort_min_index]
  
  # calculate the length of time for which each reflectivity value is valid
  validTime <- get_valid_time(minutes_past)
  modRef <- get_mod_ref(ref,a,b)
  sumMp = sum(validTime*modRef, na.rm = T)
  return(sumMp)
  
}

output_marshall_palmer <- function(test) {
  results <- test %>% group_by(Id) %>% summarize(Expected=get_marshall_palmer(Ref,minutes_past,0.625,200))
  return(results)
}

output_marshall_palmer_new <- function(test,a,b) {
  results <- test %>% group_by(Id) %>% summarize(Expected=get_marshall_palmer(Ref,minutes_past,a,b))
  return(results)
}

#USE THIS METHOD from now on.. this method assumes a new column 'valid_time' in the csv file
output_marshall_palmer_new_with_valid_time <- function(test,a,b) {
  #results <- test %>% group_by(Id) %>% summarize(Expected=sum(valid_time*get_mod_ref(Ref,a,b), na.rm = T))
  results <- test %>% group_by(Id) %>% summarize(Expected=sum(valid_time*get_mod_ref(rowMeans(cbind (Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th), na.rm=TRUE),a,b), na.rm = T))
  return(results)
}



