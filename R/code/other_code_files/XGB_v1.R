library(data.table)
library(ggplot2)
library(fastmatch)
library(xgboost)

#setwd("Documents/Machine Learning/Project")
#Fast %in%
`%fin%` <- function(x, lkup) {
  fmatch(x, lkup, nomatch = 0L) > 0L
}

#Get the time differences between each measure
time_difference <- function(times, num_per_segment = 60) {
  n <- length(times)
  valid_time <- vector(mode="numeric", length = n)
  valid_time[1] <- times[1]
  valid_time[-1] <- diff(times, 1)
  valid_time[n] <- valid_time[n] + num_per_segment - sum(valid_time)
  valid_time <- valid_time / num_per_segment
  valid_time
}

#Convert reflectivity (dbz) to mm/hr
marshall_palmer <- function(dbz) {
  ((10**(dbz/10))/200) ** 0.625
}


##Valid values based on 0.01in measurements
valid_vals <- 0.254 * 1:300

train <- fread("train.csv",select = c(
 "Id", 
 "minutes_past", 
 "radardist_km",
 "Ref", 
 "RefComposite", 
 "Expected"))

"
idNA <- train %>%
  group_by(Id) %>%
  summarise(TotalNA = sum(!is.na(Ref)))
validIDs <- idNA[TotalNA > 0, ]
validIDs <- select(validIDs, Id)
rm(idNA)

tr_raw <- train[Id %in% validIDs$Id] 
rm(train)
rm(validIDs)
"
tr_raw <- select(train, Id, minutes_past, radardist_km, Ref, RefComposite, Expected) # added

tr_raw <- tr_raw[round(Expected, 4) %fin% valid_vals]
tr_raw$dt <- time_difference(tr_raw$minutes_past,1)
tr_raw$mp <- marshall_palmer(tr_raw$Ref)

#Collapse to one record per Id
tr <- tr_raw[, .(
  target = log1p(mean(Expected, na.rm = T)),
  ref = mean(dt * Ref, na.rm = T),
  ref1 = mean(dt * RefComposite, na.rm = T),
  mp = sum(dt * mp, na.rm = T),
  rd = mean(radardist_km, na.rm = T),
  records = .N,
  naCounts = sum(is.na(Ref))
), Id]

cs <- c("ref", "ref1",   "mp", "rd", "records")
y<-tr$target
tr<-as.data.frame(tr)
tr<-tr[,cs]
param0 <- list("objective"  = "reg:linear" 
               , "eval_metric" = "rmse"
               , "eta" = 0.007
               , "subsample" = 0.8
               , "min_child_weight" =10    
               , "max_depth" = 8
               , "nthreads" = 4
)
xgtrain = xgb.DMatrix(as.matrix(tr), label = y, missing = NA)
rm(tr,tr_raw)
gc()

x.mod.t  <- xgb.train(params = param0, data = xgtrain , nrounds =1955)

print("Processing test data...")
te_raw<-fread("test.csv", select=c(    
  "Id", 
  "minutes_past", 
  "radardist_km",
  "Ref", 
  "RefComposite", 
  "Expected")
)
te_raw$dt <- time_difference(te_raw$minutes_past)
te_raw$mp <- marshall_palmer(te_raw$Ref)

te <- te_raw[, .(
  ref = mean(dt * Ref, na.rm = T),
  ref1 = mean(dt * RefComposite, na.rm = T),
  mp = sum(dt * mp, na.rm = T),
  rd = mean(radardist_km),
  records = .N
),Id]
te<-as.data.frame(te)

xgtest = xgb.DMatrix(as.matrix(te[,cs ]), missing = NA)

pr  <- predict(x.mod.t,xgtest)
#sample_sol <-fread("sample_solution.csv")

xgb_prediction <- expm1(pr)

res <- data.frame(
  Id = te$Id,
  Expected = 0.77 * xgb_prediction #+ 0.23 * sample_sol$Expected
)

#convert expected values to 0.01in values
res$Expected <- round(res$Expected / 0.254) * 0.254

write.csv(res, "myresult.csv", row.names = FALSE, col.names = TRUE)