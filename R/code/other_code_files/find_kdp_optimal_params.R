#to find optimal params for when using both Z and Zdr


source("utility_functions.R")

#original values
trgData <- trData[, .(
  target = mean(Expected)
), Id]


tempfun <- function() {
  
c<-0
aiseq <- seq(2,9)*10
aiseq <- c(aiseq,1,100)
#aiseq <- c(40.6)

biseq <- seq(0,1)/10
biseq <- c(biseq, 100)
#biseq <- c(0.945)

#aiseq <- 0.005
#biseq <- 0.866

resultMat <- matrix(NA, 1, 3)
iter <- length(aiseq)*length(biseq)
rtm <- 7.17*iter/60
print(paste('loop will run for iterations=',iter,', and time(min) = ',rtm))
for (ai in aiseq) {
  for (bi in biseq) {
    c<-c+1
    predictionsMP <-output_kdp_valid_time(trData,ai,bi,ci)
    minusV <- trgData$target - predictionsMP$Expected
    p <- sum(abs(minusV))/length(minusV)
    pVal <- c(ai,bi,p)
    resultMat <- rbind(resultMat, pVal)
    print(paste('count=',c, ', ai=',ai,', bi=',bi,', p=',p));
  }
}
summary(resultMat)
resultMatDT <- data.table(resultMat, key="V3")
print(resultMatDT)
write.table(resultMatDT, append = T, "kdp_mod.txt")
}

