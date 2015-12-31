#to find optimal params for marshall_palmer
# using result using all <trData,1-10> => alpha=0.4735 beta=132 p=23.2697976851924
# *current best* 0.475 131 23.257258779355
source("marshall_palmer.R")

#old way
predictionsMP <-output_marshall_palmer(trData);
minusV <- trgData$target - predictionsMP$Expected
p <- sum(abs(minusV))/length(minusV)
p
#


#original values
trgData <- trData[, .(
  target = mean(Expected)
), Id]

c<-0
aiseq <- seq(476,483)/1000
aiseq <- c(aiseq)

biseq <- seq(128,132)
biseq <- c(biseq)

resultMat <- matrix(NA, 1, 3)
iter <- length(aiseq)*length(biseq)
rtm <- 7.17*iter/60
print(paste('loop will run for iterations=',length(aiseq)*length(biseq),', and time(min) = ',rtm))
for (ai in aiseq) {
  for (bi in biseq) {
        c<-c+1
        predictionsMP <-output_marshall_palmer_new_with_valid_time(trData,ai,bi)
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
write.table(resultMatDT, append = T, "mp_mod.txt")