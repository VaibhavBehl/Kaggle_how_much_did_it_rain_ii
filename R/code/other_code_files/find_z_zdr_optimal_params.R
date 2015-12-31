#to find optimal params for when using both Z and Zdr


source("utility_functions.R")

#original values
trgData <- trData[, .(
  target = mean(Expected)
), Id]


tempfun <- function() {
  
c<-0
aiseq <- seq(4,7)/1000
aiseq <- c(aiseq,1,100)
#aiseq <- c(0.00746)

biseq <- seq(8,10)/10
biseq <- c(biseq, 100)
#biseq <- c(0.945)

ciseq <- seq(-2,2,by=0.05)
ciseq <- c(ciseq,-100,100)
ciseq <- c(0.25,0.20,0.30,0.15,0.35,0.10,-0.05,-0.10,-0.15,-0.20)

#aiseq <- 0.005
biseq <- 0.8
ciseq <- seq(-20,-10,by=2)/100

resultMat <- matrix(NA, 1, 4)
iter <- length(aiseq)*length(biseq)*length(ciseq)
rtm <- 7.17*iter/60
print(paste('loop will run for iterations=',iter,', and time(min) = ',rtm))
for (ai in aiseq) {
  for (bi in biseq) {
    for (ci in ciseq) {
        c<-c+1
        predictionsMP <-output_z_zdr_valid_time(trData,ai,bi,ci)
        minusV <- trgData$target - predictionsMP$Expected
        p <- sum(abs(minusV))/length(minusV)
        pVal <- c(ai,bi,ci,p)
        resultMat <- rbind(resultMat, pVal)
        print(paste('count=',c, ', ai=',ai,', bi=',bi,', ci=',ci,', p=',p));
    }
  }
}
summary(resultMat)
resultMatDT <- data.table(resultMat, key="V4")
print(resultMatDT)
write.table(resultMatDT, append = T, "z_zdr_mod.txt")
}

