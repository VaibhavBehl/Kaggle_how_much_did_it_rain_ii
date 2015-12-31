library(dplyr)
library(data.table)
library(ggplot2)

train <- fread("../../data_CSV/train.csv")

# browser()

idNA <- train %>%
  group_by(Id) %>%
  summarise(isAllNA = all(is.na(Ref)))
validIDs <- idNA[isAllNA == FALSE ]
validIDs <- select(validIDs, Id) ## These are the IDs from the Train dataset that can be used for analysis/training a model
rm(idNA)

# FIX IT # gives error -> write(validIDs, file = "data")

train <- train[Id %in% validIDs$Id] ## This sould reduce train data from 13,765,201 rows to 3,350,827 rows.
rm(validIDs)

## The forums from the first competition suggested that training models are created by excluding 
## all rows where the outcome variable is above 69mm.
## Removal of sensors where the "Expected" variable is above 69mm further reduced the data from
## 9,125,329(not 3,350,827) rows to 8,914,175(not 3,300,192) rows
trainFull <- train
train <- train %>%
  filter(Expected < 70)

rm(temp2)

## till here need to run only once

#temp <- select(train, Id, radardist_km, Ref, RefComposite, RhoHV, Zdr, Kdp, Expected)
temp <- select(train, Id, radardist_km, Expected)

#hist(log1p(temp$Expected))
#temp <- temp %>% filter(Expected<70)

temp2 <- temp %>%
  group_by(Id) %>%
  summarise(
    radardist_km = mean(radardist_km),
    Expected = mean(Expected))

temp2 <- temp %>%
  group_by(Id) %>%
  summarise(radardist_km = mean(radardist_km),
            varRef = var(Ref, na.rm = TRUE),
            varRef5x5_10P = var(Ref_5x5_10th, na.rm = TRUE),
            varRef5x5_50P = var(Ref_5x5_50th, na.rm = TRUE),
            varRef5x5_90P = var(Ref_5x5_90th, na.rm = TRUE),
            
            meanRef = mean(Ref, na.rm = TRUE),
            meanRef5x5_10P = mean(Ref_5x5_10th, na.rm = TRUE),
            meanRef5x5_50P = mean(Ref_5x5_50th, na.rm = TRUE),
            meanRef5x5_90P = mean(Ref_5x5_90th, na.rm = TRUE),
            Expected = mean(Expected)
            )

##---Ref------------------------------------------------------------------
## varRef by radardist_km 
## /R/plots/VarRef_by_RadarDist.png
ggplot(temp2, aes(x = factor(radardist_km), y = varRef)) + 
  geom_jitter(alpha = 0.1) +
  geom_boxplot(colour = "orange", outlier.colour = NA, fill = NA) +
  labs(title = "Variance of Reflectivity by Radar Distance",
       x = "Radar Distance (Km) -- factor(mean(radardist_km))",
       y = "Variance(Reflectivity) -- var(Ref, na.rm = TRUE)")
## Expected by meanRef
## /R/plots/MeanRef_by_Expected.png
ggplot(temp2, aes(x = meanRef, y = Expected))  + 
  labs(title = "meanRef by Expected",
       x = "mean(Ref)",
       y = "mean(Expected)") + geom_point()
## Expected by varRef5x5_10P
## /R/plots/Expected_by_VarRef5x5_10P.png
ggplot(temp2, aes(x = radardist_km, y = Expected))  + 
  labs(title = "Expected by VarRef5x5_10P",
       x = "var(Ref5x5_10P)",
       y = "mean(Expected)") + geom_point()

## varRef5x5_10P by radardist_km 
## /R/plots/varRef5x5_10P_by_RadarDist.png
ggplot(temp2, aes(x = factor(radardist_km), y = varRef5x5_90P)) + 
  geom_jitter(alpha = 0.1) +
  geom_boxplot(colour = "orange", outlier.colour = NA, fill = NA) +
  labs(title = "Variance of Reflectivity_5x5_10P by Radar Distance",
       x = "Radar Distance (Km) -- factor(mean(radardist_km))",
       y = "Variance(Reflectivity) -- var(varRef5x5_10P, na.rm = TRUE)")
## Expected by meanRef5x5_10P
## /R/plots/MeanRef_by_Expected.png
ggplot(temp2, aes(x = meanRef5x5_90P, y = Expected))  + 
  labs(title = "meanRef5x5_10P by Expected",
       x = "mean(Ref5x5_10P)",
       y = "mean(Expected)") + geom_point()


##---RefComposite------------------------------------------------------------------
## varRefComposite by radardist_km 
## /R/plots/VarRefComposite_by_RadarDist.png
ggplot(temp2, aes(x = factor(radardist_km), y = varRefComposite)) + 
  geom_jitter(alpha = 0.1) +
  geom_boxplot(colour = "orange", outlier.colour = NA, fill = NA) +
  labs(title = "Variance of Reflectivity Composite by Radar Distance",
       x = "Radar Distance (Km) -- factor(mean(radardist_km))",
       y = "Variance(Reflectivity Composite) -- var(RefComposite, na.rm = TRUE)")
## Expected by meanRefComposite
## /R/plots/MeanRefComposite_by_Expected.png
ggplot(temp2, aes(x = meanRefComposite, y = Expected))  + 
  labs(title = "meanRefComposite by Expected",
       x = "mean(RefComposite)",
       y = "mean(Expected)") + geom_point()
## Expected by varRefComposite5x5_10P
## /R/plots/Expected_by_RefComposite5x5_90P.png
ggplot(temp2, aes(x = varRefComposite5x5_90P, y = Expected))  + 
  labs(title = "Expected by VarRefComposite5x5_90P",
       x = "var(RefComposite5x5_90P)",
       y = "mean(Expected)") + geom_point()
##---------------------------------------------------------------------


##---RhoHV------------------------------------------------------------------
## varRhoHV by radardist_km 
## /R/plots/VarRhoHV_by_RadarDist.png
ggplot(temp2, aes(x = factor(radardist_km), y = varRhoHV)) + 
  geom_jitter(alpha = 0.1) +
  geom_boxplot(colour = "orange", outlier.colour = NA, fill = NA) +
  labs(title = "Variance of RhoHV by Radar Distance",
       x = "Radar Distance (Km) -- factor(mean(radardist_km))",
       y = "Variance(RhoHV) -- var(RhoHV, na.rm = TRUE)")
## Expected by meanRhoHV
## /R/plots/MeanRhoHV_by_Expected.png
ggplot(temp2, aes(x = meanRhoHV, y = Expected))  + 
  labs(title = "meanRhoHV by Expected",
       x = "mean(RhoHV)",
       y = "mean(Expected)") + geom_point()
## Expected by varRhoHV
## /R/plots/Expected_by_VarRhoHV.png
ggplot(temp2, aes(x = varRhoHV, y = Expected))  + 
  labs(title = "Expected by VarRhoHV",
       x = "var(RhoHV)",
       y = "mean(Expected)") + geom_point()

## varRhoHV5x5_50P by radardist_km 
## /R/plots/varRhoHV5x5_90P_by_RadarDist.png
ggplot(temp2, aes(x = factor(radardist_km), y = varRhoHV5x5_90P)) + 
  geom_jitter(alpha = 0.1) +
  geom_boxplot(colour = "orange", outlier.colour = NA, fill = NA) +
  labs(title = "Variance of RhoHV_5x5_90P by Radar Distance",
       x = "Radar Distance (Km) -- factor(mean(radardist_km))",
       y = "Variance(RhoHV) -- var(RhoHV5x5_90P, na.rm = TRUE)")
## Expected by meanRhoHV5x5_10P
## /R/plots/meanRhoHV5x5_10P_by_Expected.png
ggplot(temp2, aes(x = meanRhoHV5x5_10P, y = Expected))  + 
  labs(title = "meanRhoHV5x5_10P by Expected",
       x = "mean(meanRhoHV5x5_10P)",
       y = "mean(Expected)") + geom_point()
## Expected by VarRhoHV5x5_90P
## /R/plots/Expected_by_VarRhoHV5x5_90P.png
ggplot(temp2, aes(x = varRhoHV5x5_90P, y = Expected))  + 
  labs(title = "Expected by VarRhoHV5x5_90P",
       x = "var(RhoHV5x5_90P)",
       y = "mean(Expected)") + geom_point()
##---------------------------------------------------------------------

##---Zdr------------------------------------------------------------------
## varZdr by radardist_km 
## /R/plots/VarZdr_by_RadarDist.png
ggplot(temp2, aes(x = factor(radardist_km), y = varZdr)) + 
  geom_jitter(alpha = 0.1) +
  geom_boxplot(colour = "orange", outlier.colour = NA, fill = NA) +
  labs(title = "Variance of Zdr by Radar Distance",
       x = "Radar Distance (Km) -- factor(mean(radardist_km))",
       y = "Variance(Zdr) -- var(Zdr, na.rm = TRUE)")
## Expected by meanZdr
## /R/plots/MeanZdr_by_Expected.png
ggplot(temp2, aes(x = meanZdr, y = Expected))  + 
  labs(title = "meanZdr by Expected",
       x = "mean(Zdr)",
       y = "mean(Expected)") + geom_point()
## Expected by varZdr
## /R/plots/Expected_by_VarZdr.png
ggplot(temp2, aes(x = varZdr, y = Expected))  + 
  labs(title = "Expected by VarZdr",
       x = "var(Zdr)",
       y = "mean(Expected)") + geom_point()

## varZdr5x5_10P by radardist_km 
## /R/plots/varZdr5x5_90P_by_RadarDist.png
ggplot(temp2, aes(x = factor(radardist_km), y = varZdr5x5_90P)) + 
  geom_jitter(alpha = 0.1) +
  geom_boxplot(colour = "orange", outlier.colour = NA, fill = NA) +
  labs(title = "Variance of Zdr5x5_90P by Radar Distance",
       x = "Radar Distance (Km) -- factor(mean(radardist_km))",
       y = "Variance(Zdr) -- var(Zdr5x5_90P, na.rm = TRUE)")
## Expected by meanZdr5x5_10P
## /R/plots/meanZdr5x5_90P_by_Expected.png
ggplot(temp2, aes(x = meanZdr5x5_90P, y = Expected))  + 
  labs(title = "meanZdr5x5_90P by Expected",
       x = "mean(meanZdr5x5_90P)",
       y = "mean(Expected)") + geom_point()
## Expected by VarZdr5x5_90P
## /R/plots/Expected_by_VarZdr5x5_90P.png
ggplot(temp2, aes(x = varZdr5x5_90P, y = Expected))  + 
  labs(title = "Expected by Zdr5x5_90P",
       x = "var(Zdr5x5_90P)",
       y = "mean(Expected)") + geom_point()
##---------------------------------------------------------------------

##---Kdp------------------------------------------------------------------
## varKdp by radardist_km 
## /R/plots/VarKdp_by_RadarDist.png
ggplot(temp2, aes(x = factor(radardist_km), y = varKdp)) + 
  geom_jitter(alpha = 0.1) +
  geom_boxplot(colour = "orange", outlier.colour = NA, fill = NA) +
  labs(title = "Variance of Kdp by Radar Distance",
       x = "Radar Distance (Km) -- factor(mean(radardist_km))",
       y = "Variance(Kdp) -- var(Kdp, na.rm = TRUE)")
## Expected by meanKdp
## /R/plots/MeanKdp_by_Expected.png
ggplot(temp2, aes(x = meanKdp, y = Expected))  + 
  labs(title = "meanKdp by Expected",
       x = "mean(Kdp)",
       y = "mean(Expected)") + geom_point()
## Expected by varKdp
## /R/plots/Expected_by_VarKdp.png
ggplot(temp2, aes(x = varKdp, y = Expected))  + 
  labs(title = "Expected by VarKdp",
       x = "var(Kdp)",
       y = "mean(Expected)") + geom_point()

## varKdp5x5_10P by radardist_km 
## /R/plots/varKdp5x5_90P_by_RadarDist.png
ggplot(temp2, aes(x = factor(radardist_km), y = varKdp5x5_90P)) + 
  geom_jitter(alpha = 0.1) +
  geom_boxplot(colour = "orange", outlier.colour = NA, fill = NA) +
  labs(title = "Variance of Kdp5x5_90P by Radar Distance",
       x = "Radar Distance (Km) -- factor(mean(radardist_km))",
       y = "Variance(Kdp) -- var(Kdp5x5_90P, na.rm = TRUE)")
## Expected by meanKdp5x5_10P
## /R/plots/meanKdp5x5_90P_by_Expected.png
ggplot(temp2, aes(x = meanKdp5x5_90P, y = Expected))  + 
  labs(title = "meanKdp5x5_90P by Expected",
       x = "mean(meanKdp5x5_90P)",
       y = "mean(Expected)") + geom_point()
## Expected by Kdp5x5_90P
## /R/plots/Expected_by_VarKdp5x5_90P.png
ggplot(temp2, aes(x = varKdp5x5_90P, y = Expected))  + 
  labs(title = "Expected by Kdp5x5_90P",
       x = "var(Kdp5x5_90P)",
       y = "mean(Expected)") + geom_point()
##---------------------------------------------------------------------

















## varRefComposite
ggplot(temp2, aes(x = factor(radardist_km), y = varRefComposite)) + 
  geom_jitter(alpha = 0.1) +
  geom_boxplot(colour = "orange", outlier.colour = NA, fill = NA) +
  labs(title = "Variance of Composite Reflectivity by Radar Distance",
       x = "Radar Distance (Km)",
       y = "Variance(Composite Reflectivity)")

## varRhoHV
ggplot(temp2, aes(x = factor(radardist_km), y = varRhoHV)) + 
  geom_jitter(alpha = 0.1) +
  geom_boxplot(colour = "orange", outlier.colour = NA, fill = NA) +
  labs(title = "Variance of varRhoHV by Radar Distance",
       x = "Radar Distance (Km)",
       y = "Variance(Composite Reflectivity)")


## varZdr
ggplot(temp2, aes(x = factor(radardist_km), y = varZdr)) + 
  geom_jitter(alpha = 0.1) +
  geom_boxplot(colour = "orange", outlier.colour = NA, fill = NA) +
  labs(title = "Variance of varZdr by Radar Distance",
       x = "Radar Distance (Km)",
       y = "Variance(Composite Reflectivity)")

## varKdp
ggplot(temp2, aes(x = factor(radardist_km), y = varKdp)) + 
  geom_jitter(alpha = 0.1) +
  geom_boxplot(colour = "orange", outlier.colour = NA, fill = NA) +
  labs(title = "Variance of varKdp by Radar Distance",
       x = "Radar Distance (Km)",
       y = "Variance(Composite Reflectivity)")

## Expected
d <- ggplot(temp2, aes(x = factor(meanZdr), y = Expected)) +
  labs(title = "meanZdr by Expected",
       x = "factor(meanNA.Zdr)",
       y = "mean(Expected)")
d + geom_point()