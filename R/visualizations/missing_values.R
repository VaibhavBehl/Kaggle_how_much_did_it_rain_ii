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


# all columns
Ref	
Ref_5x5_10th	
Ref_5x5_50th	
Ref_5x5_90th	
RefComposite	
RefComposite_5x5_10th	
RefComposite_5x5_50th	
RefComposite_5x5_90th	
RhoHV	
RhoHV_5x5_10th	
RhoHV_5x5_50th	
RhoHV_5x5_90th	
Zdr	
Zdr_5x5_10th	
Zdr_5x5_50th	
Zdr_5x5_90th	
Kdp	
Kdp_5x5_10th	
Kdp_5x5_50th	
Kdp_5x5_90th	

#temp <- select(train, Id, radardist_km, Ref, RefComposite, RhoHV, Zdr, Kdp, Expected)
temp <- select(train, Id, Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th,RefComposite,RefComposite_5x5_10th,
  RefComposite_5x5_50th,RefComposite_5x5_90th,RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th,
  Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th,Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th)

#don't show in scientific notation
options(scipen=5)
# control ticks of y-axis
setTicks <- function(n) {function(limits) pretty(limits, n)}

temp <- select(train, Id, Kdp_5x5_90th)
               
temp2 <- temp %>%
  group_by(Id) %>%
  summarise(percNullRef = sum(is.na(Kdp_5x5_90th)*100/length(Kdp_5x5_90th)))

## Kdp_5x5_90th_hist_nullPerc.png
qplot(temp2$percNullRef, geom="histogram") + 
  scale_y_continuous(breaks=setTicks(6)) +
  labs(title = "Kdp_5x5_90th",
     x = "percentage of null values",
     y = "histogram count of groups")
