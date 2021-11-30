#CHECKING TO SEE IF BOTH METHODS OF GETTING MDS INTO DATSET PRODUCE THE SAME THING

#importing data
#######################################
rm(list = ls())

## Packages 
library(tidyverse)
meta5 <- "/Users/DidiTrifonova/Documents/GitHub/Git_ADOR/Mothers-Milk-Diet-Score-and-BMI/HMO/code01Nov2021"
meta6 <- "/Users/DidiTrifonova/Documents/GitHub/Git_ADOR/Mothers-Milk-Diet-Score-and-BMI/HMO/code01Nov2021/plots/"
#reading in data 
# long data - does not have correct MDS scores
data <- read.delim(paste0(meta5,"/tidyLong10Nov2021.txt"), header=TRUE) #pulling MDS from wide dataset 
data1<- read.delim(paste0(meta5,"/tidyLongMds10Nov2021.txt"), header=TRUE) #making MDS score in R

#data sets are equivalent
data <- data[order(data$dyad_id),]
data1 <- data1[order(data1$dyad_id),]
data$mom_BMI == data1$mom_BMI
data$MDS_Mom == data1$MDS_Mom
