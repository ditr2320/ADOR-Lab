#importing data
#######################################
rm(list = ls())

## Packages 
library(tidyverse)
library(data.table)

meta <- "/Volumes/ADORLab/Master Mothers Milk Data Folder/Input/"
meta1 <- "/Volumes/ADORLab/Current Lab Projects/Hyphy/IRB COVID procedures/ASA24/20210913_HYPHYCOV_analysis_files/"
meta2 <- "/Volumes/ADORLab/__Users/ditr2320/datasets/"
meta3 <- "/Volumes/ADORLab/__Users/ditr2320/hyphyData/"
meta4 <- "/Users/DidiTrifonova/Documents/GitHub/Git_ADOR/Mothers-Milk-Diet-Score-and-BMI/"

#reading in data 
# long data - does not have correct MDS scores
totalsMeta <- read.csv(paste0(meta1,"HYPHYCOV_2021-09-13_69392_Totals.csv"), header=TRUE)


#REMOVING PARTICIPANT 63 - Removed from study all together?
totals <- totalsMeta %>% filter(UserName != "HYPHYCOV1063")
ID <- totals$UserName %>% substr(11,12) %>% as.numeric()
totals <- cbind(ID,totals)


###############################################
###############################################
###############################################
# FIXING RECALL NUMBERS

#participants that had a third recall: 34,44,62
#making recall numbers 1 and 2 instead 
#participant 2 is missing recall 1 but doesn't have third recall 

#3 participants with a third recall 
totals %>% count(RecallNo)
recalls3<- c(34,44,62)

#participant 34: 
#previously 3 
totals[54,5] <- 2
#previously 2 
totals[55,5] <- 1

#participant 44: 
#previously 3 
totals[74,5] <- 2

#participant 62: 
#previously 2
totals[105,5] <- 1 
#previously 3
totals[106,5] <- 2 

#participant 2:
#previously 2
totals[3,5] <- 1

#check to see if fixed
totals %>% count(RecallNo)
###############################################
###############################################
###############################################
# AVERAGING RECALLS 

diet <- totals %>% select(ID, UserName, RecallNo, c(15:116))

#66 with first dietary recall and 58 with 2 dietary recalls (8 with only 1 recall) 
diet1 <- diet %>% filter(RecallNo == 1)
diet2 <- diet %>% filter(RecallNo == 2)

#Recall 1 diet data of participants who also have second recall 
diet1sub <- diet1 %>% filter(ID %in% diet2$ID)

#new data set to hold average diet 
avData <- diet1sub %>% select(c(1:3))
#creating vector to hold variable names 
vars <- diet1sub %>% names()
vars <- vars[4:105]
#varsa <- paste0(vars,"_a")
#loop for making average variables from two datasets 
for(i in 1:length(vars)){
  
  tempdata <- data.frame(variable = diet1sub[,names(diet1sub) %in% vars[i]])
  tempdata1 <- data.frame(variable = diet2[,names(diet2) %in% vars[i]])
  avData <- avData %>% mutate(!!sym(vars[i]) := (tempdata$variable + tempdata1$variable)/2)
}
avData$RecallNo = "average"

###############################################
###############################################
###############################################
#COMBINING AVERAGED RECALLS WITH PARTICPANTS THAT HAVE ONE RECALL

#dataset of participants with 1 recall (n = 8)
singles <- diet1 %>% filter(!(ID %in% diet2$ID))
#merging dataset and checking n 
totalData <- rbind(avData,singles)
totalData %>% count(RecallNo)

write.table(totalData,paste0(meta3,"/hDietData15Sep2021.txt",sep=""),quote=FALSE, sep="\t",append=FALSE, col.names=TRUE, row.names=FALSE)