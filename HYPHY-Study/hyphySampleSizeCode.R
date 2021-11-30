
#######################################
rm(list = ls())

## Packages 
library(tidyverse)
library(data.table)
library(withr)
library(dplyr)
library(janitor)

meta <- "/Volumes/ADORLab/Master Mothers Milk Data Folder/Input/"
meta1 <- "/Volumes/ADORLab/Current Lab Projects/Hyphy/IRB COVID procedures/ASA24/20210913_HYPHYCOV_analysis_files/"
meta2 <- "/Volumes/ADORLab/__Users/ditr2320/datasets/"
meta3 <- "/Volumes/ADORLab/__Users/ditr2320/hyphyData/"
meta4 <- "/Users/DidiTrifonova/Documents/GitHub/Git_ADOR/Mothers-Milk-Diet-Score-and-BMI/"
meta5 <- "/Volumes/ADORLab/__Users/ditr2320/hyphyData/sampleSizes/"

############ COVID PARTICIPANTS
pre<- read.csv(paste0(meta5,"preCov28Nov2021.csv"), header=TRUE) 
preCovid <- pre %>% filter(stoolstoredate != "") %>% select(study_id_checklist, stoolstoredate) %>% rename("record_id" = "study_id_checklist")
preCovid$blood_store_date <- 0
preCovid$saliva_store_date <- 0
preCovid$stoolstoredate <- 1

covid <- read.csv(paste0(meta5,"cov28Nov2021.csv"), header=TRUE,na.strings=c(""," ","NA")) 
covid$stoolstoredate <- ifelse(is.na(covid$stoolstoredate) == FALSE, 1, 0)
covid$saliva_store_date <- ifelse(is.na(covid$saliva_store_date) == FALSE, 1, 0)
covid$blood_store_date <- ifelse(is.na(covid$blood_store_date) == FALSE, 1, 0)

#freezer<- read.csv(paste0(meta5,"hyphySampleSizes04Nov2021_JC.csv"), header=TRUE) 
#freezerN <- freezer %>% filter(IN.FREEZER == "Yes")

totals <- full_join(covid,preCovid)
totals <- totals %>%
  adorn_totals("row") 

write.table(totals,paste0(meta3,"/hyphySampleSizes28Nov2021.csv",sep=""),quote=FALSE, sep=",",append=FALSE, col.names=TRUE, row.names=FALSE)
