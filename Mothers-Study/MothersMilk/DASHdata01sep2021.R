############################################################
####################### Create DASH ########################
############################################################

# Clear out working space
rm(list=ls())
library(tidyverse)
library(withr)
library(huxtable)

# Set working directories
meta <- "/Volumes/ADORLab/Master Mothers Milk Data Folder/Input/"
meta2 <- "/Volumes/ADORLab/__Users/ditr2320/datasets/"
meta3 <- "/Volumes/ADORLab/HEI Study/16S Data/MM 16S Processed Files for Mom and Infants 05Jan21/alpha diversity"
meta4 <- "/Users/DidiTrifonova/Documents/GitHub/Git_ADOR/Mothers-Milk-Diet-Score-and-BMI"
meta5 <- "/Volumes/ADORLab/HEI Study/16S Data/MM 16S Processed Files for Mom and Infants 05Jan21/taxonomy/"

# Read in data new metadata (UPDATED)
dataSet <- read.delim(paste0(meta2,"longData16Aug2021.txt"), header=TRUE)
dashData <- read.delim(paste0(meta2,"longData01Sep2021.txt"), header=TRUE)

dashData %>% summarise_all((funs(sum(is.na(.))))) 
dashData <- dashData  %>% select(-SES_index_final,-exercise)
dashData  %>% summarise_all((funs(sum(is.na(.))))) 
DietData <- dashData

DietData$SFA <- DietData$m_sfa_Mom
DietData$FAT <- DietData$m_fat_Mom
DietData$PRO <- DietData$m_pro_Mom
DietData$Chol <- DietData$m_chol_Mom
DietData$Fib <- DietData$m_fib_Mom
DietData$Mg <- DietData$m_magnesium_Mom
DietData$Ca <- DietData$m_calcium_Mom
DietData$K <- DietData$m_potassium_Mom
DietData$Na <- DietData$m_sodium_Mom

DietData$SFA_kcals <- DietData$SFA*9
DietData$FAT_kcals <- DietData$FAT*9
DietData$PRO_kcals <- DietData$PRO*4
DietData$kcal1000_scale <- DietData$m_ener_Mom/1000

DietData$SFA_dash <- DietData$SFA_kcals/DietData$m_ener_Mom
DietData$FAT_dash <- DietData$FAT_kcals/DietData$m_ener_Mom
DietData$PRO_dash <- DietData$PRO_kcals/DietData$m_ener_Mom
DietData$Chol_dash <- DietData$Chol/DietData$kcal1000_scale
DietData$Fib_dash <- DietData$Fib/DietData$kcal1000_scale
DietData$Mg_dash <- DietData$Mg/DietData$kcal1000_scale
DietData$Ca_dash <- DietData$Ca/DietData$kcal1000_scale
DietData$K_dash <- DietData$K/DietData$kcal1000_scale
DietData$Na_dash <- DietData$Na/DietData$kcal1000_scale

DietData$SFA_dash_score <- ifelse(DietData$SFA_dash <= 0.06, 1, ifelse(DietData$SFA_dash <= 0.11, 0.5, 0))
DietData$FAT_dash_score <- ifelse(DietData$FAT_dash <= 0.27, 1, ifelse(DietData$FAT_dash <= 0.32, 0.5, 0))
DietData$PRO_dash_score <- ifelse(DietData$PRO_dash >= 0.18, 1, ifelse(DietData$PRO_dash >= 0.165, 0.5, 0))
DietData$Chol_dash_score <- ifelse(DietData$Chol_dash <= 71.4, 1, ifelse(DietData$Chol_dash <= 107.1, 0.5, 0))
DietData$Fib_dash_score <- ifelse(DietData$Fib_dash >= 14.8, 1, ifelse(DietData$Fib_dash >= 9.5, 0.5, 0))
DietData$Mg_dash_score <- ifelse(DietData$Mg_dash >= 238, 1, ifelse(DietData$Mg_dash >= 158, 0.5, 0))
DietData$Ca_dash_score <- ifelse(DietData$Ca_dash >= 590, 1, ifelse(DietData$Ca_dash >= 402, 0.5, 0))
DietData$K_dash_score <- ifelse(DietData$K_dash >= 2238, 1, ifelse(DietData$K_dash >= 1534, 0.5, 0))
DietData$Na_dash_score <- ifelse(DietData$Na_dash <= 1143, 1, ifelse(DietData$Na_dash <= 1286, 0.5, 0))

DietData$dash_score <- DietData$SFA_dash_score + DietData$FAT_dash_score + DietData$PRO_dash_score + DietData$Chol_dash_score + DietData$Fib_dash_score + DietData$Mg_dash_score + DietData$Ca_dash_score + DietData$K_dash_score + DietData$Na_dash_score
#write.table(DietData,paste0(meta2,"/dashData01Sep2021.txt",sep=""),quote=FALSE, sep="\t",append=FALSE, col.names=TRUE, row.names=FALSE)



