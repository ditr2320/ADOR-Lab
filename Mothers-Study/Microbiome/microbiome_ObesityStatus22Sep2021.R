# Clear out working space
rm(list=ls())
library(tidyverse)
library(withr)
library(huxtable)
library(corrplot) 
library(ggpubr)

# Set working directories
meta <- "/Volumes/ADORLab/Master Mothers Milk Data Folder/Input/"
meta2 <- "/Volumes/ADORLab/__Users/ditr2320/datasets/"
meta3 <- "/Volumes/ADORLab/HEI Study/16S Data/MM 16S Processed Files for Mom and Infants 05Jan21/alpha diversity"
meta4 <- "/Users/DidiTrifonova/Documents/GitHub/Git_ADOR/Mothers-Milk-Diet-Score-and-BMI"
meta5 <- "/Volumes/ADORLab/HEI Study/16S Data/MM 16S Processed Files for Mom and Infants 05Jan21/taxonomy/"
meta6 <- "/Volumes/ADORLab/__Users/ditr2320/datasets/sb_Uni_CS/"
meta7 <- "/Volumes/ADORLab/HEI Study/16S Data/MM 16S Processed Files for Mom and Infants 05Jan21/"
meta8 <- "/Volumes/ADORLab/__Users/ditr2320/datasets/sb_Multi_CS/"
meta9 <- "/Volumes/ADORLab/__Users/ditr2320/datasets/songbirdData/"

dataSet <- read.delim(paste0(meta2,"longData03Jun2021.txt"), header=TRUE)
dataSet$obesityStatus <- ifelse(dataSet$mom_BMI > 30, "Obese","Non-Obese")
dataSet$weight25 <- ifelse(dataSet$mom_BMI > 25, "Over","Under")
baseline <- dataSet %>% filter(timepoint ==1 )
six <- dataSet %>% filter(timepoint == 6)

healthy_b <- baseline %>% filter(obesityStatus == "Non-Obese")
obese_b <- baseline %>% filter(obesityStatus == "Obese")

healthy_6 <- six %>% filter(obesityStatus == "Non-Obese")
obese_6 <- six %>% filter(obesityStatus == "Obese")


#SHANNON
#baseline
wilcox.test(healthy_b$shannon_entropy, obese_b$shannon_entropy)
#6m
wilcox.test(healthy_6$shannon_entropy, obese_6$shannon_entropy)

#FAITH
#baseline
wilcox.test(healthy_b$faith_pd, obese_b$faith_pd)
#6m
wilcox.test(healthy_6$faith_pd, obese_6$faith_pd)

#RICHNESS
#baseline
wilcox.test(healthy_b$gut_richness, obese_b$gut_richness)
#6m
wilcox.test(healthy_6$gut_richness, obese_6$gut_richness)


under25_b <- baseline %>% filter(weight25 == "Under")
over25_b <- baseline %>% filter(weight25 == "Over")

under25_6 <- six %>% filter(weight25 == "Under")
over25_6 <- six %>% filter(weight25 == "Over")

#SHANNON
#baseline
wilcox.test(under25_b$shannon_entropy, over25_b$shannon_entropy)
#6m
wilcox.test(under25_6$shannon_entropy, over25_6$shannon_entropy)

#FAITH
#baseline
wilcox.test(under25_b$faith_pd, over25_b$faith_pd)
#6m
wilcox.test(under25_6$faith_pd, over25_6$faith_pd)

#RICHNESS
#baseline
wilcox.test(under25_b$gut_richness, over25_b$gut_richness)
#6m
wilcox.test(under25_6$gut_richness, over25_6$gut_richness)

