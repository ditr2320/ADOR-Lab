# Clear out working space
rm(list=ls())
library(tidyverse)
library(withr)
library(qiime2R)

# Set working directories
meta <- "/Volumes/ADORLab/Master Mothers Milk Data Folder/Input/"
meta2 <- "/Volumes/ADORLab/__Users/ditr2320/datasets/"
meta3 <- "/Volumes/ADORLab/HEI Study/16S Data/MM 16S Processed Files for Mom and Infants 05Jan21/alpha diversity"
meta4 <- "/Users/DidiTrifonova/Documents/GitHub/Git_ADOR/Mothers-Milk-Diet-Score-and-BMI"
meta5 <- "/Volumes/ADORLab/HEI Study/16S Data/MM 16S Processed Files for Mom and Infants 05Jan21/taxonomy/"
meta6 <- "/Volumes/ADORLab/__Users/ditr2320/datasets/songbirdData/"
meta7 <- "/Volumes/ADORLab/HEI Study/16S Data/MM 16S Processed Files for Mom and Infants 05Jan21/"

# Read in data


#aHEI - 6m ######################

aHEI_full_6m <- read.delim(paste0(meta6,"aHEI-6m/mm_differentials-aHEI_6m_full/sample_plot_data.tsv"), header=TRUE)
aHEI_full_6m <- aHEI_full_6m[-c(1),]
aHEI_full_6m$Current_Natural_Log_Ratio <- aHEI_full_6m$Current_Natural_Log_Ratio %>% as.numeric()
aHEI_full_6m$HEI2015_TOTAL_SCORE_Mom <- aHEI_full_6m$HEI2015_TOTAL_SCORE_Mom %>% as.numeric()
HEIregression2 <- summary(lm(HEI2015_TOTAL_SCORE_Mom ~ Current_Natural_Log_Ratio, aHEI_full_6m))
ggplot(aHEI_full_6m, aes(x = HEI2015_TOTAL_SCORE_Mom,y= Current_Natural_Log_Ratio)) + geom_point() + geom_smooth(method = "lm")

#aMDS - 6m ####################
aMDS_6 <- read.delim(paste0(meta6,"aMDS-6m/mm_differentials-aMDS_6m/differentials.tsv"), header=TRUE)
aMDS_6 <- aMDS_6[-c(1),]
aMDS_6$Intercept <- aMDS_6$Intercept %>% as.numeric()
aMDS_6$MDS_Mom <- aMDS_6$MDS_Mom %>% as.numeric()
MDSregression1 <- summary(lm(Intercept ~ MDS_Mom, aMDS_6))
ggplot(aMDS_6, aes(x= MDS_Mom, y = Intercept)) + geom_point()+ geom_smooth(method = "lm")

aMDS_6 <- read.delim(paste0(meta6,"aMDS-6m/mm_differentials-aMDS_6m/sample_plot_data.tsv"), header=TRUE)
aMDS_6 <- aMDS_6[-c(1),]
aMDS_6$Current_Natural_Log_Ratio <- aMDS_6$Current_Natural_Log_Ratio %>% as.numeric()
aMDS_6$MDS_Mom <- aMDS_6$MDS_Mom %>% as.numeric()
MDSregression1 <- summary(lm(Current_Natural_Log_Ratio ~ MDS_Mom, aMDS_6))
ggplot(aMDS_6, aes(x= MDS_Mom, y = Current_Natural_Log_Ratio)) + geom_point()+ geom_smooth(method = "lm")

aMDS_full_6m <- read.delim(paste0(meta6,"aMDS-6m/mm_differentials-aMDS_6m_full/differentials.tsv"), header=TRUE)
aMDS_full_6m <- aMDS_full_6m[-c(1),]
aMDS_full_6m$Intercept <- aMDS_full_6m$Intercept %>% as.numeric()
aMDS_full_6m$MDS_Mom <- aMDS_full_6m$MDS_Mom %>% as.numeric()
MDSregression2 <- summary(lm(Intercept ~ MDS_Mom, aMDS_full_6m))
ggplot(aMDS_full_6m, aes(x= MDS_Mom, y = Intercept)) + geom_point() + geom_smooth(method = "lm")#aHEI - 6m ####################
aHEI_6 <- read.delim(paste0(meta6,"aHEI-6m/mm_differentials-aHEI_6m/differentials.tsv"), header=TRUE)
aHEI_6 <- aHEI_6[-c(1),]
aHEI_6$Intercept <- aHEI_6$Intercept %>% as.numeric()
aHEI_6$HEI2015_TOTAL_SCORE_Mom <- aHEI_6$HEI2015_TOTAL_SCORE_Mom %>% as.numeric()
HEIregression1 <- summary(lm(Intercept ~ HEI2015_TOTAL_SCORE_Mom, aHEI_6))
ggplot(aHEI_6, aes(x= HEI2015_TOTAL_SCORE_Mom, y = Intercept)) + geom_point()+ geom_smooth(method = "lm")

aHEI_6 <- read.delim(paste0(meta6,"aHEI-6m/mm_differentials-aHEI_6m/sample_plot_data.tsv"), header=TRUE)
aHEI_6 <- aHEI_6[-c(1),]
aHEI_6$Current_Natural_Log_Ratio <- aHEI_6$Current_Natural_Log_Ratio %>% as.numeric()
aHEI_6$HEI2015_TOTAL_SCORE_Mom <- aHEI_6$HEI2015_TOTAL_SCORE_Mom %>% as.numeric()
HEIregression1 <- summary(lm(Current_Natural_Log_Ratio ~ HEI2015_TOTAL_SCORE_Mom, aHEI_6))
ggplot(aHEI_6, aes(x= HEI2015_TOTAL_SCORE_Mom, y = Current_Natural_Log_Ratio)) + geom_point()+ geom_smooth(method = "lm")

aHEI_full_6m <- read.delim(paste0(meta6,"aHEI-6m/mm_differentials-aHEI_6m_full/differentials.tsv"), header=TRUE)
aHEI_full_6m <- aHEI_full_6m[-c(1),]
aHEI_full_6m$Intercept <- aHEI_full_6m$Intercept %>% as.numeric()

aMDS_full_6m <- read.delim(paste0(meta6,"aMDS-6m/mm_differentials-aMDS_6m_full/sample_plot_data.tsv"), header=TRUE)
aMDS_full_6m <- aMDS_full_6m[-c(1),]
aMDS_full_6m$Current_Natural_Log_Ratio <- aMDS_full_6m$Current_Natural_Log_Ratio %>% as.numeric()
aMDS_full_6m$MDS_Mom <- aMDS_full_6m$MDS_Mom %>% as.numeric()
MDSregression2 <- summary(lm(MDS_Mom ~ Current_Natural_Log_Ratio, aMDS_full_6m))
ggplot(aMDS_full_6m, aes(x = MDS_Mom,y= Current_Natural_Log_Ratio)) + geom_point() + geom_smooth(method = "lm")

#aDII - 6m 
aDII_6 <- read.delim(paste0(meta6,"aDII-6m/mm_differentials-aDII_6m/differentials.tsv"), header=TRUE)
aDII_6 <- aDII_6[-c(1),]
aDII_6$Intercept <- aDII_6$Intercept %>% as.numeric()
aDII_6$DII_Mom <- aDII_6$DII_Mom %>% as.numeric()
DIIregression1 <- summary(lm(Intercept ~ DII_Mom, aDII_6))
ggplot(aDII_6, aes(x= DII_Mom, y = Intercept)) + geom_point()+ geom_smooth(method = "lm")

aDII_6 <- read.delim(paste0(meta6,"aDII-6m/mm_differentials-aDII_6m/sample_plot_data.tsv"), header=TRUE)
aDII_6 <- aDII_6[-c(1),]
aDII_6$Current_Natural_Log_Ratio <- aDII_6$Current_Natural_Log_Ratio %>% as.numeric()
aDII_6$DII_Mom <- aDII_6$DII_Mom %>% as.numeric()
DIIregression1 <- summary(lm(Current_Natural_Log_Ratio ~ DII_Mom, aDII_6))
ggplot(aDII_6, aes(x= DII_Mom, y = Current_Natural_Log_Ratio)) + geom_point()+ geom_smooth(method = "lm")

aDII_full_6m <- read.delim(paste0(meta6,"aDII-6m/mm_differentials-aDII_6m_full/differentials.tsv"), header=TRUE)
aDII_full_6m <- aDII_full_6m[-c(1),]
aDII_full_6m$Intercept <- aDII_full_6m$Intercept %>% as.numeric()
aDII_full_6m$DII_Mom <- aDII_full_6m$DII_Mom %>% as.numeric()
DIIregression2 <- summary(lm(Intercept ~ DII_Mom, aDII_full_6m))
ggplot(aDII_full_6m, aes(x= DII_Mom, y = Intercept)) + geom_point() + geom_smooth(method = "lm")

aDII_full_6m <- read.delim(paste0(meta6,"aDII-6m/mm_differentials-aDII_6m_full/sample_plot_data.tsv"), header=TRUE)
aDII_full_6m <- aDII_full_6m[-c(1),]
aDII_full_6m$Current_Natural_Log_Ratio <- aDII_full_6m$Current_Natural_Log_Ratio %>% as.numeric()
aDII_full_6m$DII_Mom <- aDII_full_6m$DII_Mom %>% as.numeric()
DIIregression2 <- summary(lm(DII_Mom ~ Current_Natural_Log_Ratio, aDII_full_6m))
ggplot(aDII_full_6m, aes(x = DII_Mom,y= Current_Natural_Log_Ratio)) + geom_point() + geom_smooth(method = "lm")


#BMI CHANGE - 6m 
BMI_6 <- read.delim(paste0(meta6,"BMI_change-6m/mm_differentials-BMIchange_6m/differentials.tsv"), header=TRUE)
BMI_6 <- BMI_6[-c(1),]
BMI_6$Intercept <- BMI_6$Intercept %>% as.numeric()
BMI_6$BMI_change <- BMI_6$BMI_change %>% as.numeric()
BMIregression1 <- summary(lm(Intercept ~ BMI_change, BMI_6))
ggplot(BMI_6, aes(x= BMI_change, y = Intercept)) + geom_point()

BMI_6 <- read.delim(paste0(meta6,"BMI_change-6m/mm_differentials-BMIchange_6m/sample_plot_data.tsv"), header=TRUE)
BMI_6 <- BMI_6[-c(1),]
BMI_6$Current_Natural_Log_Ratio <- BMI_6$Current_Natural_Log_Ratio %>% as.numeric()
BMI_6$BMI_change <- BMI_6$BMI_change %>% as.numeric()
BMIregression1 <- summary(lm(Current_Natural_Log_Ratio ~ BMI_change, BMI_6))
ggplot(BMI_6, aes(x= BMI_change, y = Current_Natural_Log_Ratio)) + geom_point()+ geom_smooth(method = "lm")

BMI_full_6 <- read.delim(paste0(meta6,"BMI_change-6m/mm_differentials-BMIchange_6m_full/differentials.tsv"), header=TRUE)
BMI_full_6 <- BMI_full_6[-c(1),]
BMI_full_6$Intercept <- BMI_full_6$Intercept %>% as.numeric()
BMI_full_6$BMI_change <- BMI_full_6$BMI_change %>% as.numeric()
BMIregression2 <- summary(lm(Intercept ~ BMI_change, BMI_full_6))
ggplot(BMI_full_6, aes(x= BMI_change, y = Intercept)) + geom_point()

BMI_full_6m <- read.delim(paste0(meta6,"BMI_change-6m/mm_differentials-BMIchange_6m_full/sample_plot_data.tsv"), header=TRUE)
BMI_full_6m <- BMI_full_6m[-c(1),]
BMI_full_6m$Current_Natural_Log_Ratio <- BMI_full_6m$Current_Natural_Log_Ratio %>% as.numeric()
BMI_full_6m$BMI_change <- BMI_full_6m$BMI_change %>% as.numeric()
BMIregression2 <- summary(lm(Current_Natural_Log_Ratio ~ BMI_change, BMI_full_6m))
ggplot(BMI_full_6m, aes(x= BMI_change, y = Current_Natural_Log_Ratio)) + geom_point()+ geom_smooth(method = "lm")


#SELECTED FEATURES
sequences <- read_qza(paste0(meta6,"deblur-sequences-taxonomy-MM.qza"))
#deblur-sequences-taxonomy-MM.qza
  
#aDII - 6m 
aDII_6m_features <- read.delim(paste0(meta6,"aDII-6m/mm_differentials-aDII_6m/selected_features.tsv"), header=TRUE)
aDII_6m_data <- read.delim(paste0(meta6,"aDII-6m/mm_differentials-aDII_6m/metadata.tsv"), header=TRUE)


