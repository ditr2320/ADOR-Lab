# Clear out working space
rm(list=ls())
library(tidyverse)
library(withr)

# Set working directories
meta <- "/Volumes/ADORLab/Master Mothers Milk Data Folder/Input/"
meta2 <- "/Volumes/ADORLab/__Users/ditr2320/datasets/"
meta3 <- "/Volumes/ADORLab/HEI Study/16S Data/MM 16S Processed Files for Mom and Infants 05Jan21/alpha diversity"
meta4 <- "/Users/DidiTrifonova/Documents/GitHub/Git_ADOR/Mothers-Milk-Diet-Score-and-BMI"
meta5 <- "/Volumes/ADORLab/HEI Study/16S Data/MM 16S Processed Files for Mom and Infants 05Jan21/taxonomy/"
meta6 <- "/Volumes/ADORLab/__Users/ditr2320/datasets/songbirdData/"
meta7 <- "/Volumes/ADORLab/HEI Study/16S Data/MM 16S Processed Files for Mom and Infants 05Jan21/"

# Read in data

DII_1 <- read.delim(paste0(meta2,"sb_Uni_CS/DII_Uni_CS/mm_differentials-DII_1m/differentials.tsv"), header=TRUE)
DII_1 <- DII_1[-c(1),]
DII_1$Intercept <- DII_1$Intercept %>% as.numeric()
DII_1$DII_Mom <- DII_1$DII_Mom %>% as.numeric()
DIIregression1 <- summary(lm(Intercept ~ DII_Mom, DII_1))

ggplot(BMI_1, aes(x= DII_Mom, y = Intercept)) + geom_point()

BMI_1 <- read.delim(paste0(meta2,"sb_Uni_CS/BMI_Uni_CS/mm_differentials-BMI_1m/differentials.tsv"), header=TRUE)
BMI_1 <- BMI_1[-c(1),]
BMI_1$Intercept <- BMI_1$Intercept %>% as.numeric()
BMI_1$mom_BMI <- BMI_1$mom_BMI %>% as.numeric()
BMIregression1 <- summary(lm(Intercept ~ mom_BMI, BMI_1))

ggplot(BMI_1, aes(x= mom_BMI, y = Intercept)) + geom_point()
