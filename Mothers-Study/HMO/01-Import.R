###Increase max.print
rm(list = ls())
options(max.print = 1000000)

###Packages
library(tidyverse)
library(ggpubr)
library(tidymodels)
library(car)
library(rstatix)
library(ggplot2)
library(reshape2)
library(psych)
library(janitor)
library(sjPlot)
library(withr)


###Load in the data
#dataSet <- read.csv("~/desktop/KS Goran Lab Resources/Studies/Mother's Milk/mothersMilk_metadata_timepointsAsColumns_updated82120_Temporary24mDiet.csv",header=TRUE,check.names = FALSE, na.strings=c(" ","n/a","N/A",""))
meta <- "/Volumes/ADORLab/Master Mothers Milk Data Folder/Input/Old Datasets"
meta1 <- "/Volumes/ADORLab/Master Mothers Milk Data Folder/Input"

dataSet  <- read.csv(paste0(meta,"/mothersMilk_metadata_timepointsAsColumns_updated82120_Temporary24mDiet.csv"), header=TRUE)
dataSet1  <- read.delim(paste0(meta1,"/mothersMilk_metadata_timepointsAsColumns_updated092120_Temporary24mDiet.txt"), header=TRUE)


