#importing data
#######################################
rm(list = ls())

## Packages 
library(tidyverse)
library(MedDietCalc)

meta <- "/Volumes/ADORLab/Master Mothers Milk Data Folder/Input/"
meta2 <- "/Volumes/ADORLab/__Users/ditr2320/datasets/"
meta4 <- "/Users/DidiTrifonova/Documents/GitHub/Git_ADOR/Mothers-Milk-Diet-Score-and-BMI/"
meta5 <- "/Users/DidiTrifonova/Documents/GitHub/Git_ADOR/Mothers-Milk-Diet-Score-and-BMI/HMO/code01Nov2021"
#reading in data 
# long data - does not have correct MDS scores
metadata <- read.delim(paste0(meta,"mothersMilk_metadata_timepointsAsRows_updated091620_Temporary24mDiet.txt"), header=TRUE)
metadata$Sex <- "female" 

mds1m <- metadata %>% filter(timepoint == 1)
mds6m <- metadata %>% filter(timepoint == 6)
mds1m$MDS_Mom <- computeMDS05(data = mds1m, 
                        Vegetables = veg_nostarch_Mom, 
                        Legumes = m_VEG0700_Mom, 
                        FruitAndNuts = (mds1m$fruit_nj_Mom) + (mds1m$nuts_seeds_serv_Mom),
                        Cereals = grains_whole_Mom, 
                        Potatoes = NULL, 
                        Fish = fish_serv_Mom, 
                        Meat = meat_serv_Mom, 
                        Dairy = dairy_serv_Mom, 
                        Alcohol = 14 *(mds1m$m_BVE0100_Mom + mds1m$m_BVE0200_Mom + mds1m$m_BVE0300_Mom + mds1m$m_BVE0400_Mom),
                        Fats = NULL, 
                        MUFA = m_mufa_Mom, 
                        PUFA = m_pufa_Mom, 
                        SFA = m_sfa_Mom,
                        Sex, men = "male", women= "female",
                        frequency = "daily", output = "absolute", rm.na = FALSE)

mds6m$MDS_Mom <- computeMDS05(data = mds6m, 
                                Vegetables = veg_nostarch_Mom, 
                                Legumes = m_VEG0700_Mom, 
                                FruitAndNuts = (mds6m$fruit_nj_Mom) + (mds6m$nuts_seeds_serv_Mom),
                                Cereals = grains_whole_Mom, 
                                Potatoes = NULL, 
                                Fish = fish_serv_Mom, 
                                Meat = meat_serv_Mom, 
                                Dairy = dairy_serv_Mom, 
                                Alcohol = 14 *(mds6m$m_BVE0100_Mom + mds6m$m_BVE0200_Mom + mds6m$m_BVE0300_Mom + mds6m$m_BVE0400_Mom),
                                Fats = NULL, 
                                MUFA = m_mufa_Mom, 
                                PUFA = m_pufa_Mom, 
                                SFA = m_sfa_Mom,
                                Sex, men = "male", women= "female",
                                frequency = "daily", output = "absolute", rm.na = FALSE)

metadata <- rbind(mds1m,mds6m)

#selecting variable names to make smaller dataset
covariates <- c("SES_index_final", "mother_age", "mom_BMI", "day1_metscore", "day2_metscore", "day3_metscore")
dietaryFactors <- c("DII_Mom", "HEI2015_TOTAL_SCORE_Mom", "m_ener_Mom","MDS_Mom")
x <- which(colnames(metadata)=="Secretor")
y <- which(colnames(metadata)=="SUM..ug.mL.")
HMO <- names(metadata)[x:y]

# Selecting variables and filtering timepoint 
Metasub <- select(metadata, c(1:4), timepoint, all_of(covariates), all_of(dietaryFactors),all_of(HMO) )  
MMdata <- filter(Metasub, (timepoint == 1 | timepoint == 6))
MMdata %>% count(timepoint)

# BMI CLASS 
#Normal 
MMdata$BMIclass[MMdata$mom_BMI  < 25] = "Normal"
#Overweight
MMdata$BMIclass[MMdata$mom_BMI  >= 25 & MMdata$mom_BMI  < 30] = "Overweight"
#Obese
MMdata$BMIclass[MMdata$mom_BMI  >= 30] = "Obese"
MMdata$BMIclass <- ordered(MMdata$BMIclass, levels=c('Normal', 'Overweight', 'Obese'))	

#DII CATEGORIES 
MMdata$DIIcategory [MMdata$DII_Mom  < 0] = "Anti-Inflammatory" 
MMdata$DIIcategory [MMdata$DII_Mom  >=0 ] = "Pro-Inflammatory"
MMdata$DIIcategory  <- ordered(MMdata$DIIcategory , levels=c('Pro-Inflammatory', 'Anti-Inflammatory'))

#MDS CATEGORIES
MMdata$MDS3category [MMdata$MDS_Mom  <=3] = 'Low'
MMdata$MDS3category [MMdata$MDS_Mom  ==4 | MMdata$MDS_Mom == 5] = 'Medium'
MMdata$MDS3category [MMdata$MDS_Mom  >=6] = 'High'
MMdata$MDS3category  <- ordered(MMdata$MDS3category , levels=c('Low', 'Medium', 'High'))	

MMdata$MDS2category [MMdata$MDS_Mom  <=4] = "Low" #'0-4'
MMdata$MDS2category [MMdata$MDS_Mom  >=5] = "High" #'5-9'
MMdata$MDS2category  <- ordered(MMdata$MDS2category , levels=c('Low', 'High'))	

# HEI CLASSES BASELINE
MMdata$HEIcategory [MMdata$HEI2015_TOTAL_SCORE_Mom  >= median(MMdata$HEI2015_TOTAL_SCORE_Mom )] = "High"
MMdata$HEIcategory [MMdata$HEI2015_TOTAL_SCORE_Mom  < median(MMdata$HEI2015_TOTAL_SCORE_Mom )] = "Low"
MMdata$HEIcategory  <- ordered(MMdata$HEIcategory , levels=c("Low", "High"))

#CREATING PHYSICAL ACTIVITY VARIABLE
MMdata <- mutate(MMdata, exercise  = (day1_metscore  + day2_metscore  + day3_metscore )/3)

#CHANGING MDS CLASS TO NUMERIC
#sapply(MMdata,class) #can use for checking which variables are numeric or not
MMdata$MDS_Mom <- as.numeric(MMdata$MDS_Mom) #changing class to numeric
MMdata %>% count(timepoint)


#filtering secretors only
MMdata <- MMdata %>% filter(Secretor == 1)
MMdata %>% count(timepoint)
#dataset for each timepoint
dataSet1m <- MMdata %>% filter(timepoint ==1)
dataSet6m <- MMdata %>% filter(timepoint ==6)

#removing 1m outliers
outliersRm1m <- dataSet1m %>% 
  mutate_if(is.numeric, ~replace(.x, abs(scale(.x)) > 3, NA)) #method using z-scores to remove 3sd, abs() is for absolute value, scale() is for z-score
#creating table that keeps track of how many participants are changed to NA by variable 
NAcountBefore1m <- dataSet1m  %>% summarise_all((funs(sum(is.na(.))))) 
NAcountAfter1m<- outliersRm1m %>% summarise_all((funs(sum(is.na(.))))) 
NAdiff1m <- (NAcountAfter1m - NAcountBefore1m) %>% t()

#removing 6m outliers 
outliersRm6m <- dataSet6m %>% 
  mutate_if(is.numeric, ~replace(.x, abs(scale(.x)) > 3, NA)) #method using z-scores to remove 3sd, abs() is for absolute value, scale() is for z-score
#creating table that keeps track of how many participants are changed to NA by variable 
NAcountBefore6m <- dataSet6m  %>% summarise_all((funs(sum(is.na(.))))) 
NAcountAfter6m<- outliersRm6m %>% summarise_all((funs(sum(is.na(.))))) 
NAdiff6m <- (NAcountAfter6m - NAcountBefore6m) %>% t()

#merging data back together
df <- rbind(outliersRm1m, outliersRm6m)
df %>% count(timepoint)
#DATASET - OUTLIERS REMOVED 
#write.table(df,paste0(meta5,"/tidyLongMds10Nov2021.txt",sep=""),quote=FALSE, sep="\t",append=FALSE, col.names=TRUE, row.names=FALSE)
