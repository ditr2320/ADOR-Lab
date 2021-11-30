#importing data
#######################################
rm(list = ls())

## Packages 
library(tidyverse)

meta <- "/Volumes/ADORLab/Master Mothers Milk Data Folder/Input/"
meta2 <- "/Volumes/ADORLab/__Users/ditr2320/datasets/"
meta4 <- "/Users/DidiTrifonova/Documents/GitHub/Git_ADOR/Mothers-Milk-Diet-Score-and-BMI/"
meta5 <- '/Volumes/ADORLab/HEI Study/Exposures/Exposure Estimates/Processed Exposures/Batch 2/Ambient/Processed Exposures/Master Processed Exposure Windows/zm_Ambient_Weighted_Birth_weightd_Preg1m_Preg6m_and_PregOnly_and_Blv762020.csv'

#reading in data 
# long data - does not have correct MDS scores
metadata <- read.delim(paste0(meta,"mothersMilk_metadata_timepointsAsRows_updated091620_Temporary24mDiet.txt"), header=TRUE)
# wide data - has correct MDS scores
mdsData <- read.delim(paste0(meta,"/mothersMilk_metadata_timepointsAsColumns_updated092120_Temporary24mDiet.txt"), header=TRUE)
birth6m <- read.csv(paste0(meta2,"/ambient_Birth_6m.csv"), header=TRUE)
preg6m <- read.csv(paste0(meta2,"/ambient_preg_6m.csv"), header=TRUE)


# Selecting variables and filtering timepoint
Metasub <- select(metadata, c(1:4), timepoint,SES_index_final, mother_age, 
                          m_weight_kg, m_height_cm, mom_BMI,DII_Mom, 
                          HEI2015_TOTAL_SCORE_Mom, m_ener_Mom, day2_metscore,
                          day3_metscore, day1_metscore, prepreg_bmi_kgm2,m_sfa_Mom, 
                          m_fat_Mom, m_pro_Mom, m_chol_Mom, m_fib_Mom, m_magnesium_Mom, m_calcium_Mom, m_potassium_Mom, m_sodium_Mom )  
MMdata0 <- filter(Metasub, (timepoint == 1 | timepoint == 6))

#selecting only MDS and ID from wide
mdsData1 <- mdsData %>% select(dyad_id,MDS_baseline, MDS_6m)
#makign data long
mdsData2<-  pivot_longer(mdsData1, cols = c(2:3), names_to = "timepoint", values_to = "MDS_Mom")
mdsData2$timepoint <- ifelse(mdsData2$timepoint == "MDS_baseline", 1, 6)
#merging MDS into data 
#Note - difference in sample sizes are from participants with only NA values which get removed in merged 
MMdata1 <- merge(MMdata0,mdsData2,by=c("dyad_id","timepoint"))

#Participants must have baseline visit 
#creating a subset with a baseline visit to track dyad id 
visitBaseline <- MMdata1 %>% filter(!is.na(MMdata1$m_weight_kg) & MMdata1$timepoint == 1) 
# filtering ids 
MMdata2 <- MMdata1 %>% filter(dyad_id %in% visitBaseline$dyad_id)

# Participants must have diet data 
MMdata2$dietData <- ifelse(is.na(MMdata2$DII_Mom) , "No", "Yes")  
MMdata <- filter(MMdata2, dietData == "Yes") 

# checking sample sizes
#baseline = 209, 6m = 188
MMdata %>% count(timepoint)

# BMI CLASS PRE PREG 
#Normal
MMdata$BMIclass_pre[MMdata$prepreg_bmi_kgm2 < 25] = "Normal"
#Overweight
MMdata$BMIclass_pre[MMdata$prepreg_bmi_kgm2 >= 25 & MMdata$prepreg_bmi_kgm2 < 30] = "Overweight"
#Obese
MMdata$BMIclass_pre[MMdata$prepreg_bmi_kgm2 >= 30] = "Obese"
MMdata$BMIclass_pre <- ordered(MMdata$BMIclass_pre, levels=c('Normal', 'Overweight', 'Obese'))	

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

#SES median replacement
summary(MMdata$SES_index_final) # 4 NA values (two participants)
MMdata$SES_r=ifelse(is.na(MMdata$SES_index_final),median(MMdata$SES_index_final,na.rm=T),MMdata$SES_index_final)
MMdata$SES_r %>% summary()

#physical activity median replacement 
summary(MMdata$exercise) # 1 NA value
MMdata$exercise_r=ifelse(is.na(MMdata$exercise),median(MMdata$exercise,na.rm=T),MMdata$exercise)
MMdata$exercise_r %>% summary()

#particpants with both timepoints 
dataPaired <- MMdata
dataSet1m <- dataPaired[dataPaired$timepoint %in% "1",]
dataSet6m <- dataPaired[dataPaired$timepoint %in% "6",]
dataPaired <- dataPaired %>% filter(dyad_id %in% dataSet6m$dyad_id)
#checking sample size
dataPaired %>% count(timepoint)

birth6m$dyad_id <- substr(birth6m$GeoID,3,6) %>% as.numeric()
preg6m$dyad_id <- substr(preg6m$GeoID,3,6) %>% as.numeric()

dataAP <- left_join(dataPaired,birth6m)
dataAP <- left_join(dataAP, preg6m)
#only participants with both timepoints 
df <- dataAP %>% 
  group_by(dyad_id) %>%
  filter(n()>1)
df <- df %>% as_tibble()
df %>% count(timepoint)

#DATASET - NO OUTLIERS EXCLUDED
write.table(df,paste0(meta2,"/apData30Nov2021.txt",sep=""),quote=FALSE, sep="\t",append=FALSE, col.names=TRUE, row.names=FALSE)
