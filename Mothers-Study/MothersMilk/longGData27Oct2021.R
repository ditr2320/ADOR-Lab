#importing data
#######################################
rm(list = ls())

## Packages 
library(tidyverse)

meta <- "/Volumes/ADORLab/Master Mothers Milk Data Folder/Input/"
meta2 <- "/Volumes/ADORLab/__Users/ditr2320/datasets/"
meta4 <- "/Users/DidiTrifonova/Documents/GitHub/Git_ADOR/Mothers-Milk-Diet-Score-and-BMI/"
meta5 <- "/Volumes/ADORLab/HEI Study/GreenSpace/Buffers from Kate/"

#reading in data 
# long data - does not have correct MDS scores
metadata <- read.delim(paste0(meta,"mothersMilk_metadata_timepointsAsRows_updated091620_Temporary24mDiet.txt"), header=TRUE)
# wide data - has correct MDS scores
mdsData <- read.delim(paste0(meta,"/mothersMilk_metadata_timepointsAsColumns_updated092120_Temporary24mDiet.txt"), header=TRUE)
# greenspace data 
green <- read.csv(paste0(meta5,"All_Buffers_Cleaned.csv"),header = TRUE)

# Selecting variables and filtering timepoint
Metasub <- select(metadata, c(1:4), timepoint,SES_index_final, mother_age, 
                          m_weight_kg, m_height_cm, mom_BMI,DII_Mom, 
                          HEI2015_TOTAL_SCORE_Mom, m_ener_Mom, day2_metscore,
                          day3_metscore, day1_metscore, prepreg_bmi_kgm2)  
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


#ADDING GREENSPACE
greenS <- green %>% filter(period == 2 | period == 3) %>% select(dyad_id,period,NewLatitud,NewLongitu,Season,M_100,M_300,M_500,M_1000) %>% filter(M_100 != 0)

ggplot(data = greenS) + 
  geom_histogram(mapping = aes(x=M_100), binwidth = 0.01, fill = 'slategray', color = 'black') + facet_wrap(~Season)
ggplot(data = greenS) + 
  geom_histogram(mapping = aes(x=M_100), binwidth = 0.01, fill = 'slategray', color = 'black') + facet_wrap(~period)

greenS %>% filter(period ==2) %>% select(M_100) %>% summary()
greenS %>% filter(period ==3) %>% select(M_100) %>% summary()

green1 <- greenS %>% filter(period ==2)
green6 <- greenS %>% filter(period ==3)

g1 <- green1 %>% filter(dyad_id %in% dataSet1m$dyad_id) 
g6 <- green6 %>% filter(dyad_id %in% dataSet6m$dyad_id) 

merged1 <- left_join(dataSet1m,green1)
merged6 <- left_join(dataSet6m,green6)

#checking sample size
dataPaired <- dataPaired[order(dataPaired$dyad_id),]
dataPaired %>% count(timepoint)

totalMerged <- rbind(merged1,merged6)
totalMerged <- totalMerged[order(totalMerged$dyad_id),]
totalMerged <- totalMerged %>% filter(dyad_id %in% dataSet6m$dyad_id)
totalMerged$dyad_id == dataPaired$dyad_id
totalMerged %>% count(timepoint)


## REMOVING OUTLIERS
numVars <- c("MDS_Mom","DII_Mom","HEI2015_TOTAL_SCORE_Mom","mom_BMI") 

dataF <- dataSet1m
print(paste("*****************1*****************"))
for(i in 1: length(numVars))
{
  temp <- data.frame(variable=dataSet1m[,names(dataSet1m) %in% numVars[i]])
  temp1 <- data.frame(variable=dataF[,names(dataF) %in% numVars[i]])
  minoutlier <- mean(temp$variable, na.rm = T) - (3 * sd(temp$variable, na.rm = T))
  maxoutlier <- mean(temp$variable, na.rm = T) + (3 * sd(temp$variable, na.rm = T))
  dataF <- filter(dataF, (temp1$variable >= minoutlier & temp1$variable <= maxoutlier) | is.na(temp1$variable) == TRUE)
  numVars[i] %>% print()
  nrow(dataF) %>% print()
} 

#finding Dyad id that was removed (BMI at Baseline)
BMI_min <- mean(dataSet1m$mom_BMI, na.rm = T) - (3 * sd(dataSet1m$mom_BMI, na.rm = T))
BMI_max <- mean(dataSet1m$mom_BMI, na.rm = T) + (3 * sd(dataSet1m$mom_BMI, na.rm = T))
for(i in 1:nrow(dataSet1m)){
  if(dataSet1m$mom_BMI[i] < BMI_min || dataSet1m$mom_BMI[i] > BMI_max){
    print(dataSet1m$dyad_id[i])
  }
}

dataG <- dataSet6m
print(paste("*****************1*****************"))
for(i in 1: length(numVars))
{
  temp <- data.frame(variable=dataSet6m[,names(dataSet6m) %in% numVars[i]])
  temp1 <- data.frame(variable=dataG[,names(dataG) %in% numVars[i]])
  minoutlier <- mean(temp$variable, na.rm = T) - (3 * sd(temp$variable, na.rm = T))
  maxoutlier <- mean(temp$variable, na.rm = T) + (3 * sd(temp$variable, na.rm = T))
  dataG <- filter(dataG, (temp1$variable >= minoutlier & temp1$variable <= maxoutlier) | is.na(temp1$variable) == TRUE)
  numVars[i] %>% print()
  nrow(dataG) %>% print()
} 

#finding Dyad id that was removed HEI at 6m)
HEI_min <- mean(dataSet6m$HEI2015_TOTAL_SCORE_Mom, na.rm = T) - (3 * sd(dataSet6m$HEI2015_TOTAL_SCORE_Mom, na.rm = T))
HEI_max <- mean(dataSet6m$HEI2015_TOTAL_SCORE_Mom, na.rm = T) + (3 * sd(dataSet6m$HEI2015_TOTAL_SCORE_Mom, na.rm = T))
for(i in 1:nrow(dataSet6m)){
  if(dataSet6m$HEI2015_TOTAL_SCORE_Mom[i] < HEI_min || dataSet6m$HEI2015_TOTAL_SCORE_Mom[i] > HEI_max){
    print(dataSet6m$dyad_id[i])
  }
}

#finding Dyad id that was removed BMI at 6m)
BMI6_min <- mean(dataSet6m$mom_BMI, na.rm = T) - (3 * sd(dataSet6m$mom_BMI, na.rm = T))
BMI6_max <- mean(dataSet6m$mom_BMI, na.rm = T) + (3 * sd(dataSet6m$mom_BMI, na.rm = T))
for(i in 1:nrow(dataSet6m)){
  if(dataSet6m$mom_BMI[i] < BMI6_min || dataSet6m$mom_BMI[i] > BMI6_max){
    print(dataSet6m$dyad_id[i])
  }
}

#merging datasets without outliers
data1 <- rbind(dataG,dataF)

#only participants with both timepoints 
df <- dataPaired %>% 
  group_by(dyad_id) %>%
  filter(n()>1)
df <- df %>% as.tibble()
df %>% count(timepoint)

df1 <- totalMerged %>% 
  group_by(dyad_id) %>%
  filter(n()>1)
df1 <- df1 %>% as.tibble()
df1 %>% count(timepoint)


#DATASET - NO OUTLIERS EXCLUDED
#write.table(df1,paste0(meta2,"/longGData27Oct2021.txt",sep=""),quote=FALSE, sep="\t",append=FALSE, col.names=TRUE, row.names=FALSE)
