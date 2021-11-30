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

# Read in data new metadata (UPDATED)
#dataSet <- read.delim(paste0(meta2,"dataset01Apr2021.txt"),na = c("NA", "don't know", "","    .","Missing: Not provided",999.00))
dataSet <- read.delim(paste0(meta2,"longData16Aug2021.txt"), header=TRUE)
dash <- read.delim(paste0(meta2,"dashData01Sep2021.txt"), header=TRUE)
dataSet <- dash
####### META DATA ####### ####### ####### ####### ####### ####### 
####### ####### ####### ####### ####### ####### ####### ####### 
# Create susbets of data based on timepoint (visit number)

# Create susbets of data based on timepoint (visit number)
subset1m <- dataSet[dataSet$timepoint %in% "1",]
subset6m <- dataSet[dataSet$timepoint %in% "6",]

subset1m <- subset1m %>% select(- c(SES_index_final, day1_metscore, day2_metscore, day3_metscore, exercise))
subset1m %>% summarise_all((funs(sum(is.na(.))))) 
dataSet1m <- subset1m

subset6m <- subset6m %>% select(- c(SES_index_final, day1_metscore, day2_metscore, day3_metscore, exercise))
subset6m %>% summarise_all((funs(sum(is.na(.))))) 
dataSet6m <- subset6m

#not necessary for this data version
#dataSet1m <- dataSet1m %>% filter(dyad_id %in% dataSet6m$dyad_id)
#dataSet6m <- dataSet6m %>% filter(dyad_id %in% dataSet1m$dyad_id)

dataSetAv <- dataSet1m %>% select(c(1:3), SES_r)
dataSetAv$timepoint <- "average"
dataSetAv$mother_age <- (dataSet6m$mother_age + dataSet1m$mother_age)/2
dataSetAv$DII_Mom <- (dataSet6m$DII_Mom + dataSet1m$DII_Mom)/2 
dataSetAv$MDS_Mom <- (dataSet6m$MDS_Mom + dataSet1m$MDS_Mom)/2 
dataSetAv$HEI2015_TOTAL_SCORE_Mom <- (dataSet6m$HEI2015_TOTAL_SCORE_Mom + dataSet1m$HEI2015_TOTAL_SCORE_Mom)/2 
dataSetAv$exercise_r <- (dataSet6m$exercise_r + dataSet1m$exercise_r)/2 
dataSetAv$mom_BMI <- (dataSet6m$mom_BMI + dataSet1m$mom_BMI)/2 
dataSetAv$m_ener_Mom <- (dataSet6m$m_ener_Mom + dataSet1m$m_ener_Mom)/2 
dataSetAv$BMI_change <- dataSet6m$mom_BMI - dataSet1m$mom_BMI
dataSetAv$kcal_change <- (dataSet6m$m_ener_Mom - dataSet1m$m_ener_Mom)/2

# TABLE 1 ############ ############ ############ ############ ############ 
age_t <- t.test(dataSet1m$mother_age, dataSet6m$mother_age, paired= TRUE)
age_t <- as.numeric(age_t[3][1]) 
age_t <- format(age_t, digits=3)

weight_t <- t.test(dataSet1m$m_weight_kg, dataSet6m$m_weight_kg, paired= TRUE)
weight_t <- as.numeric(weight_t[3][1])
weight_t <- format(weight_t, digits =3)

BMI_t <- t.test(dataSet1m$mom_BMI, dataSet6m$mom_BMI, paired= TRUE)
BMI_t <- as.numeric(BMI_t[3][1])
BMI_t <- format(BMI_t, digits =3)

#TABLE 2 ############ ############ ############ ############ ############ 
MDS_t <- t.test(dataSet1m$MDS_Mom, dataSet6m$MDS_Mom, paired= TRUE)
MDS_t <- as.numeric(MDS_t[3][1])
MDS_t <- format(MDS_t, digits = 3) 

DII_t <- t.test(dataSet1m$DII_Mom, dataSet6m$DII_Mom, paired= TRUE)
DII_t <- as.numeric(DII_t[3][1])
DII_t <- format(DII_t, digits = 3) 

HEI_t <- t.test(dataSet1m$HEI2015_TOTAL_SCORE_Mom, dataSet6m$HEI2015_TOTAL_SCORE_Mom, paired= TRUE)
HEI_t <- as.numeric(HEI_t[3][1])
HEI_t <- format(HEI_t, digits = 3) 

Kcal_t <- t.test(dataSet1m$m_ener_Mom, dataSet6m$m_ener_Mom, paired= TRUE)
Kcal_t <- as.numeric(Kcal_t[3][1])
Kcal_t <- format(Kcal_t, digits = 3) 

exercise_t <- t.test(dataSet1m$exercise, dataSet6m$exercise, paired= TRUE)
exercise_t <- as.numeric(exercise_t[3][1])
exercise_t <- format(exercise_t, digits = 3) 

############ TABLE 1 ############ ############ ############ ############ ############ 
metadata <- dataSet1m
DespFactors  <- c("mother_age", "m_height_cm","m_weight_kg", "mom_BMI", "BMIclass")

table1 <- character(0)
overallColumn <- character(0)

for(i in DespFactors)
{
  if(i %in% c("BMIclass"))
  {
    if(i %in% c())
    {
      table1 <- c(table1,i)
      table1 <- c(table1,names(table(metadata[,names(metadata) %in% i])))
      overallColumn[[length(overallColumn)+1]] <- ""
      overallColumn <- c(overallColumn, as.numeric(table(metadata[,names(metadata) %in% i])))
    }
    else{
      table1[[length(table1)+1]] <- i
      countFactor1 <- table(metadata[,names(metadata) %in% i])[1]
      countFactor2 <- table(metadata[,names(metadata) %in% i])[2]
      countFactor3 <- table(metadata[,names(metadata) %in% i])[3]
      percentFactor1 <- format(countFactor1/nrow(metadata)*100,digits = 3)
      percentFactor2 <- format(countFactor2/nrow(metadata)*100,digits = 3)
      percentFactor3 <- format(countFactor3/nrow(metadata)*100,digits = 3)
      thisAllColumn <- paste(countFactor1, " (", percentFactor1,"%)  ",countFactor3," (",percentFactor3,"%)  ",
                             countFactor2," (",percentFactor2,"%)",sep="")
      #thisAllColumn <- paste(countFactor1, ", ", countFactor2,", ",percentFactor1,"%",sep="")
      overallColumn[[length(overallColumn)+1]] <- thisAllColumn
    }
  }
  else{
    table1[[length(table1)+1]] <- i
    meanAll<- format(mean(metadata[,names(metadata) %in% i],na.rm=TRUE),digits=3)
    sdAll <- format(sd(metadata[,names(metadata) %in% i],na.rm=TRUE),digits=3)
    thisAllColumn <- paste(meanAll, "±", sdAll)
    overallColumn[[length(overallColumn)+1]] <- thisAllColumn;
  }
}

table1 <- cbind(table1,overallColumn) 
table1 <- data.frame(table1)
Observations <- c("N", nrow(metadata)) 
agePval <- c("Age P-value", age_t)
weightPval <- c("Weight P-value", weight_t)
BMIPval <- c("BMI P-value", BMI_t) 
table1 <- rbind(table1,Observations, agePval, weightPval, BMIPval)
table1 <- table1 %>% rename("1-Month" = "overallColumn", "Variable" = "table1")
table1

###### 6m 
metadata1 <- dataSet6m
DespFactors  <- c("mother_age", "m_height_cm","m_weight_kg", "mom_BMI", "BMIclass")

table1.6 <- character(0)
overallColumn <- character(0)

for(i in DespFactors)
{
  if(i %in% c("BMIclass"))
  {
    if(i %in% c())
    {
      table1.6 <- c(table1.6,i)
      table1.6 <- c(table1.6,names(table(metadata1[,names(metadata1) %in% i])))
      overallColumn[[length(overallColumn)+1]] <- ""
      overallColumn <- c(overallColumn, as.numeric(table(metadata1[,names(metadata1) %in% i])))
    }
    else{
      table1.6[[length(table1.6)+1]] <- i
      countFactor1 <- table(metadata1[,names(metadata1) %in% i])[1]
      countFactor2 <- table(metadata1[,names(metadata1) %in% i])[2]
      countFactor3 <- table(metadata1[,names(metadata1) %in% i])[3]
      percentFactor1 <- format(countFactor1/nrow(metadata1)*100,digits = 3)
      percentFactor2 <- format(countFactor2/nrow(metadata1)*100,digits = 3)
      percentFactor3 <- format(countFactor3/nrow(metadata1)*100,digits = 3)
      thisAllColumn <- paste(countFactor1, " (", percentFactor1,"%)  ",countFactor3," (",percentFactor3,"%)  ",
                             countFactor2," (",percentFactor2,"%)",sep="")
      #thisAllColumn <- paste(countFactor1, ", ", countFactor2,", ",percentFactor1,"%",sep="")
      overallColumn[[length(overallColumn)+1]] <- thisAllColumn
    }
  }
  else{
    table1.6[[length(table1.6)+1]] <- i
    meanAll<- format(mean(metadata1[,names(metadata1) %in% i],na.rm=TRUE),digits=3)
    sdAll <- format(sd(metadata1[,names(metadata1) %in% i],na.rm=TRUE),digits=3)
    thisAllColumn <- paste(meanAll, "±", sdAll)
    overallColumn[[length(overallColumn)+1]] <- thisAllColumn;
  }
}

table1.6 <- cbind(table1.6,overallColumn)
table1.6 <- data.frame(table1.6)
Observations <- c("N", nrow(metadata1))
agePval <- c("Age P-value", age_t) 
weightPval <- c("Weight P-value", weight_t)
BMIPval <- c("BMI P-value", BMI_t)
table1.6 <- rbind(table1.6,Observations, agePval, weightPval, BMIPval)
table1.6 <- table1.6 %>% rename("6-Month" = "overallColumn", "Variable" = "table1.6")
table1.6

combinedTable1 <- full_join(table1,table1.6, by = "Variable")


################## COMBINED TABLE 2 
#1m 
DespFactors  <- c("DII_Mom",
                  "HEI2015_TOTAL_SCORE_Mom", 
                  "MDS_Mom", 
                  "m_ener_Mom", 
                  "exercise_r")

table2 <- character(0)
overallColumn <- character(0)
for(i in DespFactors)
{
  table2[[length(table2)+1]] <- i
  meanAll<- format(mean(metadata[,names(metadata) %in% i],na.rm=TRUE),digits=3)
  sdAll <- format(sd(metadata[,names(metadata) %in% i],na.rm=TRUE),digits=3)
  thisAllColumn <- paste(meanAll, "±", sdAll)
  overallColumn[[length(overallColumn)+1]] <- thisAllColumn;
}

table2 <- cbind(table2,overallColumn)
table2 <- data.frame(table2)
Observations <- c("N", nrow(metadata))
DIIPval <- c("DII P-value", DII_t)
HEIPval <- c("HEI P-value", HEI_t)
MDSPval <- c("MDS P-value", MDS_t)
KcalPval <- c("Kcal P-value", Kcal_t)
exercisePval <- c("Exercise P-value", exercise_t)
table2 <- rbind(table2,Observations, DIIPval, HEIPval, MDSPval, KcalPval, exercisePval)
table2 <- table2 %>% rename("1-Month" = "overallColumn", "Variable" = "table2")
table2

#### 6m 
DespFactors  <- c("DII_Mom",
                  "HEI2015_TOTAL_SCORE_Mom", 
                  "MDS_Mom", 
                  "m_ener_Mom", 
                  "exercise_r")

table2.6 <- character(0)
overallColumn <- character(0)
for(i in DespFactors)
{
  table2.6[[length(table2.6)+1]] <- i
  meanAll<- format(mean(metadata1[,names(metadata1) %in% i],na.rm=TRUE),digits=3)
  sdAll <- format(sd(metadata1[,names(metadata1) %in% i],na.rm=TRUE),digits=3)
  thisAllColumn <- paste(meanAll, "±", sdAll)
  overallColumn[[length(overallColumn)+1]] <- thisAllColumn;
}

table2.6 <- cbind(table2.6,overallColumn)
table2.6 <- data.frame(table2.6)
Observations <- c("N", nrow(metadata1))
DIIPval <- c("DII P-value", DII_t)
HEIPval <- c("HEI P-value", HEI_t)
MDSPval <- c("MDS P-value", MDS_t)
KcalPval <- c("Kcal P-value", Kcal_t)
exercisePval <- c("Exercise P-value", exercise_t)
table2.6 <- rbind(table2.6,Observations, DIIPval, HEIPval, MDSPval, KcalPval, exercisePval)
table2.6 <- table2.6 %>% rename("6-Month" = "overallColumn", "Variable" = "table2.6")
table2.6

combinedTable2 <- full_join(table2,table2.6, by = "Variable")

#for making combined Table 2 into two sub tables 
#combinedTable2.1 <- combinedTable2[c(1:9),]
#combinedTable2.2<- combinedTable2[c(10:17),]
#combinedTable2.2 <- combinedTable2.2 %>% rename ("P-value" = "1-Month") %>% select(- "6-Month")

hux <- combinedTable1 %>% as_huxtable() %>% set_all_borders(1) %>% set_bold(1, everywhere, TRUE) %>% map_text_color(by_values("0.00345" = "red"))
#quick_pdf(hux)
hux <- combinedTable2 %>% as_huxtable() %>% set_all_borders(1) %>% set_bold(1, everywhere, TRUE) %>% map_text_color(by_values("0.00345" = "red"))
#quick_pdf(hux)


### TRENDS, CORRELATIONS, MISC 
subset <- dataSet %>% select(DII_Mom, MDS_Mom, HEI2015_TOTAL_SCORE_Mom)
subset <- subset %>% rename("HEI" = "HEI2015_TOTAL_SCORE_Mom", "MDS" = "MDS_Mom", "DII" = "DII_Mom")
s <- cor(subset) 
corrplot(s, method = "circle",type = "upper",diag=FALSE)

####### BOTH TIMEPOINTS
subset1 <- dataSet %>% select(DII_Mom, MDS_Mom, HEI2015_TOTAL_SCORE_Mom, dash_score, mom_BMI, mother_age, exercise_r, SES_r)
subset1 <- subset1 %>% rename("HEI" = "HEI2015_TOTAL_SCORE_Mom", "MDS" = "MDS_Mom", "DII" = "DII_Mom","DASH" = "dash_score", "BMI" = "mom_BMI", "Age" = "mother_age", "Physical Activity" = "exercise_r", "SES" = "SES_r")
s1 <- cor(subset1, method = "pearson") 
testRes = cor.mtest(subset1, conf.level = 0.95)

corrplot(s1, method = "circle",type = "upper",diag=FALSE)
corrplot(s1, p.mat = testRes$p, insig = 'p-value', sig.level = -1)
corrplot(s1, p.mat = testRes$p, sig.level = 0.10, order = 'hclust', addrect = 2) 
corrplot(s1, p.mat = testRes$p, method = 'color', diag = FALSE, type = 'upper',
         sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9, 
         insig = 'label_sig', pch.col = 'grey20')

subset1 <- dataSet %>% select(DII_Mom, MDS_Mom, HEI2015_TOTAL_SCORE_Mom, m_ener_Mom, mom_BMI, mother_age, exercise_r, SES_r)
subset1 <- subset1 %>% rename("HEI" = "HEI2015_TOTAL_SCORE_Mom", "MDS" = "MDS_Mom", "DII" = "DII_Mom","KCAL" = "m_ener_Mom", "BMI" = "mom_BMI", "Age" = "mother_age", "Physical Activity" = "exercise_r", "SES" = "SES_r")
s1 <- cor(subset1, method = "pearson") 
testRes = cor.mtest(subset1, conf.level = 0.95)

corrplot(s1, method = "circle",type = "upper",diag=FALSE)
corrplot(s1, p.mat = testRes$p, insig = 'p-value', sig.level = -1)
corrplot(s1, p.mat = testRes$p, sig.level = 0.10, order = 'hclust', addrect = 2) 
corrplot(s1, p.mat = testRes$p, method = 'color', diag = FALSE, type = 'upper',
         sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9, 
         insig = 'label_sig', pch.col = 'grey20')




#### FOR INDIVIDUAL TIMEPOINTS
corr1m <- dataSet1m %>% select(DII_Mom, MDS_Mom, HEI2015_TOTAL_SCORE_Mom, mom_BMI, mother_age, exercise_r, SES_r)
corr1m <- corr1m %>% rename("HEI" = "HEI2015_TOTAL_SCORE_Mom", "MDS" = "MDS_Mom", "DII" = "DII_Mom", "BMI" = "mom_BMI", "Age" = "mother_age", "Physical Activity" = "exercise_r", "SES" = "SES_r")
s1 <- cor(corr1m) 
corrplot(s1, method = "circle",type = "upper",diag=FALSE)

corr6m <- dataSet6m %>% select(DII_Mom, MDS_Mom, HEI2015_TOTAL_SCORE_Mom, mom_BMI, mother_age, exercise_r, SES_r)
corr6m <- corr6m %>% rename("HEI" = "HEI2015_TOTAL_SCORE_Mom", "MDS" = "MDS_Mom", "DII" = "DII_Mom", "BMI" = "mom_BMI", "Age" = "mother_age", "Physical Activity" = "exercise_r", "SES" = "SES_r")
s1 <- cor(corr6m) 
corrplot(s1, method = "circle",type = "upper",diag=FALSE)


dataSetAv$BMIchange_cat <- ifelse(dataSetAv$BMI_change > 0, "gain", "loss")
dataSetAv$energychange_cat <- ifelse(dataSetAv$kcal_change > 0, "more", "less")
dataSetAv %>% count(BMIchange_cat)
dataSetAv %>% count(energychange_cat)
dataSet1m %>% count(DIIcategory)
dataSet6m %>% count(DIIcategory)


ggplot(data = dataSetAv) + 
  geom_point(mapping = aes(x = DII_Mom, y = m_ener_Mom)) + 
  geom_smooth(mapping = aes(x = DII_Mom, y = m_ener_Mom), method = "lm")


#DESCRIPTIVES TABLE FOR BSI APPLICATION

BSI_table <- combinedTable1[1:5,]
BSI_table[1,1] <- "Age, y"
BSI_table[2,1] <- "Height, cm"
BSI_table[3,1] <- "Weight, kg"
BSI_table[4,1] <- "BMI"
BSI_table[5,1] <- "Weight Status"
hux <- BSI_table %>% as_huxtable() %>% set_all_borders(1) %>% set_bold(1, everywhere, TRUE) %>% map_text_color(by_values("0.00345" = "red"))
#quick_pdf(hux)

#HOLLINGSHEAD INDEX
dataSet1m$SES_r %>% mean()
dataSet1m$SES_r %>% sd()

#DII STATUS BY AGE 
dataSet1m$DIIcategory %>% 
dataSet6m$DIIcategory
dataSet$DIIcategory
dataSet %>% count(timepoint)


DII_age <- t.test(dataSet1m$mother_age ~ dataSet1m$DIIcategory)
DII_agep <- as.numeric(DII_age[3][1])
DII_agep <- format(DII_agep, digits = 3) 
