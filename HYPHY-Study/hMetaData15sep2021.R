#importing data
#######################################
rm(list = ls())

## Packages 
library(tidyverse)
library(data.table)
library(withr)
library(MedDietCalc)
library(dplyr)

meta <- "/Volumes/ADORLab/Master Mothers Milk Data Folder/Input/"
meta1 <- "/Volumes/ADORLab/Current Lab Projects/Hyphy/IRB COVID procedures/ASA24/20210913_HYPHYCOV_analysis_files/"
meta2 <- "/Volumes/ADORLab/__Users/ditr2320/datasets/"
meta3 <- "/Volumes/ADORLab/__Users/ditr2320/hyphyData/"
meta4 <- "/Users/DidiTrifonova/Documents/GitHub/Git_ADOR/Mothers-Milk-Diet-Score-and-BMI/"

#reading in data 
dietaryData <- read.delim(paste0(meta3,"hDietData15Sep2021.txt"), header=TRUE) %>% as_tibble()
dietaryData <- dietaryData[order(dietaryData$ID, decreasing=FALSE),]

chars <- read.csv(paste0(meta3,"HYPHYVirtualVisitQue-Characteristics_DATA_2021-09-15_1357.csv"), header=TRUE)
IPAQ <- read.csv(paste0(meta3,"IPAQ.csv"), header=TRUE)
questions <- read.csv(paste0(meta3,"questions2.csv"), header=TRUE)
hipAvg <- read.csv(paste0(meta3,"hipAvg.csv"), header=TRUE)
#merging dataset 
full <- full_join(chars, dietaryData, by = c("record_id" = "ID")) #n = 74
#removing participants that don't have both bmi AND diet 
full <- full %>% filter(is.na(bmi_calc) == FALSE | is.na(RecallNo) == FALSE) #n=73 (participants with bmi OR diet )


#adding dietary indices
###############################################
###############################################
###############################################

#Mediterranean Diet Score
###############################################
Vegetables <- full$V_TOTAL
Legumes <- full$V_LEGUMES
FruitAndNuts <- full$F_CITMLB + full$F_OTHER + full$PF_NUTSDS
Cereals <- full$G_WHOLE
Fish <- full$PF_SEAFD_HI + full$PF_SEAFD_LOW
Meat <- full$PF_MEAT
Dairy <- full$D_TOTAL
Alcohol <- full$ALC
MUFA = full$MFAT
PUFA = full$PFAT
SFA = full$SFAT

full$MDS <- computeMDS05(full, Vegetables, Legumes, FruitAndNuts,
                         Cereals, Potatoes = NULL, Fish, Meat, Dairy, Alcohol,
                         Fats = NULL, MUFA, PUFA, SFA,
                         biosex_v2, men = 1, women= 2,
                         frequency = "daily", output = "absolute", rm.na = FALSE)

#Dietary Inflammatory Index 
###############################################
##1 Vitamin B12 (OIS=0.106) (MEAN/SD=5.15/2.70)
pc_vit12 <- ((2*(pnorm((full$VB12-5.15)/2.70)))-1)
DII_vit12 <-(pc_vit12 * .106) 

##2 Vitamin B6 (OIS= -.365) (MEAN/SD=1.47/0.74)
pc_vitB6 <- ((2*(pnorm((full$VB6-1.47)/0.74)))-1)
DII_vitB6 <-(pc_vit12 * (-.365)) 

##3 Beta-Carotene (OIS= -0.584) (MEAN/SD=3718/1720)
pc_betaCarotene <- ((2*(pnorm((full$BCAR-3718)/1720)))-1)
DII_betaCarotene <-(pc_betaCarotene * (-0.584)) 

##4 Folic acid (OIS= -0.190) (MEAN/SD=273/70.7)
pc_folate <- ((2*(pnorm((full$FA-273)/70.7)))-1)
DII_folate <-(pc_folate * (-0.190)) 

##5 Iron (OIS= 0.032) (MEAN/SD= 13.35/3.71)
pc_iron <- ((2*(pnorm((full$IRON-13.35)/3.71)))-1)
DII_iron <-(pc_iron * 0.032) 

##6 Magnesium (OIS= -0.484) (MEAN/SD = 310.1/139.4)
pc_magnesium <- ((2*(pnorm((full$MAGN-310.1)/139.4)))-1)
DII_magnesium <-(pc_magnesium * (-0.484)) 

##7 Niacin (OIS=-0.246) (MEAN/SD = 25.90/11.77)
pc_niacin <- ((2*(pnorm((full$NIAC-25.90)/11.77)))-1)
DII_niacin <-(pc_niacin * -0.246) 

##8 Riboflavin (OIS=-0.068) (MEAN/SD = 1.70/0.79)
pc_riboflavin <- ((2*(pnorm((full$VB2-1.70)/0.79)))-1)
DII_riboflavin <-(pc_riboflavin * -0.068) 

##9 Selenium (OIS=-0.191) (MEAN/SD = 67/25.1)  
pc_selenium <- ((2*(pnorm((full$SELE-67)/25.1)))-1)
DII_selenium <-(pc_selenium * (-0.191)) 

##10 Thiamin (OIS=-0.098) (MEAN/SD = 1.70/0.66)  
pc_thiamin <- ((2*(pnorm((full$VB1-1.70)/0.66)))-1)
DII_thiamin <-(pc_thiamin * (-0.098)) 

##11 Vitamin A (OIS=-0.401) (MEAN/SD = 983.9/518.6)  
pc_vitA <- ((2*(pnorm((full$VARA-983.9)/518.6)))-1)
DII_vitA <-(pc_vitA * (-0.401)) 

##12 Vitamin C (OIS=-0.424) (MEAN/SD = 118.2/43.46)
pc_vitC <- ((2*(pnorm((full$VC-118.2)/43.46)))-1)
DII_vitC <-(pc_vitC * (-0.424)) 

##13 Vitamin D (OIS=-0.446) (MEAN/SD = 6.26/2.21)
pc_vitD <- ((2*(pnorm((full$VITD-6.26)/2.21)))-1)
DII_vitD <-(pc_vitD * (-0.446)) 

##14 Vitamin E (OIS=-0.419) (MEAN/SD = 8.73/1.49)
full$VITE_TOTAL <- full$ATOC + full$VITE_ADD
pc_vitE_mg <- ((2*(pnorm((full$VITE_TOTAL-8.73)/1.49)))-1)
DII_vitE_mg <-(pc_vitE_mg * (-0.419)) 

##15 Zinc (OIS=-0.313) (MEAN/SD = 9.84/2.19)
pc_zinc <- ((2*(pnorm((full$ZINC-9.84)/2.19)))-1)
DII_zinc <-(pc_zinc * (-0.313)) 

##16 MUFA (OIS=-0.009) (MEAN/SD = 27/6.1)
pc_mufa <- ((2*(pnorm((full$MFAT-27)/6.1)))-1)
DII_mufa <-(pc_mufa * (-0.009)) 

##17 Carbohydrates (OIS=0.097) (MEAN/SD = 272.2/40.0)
pc_cho <- ((2*(pnorm((full$CARB-272.2)/40)))-1)
DII_cho <-(pc_cho * 0.097) 

##18 Cholesterol (OIS=0.110) (MEAN/SD = 279.4/51.2)
pc_chol <- ((2*(pnorm((full$CHOLE-279.4)/51.2)))-1)
DII_chol <-(pc_chol * 0.110) 

##19 Energy (OIS=0.180) (MEAN/SD = 2056/338)
pc_ener <- ((2*(pnorm((full$KCAL-2056)/338)))-1)
DII_ener <-(pc_ener * 0.180) 

##20 Total Fat (OIS=0.298) (MEAN/SD = 71.4/19.4)
pc_fat <- ((2*(pnorm((full$TFAT-71.4)/19.4)))-1)
DII_fat <-(pc_fat * 0.298) 

##21 Fiber (OIS=-0.663) (MEAN/SD = 18.8/4.9)
pc_fib <- ((2*(pnorm((full$FIBE-18.8)/4.9)))-1)
DII_fib <-(pc_fib * (-0.663)) 

##22 Omega 3 FA (OIS=-0.436) (MEAN/SD = 1.06/1.06)
full$n3 <- full$P183 + full$P184 + full$P204 + full$P205 + full$P225 + full$P226
pc_om3 <- ((2*(pnorm((full$n3-1.06)/1.06)))-1)
DII_om3 <-(pc_om3 * -0.436) 

##23 Omega 6 FA (OIS=-0.159) (MEAN/SD = 10.80/7.50) #NO OMEGA 6 FOR HYPHY 
#pc_om6 <- ((2*(pnorm((m_om6-10.80)/7.50)))-1)
#DII_om6 <-(pc_om6 * (-0.159)) 

##23 Protein (OIS=0.021) (MEAN/SD = 79.4/13.9)
pc_pro <- ((2*(pnorm((full$PROT-79.4)/13.9)))-1)
DII_pro <-(pc_pro * (0.021))

##24 PUFA (OIS=-0.337) (MEAN/SD = 13.88/3.76)
pc_pufa <- ((2*(pnorm((full$PFAT-13.88)/3.76)))-1)
DII_pufa <-(pc_pufa * (-0.337)) 

##25 SFA (OIS=0.373) (MEAN/SD = 28.6/8.0)
pc_sfa <- ((2*(pnorm((full$SFAT-28.6)/8.0)))-1)
DII_sfa <-(pc_sfa * 0.373) 

##26 Alcohol (OIS=-0.278) (MEAN/SD = 13.98/3.72)
pc_Alcohol <- ((2*(pnorm((full$ALC-13.98)/3.72)))-1)
DII_Alcohol <-(pc_Alcohol * (-0.278)) 

##27 Caffeine (OIS=-0.110) (MEAN/SD = 8.05/6.67)
pc_caffeine <- ((2*(pnorm(((full$CAFF*.001)-8.05)/6.67)))-1)
DII_caffeine <-(pc_caffeine * (-0.110)) 

## TOTAL SCORE 
full$DII <- (DII_vit12 + DII_vitB6 + DII_betaCarotene + 
                          DII_folate + DII_iron + DII_magnesium +
                          DII_niacin + DII_riboflavin + DII_selenium + 
                          DII_thiamin + DII_vitA + DII_vitC + DII_vitD + 
                          DII_vitE_mg + DII_zinc + DII_mufa + DII_cho + 
                          DII_chol + DII_ener + DII_fat + DII_fib + 
                          DII_om3 + + DII_pro + DII_pufa + 
                          DII_sfa + DII_Alcohol + DII_caffeine)

#DASH SCORE
###############################################
full$SFA_kcals <- full$SFAT*9
full$FAT_kcals <- full$TFAT*9
full$PRO_kcals <- full$PROT*4
full$kcal1000_scale <- full$KCAL/1000

full$SFA_dash <- full$SFA_kcals/full$KCAL
full$FAT_dash <- full$FAT_kcals/full$KCAL
full$PRO_dash <- full$PRO_kcals/full$KCAL
full$Chol_dash <- full$CHOLE/full$kcal1000_scale
full$Fib_dash <- full$FIBE/full$kcal1000_scale
full$Mg_dash <- full$MAGN/full$kcal1000_scale
full$Ca_dash <- full$CALC/full$kcal1000_scale
full$K_dash <- full$POTA/full$kcal1000_scale
full$Na_dash <- full$SODI/full$kcal1000_scale

full$SFA_dash_score <- ifelse(full$SFA_dash <= 0.06, 1, ifelse(full$SFA_dash <= 0.11, 0.5, 0))
full$FAT_dash_score <- ifelse(full$FAT_dash <= 0.27, 1, ifelse(full$FAT_dash <= 0.32, 0.5, 0))
full$PRO_dash_score <- ifelse(full$PRO_dash >= 0.18, 1, ifelse(full$PRO_dash >= 0.165, 0.5, 0))
full$Chol_dash_score <- ifelse(full$Chol_dash <= 71.4, 1, ifelse(full$Chol_dash <= 107.1, 0.5, 0))
full$Fib_dash_score <- ifelse(full$Fib_dash >= 14.8, 1, ifelse(full$Fib_dash >= 9.5, 0.5, 0))
full$Mg_dash_score <- ifelse(full$Mg_dash >= 238, 1, ifelse(full$Mg_dash >= 158, 0.5, 0))
full$Ca_dash_score <- ifelse(full$Ca_dash >= 590, 1, ifelse(full$Ca_dash >= 402, 0.5, 0))
full$K_dash_score <- ifelse(full$K_dash >= 2238, 1, ifelse(full$K_dash >= 1534, 0.5, 0))
full$Na_dash_score <- ifelse(full$Na_dash <= 1143, 1, ifelse(full$Na_dash <= 1286, 0.5, 0))

full$DASH <- full$SFA_dash_score + full$FAT_dash_score + full$PRO_dash_score + full$Chol_dash_score + 
  full$Fib_dash_score + full$Mg_dash_score + full$Ca_dash_score + full$K_dash_score + full$Na_dash_score

#participants with bmi AND diet
paired <- full %>% filter(is.na(bmi_calc) == FALSE & is.na(RecallNo) == FALSE) #n=60 (participants with bmi AND diet)
paired <- paired %>% select(c(1:15), MDS, DII, DASH)

# BMI CLASS 
#Normal 
paired$BMIclass[paired$bmi_calc  < 25] = "Normal"
#Overweight
paired$BMIclass[paired$bmi_calc  >= 25 & paired$bmi_calc  < 30] = "Overweight"
#Obese
paired$BMIclass[paired$bmi_calc  >= 30] = "Obese"
paired$BMIclass <- ordered(paired$BMIclass, levels=c('Normal', 'Overweight', 'Obese'))	

#DII CATEGORIES 
paired$DIIcategory [paired$DII  < 0] = "Anti-Inflammatory" 
paired$DIIcategory [paired$DII  >=0 ] = "Pro-Inflammatory"
paired$DIIcategory  <- ordered(paired$DIIcategory , levels=c('Pro-Inflammatory', 'Anti-Inflammatory'))

#MDS CATEGORIES
paired$MDS3category [paired$MDS  <=3] = 'Low'
paired$MDS3category [paired$MDS  ==4 | paired$MDS == 5] = 'Medium'
paired$MDS3category [paired$MDS  >=6] = 'High'
paired$MDS3category  <- ordered(paired$MDS3category , levels=c('Low', 'Medium', 'High'))	

paired$MDS2category [paired$MDS  <=4] = "Low" #'0-4'
paired$MDS2category [paired$MDS  >=5] = "High" #'5-9'
paired$MDS2category  <- ordered(paired$MDS2category , levels=c('Low', 'High'))	

paired$project <- "HYPHY"

paired <- rename(paired, White = race_v2___1)
paired <- rename(paired, Black = race_v2___2)
paired <- rename(paired, Asian = race_v2___3)
paired <- rename(paired, NHPI = race_v2___4)
paired <- rename(paired, AIAN = race_v2___5)
paired <- rename(paired, OtherRace = race_v2___6)

#Physical activity varaibles
IPAQ <- rename(IPAQ, VigDays = vigorous_v2, VigHours = hours_vigorous_v2, VigMin = mins_vigorous_v2,
               ModDays = moderate_v2, ModHours = hours_moderate_v2, ModMin = mins_moderate_v2, 
               WalkDays = walking_v2, WalkHours = hours_walking_v2, WalkMin = mins_walking_v2, 
               SitHours = hours_sitting_v2, SitMin = mins_sitting_v2)

IPAQ <- IPAQ %>% select(-international_physical_activity_short_form_questio_timestamp)
IPAQ <- IPAQ %>% filter(IPAQ$record_id %in% paired$record_id )

IPAQ$totalVigMin <- IPAQ$VigMin + IPAQ$VigHours*60

for(i in 1: nrow(IPAQ))
{
  if(is.na(IPAQ$VigMin[i]) == FALSE & is.na(IPAQ$VigHours[i]) == FALSE)
  {
    IPAQ$totalVigMin[i] <- IPAQ$VigMin[i] + IPAQ$VigHours[i]*60
  }
  if(is.na(IPAQ$VigMin[i]) == TRUE  & is.na(IPAQ$VigHours[i]) == FALSE)
  {
    IPAQ$totalVigMin[i] <- IPAQ$VigHours[i]*60
  }
  if(is.na(IPAQ$VigMin[i]) == FALSE & is.na(IPAQ$VigHours[i]) == TRUE)
  {
    IPAQ$totalVigMin[i] <- IPAQ$VigMin[i]
  }
  if(IPAQ$VigDays[i] == 0)
  {
    IPAQ$totalVigMin[i] <- 0
  }
}

for(i in 1: nrow(IPAQ))
{
  if(is.na(IPAQ$ModMin[i]) == FALSE & is.na(IPAQ$ModHours[i]) == FALSE)
  {
    IPAQ$totalModMin[i] <- IPAQ$ModMin[i] + IPAQ$ModHours[i]*60
  }
  if(is.na(IPAQ$ModMin[i]) == TRUE  & is.na(IPAQ$ModHours[i]) == FALSE)
  {
    IPAQ$totalModMin[i] <- IPAQ$ModHours[i]*60
  }
  if(is.na(IPAQ$ModMin[i]) == FALSE & is.na(IPAQ$ModHours[i]) == TRUE)
  {
    IPAQ$totalModMin[i] <- IPAQ$ModMin[i]
  }
  if(IPAQ$ModDays[i] == 0)
  {
    IPAQ$totalModMin[i] <- 0
  }
}

for(i in 1: nrow(IPAQ))
{
  if(is.na(IPAQ$WalkMin[i]) == FALSE & is.na(IPAQ$WalkHours[i]) == FALSE)
  {
    IPAQ$totalWalkMin[i] <- IPAQ$WalkMin[i] + IPAQ$WalkHours[i]*60
  }
  if(is.na(IPAQ$WalkMin[i]) == TRUE  & is.na(IPAQ$WalkHours[i]) == FALSE)
  {
    IPAQ$totalWalkMin[i] <- IPAQ$WalkHours[i]*60
  }
  if(is.na(IPAQ$WalkMin[i]) == FALSE & is.na(IPAQ$WalkHours[i]) == TRUE)
  {
    IPAQ$totalWalkMin[i] <- IPAQ$WalkMin[i]
  }
  if(IPAQ$WalkDays[i] == 0)
  {
    IPAQ$totalWalkMin[i] <- 0
  }
}


IPAQ$WalkMET <- 3.3*IPAQ$totalWalkMin * IPAQ$WalkDays
IPAQ$ModMET <- 4.0*IPAQ$totalModMin * IPAQ$ModDays
IPAQ$VigMET <- 8.0*IPAQ$totalVigMin * IPAQ$VigDays

IPAQ$MET_score <- IPAQ$WalkMET + IPAQ$ModMET + IPAQ$VigMET

IPAQ[is.na(IPAQ)] <- 0
IPAQ$totalHour <- (IPAQ$totalVigMin + IPAQ$totalModMin + IPAQ$totalWalkMin)/60
IPAQ$IPAQ_outlier <- if_else(IPAQ$totalHour >= 10, "yes","no")
IPAQ$IPAQ_over <- if_else(IPAQ$MET_score > 17000, "yes","no") #removes 5 outliers 
IPAQ$MET_adj <- if_else(IPAQ$MET_score > 17000 , IPAQ$MET_score %>% median(), IPAQ$MET_score) #outliers removed, median replacement 


IPAQ$HEPAcat <- 1
for(i in 1: nrow(IPAQ))
{
  if( (IPAQ$VigDays[i] >= 3 & IPAQ$MET_score[i] >= 1500) | ((IPAQ$VigDays[i] + IPAQ$WalkDays[i] + IPAQ$ModDays[i]) > 7) & IPAQ$MET_score[i] >= 3000 )
  {
    IPAQ$HEPAcat[i] <- 3
  }
}

for(i in 1: nrow(IPAQ))
{
  if(IPAQ$HEPAcat[i] != 3)
  {
    if((IPAQ$VigDays[i] >3 & IPAQ$totalVigMin[i] > 20) | (IPAQ$ModDays[i] >5 & IPAQ$totalModMin[i] > 30) | (IPAQ$VigDays[i] + IPAQ$WalkDays[i] + IPAQ$ModDays[i]) > 5 & IPAQ$MET_score[i] >= 600 )
    {
      IPAQ$HEPAcat[i] <- 2
    }
  }
}
paired <- merge(paired,IPAQ)

#SES - Hollingshead Index 
    #DO LATER

#COVID SURVEY
questions <- questions %>% filter(questions$record_id %in% paired$record_id )
paired <- merge(paired,questions)

paired$self_pa_v2_n <- ifelse(paired$self_pa_v2 == 'A',"10",paired$self_pa_v2)
paired$self_pa_v2_n <- paired$self_pa_v2_n %>% as.numeric()
paired$self_pa_v2_n <- (paired$self_pa_v2_n -1) * 10


hip<- hipAvg %>% filter(hipAvg$record_id %in% paired$record_id)
paired$hip_avg <- hip$hip_avg
paired$WHR <- paired$waist_avg/paired$hip_avg
write.table(paired,paste0(meta3,"/hMetaData20Sep2021.txt",sep=""),quote=FALSE, sep="\t",append=FALSE, col.names=TRUE, row.names=FALSE)
