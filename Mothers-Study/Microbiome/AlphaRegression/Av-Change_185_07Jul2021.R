# Clear out working space
rm(list=ls())
library(tidyverse)
library(withr)
library(formattable)
library(huxtable)
library(tinytex)
library(mudata2)
# Set working directories
meta <- "/Volumes/ADORLab/Master Mothers Milk Data Folder/Input/"
meta2 <- "/Volumes/ADORLab/__Users/ditr2320/datasets/"
meta3 <- "/Volumes/ADORLab/HEI Study/16S Data/MM 16S Processed Files for Mom and Infants 05Jan21/alpha diversity"
meta4 <- "/Users/DidiTrifonova/Documents/GitHub/Git_ADOR/Mothers-Milk-Diet-Score-and-BMI"
meta5 <- "/Volumes/ADORLab/HEI Study/16S Data/MM 16S Processed Files for Mom and Infants 05Jan21/taxonomy/"

# Read in data new metadata (UPDATED)
#dataSet <- read.delim(paste0(meta2,"dataset01Apr2021.txt"),na = c("NA", "don't know", "","    .","Missing: Not provided",999.00))
dataSet <- read.delim(paste0(meta2,"longData10May2021.txt"), header=TRUE)
taxaTable <- read.delim(paste0(meta5,"mm_150bp_deblur_sepp_noMit_noChl_rmsingletons_level2.txt",sep=""),header=TRUE,row.names=1,check.names = FALSE)

# Grabbing just the infnat gut microbiota at baseline
taxaTableMom6 <- taxaTable[,grepl("Mom.6",colnames(taxaTable))] #either chance this line or create 2d loop
myT <- t(taxaTableMom6)
totalReads <- colSums(myT)
# Normalzing sequences at each taxanomic level
myTLogged <- log10((myT/(rowSums(myT) + 1)) * (sum(rowSums(myT))/nrow(myT)) + 1)
# Filtering normalized sequences at each taxanomic level
#totalReadsFiltered <- totalReads[totalReads >= 10]
#myT3 <- myTLogged[ ,colnames(myTLogged) %in% names(totalReadsFiltered)]
myT3 <- myTLogged 
# Filtering normalized sequences (that meet some threshold of presence across samples)
myT4 <- myT3[ ,(colSums(myT3 == 0) / nrow(myT3)) <= 0.9] # Removing bacteria that are observed in <= 10% of samples
myT5 <- as.data.frame(myT4)
# Extracting ID number using character positions (e.g., 10 to 13)
myT5$dyad_id <- substr(row.names(myT5),10,13) %>%  as.numeric()
# Grabbing just the infnat gut microbiota at baseline
taxaTableMom1 <- taxaTable[,grepl("Mom.Bl",colnames(taxaTable))] #either chance this line or create 2d loop
myS <- t(taxaTableMom1)
totalReads <- colSums(myS)
# Normalzing sequences at each taxanomic level
mySLogged <- log10((myS/(rowSums(myS) + 1)) * (sum(rowSums(myS))/nrow(myS)) + 1)
# Filtering normalized sequences at each taxanomic level
#totalReadsFiltered <- totalReads[totalReads >= 10]
#myT3 <- myTLogged[ ,colnames(myTLogged) %in% names(totalReadsFiltered)]
myS3 <- mySLogged 
# Filtering normalized sequences (that meet some threshold of presence across samples)
myS4 <- myS3[ ,(colSums(myS3 == 0) / nrow(myS3)) <= 0.9] # Removing bacteria that are observed in <= 10% of samples
myS5 <- as.data.frame(myS4)
# Extracting ID number using character positions (e.g., 10 to 13)
myS5$dyad_id <- substr(row.names(myS5),10,13) %>% as.numeric()

# Create susbets of data based on timepoint (visit number)
dataSet1m <- dataSet[dataSet$timepoint %in% "1",]
dataSet6m <- dataSet[dataSet$timepoint %in% "6",]

dataSet6m <- left_join(dataSet6m,myT5, by = "dyad_id")
dataSet6m$FBratio <- dataSet6m$`k__Bacteria;p__Firmicutes`/dataSet6m$`k__Bacteria;p__Bacteroidetes`

dataSet1m <- left_join(dataSet1m,myS5, by = "dyad_id")
dataSet1m$FBratio <- dataSet1m$`k__Bacteria;p__Firmicutes`/dataSet1m$`k__Bacteria;p__Bacteroidetes`

dataSetAv <- dataSet1m %>% select(c(1:3), SES_r)
dataSetAv$timepoint <- "average"
dataSetAv$mother_age <- (dataSet6m$mother_age + dataSet1m$mother_age)/2
dataSetAv$DII_Mom <- (dataSet6m$DII_Mom + dataSet1m$DII_Mom)/2 
dataSetAv$MDS_Mom <- (dataSet6m$MDS_Mom + dataSet1m$MDS_Mom)/2 
dataSetAv$HEI2015_TOTAL_SCORE_Mom <- (dataSet6m$HEI2015_TOTAL_SCORE_Mom + dataSet1m$HEI2015_TOTAL_SCORE_Mom)/2 
dataSetAv$exercise_r <- (dataSet6m$exercise_r + dataSet1m$exercise_r)/2 
dataSetAv$mom_BMI <- (dataSet6m$mom_BMI + dataSet1m$mom_BMI)/2 
dataSetAv$m_ener_Mom <- (dataSet6m$m_ener_Mom + dataSet1m$m_ener_Mom)/2 

dataSetAv$FBratio_change <- dataSet6m$FBratio - dataSet1m$FBratio
dataSetAv$FBratio_change <- dataSetAv$FBratio_change %>%  na_if(-Inf)
 
metadata <- dataSetAv
metadata$BMI_change = (dataSet6m$mom_BMI - dataSet1m$mom_BMI)
metadata$shannon_change = (dataSet6m$shannon_entropy - dataSet1m$shannon_entropy)
metadata$richness_change = (dataSet6m$gut_richness - dataSet1m$gut_richness)
metadata$faith_change = (dataSet6m$faith_pd - dataSet1m$faith_pd)


#REGRESSION LOOP

#plot theme for graphs
mytheme <- theme(axis.line = element_line(size = 0.5, colour = "black"), panel.background = element_rect(fill = "white"))

#setting up exposure to loop through
ExposureFactors <- c("HEI2015_TOTAL_SCORE_Mom", "DII_Mom", "MDS_Mom") 

#setting up outcomes to loop through
MaternalFactors <- c("BMI_change","shannon_change","faith_change", "richness_change", "FBratio_change")
baselineFactors <- c("mom_BMI", "shannon_entropy","faith_pd","gut_richness", "FBratio")

#output plots from loop to pdf
#pdf(file = paste0(meta2,"test.pdf",sep=""))

exposureNamesList <- character(0)
pVal0ExposureList <- numeric(0)
beta0ExposureList <- numeric(0)
pVal1ExposureList <- numeric(0)
beta1ExposureList <- numeric(0)
pVal2ExposureList <- numeric(0)
beta2ExposureList <- numeric(0)
pVal3ExposureList <- numeric(0)
beta3ExposureList <- numeric(0)
pVal4ExposureList <- numeric(0)
beta4ExposureList <- numeric(0)
pVal5ExposureList <- numeric(0)
beta5ExposureList <- numeric(0)
Exposure0List <- numeric(0)
allExposures <- ExposureFactors

CI0ExposureLowerList <- numeric(0)
CI0ExposureUpperList <- numeric(0)
CI1ExposureLowerList <- numeric(0)
CI1ExposureUpperList <- numeric(0)
CI2ExposureLowerList <- numeric(0)
CI2ExposureUpperList <- numeric(0)
CI3ExposureLowerList <- numeric(0)
CI3ExposureUpperList <- numeric(0)
sampleSizeList <- numeric(0)


for(j in seq(1:length(MaternalFactors)))
{
  for(i in seq(1:length(ExposureFactors))) 
    
  {
    #create dateframe without NAs (include all variables to be adjusted for in model)
    thisDataInstance <- na.omit(data.frame(dyad_id = metadata$dyad_id,
                                           thisOutcome=metadata[,names(metadata) %in% MaternalFactors[j]],
                                           thisExposureFactor=metadata[,names(metadata) %in% ExposureFactors[i]], 
                                           baselineAdjust = dataSet1m[,names(dataSet1m) %in% baselineFactors[j]],
                                           mother_age_baseline = dataSet1m$mother_age,
                                           aBMI = metadata$mom_BMI,
                                           aKcal = metadata$m_ener_Mom,
                                           aExercise = metadata$exercise_r, 
                                           SES_index_final = metadata$SES_r))
  
    
    #linear model with adjustment factors. 
    if(MaternalFactors[j] == "BMI_change"){
      print("BMI")
      #0) Model 0 base model
      modelForm0=as.formula("thisOutcome ~ thisExposureFactor + mother_age_baseline + baselineAdjust")
      #1) Model 1 base model + Kcal
      modelForm1=as.formula("thisOutcome ~ thisExposureFactor + mother_age_baseline + baselineAdjust + aKcal")
      #2) Model 2 base model + Kcal + Physical Activity 
      modelForm2=as.formula("thisOutcome ~ thisExposureFactor + mother_age_baseline + baselineAdjust + aKcal + aExercise")
      #3) Model 3 base model + Kcal + Physical Activity + SES 
      modelForm3=as.formula("thisOutcome ~ thisExposureFactor + mother_age_baseline + baselineAdjust + aKcal + aExercise + SES_index_final")
      
    } else if(MaternalFactors[j] == "shannon_change" || MaternalFactors[j] == "faith_change" || MaternalFactors[j] == "richness_change")
      {
      print("diversity")
      #0) Model 0 base model
      modelForm0=as.formula("thisOutcome ~ thisExposureFactor + mother_age_baseline + aBMI + baselineAdjust")
      #1) Model 1 base model + Kcal
      modelForm1=as.formula("thisOutcome ~ thisExposureFactor + mother_age_baseline + aBMI + baselineAdjust + aKcal")
      #2) Model 2 base model + Kcal + Physical Activity 
      modelForm2=as.formula("thisOutcome ~ thisExposureFactor + mother_age_baseline + aBMI + baselineAdjust + aKcal + aExercise")
      #3) Model 3 base model + Kcal + Physical Activity + SES 
      modelForm3=as.formula("thisOutcome ~ thisExposureFactor + mother_age_baseline + aBMI + baselineAdjust + aKcal + aExercise + SES_index_final")
      
    }else if((MaternalFactors[j] == "FBratio_change"))
      {
      print("other")
      #0) Model 0 base model
      modelForm0=as.formula("thisOutcome ~ thisExposureFactor + mother_age_baseline + aBMI")
      #1) Model 1 base model + Kcal
      modelForm1=as.formula("thisOutcome ~ thisExposureFactor + mother_age_baseline + aBMI + aKcal")
      #2) Model 2 base model + Kcal + Physical Activity 
      modelForm2=as.formula("thisOutcome ~ thisExposureFactor + mother_age_baseline + aBMI + aKcal + aExercise")
      #3) Model 3 base model + Kcal + Physical Activity + SES 
      modelForm3=as.formula("thisOutcome ~ thisExposureFactor + mother_age_baseline + aBMI + aKcal + aExercise + SES_index_final")
    }
    print(modelForm3)
    
    #Save Beta and Pvalue and put into a running list
    statMod0 <- lm(modelForm0,data=thisDataInstance)
    beta0Exposure <- format(summary(statMod0)$coefficients[2,1], digits = 3) 
    pVal0Exposure <- format(summary(statMod0)$coefficients[2,4], digits = 3) 
    beta0ExposureList[[length(beta0ExposureList)+1]] <- beta0Exposure
    pVal0ExposureList[[length(pVal0ExposureList)+1]] <- pVal0Exposure
    CI0ExposureLower <- format(confint(statMod0, level=0.95)[2,1], digits = 3) 
    CI0ExposureUpper <- format(confint(statMod0, level=0.95)[2,2], digits = 3) 
    CI0ExposureLowerList[[length(CI0ExposureLowerList)+1]] <- CI0ExposureLower 
    CI0ExposureUpperList[[length(CI0ExposureUpperList)+1]] <- CI0ExposureUpper
    Exposure0List[[length(Exposure0List)+1]] <- MaternalFactors[j]
    
    statMod1 <- lm(modelForm1,data=thisDataInstance)
    beta1Exposure <- format(summary(statMod1)$coefficients[2,1], digits = 3) 
    pVal1Exposure <- format(summary(statMod1)$coefficients[2,4], digits = 3) 
    beta1ExposureList[[length(beta1ExposureList)+1]] <- beta1Exposure
    pVal1ExposureList[[length(pVal1ExposureList)+1]] <- pVal1Exposure
    CI1ExposureLower <- format(confint(statMod1, level=0.95)[2,1], digits = 3) 
    CI1ExposureUpper <- format(confint(statMod1, level=0.95)[2,2], digits = 3) 
    CI1ExposureLowerList[[length(CI1ExposureLowerList)+1]] <- CI1ExposureLower
    CI1ExposureUpperList[[length(CI1ExposureUpperList)+1]] <- CI1ExposureUpper
    
    statMod2 <- lm(modelForm2,data=thisDataInstance)
    beta2Exposure <- format(summary(statMod2)$coefficients[2,1], digits = 3) 
    pVal2Exposure <- format(summary(statMod2)$coefficients[2,4], digits = 3) 
    beta2ExposureList[[length(beta2ExposureList)+1]] <- beta2Exposure
    pVal2ExposureList[[length(pVal2ExposureList)+1]] <- pVal2Exposure
    CI2ExposureLower <- format(confint(statMod2, level=0.95)[2,1], digits = 3) 
    CI2ExposureUpper <- format(confint(statMod2, level=0.95)[2,2], digits = 3) 
    CI2ExposureLowerList[[length(CI2ExposureLowerList)+1]] <- CI2ExposureLower
    CI2ExposureUpperList[[length(CI2ExposureUpperList)+1]] <- CI2ExposureUpper
    
    statMod3 <- lm(modelForm3,data=thisDataInstance)
    beta3Exposure <- format(summary(statMod3)$coefficients[2,1], digits = 3) 
    pVal3Exposure <- format(summary(statMod3)$coefficients[2,4], digits = 3) 
    beta3ExposureList[[length(beta3ExposureList)+1]] <- beta3Exposure
    pVal3ExposureList[[length(pVal3ExposureList)+1]] <- pVal3Exposure
    CI3ExposureLower <- format(confint(statMod3, level=0.95)[2,1], digits = 3) 
    CI3ExposureUpper <- format(confint(statMod3, level=0.95)[2,2], digits = 3) 
    CI3ExposureLowerList[[length(CI3ExposureLowerList)+1]] <- CI3ExposureLower
    CI3ExposureUpperList[[length(CI3ExposureUpperList)+1]] <- CI3ExposureUpper
    sampleSizeVal <- nrow(thisDataInstance)
    sampleSizeList[[length(sampleSizeList)+1]] <- sampleSizeVal
    
  }
}
    
LMavg <- cbind(allExposures, 
               Exposure0List,
               beta0ExposureList,pVal0ExposureList,CI0ExposureLowerList,CI0ExposureUpperList, 
               beta1ExposureList,pVal1ExposureList,CI1ExposureLowerList,CI1ExposureUpperList, 
               beta2ExposureList,pVal2ExposureList,CI2ExposureLowerList,CI2ExposureUpperList, 
               beta3ExposureList,pVal3ExposureList,CI3ExposureLowerList,CI3ExposureUpperList, sampleSizeList) 

colnames(LMavg) <- c("Dietary Index", 
                     "Outcome",
                     "Beta Mod0","pvalue Mod0","Lower_CI_Mod0","Upper_CI_Mod0",
                     "Beta Mod1","pvalue Mod1","Lower_CI_Mod1","Upper_CI_Mod1",
                     "Beta Mod2","pvalue Mod2","Lower_CI_Mod2","Upper_CI_Mod2",
                     "Beta Mod3","pvalue_Mod3","Lower_CI_Mod3","Upper_CI_Mod3", "sampleSize")
LMavg <- LMavg %>% as_tibble()

trimmedTable <- LMavg[,c(1,2,15,16,19)] %>% as_tibble()
trimmedTable <- trimmedTable %>% rename("Average Index" = "Dietary Index","Beta" = "Beta Mod3", "P-value" = "pvalue_Mod3", "n" = "sampleSize")
trimmedTable <- trimmedTable[order(trimmedTable$'P-value',decreasing=FALSE),]
data1 <- trimmedTable
data1[data1 == "DII_Mom"] <- "DII"
data1[data1 == "HEI2015_TOTAL_SCORE_Mom"] <- "HEI"
data1[data1 == "MDS_Mom"] <- "MDS"
data1
hux <- data1 %>% as_huxtable() %>% set_all_borders(1) %>% set_bold(1, everywhere, TRUE) %>% map_text_color(by_values("0.00345" = "red","0.0185" = "red","0.0292" = "red"))
#quick_pdf(hux)





