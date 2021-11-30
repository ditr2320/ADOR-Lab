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

# Read in data new metadata (UPDATED)
#dataSet <- read.delim(paste0(meta2,"dataset01Apr2021.txt"),na = c("NA", "don't know", "","    .","Missing: Not provided",999.00))
dataSet <- read.delim(paste0(meta2,"longData03Jun2021.txt"), header=TRUE)

# Create susbets of data based on timepoint (visit number)
data <- dataSet[dataSet$timepoint %in% "1",]
data %>% summarise_all((funs(sum(is.na(.))))) 
data <- data %>% select(-SES_index_final,-exercise)
metadata <- na.omit(data)

#REGRESSION LOOP
#plot theme for graphs
mytheme <- theme(axis.line = element_line(size = 0.5, colour = "black"), panel.background = element_rect(fill = "white"))

#setting up exposure to loop through
ExposureFactors <- c("HEI2015_TOTAL_SCORE_Mom", "DII_Mom", "MDS_Mom") 
ExposureNames <- c("HEI at 1-Month", "DII at 1-Month", "MDS at 1-Month")

#setting up outcomes to loop through
MaternalFactors <- c("shannon_entropy", "faith_pd", "gut_richness") 
MaternalNames <- c("BMI at 1-Month", "Shannon at 1-Month", "Faith at 1-Month", "Richness at 1-Month")

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

for(j in seq(1:length(MaternalFactors)))
{
  
  for(i in seq(1:length(ExposureFactors))) 
    
  {
    
    #create dateframe without NAs (include all variables to be adjusted for in model)
    thisDataInstance <- na.omit(data.frame(thisOutcome=metadata[,names(metadata) %in% MaternalFactors[j]],
                                           thisExposureFactor=metadata[,names(metadata) %in% ExposureFactors[i]],
                                           metadata$mother_age,
                                           metadata$m_ener_Mom,
                                           metadata$SES_r,
                                           metadata$exercise))
    
    #linear model for plot
    modelFormForPlot <- as.formula(paste(MaternalFactors[j],"~",ExposureFactors[i]))
    statModForPlot <- lm(modelFormForPlot, data=metadata)
    
    #linear model with adjustment factors. 
    if(MaternalFactors[j] == "mom_BMI")
    {
      #0) Model 0 base model
      modelForm0=as.formula(paste(MaternalFactors[j],"~",ExposureFactors[i],"+",
                                  "mother_age"))
      #1) Model 1 base model + Kcal
      modelForm1=as.formula(paste(MaternalFactors[j],"~",ExposureFactors[i],"+",
                                  "mother_age","+","m_ener_Mom"))
      #2) Model 2 base model + Kcal + Physical Activity 
      modelForm2=as.formula(paste(MaternalFactors[j],"~",ExposureFactors[i],"+",
                                  "mother_age","+","m_ener_Mom",
                                  "+","exercise_r"))
      #3) Model 3 base model + Kcal + Physical Activity + SES 
      modelForm3=as.formula(paste(MaternalFactors[j],"~",ExposureFactors[i],"+",
                                  "mother_age","+","m_ener_Mom",
                                  "+","exercise_r","+","SES_r"))
    }
    else
    {
      #0) Model 0 base model
      modelForm0=as.formula(paste(MaternalFactors[j],"~",ExposureFactors[i],"+",
                                  "mother_age","+","mom_BMI"))
      #1) Model 1 base model + Kcal
      modelForm1=as.formula(paste(MaternalFactors[j],"~",ExposureFactors[i],"+",
                                  "mother_age","+","mom_BMI","+",
                                  "m_ener_Mom"))
      #2) Model 2 base model + Kcal + Physical Activity 
      modelForm2=as.formula(paste(MaternalFactors[j],"~",ExposureFactors[i],"+",
                                  "mother_age","+","mom_BMI","+",
                                  "m_ener_Mom","+","exercise_r"))
      #3) Model 3 base model + Kcal + Physical Activity + SES 
      modelForm3=as.formula(paste(MaternalFactors[j],"~",ExposureFactors[i],"+",
                                  "mother_age","+","mom_BMI","+",
                                  "m_ener_Mom","+","exercise_r","+","SES_r"))
    }
    #Save Beta and Pvalue and put into a running list
    statMod0 <- lm(modelForm0,data=metadata)
    beta0Exposure <- format(summary(statMod0)$coefficients[2,1], digits = 3) 
    pVal0Exposure <- format(summary(statMod0)$coefficients[2,4], digits = 3) 
    beta0ExposureList[[length(beta0ExposureList)+1]] <- beta0Exposure
    pVal0ExposureList[[length(pVal0ExposureList)+1]] <- pVal0Exposure
    CI0ExposureLower <- format(confint(statMod0, level=0.95)[2,1], digits = 3) 
    CI0ExposureUpper <- format(confint(statMod0, level=0.95)[2,2], digits = 3) 
    CI0ExposureLowerList[[length(CI0ExposureLowerList)+1]] <- CI0ExposureLower 
    CI0ExposureUpperList[[length(CI0ExposureUpperList)+1]] <- CI0ExposureUpper
    Exposure0List[[length(Exposure0List)+1]] <- MaternalFactors[j]
    
    statMod1 <- lm(modelForm1,data=metadata)
    beta1Exposure <- format(summary(statMod1)$coefficients[2,1], digits = 3) 
    pVal1Exposure <- format(summary(statMod1)$coefficients[2,4], digits = 3) 
    beta1ExposureList[[length(beta1ExposureList)+1]] <- beta1Exposure
    pVal1ExposureList[[length(pVal1ExposureList)+1]] <- pVal1Exposure
    CI1ExposureLower <- format(confint(statMod1, level=0.95)[2,1], digits = 3) 
    CI1ExposureUpper <- format(confint(statMod1, level=0.95)[2,2], digits = 3) 
    CI1ExposureLowerList[[length(CI1ExposureLowerList)+1]] <- CI1ExposureLower
    CI1ExposureUpperList[[length(CI1ExposureUpperList)+1]] <- CI1ExposureUpper
    
    statMod2 <- lm(modelForm2,data=metadata)
    beta2Exposure <- format(summary(statMod2)$coefficients[2,1], digits = 3) 
    pVal2Exposure <- format(summary(statMod2)$coefficients[2,4], digits = 3) 
    beta2ExposureList[[length(beta2ExposureList)+1]] <- beta2Exposure
    pVal2ExposureList[[length(pVal2ExposureList)+1]] <- pVal2Exposure
    CI2ExposureLower <- format(confint(statMod2, level=0.95)[2,1], digits = 3) 
    CI2ExposureUpper <- format(confint(statMod2, level=0.95)[2,2], digits = 3) 
    CI2ExposureLowerList[[length(CI2ExposureLowerList)+1]] <- CI2ExposureLower
    CI2ExposureUpperList[[length(CI2ExposureUpperList)+1]] <- CI2ExposureUpper
    
    statMod3 <- lm(modelForm3,data=metadata)
    beta3Exposure <- format(summary(statMod3)$coefficients[2,1], digits = 3) 
    pVal3Exposure <- format(summary(statMod3)$coefficients[2,4], digits = 3) 
    beta3ExposureList[[length(beta3ExposureList)+1]] <- beta3Exposure
    pVal3ExposureList[[length(pVal3ExposureList)+1]] <- pVal3Exposure
    CI3ExposureLower <- format(confint(statMod3, level=0.95)[2,1], digits = 3) 
    CI3ExposureUpper <- format(confint(statMod3, level=0.95)[2,2], digits = 3) 
    CI3ExposureLowerList[[length(CI3ExposureLowerList)+1]] <- CI3ExposureLower
    CI3ExposureUpperList[[length(CI3ExposureUpperList)+1]] <- CI3ExposureUpper
    
    
    #Currently plots with show beta and p value for statMod3, modify as needed
    if(summary(statMod3)$coefficients[2,4] < 0.05) 
    {
      title <- paste("beta = ",
                     format(beta3Exposure,digits=3),
                     ", p-value = ",
                     format(pVal3Exposure,digits=3), 
                     ", n = ",nrow(thisDataInstance), sep="")
      
      print(qplot(thisExposureFactor,thisOutcome, data = thisDataInstance) +
              ggtitle(title) + labs(x=ExposureNames[i], y=MaternalNames[j]) + mytheme + geom_smooth(method='lm',color ="black"))
    }
  }
  
}

LM_baseline <- cbind(allExposures, 
                     Exposure0List,
                     beta0ExposureList,pVal0ExposureList,CI0ExposureLowerList,CI0ExposureUpperList, 
                     beta1ExposureList,pVal1ExposureList,CI1ExposureLowerList,CI1ExposureUpperList, 
                     beta2ExposureList,pVal2ExposureList,CI2ExposureLowerList,CI2ExposureUpperList, 
                     beta3ExposureList,pVal3ExposureList,CI3ExposureLowerList,CI3ExposureUpperList) 

colnames(LM_baseline) <- c("Dietary Index", 
                           "Outcome",
                           "Beta Mod0","pvalue Mod0","Lower_CI_Mod0","Upper_CI_Mod0",
                           "Beta Mod1","pvalue Mod1","Lower_CI_Mod1","Upper_CI_Mod1",
                           "Beta Mod2","pvalue Mod2","Lower_CI_Mod2","Upper_CI_Mod2",
                           "Beta Mod3","pvalue_Mod3","Lower_CI_Mod3","Upper_CI_Mod3")

#write.table(LM_baseline,paste0(meta2,"188-129baseline.txt",sep=""),quote=FALSE, sep="\t",append=FALSE, col.names=TRUE, row.names=FALSE)