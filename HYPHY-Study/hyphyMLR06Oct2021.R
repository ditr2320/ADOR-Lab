#importing data
#######################################
rm(list = ls())

## Packages 
library(tidyverse)
library(data.table)
library(withr)
library(huxtable)
library(dplyr)

meta <- "/Volumes/ADORLab/Master Mothers Milk Data Folder/Input/"
meta1 <- "/Volumes/ADORLab/Current Lab Projects/Hyphy/IRB COVID procedures/ASA24/20210913_HYPHYCOV_analysis_files/"
meta2 <- "/Volumes/ADORLab/__Users/ditr2320/datasets/"
meta3 <- "/Volumes/ADORLab/__Users/ditr2320/hyphyData/"
meta4 <- "/Users/DidiTrifonova/Documents/GitHub/Git_ADOR/Mothers-Milk-Diet-Score-and-BMI/"

#reading in data 
data <- read.delim(paste0(meta3,"hMetaData20Sep2021.txt"), header=TRUE) #n=60 - participants with BMI and Diet 
metadata <- data

#REGRESSION LOOP
#plot theme for graphs
mytheme <- theme(axis.line = element_line(size = 0.5, colour = "black"), panel.background = element_rect(fill = "white"))

#setting up exposure to loop through
ExposureFactors <- c("DII","MDS","DASH") 
ExposureNames <- c("DII","MDS","DASH") 

#setting up outcomes to loop through
outcomes <- c("bmi_calc","WHR") 
outcomeNames <- c("BMI","WHR")

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
CI4ExposureLowerList <- numeric(0)
CI4ExposureUpperList <- numeric(0)
CI5ExposureLowerList <- numeric(0)
CI5ExposureUpperList <- numeric(0)
sampleSizeList <- numeric(0)

for(j in seq(1:length(outcomes))) 
{
  
  for(i in seq(1:length(ExposureFactors))) 
    
  {
    
    #create dateframe without NAs (include all variables to be adjusted for in model)
    thisDataInstance <- na.omit(data.frame(thisOutcome = metadata[,names(metadata) %in% outcomes[j]],
                                           thisExposureFactor = metadata[,names(metadata) %in% ExposureFactors[i]],
                                           age = metadata$age_calc_v2,
                                           energy = metadata$KCAL,
                                           race = metadata$White,
                                           sex = metadata$biosex_v2,
                                           MET = metadata$MET_score,
                                           SRPA = metadata$self_pa_v2_n))
  
    #linear model for plot
    modelFormForPlot <- as.formula(paste(outcomes[j],"~",ExposureFactors[i]))
    statModForPlot <- lm(modelFormForPlot, data=metadata)
    
    #linear model with adjustment factors. 
   
      #0) Model 0 base model
      modelForm0=as.formula("thisOutcome ~ thisExposureFactor")
      #1) Model 1 base model
      modelForm1=as.formula("thisOutcome ~ thisExposureFactor + as.factor(sex)")
      #2) Model 2 base model
      modelForm2=as.formula("thisOutcome ~ thisExposureFactor + as.factor(sex) + age")
      #3) Model 3 base model
      modelForm3=as.formula("thisOutcome ~ thisExposureFactor + as.factor(sex) + age + as.factor(race)")
      #3) Model 4 base model
      modelForm4=as.formula("thisOutcome ~ thisExposureFactor + as.factor(sex) + age + as.factor(race) + MET")
      #3) Model 5 base model
      modelForm5=as.formula("thisOutcome ~ thisExposureFactor + as.factor(sex) + age + as.factor(race) + SRPA")

    
    print(modelForm4)
    #Save Beta and Pvalue and put into a running list
    statMod0 <- lm(modelForm0,data=thisDataInstance)
    beta0Exposure <- format(summary(statMod0)$coefficients[2,1] ) 
    pVal0Exposure <- format(summary(statMod0)$coefficients[2,4] ) 
    beta0ExposureList[[length(beta0ExposureList)+1]] <- beta0Exposure
    pVal0ExposureList[[length(pVal0ExposureList)+1]] <- pVal0Exposure
    CI0ExposureLower <- format(confint(statMod0, level=0.95)[2,1] ) 
    CI0ExposureUpper <- format(confint(statMod0, level=0.95)[2,2] ) 
    CI0ExposureLowerList[[length(CI0ExposureLowerList)+1]] <- CI0ExposureLower 
    CI0ExposureUpperList[[length(CI0ExposureUpperList)+1]] <- CI0ExposureUpper
    Exposure0List[[length(Exposure0List)+1]] <- outcomes[j]
    
    statMod1 <- lm(modelForm1,data=thisDataInstance)
    beta1Exposure <- format(summary(statMod1)$coefficients[2,1] ) 
    pVal1Exposure <- format(summary(statMod1)$coefficients[2,4] ) 
    beta1ExposureList[[length(beta1ExposureList)+1]] <- beta1Exposure
    pVal1ExposureList[[length(pVal1ExposureList)+1]] <- pVal1Exposure
    CI1ExposureLower <- format(confint(statMod1, level=0.95)[2,1] ) 
    CI1ExposureUpper <- format(confint(statMod1, level=0.95)[2,2], ) 
    CI1ExposureLowerList[[length(CI1ExposureLowerList)+1]] <- CI1ExposureLower
    CI1ExposureUpperList[[length(CI1ExposureUpperList)+1]] <- CI1ExposureUpper
    
    statMod2 <- lm(modelForm2,data=thisDataInstance)
    beta2Exposure <- format(summary(statMod2)$coefficients[2,1]) 
    pVal2Exposure <- format(summary(statMod2)$coefficients[2,4]) 
    beta2ExposureList[[length(beta2ExposureList)+1]] <- beta2Exposure
    pVal2ExposureList[[length(pVal2ExposureList)+1]] <- pVal2Exposure
    CI2ExposureLower <- format(confint(statMod2, level=0.95)[2,1]) 
    CI2ExposureUpper <- format(confint(statMod2, level=0.95)[2,2]) 
    CI2ExposureLowerList[[length(CI2ExposureLowerList)+1]] <- CI2ExposureLower
    CI2ExposureUpperList[[length(CI2ExposureUpperList)+1]] <- CI2ExposureUpper
    
    statMod3 <- lm(modelForm3,data=thisDataInstance)
    beta3Exposure <- format(summary(statMod3)$coefficients[2,1]) 
    pVal3Exposure <- format(summary(statMod3)$coefficients[2,4]) 
    beta3ExposureList[[length(beta3ExposureList)+1]] <- beta3Exposure
    pVal3ExposureList[[length(pVal3ExposureList)+1]] <- pVal3Exposure
    CI3ExposureLower <- format(confint(statMod3, level=0.95)[2,1]) 
    CI3ExposureUpper <- format(confint(statMod3, level=0.95)[2,2]) 
    CI3ExposureLowerList[[length(CI3ExposureLowerList)+1]] <- CI3ExposureLower
    CI3ExposureUpperList[[length(CI3ExposureUpperList)+1]] <- CI3ExposureUpper
    sampleSizeVal <- nrow(thisDataInstance)

    statMod4 <- lm(modelForm4,data=thisDataInstance)
    beta4Exposure <- format(summary(statMod4)$coefficients[2,1]) 
    pVal4Exposure <- format(summary(statMod4)$coefficients[2,4]) 
    beta4ExposureList[[length(beta4ExposureList)+1]] <- beta4Exposure
    pVal4ExposureList[[length(pVal4ExposureList)+1]] <- pVal4Exposure
    CI4ExposureLower <- format(confint(statMod4, level=0.95)[2,1]) 
    CI4ExposureUpper <- format(confint(statMod4, level=0.95)[2,2]) 
    CI4ExposureLowerList[[length(CI4ExposureLowerList)+1]] <- CI4ExposureLower
    CI4ExposureUpperList[[length(CI4ExposureUpperList)+1]] <- CI4ExposureUpper
    sampleSizeVal <- nrow(thisDataInstance)
    
    statMod5 <- lm(modelForm5,data=thisDataInstance)
    beta5Exposure <- format(summary(statMod5)$coefficients[2,1])
    pVal5Exposure <- format(summary(statMod5)$coefficients[2,4]) 
    beta5ExposureList[[length(beta5ExposureList)+1]] <- beta5Exposure
    pVal5ExposureList[[length(pVal5ExposureList)+1]] <- pVal5Exposure
    CI5ExposureLower <- format(confint(statMod5, level=0.95)[2,1]) 
    CI5ExposureUpper <- format(confint(statMod5, level=0.95)[2,2]) 
    CI5ExposureLowerList[[length(CI5ExposureLowerList)+1]] <- CI5ExposureLower
    CI5ExposureUpperList[[length(CI5ExposureUpperList)+1]] <- CI5ExposureUpper
    sampleSizeVal <- nrow(thisDataInstance)
    sampleSizeList[[length(sampleSizeList)+1]] <- sampleSizeVal
    
    beta4Exposure1 <- format(summary(statMod4)$coefficients[2,1], digits = 2) 
    pVal4Exposure1 <- format(summary(statMod4)$coefficients[2,4], digits = 2) 
  
    
    #Currently plots with show beta and p value for statMod4, modify as needed
    if(summary(statMod3)$coefficients[2,4] < 0.05) 
    {
      title <- paste("beta = ",
                     format(beta4Exposure1),
                     ", p-value = ",
                     format(pVal4Exposure1), 
                     ", n = ",nrow(thisDataInstance), sep="")
      
      print(qplot(thisExposureFactor,thisOutcome, data = thisDataInstance) +
              ggtitle(title) + labs(x=ExposureNames[i], y=outcomeNames[j]) + mytheme + geom_smooth(method='lm',color ="black"))
    }
  }
  
}

LM <- cbind(allExposures, 
                     Exposure0List,
                     beta0ExposureList,pVal0ExposureList,CI0ExposureLowerList,CI0ExposureUpperList, 
                     beta1ExposureList,pVal1ExposureList,CI1ExposureLowerList,CI1ExposureUpperList, 
                     beta2ExposureList,pVal2ExposureList,CI2ExposureLowerList,CI2ExposureUpperList, 
                     beta3ExposureList,pVal3ExposureList,CI3ExposureLowerList,CI3ExposureUpperList,
                     beta4ExposureList,pVal4ExposureList,CI4ExposureLowerList,CI4ExposureUpperList,
                     beta5ExposureList,pVal5ExposureList,CI5ExposureLowerList,CI5ExposureUpperList,sampleSizeList) 

colnames(LM) <- c("Dietary Index", 
                           "Outcome",
                           "Beta Mod0","pvalue Mod0","Lower_CI_Mod0","Upper_CI_Mod0",
                           "Beta Mod1","pvalue Mod1","Lower_CI_Mod1","Upper_CI_Mod1",
                           "Beta Mod2","pvalue Mod2","Lower_CI_Mod2","Upper_CI_Mod2",
                           "Beta Mod3","pvalue_Mod3","Lower_CI_Mod3","Upper_CI_Mod3", 
                           "Beta Mod4","pvalue_Mod4","Lower_CI_Mod4","Upper_CI_Mod4",
                           "Beta Mod5","pvalue_Mod5","Lower_CI_Mod5","Upper_CI_Mod5","sampleSize") 

LM1 <- cbind(allExposures, 
            Exposure0List,
            beta0ExposureList,pVal0ExposureList, 
            beta1ExposureList,pVal1ExposureList,
            beta2ExposureList,pVal2ExposureList, 
            beta3ExposureList,pVal3ExposureList,
            beta4ExposureList,pVal4ExposureList,
            beta5ExposureList,pVal5ExposureList) 

LM <- LM %>% as_tibble()
LM1 <- LM1 %>% as_tibble()

LM1$beta0ExposureList <- LM1$beta0ExposureList %>% as.numeric()
LM1$beta1ExposureList <- LM1$beta1ExposureList %>% as.numeric()
LM1$beta2ExposureList <- LM1$beta2ExposureList %>% as.numeric()
LM1$beta3ExposureList <- LM1$beta3ExposureList %>% as.numeric()
LM1$beta4ExposureList <- LM1$beta4ExposureList %>% as.numeric()
LM1$beta5ExposureList <- LM1$beta5ExposureList %>% as.numeric()

LM1$pVal0ExposureList <- LM1$pVal0ExposureList %>% as.numeric()
LM1$pVal1ExposureList <- LM1$pVal1ExposureList %>% as.numeric()
LM1$pVal2ExposureList <- LM1$pVal2ExposureList %>% as.numeric()
LM1$pVal3ExposureList <- LM1$pVal3ExposureList %>% as.numeric()
LM1$pVal4ExposureList <- LM1$pVal4ExposureList %>% as.numeric()
LM1$pVal5ExposureList <- LM1$pVal5ExposureList %>% as.numeric()


LM2 <- LM1 %>% mutate_if(is.numeric, round, digits=3)

colnames(LM2) <- c("Index", 
                   "Outcome",
                   "B0","p0",
                   "B1","p1",
                   "B2","p2",
                   "B3","p3",
                   "B4","p4",
                   "B5","p5") 

LM_bmi <- LM2 %>% filter(LM1$Exposure0List == "bmi_calc")
LM_whr <- LM2 %>% filter(LM1$Exposure0List == "WHR")


#hux <- Table %>% as_huxtable() %>% set_all_borders(1) %>% set_bold(1, everywhere, TRUE) %>% map_text_color(by_values("0.00347" = "red"))
#quick_pdf(hux)

#write.table(LM_bmi,paste0(meta3,"hyphy_lm_bmi_06Oct2021.csv",sep=""),quote=FALSE, sep=",",append=FALSE, col.names=TRUE, row.names=FALSE)
#write.table(LM_whr,paste0(meta3,"hyphy_lm_whr_06Oct2021.csv",sep=""),quote=FALSE, sep=",",append=FALSE, col.names=TRUE, row.names=FALSE)



