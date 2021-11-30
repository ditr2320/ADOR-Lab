#importing data
#######################################
rm(list = ls())

## Packages 
library(tidyverse)
meta5 <- "/Users/DidiTrifonova/Documents/GitHub/Git_ADOR/Mothers-Milk-Diet-Score-and-BMI/HMO/code01Nov2021"
meta6 <- "/Users/DidiTrifonova/Documents/GitHub/Git_ADOR/Mothers-Milk-Diet-Score-and-BMI/HMO/code01Nov2021/plots/"
#reading in data 
# long data - does not have correct MDS scores
data <- read.delim(paste0(meta5,"/tidyLong01Nov2021.txt"), header=TRUE)
dataSet <- data %>% filter(timepoint == 1)


metadata <- dataSet
#REGRESSION LOOP
#plot theme for graphs
mytheme <- theme(axis.line = element_line(size = 0.5, colour = "black"), panel.background = element_rect(fill = "white"))

#setting up exposure to loop through
ExposureFactors <- c("DII_Mom", "HEI2015_TOTAL_SCORE_Mom", "m_ener_Mom")

#setting up outcomes to loop through
x <- which(colnames(dataSet)=="Secretor")
y <- which(colnames(dataSet)=="SUM..ug.mL.")
outcomes<- names(dataSet)[x+1:4]

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
Outcome0List <- numeric(0)
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

#WARNING: DOES NOT WORK IF AS_TIBBLE() IS USED ON DATA FRAME!

for(j in seq(1:length(outcomes))) 
{
  
  for(i in seq(1:length(ExposureFactors))) 
    
  {
    pdf(file= paste0(meta6,"plots_",ExposureFactors[i],"_",outcomes[j],"baseline.pdf",sep=""))
    
    #create dateframe without NAs (include all variables to be adjusted for in model)
    thisDataInstance <- na.omit(data.frame(thisOutcome = metadata[,names(metadata) %in% outcomes[j]],
                                           thisExposureFactor = metadata[,names(metadata) %in% ExposureFactors[i]],
                                           age = metadata$mother_age,
                                           energy = metadata$m_ener_Mom,
                                           BMI = metadata$mom_BMI, 
                                           SES = metadata$SES_index_final)) 
    
  
    #linear model for plot
    modelFormForPlot <- as.formula(paste(outcomes[j],"~",ExposureFactors[i]))
    statModForPlot <- lm(modelFormForPlot, data=metadata)
    
    #linear model with adjustment factors. 
   
      #0) Model 0 base model
      modelForm0=as.formula("thisOutcome ~ thisExposureFactor")
      #1) Model 1 base model
      modelForm1=as.formula("thisOutcome ~ thisExposureFactor + age")
      #2) Model 2 base model
      modelForm2=as.formula("thisOutcome ~ thisExposureFactor + age + energy")
      #3) Model 3 base model
      modelForm3=as.formula("thisOutcome ~ thisExposureFactor + age + energy + BMI ")
      #3) Model 4 base model
      modelForm4=as.formula("thisOutcome ~ thisExposureFactor + age + energy + BMI + SES")

    
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
    Outcome0List[[length(Outcome0List)+1]] <- outcomes[j]
    
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
    
    sampleSizeList[[length(sampleSizeList)+1]] <- sampleSizeVal
    
    beta4Exposure1 <- format(summary(statMod4)$coefficients[2,1], digits = 2) 
    pVal4Exposure1 <- format(summary(statMod4)$coefficients[2,4], digits = 2) 
    
    #print(plot(statMod4))

    
    #Currently plots with show beta and p value for statMod4, modify as needed
    if(summary(statMod4)$coefficients[2,4] < 1) 
    {
      title <- paste("beta = ",
                     format(beta4Exposure1),
                     ", p-value = ",
                     format(pVal4Exposure1), 
                     ", n = ",nrow(thisDataInstance), sep="")
      
      print(qplot(thisExposureFactor,thisOutcome, data = thisDataInstance) +
              ggtitle(title) + labs(x=ExposureFactors[i], y=outcomes[j]) + mytheme + geom_smooth(method='lm',color ="black"))
    }
    plot(statMod3)
    plot(statMod4)
  }
  graphics.off()
}

LM <- cbind(allExposures, 
                     Outcome0List,
                     beta0ExposureList,pVal0ExposureList,CI0ExposureLowerList,CI0ExposureUpperList, 
                     beta1ExposureList,pVal1ExposureList,CI1ExposureLowerList,CI1ExposureUpperList, 
                     beta2ExposureList,pVal2ExposureList,CI2ExposureLowerList,CI2ExposureUpperList, 
                     beta3ExposureList,pVal3ExposureList,CI3ExposureLowerList,CI3ExposureUpperList,
                     beta4ExposureList,pVal4ExposureList,CI4ExposureLowerList,CI4ExposureUpperList,sampleSizeList) 

colnames(LM) <- c("Exposure", 
                           "Outcome",
                           "Beta Mod0","pvalueMod0","Lower_CI_Mod0","Upper_CI_Mod0",
                           "Beta Mod1","pvalueMod1","Lower_CI_Mod1","Upper_CI_Mod1",
                           "Beta Mod2","pvalueMod2","Lower_CI_Mod2","Upper_CI_Mod2",
                           "Beta Mod3","pvalueMod3","Lower_CI_Mod3","Upper_CI_Mod3", 
                           "Beta Mod4","pvalueMod4","Lower_CI_Mod4","Upper_CI_Mod4","sampleSize") 
LM <- LM %>% as_tibble()



#loops through each exposure, creates a new table, and adds a column for BH adjustment 
formula = ""
for(i in 1:length(ExposureFactors)){
  if(i != length(ExposureFactors)){
    formula = paste0(formula,"LM_",ExposureFactors[i],"<- LM %>% filter(Exposure == '",ExposureFactors[i],"') %>% mutate(adj0 = p.adjust(pvalueMod0,method = 'BH'),adj1 = p.adjust(pvalueMod1,method = 'BH'),adj2 = p.adjust(pvalueMod2,method = 'BH'),adj3 = p.adjust(pvalueMod3,method = 'BH'),adj4 = p.adjust(pvalueMod4,method = 'BH'));")
  }
  else{
    formula = paste0(formula,"LM_",ExposureFactors[i],"<- LM %>% filter(Exposure == '",ExposureFactors[i],"') %>% mutate(adj0 = p.adjust(pvalueMod0,method = 'BH'),adj1 = p.adjust(pvalueMod1,method = 'BH'),adj2 = p.adjust(pvalueMod2,method = 'BH'),adj3 = p.adjust(pvalueMod3,method = 'BH'),adj4 = p.adjust(pvalueMod4,method = 'BH'))")
  }
}
exp <- parse(text=formula)
eval(exp)








