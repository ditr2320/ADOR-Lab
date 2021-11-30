#importing data
#######################################

# Clear out working space
rm(list=ls())
library(tidyverse)
library(withr)
library(huxtable)
library(corrplot) 
library(ggpubr)
library(ggthemes)

meta <- "/Volumes/ADORLab/Master Mothers Milk Data Folder/Input/"
meta1 <- "/Volumes/ADORLab/Current Lab Projects/Hyphy/IRB COVID procedures/ASA24/20210913_HYPHYCOV_analysis_files/"
meta2 <- "/Volumes/ADORLab/__Users/ditr2320/datasets/"
meta3 <- "/Volumes/ADORLab/__Users/ditr2320/hyphyData/"
meta4 <- "/Users/DidiTrifonova/Documents/GitHub/Git_ADOR/Mothers-Milk-Diet-Score-and-BMI/"

#reading in data 
data <- read.delim(paste0(meta3,"hMetaData20Sep2021.txt"), header=TRUE) #n=60 - participants with BMI and Diet 
MM <- read.delim(paste0(meta2,"dashData01Sep2021.txt"), header=TRUE)

#TO DO
  #table 1 - DONE
  #table 2 - DONE
  #correlation plot - DONE

  #Compare with MMData - merge data using project variable and create boxplots
  #diet in context with self report covid related changes in diet 
  
  #statistical analysis - linear regression, t-test, wilcoxon
    #bmi and diet by race, age, gender, etc
    #assocations between diet,bmi,age,gender,SES, physical activity


#IPAQ outliers

    

#CREATING DESCRIPTIVES TABLE

metadata <- data
DespFactors  <- c("age_calc_v2","bmi_calc","BMIclass","WHR","biosex_v2","White","Black","Asian", "NHPI", "AIAN", "hisp_decent_v2")
Names <- c("Age","BMI","BMI Category","Waist-Hip Ratio (WHR)", "Sex (f/m)","White (y/n)","Black (y/n)","Asian (y/n)", "NHPI (y/n)", "AIAN (y/n)", "Hispanic (y/n)","N")
table1 <- character(0)
overallColumn <- character(0)

for(i in DespFactors)
{
  if(i %in% c("BMIclass","biosex_v2","White","Black","Asian", "NHPI", "AIAN", "hisp_decent_v2"))
  {
    if(i %in% c("biosex_v2","White","Black","Asian", "NHPI", "AIAN","hisp_decent_v2"))
    {
      #table1 <- c(table1,i)
      #table1 <- c(table1,names(table(metadata[,names(metadata) %in% i])))
      #overallColumn[[length(overallColumn)+1]] <- ""
      #overallColumn <- c(overallColumn, as.numeric(table(metadata[,names(metadata) %in% i])))
      table1[[length(table1)+1]] <- i
      countFactor1 <- table(metadata[,names(metadata) %in% i])[1]
      countFactor2 <- table(metadata[,names(metadata) %in% i])[2]
      percentFactor1 <- format(countFactor1/nrow(metadata)*100,digits = 3)
      percentFactor2 <- format(countFactor2/nrow(metadata)*100,digits = 3)
      thisAllColumn <- paste(countFactor2, " (", percentFactor2,"%)  ", countFactor1," (",percentFactor1,"%)",sep="")
      overallColumn[[length(overallColumn)+1]] <- thisAllColumn
      
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
Observations <- c("N",nrow(metadata))
table1 <- rbind(table1,Observations)
table1$table1 <- Names
table1 <- table1 %>% rename("Value" = "overallColumn", "Characteristic" = "table1")
table1

hux <- table1 %>% as_huxtable() %>% set_all_borders(1) %>% set_bold(1, everywhere, TRUE) %>% map_text_color(by_values("0.00345" = "red"))
hux <- hux %>% set_all_padding(4) %>% set_width(.8) 
#quick_pdf(hux)

#CREATING TABLE 2

DespFactors  <- c("KCAL", "MDS", "DII", "DASH", "DIIcategory","MDS2category","MET_score","HEPAcat")
Names  <- c("Energy Intake", "MDS", "DII", "DASH", "DII Category (Pro/Anti)","MDS Category (High/Low)","MET Score","HEPA Category","N")
table2 <- character(0)
overallColumn <- character(0)

for(i in DespFactors)
{
  if(i %in% c("DIIcategory","MDS2category","HEPAcat"))
  {
    if(i %in% c("DIIcategory","MDS2category"))
    {
      #table2 <- c(table2,i)
      #table2 <- c(table2,names(table(metadata[,names(metadata) %in% i])))
      #overallColumn[[length(overallColumn)+1]] <- ""
      #overallColumn <- c(overallColumn, as.numeric(table(metadata[,names(metadata) %in% i])))
      table2[[length(table2)+1]] <- i
      countFactor1 <- table(metadata[,names(metadata) %in% i])[1]
      countFactor2 <- table(metadata[,names(metadata) %in% i])[2]
      percentFactor1 <- format(countFactor1/nrow(metadata)*100,digits = 3)
      percentFactor2 <- format(countFactor2/nrow(metadata)*100,digits = 3)
      thisAllColumn <- paste(countFactor2, " (", percentFactor2,"%)  ", countFactor1," (",percentFactor1,"%)",sep="")
      overallColumn[[length(overallColumn)+1]] <- thisAllColumn
      
    }
    else{
      table2[[length(table2)+1]] <- i
      countFactor1 <- table(metadata[,names(metadata) %in% i])[1]
      countFactor2 <- table(metadata[,names(metadata) %in% i])[2]
      countFactor3 <- table(metadata[,names(metadata) %in% i])[3]
      percentFactor1 <- format(countFactor1/nrow(metadata)*100,digits = 3)
      percentFactor2 <- format(countFactor2/nrow(metadata)*100,digits = 3)
      percentFactor3 <- format(countFactor3/nrow(metadata)*100,digits = 3)
      thisAllColumn <- paste(countFactor1, " (", percentFactor1,"%)  ",countFactor2," (",percentFactor2,"%)  ",
                             countFactor3," (",percentFactor3,"%)",sep="")
      #thisAllColumn <- paste(countFactor1, ", ", countFactor2,", ",percentFactor1,"%",sep="")
      overallColumn[[length(overallColumn)+1]] <- thisAllColumn
    }
  }
  else{
    table2[[length(table2)+1]] <- i
    meanAll<- format(mean(metadata[,names(metadata) %in% i],na.rm=TRUE),digits=3)
    sdAll <- format(sd(metadata[,names(metadata) %in% i],na.rm=TRUE),digits=3)
    thisAllColumn <- paste(meanAll, "±", sdAll)
    overallColumn[[length(overallColumn)+1]] <- thisAllColumn;
  }
}

table2 <- cbind(table2,overallColumn) 
table2 <- data.frame(table2)
Observations <- c("N",nrow(metadata))
table2 <- rbind(table2,Observations)
table2$table2 <- Names
table2 <- table2 %>% rename("Value" = "overallColumn", "Characteristic" = "table2")
table2

hux1 <- table2 %>% as_huxtable() %>% set_all_borders(1) %>% set_bold(1, everywhere, TRUE) %>% map_text_color(by_values("0.00345" = "red"))
hux1 <- hux1 %>% set_all_padding(4) %>% set_width(1) 
#quick_pdf(hux1)


subset1 <- metadata %>% select(DII, MDS, DASH, age_calc_v2, bmi_calc, MET_score,KCAL)
subset1 <- subset1 %>% rename("BMI" = "bmi_calc", "Age" = "age_calc_v2", "MET" = "MET_score", "Kcal" = "KCAL")
testRes = cor.mtest(subset1, conf.level = 0.95)
s1 <- cor(subset1, method = "pearson") 
corrplot(s1, p.mat = testRes$p, method = 'color', diag = FALSE, type = 'upper',
         sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9, 
         insig = 'label_sig', pch.col = 'grey20')

data$study <- "HYPHY"
MM$study <- "MM"

hyphyIndices <- data %>% select(MDS,DII,DASH,bmi_calc,study,MDS2category,MDS3category)
MMIndices <- MM %>% filter(timepoint == 1)
MMIndices <- MMIndices%>% select(MDS_Mom,DII_Mom,dash_score, mom_BMI ,study, MDS2category, MDS3category)
MMIndices <- MMIndices %>% rename("MDS" = "MDS_Mom", "DII" = "DII_Mom", "DASH" = "dash_score", "bmi_calc" = "mom_BMI") 
totalIndices <- rbind(hyphyIndices,MMIndices)

ggplot(data = totalIndices) + 
  geom_boxplot(mapping = aes(x = study, y = DII), fill = 'lightgoldenrod3') + theme_fivethirtyeight() + labs(title = "DII by cohort")
ggplot(data = totalIndices) + 
  geom_boxplot(mapping = aes(x = study, y = DASH), fill = 'lightgoldenrod3') + theme_fivethirtyeight() + labs(title = "DASH by cohort")
ggplot(data = totalIndices) + 
  geom_boxplot(mapping = aes(x = study, y = MDS), fill = 'lightgoldenrod3') + theme_fivethirtyeight() + labs(title = "MDS by cohort")


ggplot(data = totalIndices) + 
  geom_bar(mapping = aes(x = MDS2category), fill= 'lightgoldenrod3',color="black" ) + facet_wrap(~ study, nrow=1)  + theme_fivethirtyeight() + labs(title = "MDS by cohort")

ggplot(data = totalIndices) + 
  geom_bar(mapping = aes(x = MDS3category), fill= 'lightgoldenrod3',color="black" ) + facet_wrap(~ study, nrow=1)  + theme_fivethirtyeight() + labs(title = "MDS by cohort")

ggplot(data = totalIndices) + 
  geom_boxplot(mapping = aes(x = study, y = bmi_calc), fill = 'lightgoldenrod3') + theme_fivethirtyeight() + labs(title = "BMI by cohort")

ggplot(data = totalIndices) + 
  geom_boxplot(mapping = aes(x = study, y = MDS), fill = 'lightgoldenrod3') + theme_fivethirtyeight() + labs(title = "MDS by cohort")


wilcox.test(MMIndices$DII, hyphyIndices$DII)
wilcox.test(MMIndices$DASH, hyphyIndices$DASH)
wilcox.test(MMIndices$bmi_calc, hyphyIndices$bmi_calc)
wilcox.test(MMIndices$MDS, hyphyIndices$MDS)

ggplot(data=data) + 
  geom_histogram(mapping = aes(x=MET_score), binwidth = 2000, fill = "lightgoldenrod3", color = 'black') + labs(title = "MET_score") + theme_fivethirtyeight() 

ggplot(data=data) + 
  geom_histogram(mapping = aes(x=totalHour), binwidth = 2, fill = "lightgoldenrod3", color = 'black') + labs(title = "Total Hours") + theme_fivethirtyeight()

ggplot(data=data) + 
  geom_histogram(mapping = aes(x=VigHours), binwidth = 1, fill = "lightgoldenrod3", color = 'black') + labs(title = "Vigorous Hours") + theme_fivethirtyeight()

#FOR CHI SQUARE
data %>% count(MDS2category)
MM %>% count(MDS2category)

#data$self_pa_v2_n <- ifelse(data$self_pa_v2 == 'A',"10",data$self_pa_v2)
#data$self_pa_v2_n <- data$self_pa_v2_n %>% as.numeric()

ggscatter(data, x = "MET_score", y = "self_pa_v2_n", 
          add = "reg.line", conf.int = FALSE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MET Score", ylab = "Self Reported") 
ggscatter((data %>% filter(MET_score < 20000)), x = "MET_score", y = "self_pa_v2_n", 
          add = "reg.line", conf.int = FALSE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MET Score", ylab = "Self Reported") 

ggplot(data) + 
  geom_smooth(mapping = aes(x =MET_score, y = self_pa_v2_n),method="lm", color = "lightgoldenrod3") + 
  geom_point(mapping = aes(x = MET_score, y = self_pa_v2_n)) + labs(x= "MET Score", y="Self Reported Exercise")

ggplot(data=data) + 
  geom_histogram(mapping = aes(x=self_pa_v2_n), binwidth = 10, fill = "lightgoldenrod3", color = 'black') + labs(title = "Self Reported Exercise") + theme_fivethirtyeight()

