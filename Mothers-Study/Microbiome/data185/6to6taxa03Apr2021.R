# Example Code for Microbiome Analysis in MM
# Tanya L Alderete
# June 29, 2020

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
dataSet <- read.delim(paste0(meta2,"longData10May2021.txt"), header=TRUE)
dataSet$dyad_id <- factor(as.character(with_options(c(scipen = 999), str_pad(dataSet$dyad_id, 4, pad = "0"))))

# Create susbets of data based on timepoint (visit number)
dataSet1m <- dataSet[dataSet$timepoint %in% "1",]
dataSet6m <- dataSet[dataSet$timepoint %in% "6",]

#TESTING WHICH COLUMNS HAVE NA VALUES AND HOW MANY
#dataSet %>% summarise_all((funs(sum(is.na(.))))) 

# Create susets of data at each visit that only includes ID and breastmilk per day
#dataSet1mBreastFeedings <- dataSet1m[,names(dataSet1m) %in% c("dyad_id","breastmilk_per_day")]
#dataSet6mBreastFeedings <- dataSet6m[,names(dataSet6m) %in% c("dyad_id","breastmilk_per_day")]
#dataSet12mBreastFeedings <- dataSet12m[,names(dataSet12m) %in% c("dyad_id","breastmilk_per_day")]
#dataSet24mBreastFeedings <- dataSet24m[,names(dataSet24m) %in% c("dyad_id","breastmilk_per_day")]

########### ########### ########### ########### ########### ########### ########### ########### 
# Regression loop for obtaining infant gut microbiota data at baseline
########### ########### ########### ########### ########### ########### ########### ########### 

# Each outcome of interest
variableList <- c("DII_Mom","MDS_Mom","HEI2015_TOTAL_SCORE_Mom")
#variableList <- c("DII_Mom_baseline","HEI2015_TOTAL_SCORE_Mom_baseline","MDS_baseline")

# Diversity and each taxanomic level
taxaLevels <- c("level2","level3","level4","level5","level6")
#taxaLevels <- c("level2")

# Start of loop for obtaining microbiota data
for(taxa in taxaLevels)
{
  taxaTable <- read.delim(paste0(meta5,"mm_150bp_deblur_sepp_noMit_noChl_rmsingletons_",taxa,".txt",sep=""),header=TRUE,row.names=1,check.names = FALSE)

    # Grabbing just the infnat gut microbiota at baseline
    taxaTableMom6m <- taxaTable[,grepl("Mom.6",colnames(taxaTable))] #either chance this line or create 2d loop
    myT <- t(taxaTableMom6m)
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
    myT5$dyad_id <- substr(row.names(myT5),10,13)
  
  # Merging the microbiota data with the metadata of interest at relevant timepoint, below is 24-month as example
  taxaMeta <- merge(dataSet6m,myT5,by="dyad_id")
  
  ########### ########### ########### ########### ########### ########### ########### ########### ########### 
  # Nested regression loop for microbiota analysis. Loops through each taxanomic level
  ########### ########### ########### ########### ########### ########### ########### ########### ########### 
  
  # Creates a list, makes it a characters and set it to empty
  exposureNamesList <- character(0)
  
  # Need to ALWAYS verify this is using the correct names based on our dataset
  allExposures <- names(taxaMeta)[(length(dataSet1m) + 1):(length(taxaMeta))]
  
  # Regression loop
  for(variableOfInterest in variableList)
  {
    pValList <- numeric(0)
    exposureBetaList <- numeric(0)
    lowerList <- numeric(0)
    upperList <- numeric(0)
    
  pdf(file = paste0(meta2,"plots/microbiome6m_",taxa,"_",variableOfInterest,"_lm_withCovariates.pdf",sep=""))
    
  # Need to ALWAYS verify this is using the correct names based on our dataset
  for(i in (length(dataSet1m) + 1):(length(taxaMeta)))
  {
    
    # Printing the name of the taxa we are looking at in each iteration of this nested loop
    print(names(taxaMeta)[i])
    
    # Creating the model you will run (with covariates) - mae sure to consider what x and  y should be
    modelForm <- as.formula(paste("thisMicrobe", "~thisVariable+age+BMI+energy+exercise+SES")) #add BMI 
    
    # Creates this data instance to use for linear regression analysis
    
    thisDataInstance <- na.omit(data.frame(dyad_id=taxaMeta$dyad_id,
                                           thisMicrobe=taxaMeta[,i],
                                           thisVariable=taxaMeta[,names(taxaMeta) %in% variableOfInterest],
                                           age = taxaMeta$mother_age,
                                           BMI=taxaMeta$mom_BMI,
                                           energy = taxaMeta$m_ener_Mom,
                                           exercise = taxaMeta$exercise_r,
                                           SES = taxaMeta$SES_r))
    
    # Running the linear regression analysis
    modelInfo <- lm(modelForm, data = thisDataInstance)
    
    # Creating a list of coefficent names and values
    coefs <- coef(modelInfo)
    
    # Generating the 95% CIs for 'thisMicrobe' beta coefficent 
    ci_est <- confint(modelInfo, 'thisVariable', level=0.95)
    
    # Grabbing and storing the upper and lower 95% CI values in a list
    lower_ci_est <- ci_est[1]
    lowerList[[length(lowerList)+1]] <- lower_ci_est
    upper_ci_est <- ci_est[2]
    upperList[[length(upperList)+1]] <- upper_ci_est
    
    # Grabbing and storing the beta value for "thisMicrobe" in a list
    thisBeta <- format(coefs["thisVariable"],digits=3)
    exposureBetaList[[length(exposureBetaList)+1]] <- thisBeta
    
    # Grabbing and storing the p-value for the beta for "thisMicrobe" in a list
    pVal <- format.pval(coef(summary(modelInfo))["thisVariable",4],digits=3)
    pValList[[length(pValList)+1]] <- pVal
    
    # Creating plots
    title <- paste("beta =",thisBeta,", p-val = ",pVal,sep = "")
    p <- ggplot(thisDataInstance,aes(x = thisVariable,y = thisMicrobe))
    print(p + geom_point( size = 8,color = "grey36") +
            geom_abline(slope = coefs["thisVariable"],intercept = coefs["(Intercept)"] + 
                          mean(thisDataInstance$age)*coefs["age"] +
                          mean(thisDataInstance$SES)*coefs["SES"],size=1.5,linetype = "dashed",colour="grey36") +
            ggtitle(title) +
            xlab(variableOfInterest) +
            ylab(paste(names(taxaMeta)[i],sep = "")) +
            theme_classic(base_size = 20) +
            theme(axis.line = element_line(size=1),
                  axis.ticks.y = element_line(size=1),
                  axis.ticks.x = element_blank(),
                  axis.text = element_text(face="bold",size=20),
                  text = element_text(face="bold",size=16),
                  legend.position = "bottom",
                  legend.title = element_blank(),
                  plot.title = element_text(hjust = 0.5)
            ) +
            theme(axis.line.x = element_line(color="black", size = 2),
                  axis.line.y = element_line(color="black", size = 2)
            )
    )
    
  }
  graphics.off()
  adjPvals <- p.adjust(pValList,method = "BH")
  allExposures <- cbind(allExposures,exposureBetaList, lowerList, upperList, pValList,adjPvals);
  exposureNamesList <- cbind(exposureNamesList,paste(variableOfInterest,"beta",sep="_"),paste(variableOfInterest,"lower_estimate_beta",sep="_"),paste(variableOfInterest,"upper_estimate_beta",sep="_"),paste(variableOfInterest,"pval",sep="_"),paste(variableOfInterest,"adj_pval",sep="_"))
  
  }
  allExposures <- data.frame(allExposures)
  names(allExposures) <- c("Taxa",exposureNamesList)
  write.table(allExposures,paste0(meta2,"statisticalTables/mom6m_",taxa,"_Microbiome_lm_withCovariates.txt",sep=""),quote=FALSE, sep="\t",append=FALSE, col.names=TRUE, row.names=FALSE)
}
