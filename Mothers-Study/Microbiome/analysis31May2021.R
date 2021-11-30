# Clear out working space
rm(list=ls())
# Load required pacakges 
library(tidyverse)

## 188 sample size and baseline microbiome

# Set working directories
meta <- "/Volumes/ADORLab/Master Mothers Milk Data Folder/Input/"
meta2 <- "/Volumes/ADORLab/__Users/ditr2320/datasets/"
meta3 <- "/Volumes/ADORLab/HEI Study/16S Data/MM 16S Processed Files for Mom and Infants 05Jan21/alpha diversity"
meta4 <- "/Users/DidiTrifonova/Documents/GitHub/Git_ADOR/Mothers-Milk-Diet-Score-and-BMI"
meta5 <- "/Volumes/ADORLab/HEI Study/16S Data/MM 16S Processed Files for Mom and Infants 05Jan21/taxonomy/"

# Read in data new metadata 
#dataSet <- read.delim(paste0(meta2,"dataset01Apr2021.txt"),na = c("NA", "don't know", "","    .","Missing: Not provided",999.00))
dataSet <- read.delim(paste0(meta2,"longData03Jun2021.txt"), header=TRUE)
dataSet$dyad_id <- factor(as.character(with_options(c(scipen = 999), str_pad(dataSet$dyad_id, 4, pad = "0"))))

# Create susbets of data based on timepoint (visit number)
subset1m <- dataSet[dataSet$timepoint %in% "1",]
subset6m <- dataSet[dataSet$timepoint %in% "6",]

################### MICROBIOME DATA ###################
#######################################################
#######################################################
#######################################################

# reading in level 2 - Phylum 
taxaTable <- read.delim(paste0(meta5,"mm_150bp_deblur_sepp_noMit_noChl_rmsingletons_level2.txt",sep=""),header=TRUE,row.names=1,check.names = FALSE)

#    # Grabbing just the infnat gut microbiota at baseline
taxaTableMomBl <- taxaTable[,grepl("Mom.Bl",colnames(taxaTable))] #either chance this line or create 2d loop
myT <- t(taxaTableMomBl)
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
myT5$dyad_id <- myT5$dyad_id %>% as.numeric()
myT5 <- myT5[order(myT5$dyad_id,decreasing=FALSE),]

combinedSub <- merge(myT5, subset1m, by = "dyad_id")

combinedSub$dyad_id1 <- combinedSub$dyad_id

#microbiome analysis 

#creating variable for ratio of firmicutes to bacteroidetes 
combinedSub$FBratio <- combinedSub$`k__Bacteria;p__Firmicutes`/combinedSub$`k__Bacteria;p__Bacteroidetes`
#obesity status - obese/non-obese
combinedSub$obesityStatus <- ifelse(combinedSub$mom_BMI >= 30, "obese", "non-obese") 

sub <- na.omit(combinedSub)
#independent t -test 
#does FB ratio differ by obesity status and DII 
t.test(FBratio ~ obesityStatus,sub)
t.test(FBratio ~ DIIcategory,sub)
t.test(FBratio ~ HEIcategory,sub)
