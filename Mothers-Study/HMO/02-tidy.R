###Tidying up the data set
#4/23/2021

############# ADDING IN CORRECT MDS VARIABLE ################

#OLDER DATA SET
augustMDS <- dataSet %>% select(dyad_id,MDS_Mom_baseline) %>% na.omit() 
#NEWER DATA SET 
septemberMDS <- dataSet1 %>% select(dyad_id,MDS_Mom_baseline,MDS_baseline) %>% na.omit()
#REMOVING PARTICIPANTS FROM NEW DATA
sepMDS <- septemberMDS %>% filter(dyad_id %in% augustMDS$dyad_id) 
sepMDS$dyad_id == augustMDS$dyad_id

sepMDS$MDS_baseline %>% summary() #THIS IS THE CORRECT MDS VARIABLE 
sepMDS$MDS_Mom_baseline %>% summary() #INCORRECT
augustMDS$MDS_Mom_baseline %>% summary() #INCORRECT 

dataSet$newMDS_baseline <- ifelse(is.na(dataSet$MDS_Mom_baseline), NA, sepMDS$MDS_baseline) #ADDING CORRECT MDS TO DATA 

#NEW DATASET 
dataSet1$MDS_baseline %>% summary() #CORRECT 
dataSet1$MDS_Mom_baseline %>% summary() #INCORRECT

#OLDER DATASET
dataSet$newMDS_baseline %>% summary()  #CORRECT - USE THIS VARIABLE!
dataSet$MDS_Mom_baseline %>% summary()

# VAR NAME MDS_baseline 


############################# - DT 

##cleaning names
#dataSet %>% clean_names()

names(dataSet) <-gsub(" ","_",names(dataSet))
names(dataSet) <-gsub("'","_prime_",names(dataSet))
names(dataSet)<-sub("/","",names(dataSet))
names(dataSet)<-gsub("\\[|\\]","",names(dataSet))
names(dataSet)<-gsub("%","percent",names(dataSet))


names(dataSet)[c(987:1050)]<-sub("2","two",names(dataSet)[c(987:1050)])
names(dataSet)[c(2486:2549)]<-sub("2","two",names(dataSet)[c(2486:2549)])
names(dataSet)[c(4003,4025,4045)]<-sub("2","two",names(dataSet)[c(4003,4025,4045)])
names(dataSet)[c(7253,7233,7211)]<-sub("2","two",names(dataSet)[c(7253,7233,7211)])                              
                                 
names(dataSet)[c(987:1050)]<-sub("3","three",names(dataSet)[c(987:1050)])
names(dataSet)[c(2486:2549)]<-sub("3","three",names(dataSet)[c(2486:2549)])
names(dataSet)[c(4001:4064)]<-sub("3","three",names(dataSet)[c(4001:4064)])
names(dataSet)[c(7209:7272)]<-sub("3","three",names(dataSet)[c(7209:7272)])      
    
names(dataSet)[c(987:1050)]<-sub("6","six",names(dataSet)[c(987:1050)])
names(dataSet)[c(2493,2515,2535)]<-sub("6","six",names(dataSet)[c(2493,2515,2535)])
names(dataSet)[c(4001:4064)]<-sub("6","six",names(dataSet)[c(4001:4064)])
names(dataSet)[c(7209:7272)]<-sub("6","six",names(dataSet)[c(7209:7272)]) 



### creating BMI category variable (normal, overweight, and obese) 
dataSet$mom_bmi_category_baseline <- ifelse(dataSet$mom_BMI_baseline<25,"normal",ifelse(dataSet$mom_BMI_baseline>30,"obese","overweight"))
dataSet$mom_bmi_category_6m <- ifelse(dataSet$mom_BMI_6m<25,"normal",ifelse(dataSet$mom_BMI_6m>30,"obese","overweight"))
dataSet$mom_bmi_category_12m <- ifelse(dataSet$mom_BMI_12m<25,"normal",ifelse(dataSet$mom_BMI_12m>30,"obese","overweight"))
dataSet$mom_bmi_category_24m <- ifelse(dataSet$mom_BMI_24m<25,"normal",ifelse(dataSet$mom_BMI_24m>30,"obese","overweight"))

### removing non-secretors 
dataSet <-subset(dataSet,Secretor==1)


##  histogram loop 

## can edit vectors for any variables 
numVars <- c("mother_age_baseline", "m_ener_Mom_baseline" ,"mom_BMI_baseline","newMDS_baseline") 

numNames <- c("age", "energy" ,"BMI","MDS") 


for(i in 1: length(numVars))
{
  if(numNames[i] == "MDS")
  {
    g <- ggplot(data = dataSet) + 
      geom_histogram(mapping = aes_string(x = numVars[i]), binwidth = 1, color = "black", fill = "slategray") + 
      labs(title = paste("Distribution of", numNames[i]), 
           caption = paste("N = ", nrow(dataSet)), x = numNames[i]) 
    print(g) 
  }
  else
  {
    h <- ggplot(data = dataSet) + 
      geom_histogram(mapping = aes_string(x = numVars[i]), color = "black", fill = "slategray") + 
      labs(title = paste("Distribution of", numNames[i]), 
           caption = paste("N = ", nrow(dataSet)), x = numNames[i]) 
    print(h) 
  }
}



#HISTOGRAM LOOP WITH PANELS

#list of variables
vars <- c("mother_age_baseline", "m_ener_Mom_baseline" ,"mom_BMI_baseline","DII_Mom_baseline", "HEI2015_TOTAL_SCORE_Mom_baseline") 
#list for labels 
names <- c("Age", "Energy" ,"BMI","DII","HEI") 

formula = "par(mfrow = c(3,2));" #Edit dimensions according to # of variables
for(i in 1:length(vars)){
  if(i != length(vars)){
    formula = paste0(formula,"hist(dataSet$",vars[i],", xlab ='",names[i],"', main='",names[i],"');") #dataSet is name of data - change if needed 
  }
  else
  {
    formula = paste0(formula,"hist(dataSet$",vars[i],", xlab ='",names[i],"', main='",names[i],"')")
  }
  print(formula)
}
exp <- parse(text=formula)
eval(exp)


data <- dataSet %>% select(dyad_id, newMDS_baseline, HEI2015_TOTAL_SCORE_Mom_baseline, DII_Mom_baseline, Diversity_baseline, 
                              mom_BMI_baseline,m_ener_Mom_6m, m_cho_Mom_baseline,m_calcium_Mom_baseline, m_chol_Mom_baseline, 
                              m_fib_Mom_baseline, m_suc_Mom_baseline, m_pufa_Mom_baseline, m_mufa_Mom_baseline )

data$newMDS_baseline <- as.numeric(data$newMDS_baseline) #changing class to numeric
for (i in colnames(data)){
  if (class(data[,i]) == "numeric"){
    m = mean(data[,i], na.rm=TRUE)
    sd = sd(data[,i], na.rm=TRUE)
    for (j in 1:nrow(data)){
      if (is.na(data[j,i])==F&(data[j,i] > (m + 3*sd)|data[j,i] < (m - 3*sd))){
        data[j,i] <- NA
      }
    }
  }
} 


