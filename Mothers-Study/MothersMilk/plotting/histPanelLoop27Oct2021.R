
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
