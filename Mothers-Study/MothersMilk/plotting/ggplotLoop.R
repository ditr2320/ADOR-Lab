numVars <- c("mother_age_baseline", "m_ener_Mom_baseline" ,"mom_BMI_baseline","newMDS_baseline") 
numNames <- c("age", "energy" ,"BMI","MDS") 
for(i in 1: length(numVars))
{
  
  h <- ggplot(data = dataSet) + 
    geom_histogram(mapping = aes_string(x = numVars[i]), color = "black", fill = "slategray") + 
    labs(title = paste("Distribution of", numNames[i]), 
         caption = paste("N = ", nrow(dataSet)), x = numNames[i]) 
  print(h) 
}