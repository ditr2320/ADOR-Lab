#Farnaz Fouladi
#08/01/2019

#Microbiome diversity and pollutants
#Model 1 Diversity ~ Pollutants (Linear Regression and Kendall)
#Model2 Diversity ~ O3 + NO2_24h_yr
#Model3 Diversity ~ O3 + BMI
#Model4 Diversity ~ O3 + m_ener
#Model5 Diversity ~ O3 + hisp

rm(list=ls())
library(vegan)
library(Kendall)
output <- "/Volumes/ADORLab/__Users/ditr2320/metaAirAdonis/"
input <- "/Volumes/ADORLab/Current Lab Projects/MetaAIR/R00 Paper Microbiome/Manuscript Code Input Output and Figures FF/Kraken2/input/"
meta <- "/Volumes/ADORLab/Current Lab Projects/MetaAIR/R00 Paper Microbiome/Manuscript Code Input Output and Figures FF/metadata/"

#output<-"/Users/farnazfouladi/Google Drive/MicrobiomeAirPollution/Kraken2/output/"
#input<-"/Users/farnazfouladi/Google Drive/MicrobiomeAirPollution/Kraken2/input/"
#meta<-"/Users/farnazfouladi/Google Drive/MicrobiomeAirPollution/metadata/"

Taxa<-c("phylum","class","order","family","genus","species")

map=read.csv(paste0(meta,"/alderete_k99_metadata_30dec18.csv"),na = c("NA", "don't know", "","    .","Missing: Not provided"))
map_gut<-map[map$sample_type=="stool",]
rownames(map_gut)<-sapply(map_gut$sample_name,function(x){strsplit(as.character(x),"[.]")[[1]][1]})
#format visitdate variable as date format
map_gut$visitdate <- as.Date(map_gut$visitdate, "%m/%d/%y")
#create month
map_gut$month <- months(as.Date(map_gut$visitdate))
#assign season
monthname <- c("May","June","July","August","September")
map_gut$season <- factor(ifelse(map_gut$month %in% monthname,"summer","winter"))


for (taxa in Taxa ){
  setwd(paste0(output,taxa))
  myT=read.table(file=paste0(input,"tanya_kraken_",taxa,".txt"),sep="\t",header=T,row.names=1, quote = "")
  #Matching rownames between map and myT
  map1<-map_gut[intersect(rownames(map_gut),rownames(myT)),]
  map2<-map1[match(rownames(myT),rownames(map1)),]
  #AOX7 has low seq depth
  map3<-map2[-which(rownames(myT)=="AOX7"),]
  myT1<-myT[-which(rownames(myT)=="AOX7"),]
  
  shannonDiversity<-diversity(myT1,index = "shannon")
  evenness<-shannonDiversity/log(specnumber(myT1))
  
  ## Model1 Diversity ~ Pollutants
  
  tab1<-map3[,c("PM25_24h_yr","PM10_24h_yr","O3_24h_yr","NO2_24h_yr","totnox")]
  pmat<-matrix(nrow=5,ncol = 2)
  pmatKendall<-matrix(nrow=5,ncol = 2)
  rmat<-matrix(nrow=5,ncol = 2)
  for (i in 1:ncol(tab1)){
    pmat[i,1]<-summary(lm(shannonDiversity~tab1[,i]))$coefficients[2,4]
    rmat[i,1]<-summary(lm(shannonDiversity~tab1[,i]))$r.squared
    pmat[i,2]<-summary(lm(evenness~tab1[,i]))$coefficients[2,4]
    rmat[i,2]<-summary(lm(evenness~tab1[,i]))$r.squared
    
    pmatKendall[i,1]<-Kendall(shannonDiversity,tab1[,i])$sl[1]
    pmatKendall[i,2]<-Kendall(evenness,tab1[,i])$sl[1]
  }
  
  #Adjust pvalues
  pAdjust<-p.adjust(as.numeric(pmat),method = "BH")
  pAdjustMat<-matrix(pAdjust,nrow=5,byrow = F)
  table<-cbind(pAdjustMat,rmat) 
  colnames(table)<-c("pShannon","pEvenness","r2Shannon","r2Evenness")
  rownames(table)<-colnames(tab1)
  write.table(table,paste0(taxa,"_Diversity_model1.tsv"),sep="\t")
  
  
  ## Model2-Model5 Controlling for cofounders
  coVariates<-c("NO2_24h_yr","bmi","m_ener","hisp")
  models<-c("Model2","Model3","Model4","Model5")
  
  coVariate<-vector()
  pvalo3Shannon<-vector()
  pvalCovariateShannon<-vector()
  pvalo3Evenness<-vector()
  pvalCovariateEvenness<-vector()
  r2Shannon<-vector()
  r2Evenness<-vector()
  index<-1
  
  for (coVar in coVariates){
    
    coVariate[index]<-coVar
    
    tab1<-map3[,c("O3_24h_yr",coVar)]
    
    pvalo3Shannon[index]<-summary(lm(shannonDiversity~tab1[,"O3_24h_yr"]+tab1[,coVar]))$coefficients[2,4]
    pvalCovariateShannon[index]<-summary(lm(shannonDiversity~tab1[,"O3_24h_yr"]+tab1[,coVar]))$coefficients[3,4]
    
    pvalo3Evenness[index]<-summary(lm(evenness~tab1[,"O3_24h_yr"]+tab1[,coVar]))$coefficients[2,4]
    pvalCovariateEvenness[index]<-summary(lm(evenness~tab1[,"O3_24h_yr"]+tab1[,coVar]))$coefficients[3,4]
    
    r2Shannon[index]<-summary(lm(shannonDiversity~tab1[,"O3_24h_yr"]+tab1[,coVar]))$r.squared
    r2Evenness[index]<-summary(lm(evenness~tab1[,"O3_24h_yr"]+tab1[,coVar]))$r.squared
    
    index<-index+1
  }
  
  #Adjust pvalues
  
  df<-data.frame(models,coVariate,pvalo3Shannon,pvalCovariateShannon,pvalo3Evenness,pvalCovariateEvenness,r2Shannon,r2Evenness)
  df$pvalo3ShannonAdjust<-p.adjust(df$pvalo3Shannon,method="BH")
  df$pvalCovariateShannonAdjust<-p.adjust(df$pvalCovariateShannon,method="BH")
  df$pvalo3EvennessAdjust<-p.adjust(df$pvalo3Evenness,method="BH")
  df$pvalCovariateEvennessAdjust<-p.adjust(df$pvalCovariateEvenness,method="BH")
  
  write.table(df,paste0(taxa,"Diversity_model2-5.tsv"),sep="\t")
}

#Figure for paper at species level
df<-data.frame(diversity=shannonDiversity,evenness=evenness,ozon=map3[,"O3_24h_yr"])
theme_set(theme_classic(base_size = 7))
plot1<-ggplot(data=df,aes(x=ozon,y=shannonDiversity))+geom_point(size=1)+
  labs(x=expression("Prior Year"~ O[3]~ "24 hr (bbp)"),y="Shannon Diversity Index",
       title=expression(atop("FDR corrected p"=="0.0003",R^2=="0.15")))+
  geom_smooth(method='lm',se = FALSE,colour="red",size=0.5)

plot2<-ggplot(data=df,aes(x=ozon,y=evenness))+geom_point(size=1)+
  labs(x=expression("Prior Year"~O[3]~"24 hr (bbp)"),y="Evenness",
       title=expression(atop("FDR corrected p"=="0.0003",R^2=="0.15")))+
  geom_smooth(method='lm',se = FALSE,colour="red",size=0.5)

setwd("/Users/farnazfouladi/Google Drive/MicrobiomeAirPollution/Kraken2/Figures")
png("Kraken_Diversity.png", units="in", width=5, height=5,res=300)
pdf("Kraken_Diversity.pdf")
plot_grid(plot1,plot2,ncol=2,nrow=2)
dev.off()



