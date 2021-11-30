#Farnaz Fouladi
#08/01/2019

##PCO
##Adonis Test:
#Model1:bacteria as outcome = 1 AP exposure at a time
#Model2: Gut microbiota ~ O3*obeseStatus
#Model3: Gut microbiota ~ O3*Sex
#Model4: Gut microbiota ~ O3+m_Ener

##Linear Regreressions
#Model 1: univariate for  air Pollutants (1 year prior) and Kendall test.
#Model 2: O3 and  prior year NO2 (this just occurred to me that we should look at)
#Model 3: O3 + BMI
#Model 4: O3 + energy intake
#Model 5: O3 + Hisp
#Model 6: O3 +sex
#Model 7: O3+season
#Model 8 O3+ carbohydrate
#Model 9 O3+fiber
#Model 10 O3+fat
#Model 11 O3+protein

##More Figures for publication

rm(list=ls())
# required package
library("pheatmap")
library("vegan")
library("ggplot2")
library(gridExtra)
library(Kendall)


output <- "/Volumes/ADORLab/__Users/ditr2320/metaAirAdonis/"
input <- "/Volumes/ADORLab/Current Lab Projects/MetaAIR/R00 Paper Microbiome/Manuscript Code Input Output and Figures FF/Kraken2/input/"
meta <- "/Volumes/ADORLab/Current Lab Projects/MetaAIR/R00 Paper Microbiome/Manuscript Code Input Output and Figures FF/metadata/"
Taxa<-c("phylum","class","order","family","genus","species")

# Reading metadata
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
map_gut$obeseStatus<-sapply(map_gut$bmi,function(x){if (x>25) return(1) else return(0)})

getTable<-function(taxa){
  
  myT=read.table(file=paste0(input,"tanya_kraken_",taxa,".txt"),sep="\t",header=T,row.names=1, quote = "")
  #Normalize table
  average = sum(rowSums(myT))/nrow(myT)
  myT_relab <- sweep(myT,1,rowSums(myT),"/")
  myT_norm<-log10(myT_relab*average +1)
  #Matching rownames between map and myT
  map1<-map_gut[intersect(rownames(map_gut),rownames(myT_norm)),]
  map2<-map1[match(rownames(myT_norm),rownames(map1)),]
  index<-c(which(rownames(myT_norm)=="AOX7"),which(rownames(myT_norm)=="AOYI"))
  map2<-map2[-index,] 
  myT_norm<-myT_norm[-index,]
  listOfTables<-list(myT_norm,map2)
  return(listOfTables)
}

for(taxa in Taxa){
  
  setwd(paste0(output,taxa))
  
  listOfTables<-getTable(taxa)
  myT<-listOfTables[[1]]
  meta<-listOfTables[[2]] 
  
  #Relative aundance for phylum:
  if (taxa=="phylum"){
    myT1<-(10^myT)-1 #unlong but still normalized
    myT2<-sweep(myT1,1,rowSums(myT1),"/")
    composition<-colMeans(myT2)[colMeans(myT2)>0.01]
    composition1<-c(composition,Others=(1-sum(composition)))
    pdf("Pie.pdf")
    pie(composition1)
    dev.off()
    "Proteobacteria  Bacteroidetes     Firmicutes Actinobacteria         Others 
    0.04991883     0.59736989     0.31508666     0.02536194     0.01226268 "
  }
  
  ## MultiDimensional Scaling
  
  pdf(paste0(taxa,"_pco_plots.pdf")) 
  myT_pcoa=capscale(myT~1,distance="bray")
  percentVariance<-myT_pcoa$CA$eig/sum(myT_pcoa$CA$eig)*100
  layout(matrix(1:2,ncol=2), width = c(2,1),height = c(1,1))
  pcoa=ordiplot(myT_pcoa,choices=c(1,2),type="none",main=taxa,
                xlab=paste0("MDS1 (",format(percentVariance[1],digits = 3),"%)"),
                ylab=paste0("MDS2 (",format(percentVariance[2],digits = 3),"%)"))
  rbPal <- colorRampPalette(c('red','blue'))
  Col<-rbPal(10)[as.numeric(cut(meta$O3_24h_yr,breaks = 10))]
  points(pcoa,"sites",col=adjustcolor(Col,alpha.f = 0.6),pch=16)
  meta_pollutants<-meta[,c("totnox", "PM25_24h_yr", "PM10_24h_yr", "NO2_24h_yr", "O3_24h_yr")]
  colnames(meta_pollutants)<-c("Total NOx", "PM2.5", "PM10","NO2", "O3")
  pcoa_fit=envfit(pcoa,meta_pollutants,perm=10000,na.rm=T)
  plot(pcoa_fit,p.max=0.05,col="darkgreen",cex=0.7)
  plot(c(0,3),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = expression("Prior year"~O[3]~"24 hr"))
  text(x=2, y = seq(0,1), labels = c("35.2 bbp","24.3 bbp"))
  legend_image <- as.raster(matrix(adjustcolor(rbPal(10),alpha.f = 0.6), ncol=1))
  rasterImage(legend_image, 0, 0, 1,1)
  dev.off()
  
  ### MultivariateAnalysis
  # Model 1:bacteria as outcome = 1 AP exposure at a time (only prior year since highly correlated with prior month)
  
  Variables= c("totnox", "PM25_24h_yr", "PM10_24h_yr", "NO2_24h_yr", "O3_24h_yr",
               "cage", "bmi", "m_ener", "hisp", "season","male")
  
  names<-c( "Prior Year Tot NOx", "Prior Year PM2.5", "Prior Year PM10", 
            "Prior Year NO2", "Prior Year O3 24 hr", "Age", "BMI", "Energy Intake",
            "Hispanic", "Season","Sex")
  
  plotNames<-c( "Prior Year Tot NOx", getExpression("Prior Year PM2.5"),
                getExpression("Prior Year PM10"),getExpression("Prior Year NO2"),
                getExpression("Prior Year O3 24 hr"),"Age", "BMI", "Energy Intake",
                "Hispanic", "Season","Sex")
  
  adonismat<-matrix(nrow = length(Variables),ncol=3)
  for (var in Variables){
    
    tab<-meta[,var]
    tab1<-cbind(myT,pollutant=tab)
    tab1<-na.omit(tab1)     #Presence of NA throws an error for adonis test
    fit=adonis(tab1[,1:ncol(myT)] ~ tab1$pollutant,permutations = 10000)
    adonismat[which(Variables==var),1]<-fit$aov.tab$`Pr(>F)`[1]
    adonismat[which(Variables==var),2]<-fit$aov.tab$R2[1]
  }
  pAdjust<-p.adjust(adonismat[,1],method="BH")
  adonismat[,3]<-pAdjust
  colnames(adonismat)<-c("p-value","R2","pAdjusted")
  rownames(adonismat)<-Variables
  df1<-as.data.frame(adonismat)
  df1$sig<-ifelse(df1$pAdjusted<0.05,"Significant","Nonsignificant")
  write.table(df1,paste0(taxa,"_AdonisModel1.txt"),sep="\t")
  
  df1$nameVariables<-factor(rownames(df1),levels=Variables)
  theme_set(theme_classic(base_size = 8))
  model1_Plot <- ggplot(data = df1, aes(x =nameVariables, y = R2,fill=sig)) + 
    geom_bar(stat = "identity")+theme(axis.title.y =element_blank(),axis.text.x = element_text(angle = 90),legend.position = "none")+
    scale_fill_manual(values=c("Significant"="red","Nonsignificant"="grey"))+scale_x_discrete(labels=plotNames)+coord_flip()+
    labs(title=paste0("Multivariate analysis (",taxa,")"),y=expression(R^2))
  
  pdf(paste0(taxa,"_BarPlot.pdf"),width = 3,height =5 )
  print(model1_Plot)
  dev.off()
  
  
  #Model2: Gut microbiota ~ O3*obeseStatus
  
  adonismat<-matrix(nrow = 3,ncol=3)
  
  tab<-meta[,c("O3_24h_yr","obeseStatus")]
  tab1<-cbind(myT,tab)
  tab1<-na.omit(tab1)     #Presence of NA throws an error for adonis test
  fit=adonis(tab1[,1:ncol(myT)] ~ tab1$O3_24h_yr* as.factor(tab1$obeseStatus),permutations = 9999)
  adonismat[1,1]<-fit$aov.tab$`Pr(>F)`[1]
  adonismat[2,1]<-fit$aov.tab$`Pr(>F)`[2]
  adonismat[3,1]<-fit$aov.tab$`Pr(>F)`[3]
  adonismat[1,2]<-fit$aov.tab$R2[1]
  adonismat[2,2]<-fit$aov.tab$R2[2]
  adonismat[3,2]<-fit$aov.tab$R2[3]
  
  pAdjust<-p.adjust(adonismat[,1],method="BH")
  adonismat[,3]<-pAdjust
  colnames(adonismat)<-c("p-value","R2","pAdjusted")
  rownames(adonismat)<-c("24-hr O3", "obeseStatus","Inteaction")
  df1<-as.data.frame(adonismat)
  write.table(df1,paste0(taxa,"_AdonisModel2.txt"),sep="\t")
  
  #Model3: Gut microbiota ~ O3*Sex
  
  adonismat<-matrix(nrow = 3,ncol=3)
  
  tab<-meta[,c("O3_24h_yr","male")]
  tab1<-cbind(myT,tab)
  tab1<-na.omit(tab1)     #Presence of NA throws an error for adonis test
  fit=adonis(tab1[,1:ncol(myT)] ~ tab1$O3_24h_yr*tab1$male,permutations = 9999)
  adonismat[1,1]<-fit$aov.tab$`Pr(>F)`[1]
  adonismat[2,1]<-fit$aov.tab$`Pr(>F)`[2]
  adonismat[3,1]<-fit$aov.tab$`Pr(>F)`[3]
  adonismat[1,2]<-fit$aov.tab$R2[1]
  adonismat[2,2]<-fit$aov.tab$R2[2]
  adonismat[3,2]<-fit$aov.tab$R2[3]
  
  pAdjust<-p.adjust(adonismat[,1],method="BH")
  adonismat[,3]<-pAdjust
  colnames(adonismat)<-c("p-value","R2","pAdjusted")
  rownames(adonismat)<-c("24-hr O3", "sex","Inteaction")
  df1<-as.data.frame(adonismat)
  write.table(df1,paste0(taxa,"_AdonisModel3.txt"),sep="\t")
  
  
  # Gut microbiota ~ O3+nutrients
  
  covar<-c("m_ener","m_cho","m_fib","m_pro","m_fat")
  adonismat<-matrix(nrow = 2,ncol=3)
  for (var in covar){
    
    tab<-meta[,c("O3_24h_yr",var)]
    tab1<-cbind(myT,tab)
    tab1<-na.omit(tab1)     #Presence of NA throws an error for adonis test
    fit=adonis(tab1[,1:ncol(myT)] ~ tab1$O3_24h_yr + tab1[,var],permutations = 9999)
    adonismat[1,1]<-fit$aov.tab$`Pr(>F)`[1]
    adonismat[2,1]<-fit$aov.tab$`Pr(>F)`[2]
    adonismat[1,2]<-fit$aov.tab$R2[1]
    adonismat[2,2]<-fit$aov.tab$R2[2]
    
    pAdjust<-p.adjust(adonismat[,1],method="BH")
    adonismat[,3]<-pAdjust
    colnames(adonismat)<-c("p-value","R2","pAdjusted")
    rownames(adonismat)<-c("24-hr O3",var)
    df1<-as.data.frame(adonismat)
    write.table(df1,paste0(taxa,"_AdonisModel_",var,".txt"),sep="\t")
    
  }
  ### Linear Regression 
  #model 1 taxa~Pollutants or demographic variables
  
  myT_r<-myT[,colMeans(myT>0)>=0.25]
  pmat=matrix(nrow = ncol(myT_r),ncol=length(Variables))
  rmat=matrix(nrow = ncol(myT_r),ncol=length(Variables))
  pmatKendall=matrix(nrow = ncol(myT_r),ncol=length(Variables))
  corcoeff=matrix(nrow = ncol(myT_r),ncol=length(Variables))
  
  for (j in Variables) {
    for (i in 1:ncol(myT_r) ){
      fit<-lm(myT_r[,i] ~ meta[,j])
      pmat[i,which(Variables==j)]<-summary(fit)$coefficients[2,4]
      rmat[i,which(Variables==j)]<-summary(fit)$r.squared
      pmatKendall[i,which(Variables==j)]<-Kendall(myT_r[,i],meta[,j])$sl[1]
      corcoeff[i,which(Variables==j)]<-Kendall(myT_r[,i],meta[,j])$tau[1]
    }
  }
  
  rownames(pmat)<-colnames(myT_r)
  colnames(pmat)<-names
  
  rownames(pmatKendall)<-colnames(myT_r)
  colnames(pmatKendall)<-names
  
  pAdjust<-p.adjust(as.numeric(pmat),method = "BH")
  pmatAdjusted<-matrix(pAdjust,nrow=ncol(myT_r),byrow = F)
  
  rownames(pmatAdjusted)<-colnames(myT_r)
  rownames(rmat)<-colnames(myT_r)
  
  colnames(pmatAdjusted)<-names
  colnames(rmat)<-names
  
  write.table(pmatAdjusted,paste0(taxa,"_p-valuesModel1.txt"),sep="\t")
  write.table(rmat,paste0(taxa,"_r-SquaredModel1.txt"),sep="\t")
  
  #HeatMap for R-squared and p-Values for all 8 variables
  pmatAdjusted1<-pmatAdjusted[rowSums(pmatAdjusted<0.1)>0,]
  rmat1<-rmat[which(rowSums(pmatAdjusted<0.1)>0),]
  pmatAdjusted1<-as.data.frame(pmatAdjusted1)
  rmat1<-as.data.frame(rmat1)
  pmatAdjusted1[pmatAdjusted1>0.1]=NA
  pmatAdjusted1<-pmatAdjusted1[order(rownames(pmatAdjusted1)),]
  rmat1<-rmat1[order(rownames(rmat1)),]
  
  if (nrow(pmatAdjusted1)>0){
    png(paste0(taxa,"_pValuesModel1.png"),width=5000,height=19000,res = 300)
    par(mfrow=c(1,1),mar=c(5,5,5,5))
    
    #pdf(paste0(taxa,"_pValuesModel1.pdf"),width = 4,height = 6)
    pheatmap(pmatAdjusted1,col=c("firebrick3",adjustcolor( "firebrick3", alpha.f = 0.5),adjustcolor( "firebrick3", alpha.f = 0.2)),breaks=c(0,0.01,0.05,0.1),
             cluster_col = F,cluster_row = F,cellwidth=10,cellheight=10,fontsize=7,
             main=paste0(taxa,"\np-values"),fontsize_row = 7,labels_col=plotNames)
    dev.off()
    png(paste0(taxa,"_RsquaredModel1.png"),width=5000,height=19000,res = 300)
    par(mfrow=c(1,1),mar=c(5,5,5,5))
    
    #pdf(paste0(taxa,"_RsquaredModel1.pdf"),width = 4,height = 6)
    pheatmap(rmat1,col=c("lightgrey",adjustcolor("firebrick3", alpha.f = 0.2),adjustcolor( "firebrick3", alpha.f = 0.5),"firebrick3"),
             breaks = c(0,0.05,0.1,0.15,max(rmat1)),cluster_col = F,cluster_row = F,cellwidth=10,
             cellheight=10,fontsize=7,main=expression(R^2),fontsize_row = 7,labels_col=plotNames)
    dev.off()
    
  }
  
  #HeatMap for R-squared and p-Values for all 8 variables Kendall
  
  rownames(corcoeff)<-colnames(myT_r)
  colnames(corcoeff)<-names
  write.table(corcoeff,paste0(taxa,"_Coefficients_Kendall.txt"),sep="\t")
  
  pAdjustKendall<-p.adjust(as.numeric(pmatKendall),method = "BH")
  pmatAdjustedKendall<-matrix(pAdjustKendall,nrow=ncol(myT_r),byrow = F)
  rownames(pmatAdjustedKendall)<-colnames(myT_r)
  colnames(pmatAdjustedKendall)<-names
  write.table(pmatAdjustedKendall,paste0(taxa,"_p-values_Kendall.txt"),sep="\t")
  
  pmatAdjusted1<-pmatAdjustedKendall[rowSums(pmatAdjustedKendall<0.1)>0,]
  pmatAdjusted1<-as.data.frame(pmatAdjusted1)
  pmatAdjusted1[pmatAdjusted1>0.1]=NA
  pmatAdjusted1<-pmatAdjusted1[order(rownames(pmatAdjusted1)),]
  
  if (nrow(pmatAdjusted1)>0){
    png(paste0(taxa,"_pValues_Kendall.png"),width=5000,height=19000,res = 300)
    par(mfrow=c(1,1),mar=c(5,5,5,5))
    
    #pdf(paste0(taxa,"_pValues_Kendall.pdf"),width = 4,height = 6)
    pheatmap(pmatAdjusted1,col=c("firebrick3",adjustcolor( "firebrick3", alpha.f = 0.5),adjustcolor( "firebrick3", alpha.f = 0.2)),breaks=c(0,0.01,0.05,0.1),
             cluster_col = F,cluster_row = F,cellwidth=10,cellheight=10,fontsize=7,
             main=paste0(taxa,"\np-values"),fontsize_row = 7,labels_col=plotNames)
    dev.off()
  }
  
  #Multiple univariate regression models
  
  coVariates<-c("NO2_24h_yr","bmi","m_ener","hisp","male","season","m_cho","m_fat","m_fib","m_pro")
  models<-c("Model2","Model3","Model4","Model5","Model6","Model7","Model8","Model9","Model10","Model11")
  myT_r<-myT[,colMeans(myT>0)>=0.25]
  
  for(coVar in coVariates){
    
    pmat=matrix(nrow = ncol(myT_r),ncol=2)
    rmat=matrix(nrow = ncol(myT_r),ncol=1)
    
    for (i in 1:ncol(myT_r) ){
      fit<-lm(myT_r[,i] ~ meta[,"O3_24h_yr"]+meta[,coVar])
      pmat[i,1]<-summary(fit)$coefficients[2,4]
      pmat[i,2]<-summary(fit)$coefficients[3,4]
      rmat[i,1]<-summary(fit)$r.squared
      
    }
    
    pAdjust<-p.adjust(as.numeric(pmat),method = "BH")
    pmatAdjusted<-matrix(pAdjust,nrow=ncol(myT_r),byrow = F)
    
    colnames(pmatAdjusted)<-c("O3_24h_yr",coVar)
    colnames(rmat)<-c(paste0("O3_24h_yr_",coVar))
    
    rownames(pmatAdjusted)<-colnames(myT_r)
    rownames(rmat)<-colnames(myT_r)
    
    write.table(pmatAdjusted,paste0(taxa,"_p-values",models[which(coVariates==coVar)],".txt"),sep="\t")
    write.table(rmat,paste0(taxa,"_r-Squared",models[which(coVariates==coVar)],".txt"),sep="\t")
  }
}

##Figures for publication

getResult<-function(taxa){
  result<-read.table(paste0(output,taxa,"/",taxa,"_p-valuesModel1.txt"),sep="\t",header = TRUE)
  result<-result[order(result$Prior.Year.O3.24.hr),]
  resultR<-read.table(paste0(output,taxa,"/",taxa,"_r-SquaredModel1.txt"),sep="\t",header = TRUE)
  resultR<-resultR[rownames(result),]
  resultList<-list(result,resultR)
  return(resultList)
}

getPlotForOzone<-function(table,resultPvals,resultR,taxa,regressionLine=FALSE){
  plot1<-ggplot(data=table,aes(x=O3_24h_yr,y=table[,taxa]))+geom_point(size=0.8)+
    labs(x=expression("Prior Year"~ O[3]~"24 hr (bbp)"),y=expression(log[10]~"normalized count"),
         title=bquote(atop(.(taxa),atop("FDR corrected p"==~.(format(resultPvals$Prior.Year.O3.24.hr[which(rownames(resultPvals)==taxa)],digits = 3)),
                                                ~R^2==~.(format(resultR$Prior.Year.O3.24.hr[which(rownames(resultR)==taxa)],digits = 3))))))
  if (regressionLine==TRUE){
    plot1<-plot1+geom_smooth(method='lm',se = FALSE,colour="red",size=0.5)
  }
  return(plot1)
}

getPlotForNo2<-function(table,resultPvals,resultR,taxa,regressionLine=FALSE){
  plot1<-ggplot(data=table,aes(x=NO2_24h_yr,y=table[,taxa]))+geom_point(size=0.8)+
    labs(x=expression("Prior Year"~ NO[2]~"(bbp)"),y=expression(log[10]~"normalized count"),
         title=bquote(atop(.(taxa),atop("FDR corrected p"==~.(format(resultPvals$Prior.Year.NO2[which(rownames(resultPvals)==taxa)],digits = 3)),
                                        ~R^2==~.(format(resultR$Prior.Year.NO2[which(rownames(resultR)==taxa)],digits = 3))))))
  if (regressionLine==TRUE){
    plot1<-plot1+geom_smooth(method='lm',se = FALSE,colour="red",size=0.5)
  }
  return(plot1)
}

getPlotForNox<-function(table,resultPvals,resultR,taxa,regressionLine=FALSE){
  plot1<-ggplot(data=table,aes(x=totnox,y=table[,taxa]))+geom_point(size=0.8)+
    labs(x="Prior Year total NOx (bbp)",y=expression(log[10]~"normalized count"),
         title=bquote(atop(.(taxa),atop("FDR corrected p"==~.(format(resultPvals$Prior.Year.Tot.NOx[which(rownames(resultPvals)==taxa)],digits = 3)),
                                        ~R^2==~.(format(resultR$Prior.Year.Tot.NOx[which(rownames(resultR)==taxa)],digits = 3))))))
  if (regressionLine==TRUE){
    plot1<-plot1+geom_smooth(method='lm',se = FALSE,colour="red",size=0.5)
  }
  return(plot1)
}
#Scatter plots for the 21 phyla significantly associated with O3
setwd("/Users/farnazfouladi/Google Drive/MicrobiomeAirPollution/Kraken2/Figures")
phylum<-cbind(getTable("phylum")[[1]],getTable("phylum")[[2]])
pvalResultPhylum<-getResult("phylum")[[1]]
rResultPhylum<-getResult("phylum")[[2]]
myList<-list()
theme_set(theme_classic(base_size = 7))
for (i in 1:21){
  
  taxaName<-rownames(pvalResultPhylum)[i]
  plot<-getPlotForOzone(phylum,pvalResultPhylum,rResultPhylum,taxaName,regressionLine=TRUE)
  myList[[i]]<-plot
  
}

pdf("significantPhylaKraken.pdf")
for (i in c(1,10,19)){
  if(i<19){
    grid.arrange(myList[[i]],myList[[i+1]],myList[[i+2]],myList[[i+3]],myList[[i+4]],myList[[i+5]],myList[[i+6]],myList[[i+7]],myList[[i+8]],ncol=3) 
  } else {
    grid.arrange(myList[[i]],myList[[i+1]],myList[[i+2]],ncol=3,nrow=3)
  }
}
dev.off()


#Associations between NO2 and Firmicutes, O3 Bacteroidetes, O3 and Actinobacteria, O3 and Proteobacteria
plot1<-getPlotForOzone(phylum,pvalResultPhylum,rResultPhylum,"Bacteroidetes",regressionLine=TRUE)
plot2<-getPlotForOzone(phylum,pvalResultPhylum,rResultPhylum,"Actinobacteria",regressionLine=TRUE)
plot3<-getPlotForOzone(phylum,pvalResultPhylum,rResultPhylum,"Proteobacteria",regressionLine=TRUE)
plot3<-getPlotForOzone(phylum,pvalResultPhylum,rResultPhylum,"Proteobacteria",regressionLine=TRUE)
plot4<-getPlotForNo2(phylum,pvalResultPhylum,rResultPhylum,"Firmicutes",regressionLine=TRUE)

#png("Kraken_phyla.png", units="in", width=5, height=5,res=300)
pdf("Kraken_phyla.pdf")
plot_grid(plot1,plot2,plot3,plot4,ncol=2,scale = 0.9)
dev.off()

##Associations with Bacteroidetes at genus and species level:

genus<-cbind(getTable("genus")[[1]],getTable("genus")[[2]])
pvalResultGenus<-getResult("genus")[[1]]
rResultGenus<-getResult("genus")[[2]]
plot5<-getPlotForOzone(genus,pvalResultGenus,rResultGenus,"Bacteroides",regressionLine=TRUE)

species<-cbind(getTable("species")[[1]],getTable("species")[[2]])
pvalResultSpecies<-getResult("species")[[1]]
rResultSpecies<-getResult("species")[[2]]
plot6<-getPlotForOzone(species,pvalResultSpecies,rResultSpecies,"Bacteroides.caecimuris",regressionLine=TRUE)

png("Kraken_Bacteroides.png", units="in", width=5, height=5,res=300)
#pdf("Kraken_Bacteroides.pdf")
plot_grid(plot5,plot6,ncol=2,nrow=2)
dev.off()

#Scatter plots for significant associations between species and o3

pvalResultSpeciesSig<-pvalResultSpecies[pvalResultSpecies$Prior.Year.O3.24.hr<0.05,]
rResultSpeciesSig<-rResultSpecies[pvalResultSpecies$Prior.Year.O3.24.hr<0.05,]

myListO3<-list()
theme_set(theme_classic(base_size = 7))
for (i in 1:nrow(pvalResultSpeciesSig)){
  
  taxName<-rownames(pvalResultSpeciesSig)[i]
  plot<-getPlotForOzone(species,pvalResultSpeciesSig,rResultSpeciesSig,taxName,regressionLine = TRUE)
  myListO3[[i]]<-plot
  
}

index<-vector()
i<-1
x<-1
while(x<147){
  index[i]<-x
  x<-x+9
  i<-i+1
}

pdf("significantSpeciesKraken.pdf")
for (i in index){
  if(i<145){
    grid.arrange(myListO3[[i]],myListO3[[i+1]],myListO3[[i+2]],myListO3[[i+3]],myListO3[[i+4]],myListO3[[i+5]],myListO3[[i+6]],myListO3[[i+7]],myListO3[[i+8]],ncol=3) 
  } else {
    grid.arrange(myListO3[[i]],myListO3[[i+1]],myListO3[[i+2]],ncol=3,nrow=3) 
  }
}

#Scatter plots for species and NO2
pvalResultSpeciesSig<-pvalResultSpecies[pvalResultSpecies$Prior.Year.NO2<0.05,]
rResultSpeciesSig<-rResultSpecies[pvalResultSpecies$Prior.Year.NO2<0.05,]

myListNO2<-list()
for (i in 1:nrow(pvalResultSpeciesSig)){
  
  taxName<-rownames(pvalResultSpeciesSig)[i]
  plot<-getPlotForNo2(species,pvalResultSpeciesSig,rResultSpeciesSig,taxName,regressionLine = TRUE)
  myListNO2[[i]]<-plot
  
}

grid.arrange(myListNO2[[1]],myListNO2[[2]],myListNO2[[3]],myListNO2[[4]],myListNO2[[5]],ncol=3,nrow=3) 

#Scatter plots for species and Nox
pvalResultSpeciesSig<-pvalResultSpecies[pvalResultSpecies$Prior.Year.Tot.NOx<0.05,]
rResultSpeciesSig<-rResultSpecies[pvalResultSpecies$Prior.Year.Tot.NOx<0.05,]

myListNOx<-list()
for (i in 1:nrow(pvalResultSpeciesSig)){
  
  taxName<-rownames(pvalResultSpeciesSig)[i]
  plot<-getPlotForNox(species,pvalResultSpeciesSig,rResultSpeciesSig,taxName,regressionLine = TRUE)
  myListNOx[[i]]<-plot
}

grid.arrange(myListNOx[[1]],myListNOx[[2]],myListNOx[[3]],myListNOx[[4]],myListNOx[[5]],myListNOx[[6]],myListNOx[[7]],myListNOx[[8]],myListNOx[[9]],ncol=3) 
grid.arrange(myListNOx[[10]],ncol=3,nrow=3)

dev.off()

















