rm(list=ls())
library(vegan)
library(picante)
library(ggpubr)
library(RColorBrewer)
library(dplyr)
getwd()
setwd('D:/R/YYZ')
#setwd("D:/my data/R-practise/α-diversity")
otu <- read.csv('yyg.csv',header=T,row.names = 1,check.names = F)
Shannon <- diversity(otu,index='shannon',MARGIN=2,base=exp(1))
Simpson <- diversity(otu,index='simpson',MARGIN=2,base=exp(1))
Richness <- specnumber(otu,MARGIN=2)
index <- as.data.frame(cbind(Shannon,Simpson,Richness))
totu <- t(otu)#转置
totu <- ceiling(as.data.frame(t(otu)))
obs_chao_ace <- t(estimateR(totu))
obs_chao_ace <- obs_chao_ace[rownames(index),]#行名的统一
index$Chao <- obs_chao_ace[,2]
index$Ace <- obs_chao_ace[,4]
index$Sobs <- obs_chao_ace[,1]
index$Pielou <- Shannon/log(Richness,2)
index$Goods_coverage <- 1-colSums(otu==1)/colSums(otu)
tree <- read.tree('otus_gcd.tre')
index$PD_whole_tree <- pd(totu,tree,include.root=FALSE)[,1]
write.csv(cbind(sample=c(rownames(index)),index),'yyzdiversityg3.csv',row.names = F)

