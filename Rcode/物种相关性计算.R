rm(list=ls())
library(ggplot2)  
library(vegan)
library(psych)
library(igraph)
library(base)
setwd("D:/R/NET")
otu<-read.csv("CK8.csv",row.names = 1)
## 转化数据格式
aa<-lapply(otu, function(x) as.numeric(as.character(x))) 
aa<-as.data.frame(aa)
rownames(aa)<-rownames(otu)
otu<-t(aa)
##进行相关矩阵分析
cor=corr.test(otu,use="pairwise",method="spearman",adjust="fdr",alpha=0.05)#根据spearman相关分析方法，并对p值进行矫正
r_value<-cor$r #提取相关系数
p_value<-cor$p #提取p值
#筛选p值小于0.05的相关分析结果
r_value[p_value>0.05|abs(r_value)<0.65] = 0
#将r_value保存为csv文件，gephi可视化
write.csv(r_value,file="CK8cor.csv")



