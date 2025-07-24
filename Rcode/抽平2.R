#首先我们需要加载vegan包
library(vegan) 
#加载工作目录，这里需要更换为你自己的
setwd("D:/R/be")
#加载otu表
otu <- read.csv(file="yy_otu.csv",header=T,check.names=FALSE ,row.names=1)
#otu = read.csv('yy_otu.csv', header=T, sep="\t", quote = "", row.names=1, comment.char="",stringsAsFactors = FALSE) 
#求和查看每个样本的和
colSums(otu)
#使用该代码进行抽平
otu_Flattening = as.data.frame(t(rrarefy(t(otu), min(colSums(otu)))))
#查看抽平后的每个样本的和
colSums(otu_Flattening)
#将抽平后的otu表保存到该工作目录，准备后面的多样性分析
write.table (otu_Flattening, file ="otu_Flattening.csv",sep =",", quote =FALSE) #结果导出
