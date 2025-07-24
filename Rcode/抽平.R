rm(list=ls())
#安装包 
#如果你没有安装过phyloseq包，你可以使用下列代码进行安装该包 if(!requireNamespace("BiocManager")){ install.packages("BiocManager") } BiocManager::install("phyloseq") 
#加载包1 
#library(BiocManager)
#BiocManager::install("phyloseq")
library(phyloseq) 
#设置一个随机种子便于重复 
set.seed(123) 
setwd('D:/R/be')
otu <- read.csv('yy_otu.csv',header=T,row.names = 1,check.names = F,comment.char="",stringsAsFactors = FALSE)
otu1 <- otu
#大于0的赋值为1
otu1[otu1>0] <- 1
#上一步大于0变成1后，下一步合算1多少个，保留下来
otu <- otu[which(rowSums(otu1) >=9 ), ]
otu <- otu[which(rowSums(otu) >=5 ), ]
otu <- otu_table(otu, taxa_are_rows = T)
otu = phyloseq(otu) 
#注意，该方法会自动去除一些低丰度的otu 
otu_Flattening1 = rarefy_even_depth(otu,replace = TRUE)
#8OTUs were removed because they are no longer present in any sample after random subsampling 
#查看抽平前后的变化 
sample_sums(otu) 
sample_sums(otu_Flattening1)
#提取抽平后的otu表格 
gcd_Flattening1 = as.data.frame(otu_Flattening1@.Data ) 
#将抽平后的otu表保存到该工作目录，准备后面的多样性分析
write.csv(gcd_Flattening1, file ="gcd_Flat.csv") 