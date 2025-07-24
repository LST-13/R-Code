library (reshape2)
library(ggplot2)
library(ggprism)
library(plyr)
getwd()
setwd("D:/R/WZ")
otu <- read.csv('xdzcg.csv',header=T,check.names = F,row.names=1)
#计算相对丰度
#otu<-apply(otu,2,function(x) x/sum(x))
#write.csv(otu,file="xdzcg.csv")

#稀有物种（RT）
otu_ART <- otu[apply(otu, 1, function(x) max(x)<0.0001), ]
write.csv(otu_ART,file = "otu_ART.CSV")#写出重要性数据到默认路径

#丰富物种（AT）
otu_AAT <- otu[apply(otu, 1, function(x) min(x)>0.01), ]
write.csv(otu_AAT,file = "otu_AAT.CSV")#写出重要性数据到默认路径
#中等丰度物种（MT）
otu_MT <- otu[apply(otu, 1, function(x) max(x)<0.01 & min(x)>0.0001), ]
write.csv(otu_MT,file = "otu_MT.CSV")#写出重要性数据到默认路径

#条件稀有物种（CRT）
otu_CRT <- otu[apply(otu, 1, function(x) max(x)<=0.01 & min(x)<0.0001), ]
otu_CRT <- otu_CRT[which(! rownames(otu_CRT) %in% rownames(otu_ART)), ]
write.csv(otu_CRT,file = "otu_CRT.CSV")#写出重要性数据到默认路径

#条件丰富物种（CAT）
otu_CAT <- otu[apply(otu, 1, function(x) min(x)>=0.0001 & max(x)>0.01), ]
otu_CAT <- otu_CAT[which(! rownames(otu_CAT) %in% rownames(otu_AAT)), ]
write.csv(otu_CAT,file = "otu_CAT.CSV")#写出重要性数据到默认路径

#条件稀有或丰富物种（CRAT）丰度跨越从稀有（最低丰度 ≤0.1%）到丰富（最高丰度 ≥1%）的 OTU
otu_CRAT <- otu[apply(otu, 1, function(x) min(x) <= 0.0001 & max(x) >= 0.01), ]
write.csv(otu_CRT,file = "otu_CRAT.CSV")#写出重要性数据到默认路径

otu[which(rownames(otu) %in% rownames(otu_ART)),'taxa'] <- 'ART'
otu[which(rownames(otu) %in% rownames(otu_AAT)),'taxa'] <- 'AAT'
otu[which(rownames(otu) %in% rownames(otu_MT)),'taxa'] <- 'MT'
otu[which(rownames(otu) %in% rownames(otu_CRT)),'taxa'] <- 'CRT'
otu[which(rownames(otu) %in% rownames(otu_CAT)),'taxa'] <- 'CAT'
otu[which(rownames(otu) %in% rownames(otu_CRAT)),'taxa'] <- 'CRAT'

write.csv(otu,file = "otu_CLASS.csv")#写出重要性数据到默认路径

#作图展示各样本中，丰富、稀有、条件稀有或丰富微生物类群的占比
for (i in 1:(ncol(otu)-1)) otu[[i]] <- ifelse(as.character(otu[[i]]) == '0', NA, otu[[ncol(otu)]])
otu_stat <- data.frame(apply(otu[-ncol(otu)], 2, table))
otu_stat$taxa <- rownames(otu_stat)
otu_stat <- reshape2::melt(otu_stat, id = 'taxa')
write.csv(otu_stat,file = "otu_stat.csv")#写出重要性数据到默认路径

library(ggplot2)

ggplot(otu_stat, aes(variable, value, fill = taxa)) +
  geom_col(position = 'fill', width = 0.6) +
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'gray', fill = 'transparent')) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = '样本', y = '不同丰富或稀有类群的占比')
