#读取 OTU 丰度表
setwd("D:/R/WZ")
otu<-read.csv("zcg.csv",row.names = 1)

#计算各 OTU 的变异度
ai <- abs(otu-apply(otu, 1, mean))/apply(otu, 1, sd)

#由于此时计算的是单个样本的 AVD，即 k=1
avd <- colSums(ai)/(1*nrow(otu))

#读取分组，合并数据并简单可视化
group<-read.csv("groupzcg.csv",row.names = 1)
group$AVD <- avd
group

write.csv(cbind(sample=c(rownames(group)),group),'AVD.csv',row.names = F)

library(ggpubr)

ggboxplot(group, x = 'group', y = 'AVD', fill = 'group', 
          color = 'gray30', width = 0.6, size = 1, legend = 'right') +
  scale_fill_manual(values = c('#E7B800', '#00AFBB','red','blue','yellow')) +
  labs(x = '', y = 'AVD', fill = '')
