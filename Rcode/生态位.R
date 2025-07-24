rm(list=ls())
setwd("D:/R/NPK")
dune<-read.csv("NPKg.csv",row.names = 1)

#site="https://mirrors.tuna.tsinghua.edu.cn/CRAN"
#install.packages(c("EcolUtils"), repos=site)
#在 R 中，可使用 spaa 包的函数 niche.width() 计算生态位宽度指数，详情加载 spaa 包后 ?niche.width
#install.packages("spaa")
library(spaa)

#这里以 Levins 生态位宽度指数为例，Shannon 生态位宽度指数可通过 method 参数修改
niche_width <- niche.width(dune, method = 'levins')
niche_width

#画图展示分布
boxplot(unlist(niche_width), ylab = 'niche breadth index')
write.csv(cbind(sample=c(rownames(niche_width)),niche_width),'stwNPKsp.csv',row.names = F)

#计算物种间的生态位重叠
niche.overlap(dune, method = c("levins"))
#计算两个物种间的生态位重叠,就是只选了第二列和第三列的物种
niche.overlap.pair(dune[,2],dune[,3],method = c("levins"))
#生态位宽度置信区间的自展分析
niche.overlap.boot(dune,method = c("levins"),times = 1000, quant = c(0.025, 0.975))

