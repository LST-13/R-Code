#读取 OTU 丰度表
setwd("D:/R/PA")
otu <- read.csv("BY.csv",header = T,row.names = 1)
otu <- t(otu)

#计算与 Beta 多样性有关的群落相异指数，例如使用 vegan 包计算 Bray-curtis 距离，详情加载 vegan 包后 ?vegdist
dis <- vegan::vegdist(otu, method = 'bray')

#以矩阵形式输出
dis <- as.matrix(dis)
write.table(dis, 'Bray-curtis.txt', sep = '\t', col.names = NA, quote = FALSE)

#读取 Bray-curtis 距离矩阵

dis <- read.delim('Bray-curtis.txt', row.names = 1)


#读取样本分组信息
#group <- read.csv("designg3.csv",header = T,row.names = 1)
group <- read.delim('group.txt', stringsAsFactors = FALSE)

##例如，比较 Env1、Env2、Env3 三组之间，群落的 Beta 多样性差异
#根据分组获得组内距离矩阵
CK <- subset(group,GROUP == 'CK')$sample
dis_CK <- dis[CK,CK]

CPR <- subset(group, GROUP == 'CPR')$sample
dis_CPR <- dis[CPR,CPR]

CPG <- subset(group, GROUP == 'CPG')$sample
dis_CPG <- dis[CPG,CPG]

SPG <- subset(group, GROUP == 'SPG')$sample
dis_SPG <- dis[SPG,SPG]

SPR <- subset(group, GROUP == 'SPR')$sample
dis_SPR <- dis[SPR,SPR]


#将矩阵转化为向量，以便用于作图和统计
dis_CK <- as.vector(as.dist(dis_CK))
dis_CPR <- as.vector(as.dist(dis_CPR))
dis_CPG <- as.vector(as.dist(dis_CPG))
dis_SPG <- as.vector(as.dist(dis_SPG))
dis_SPR <- as.vector(as.dist(dis_SPR))

#构建作图数据集
dat <- data.frame(
  dis = c(dis_CK, dis_CPR, dis_CPG, dis_SPG,dis_SPR),
  group = factor(c(
    rep('CK', length(dis_CK)), 
    rep('CPG', length(dis_CPG)), 
    rep('CPR', length(dis_CPR)),
    rep('SPG', length(dis_SPG)),
    rep('SPR', length(dis_SPR))
  ), levels = c('CK', 'CPR', 'CPG','SPR','SPG'))
)

#使用 ggplot2 绘制各组内 Bray-curtis 距离指数分布的箱线图
library(ggplot2)

p <- ggplot(dat, aes(group, dis)) +
  geom_boxplot(aes(fill = group), width = 0.6) +
  scale_fill_manual(values = c("#808080","#1597A5","#FFC24B","#99CCFF","#FEB3AE",'#D6E8F2','#8491B4FF', 'D1EBC6', '#4DBBD5FF','#CD5B45', '#228B22', '#00688B')) +
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = 'black'), legend.position = 'none') +
  labs(x = NULL, y = 'Bray-Curtis dissimilarity\n')

p

write.csv(dat,file="beta.csv")

#三组的整体差异分析，使用 Kruskal-Wallis Test 执行，详情 ?kruskal.test
kruskal.test(dis~group, data = dat)

#如果整体显著再进行两两分组的比较，使用 Wilcoxon 秩和检验执行双侧检验，详情 ?wilcox.test
wilcox.test(dis_CK, dis_CPR, alternative = 'two.sided')
wilcox.test(dis_CK, dis_CPG, alternative = 'two.sided')
wilcox.test(dis_CK, dis_SPR, alternative = 'two.sided')
wilcox.test(dis_CK, dis_SPG, alternative = 'two.sided')
wilcox.test(dis_CPR, dis_SPR, alternative = 'two.sided')
wilcox.test(dis_CPG, dis_SPR, alternative = 'two.sided')
wilcox.test(dis_CPR, dis_CPG, alternative = 'two.sided')
wilcox.test(dis_CPG, dis_SPG, alternative = 'two.sided')


#考虑到 Wilcoxon 秩和检验体现了中位数的差异，因此计算三组数据的中位数以评估 Beta 多样性的高低水平
median(dis_CK)
median(dis_CPR)
median(dis_CPG)
median(dis_SPG)
median(dis_SPR)
#基于上述统计结果，判断好组间差异后，将差异分析结果添加到箱线图中
p +
  annotate('text', label = 'Kruskal-Wallis Test', x = 1, y = 0.56, size = 3) +
  annotate('text', label = sprintf('italic(P) < %.3f', 0.001), x = 1, y = 0.53, size = 3, parse = TRUE) +
  annotate('text', label = 'c', x = 1, y = max(dis_env1)+0.05, size = 3) +
  annotate('text', label = 'a', x = 2, y = max(dis_env2)+0.05, size = 3) +
  annotate('text', label = 'b', x = 3, y = max(dis_env3)+0.05, size = 3)