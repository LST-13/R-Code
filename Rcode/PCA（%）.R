#读取数据
setwd('C:/Users/10930/Desktop/R-practise/β-diversity')
otu_raw <- read.csv('phoD_Flat.csv',header=T,row.names = 1,check.names = F)
#对数据进行转置
otu <- t(otu_raw)
#pca分析
pca <- prcomp(otu,scal=TRUE)#scal进行标准化处理
#获取pca结果概览
pca_sum <- summary(pca)
#提取出第一主成分以及第二主成分上的投影坐标
#kaiser主张将特征值平均值小于1的成分放弃，只保留特征值大于1的成分
pc12 <- pca$x[,1:2]#x为主成分的坐标
#计算每个主成分的解释度
pc <- pca_sum$importance[2,]*100
#绘图
#加载作图所需要R包
library(ggplot2)
#pc12原来是matrix,转化为data.frame
pc12 <- as.data.frame(pc12)
#给pc12添加sample变量
pc12$samples <- row.names(pc12)
head(pc12)
#绘图
p <- ggplot(pc12,aes(x=PC1,y=PC2))+ #制定数据、x轴、y轴
  geom_point(size=3)+ #绘制点图并设定大小
  theme_bw()+ #使用黑白主题
  geom_text(aes(label=samples,y=PC2),size=4,vjust=1.5)#添加数据点的标签
p
#读入分组文件
group <- read.csv('design.csv',header=T,check.names = F)
#修改列名
colnames(group) <- c('samples','group')
#将绘图数据和分组合并
df <- merge(pc12,group,by='samples')
p <- ggplot(df,aes(x=PC1,y=PC2,colour=group))+ #指定数据,x轴与y轴,颜色的映射
  geom_point(size=3)+ #绘制点图并设定大小
  geom_text(aes(label=samples,y=PC2+0.1),size=4,vjust=0) #添加数据点的标签
p
p <- p+theme_bw()+ #使用黑白主题
  theme(axis.title.x=element_text(size=15,family='sans'),#修改x轴标题文本
        axis.title.y=element_text(size=15,family='sans',angle=90),#修改y轴标题文本
        axis.text.y=element_text(size=12,family='sans'),#修改y轴刻度标签文本
        axis.text.x=element_text(size=12,family='sans'),#修改x轴刻度标签文本
        panel.grid=element_blank()#隐藏网格线
  )
p
p <- p+xlab(paste0('PC1(',pc[1],'%)'))+ylab(paste0('PC2(',pc[2],'%)'))
p