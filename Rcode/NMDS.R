#NMDS
rm(list=ls())#clear Global Environment
setwd('D:\\NMDS')#设置工作路径
#安装所需R包
install.packages("vegan")
install.packages("ggplot2")
#加载包
library(vegan)#计算距离时需要的包
library(ggplot2)#绘图包
#读取数据，一般所需是数据行名为样本名、列名为OTUxxx的数据表
otu_raw <- read.csv(file="BY.csv",header=T,check.names=FALSE ,row.names=1)
#由于排序分析函数所需数据格式原因，需要对数据进行转置
otu <- t(otu_raw)
otu.distance <- vegdist(otu, method = 'bray')
#NMDS排序分析——vegan包中的metaMDS函数
df_nmds <- metaMDS(otu.distance, k = 2)
#结果查看——关注stress、points及species三个指标
summary(df_nmds)
#应力函数值（<=0.2合理）
df_nmds_stress <- df_nmds$stress
df_nmds_stress
#检查观测值非相似性与排序距离之间的关系——没有点分布在线段较远位置表示该数据可以使用NMDS分析
stressplot(df_nmds)
#提取作图数据
df_points <- as.data.frame(df_nmds$points)
#添加samp1es变量
df_points$samples <- row.names(df_points)
#修改列名
names(df_points)[1:2] <- c('NMDS1', 'NMDS2')
head(df_points)
p <- ggplot(df_points,aes(x=NMDS1, y=NMDS2))+#指定数据、X轴、Y轴
  geom_point(size=3)+#绘制点图并设定大小
  theme_bw()#主题
p
#读入分组文件
group <- read.csv("designg3.csv", header=T)
#修改列名
colnames(group) <- c("samples","group")
#将绘图数据和分组合并
df <- merge(df_points,group,by="samples")
head(df)
#使用ggplot2包绘图
color=c("#808080","#1597A5","#FFC24B","#FEB3AE","#77FFEE")#颜色变量
p1<-ggplot(data=df,aes(x=NMDS1,y=NMDS2))+#指定数据、X轴、Y轴，颜色
  theme_bw()+#主题设置
  geom_point(aes(color = group), shape = 19, size=3)+#绘制点图并设定大小
  theme(panel.grid = element_blank())+
  geom_vline(xintercept = 0,lty="dashed", size = 1, color = 'grey50')+
  geom_hline(yintercept = 0,lty="dashed", size = 1, color = 'grey50')+#图中虚线
  geom_text(aes(label=samples, y=NMDS2+0.03,x=NMDS1+0.03,
                vjust=0, color = group),size=3.5, show.legend = F)+#添加数据点的标签
  stat_ellipse(data=df,
               geom = "polygon",level=0.95,
               linetype = 2,size=0.5,
               aes(fill=group),
               alpha=0.2)+
  scale_color_manual(values = color) +#点的颜色设置
  scale_fill_manual(values = color)+#椭圆颜色
  theme(axis.title.x=element_text(size=12),#修改X轴标题文本
        axis.title.y=element_text(size=12,angle=90),#修改y轴标题文本
        axis.text.y=element_text(size=10),#修改x轴刻度标签文本
        axis.text.x=element_text(size=10),#修改y轴刻度标签文本
        panel.grid=element_blank())+#隐藏网格线
  ggtitle(paste('Stress=',round(df_nmds_stress, 3)))#添加应力函数值
p1
#加载包，对组间进行统计检验以及组合图的拼接
library(ggpubr)
library(ggsignif)
# 绘制y轴为PC2值的分组箱线图
p2 <- ggplot(df,aes(x=group,y=NMDS2))+
  stat_boxplot(geom = "errorbar", width=0.1,size=0.5)+
  geom_boxplot(aes(fill=group), 
               outlier.colour="white",size=0.5)+
  theme(panel.background =element_blank(), 
        axis.line=element_line(color = "white"),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'none')+
  xlab("") + ylab("")+
  scale_fill_manual(values=c("#1597A5","#FFC24B","#FEB3AE"))+
  geom_signif(comparisons = list(c("A","B"),
                                 c("A","C"),
                                 c("B","C")),
              map_signif_level = T, 
              test = t.test,
              y_position = c(0.3,0.4,0.35),
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0)),
              size=0.8,color="black")
p2
# 绘制y轴为PC1值的分组箱线图
p3 <- ggplot(df,aes(x=group,y=NMDS1))+
  stat_boxplot(geom = "errorbar", width=0.1,size=0.5)+
  coord_flip()+
  geom_boxplot(aes(fill=group), 
               outlier.colour="white",size=0.5)+
  theme(panel.background =element_blank(), 
        axis.line=element_line(color = "white"),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'none')+
  xlab("") + ylab("")+
  scale_fill_manual(values=c("#1597A5","#FFC24B","#FEB3AE"))+
  geom_signif(comparisons = list(c("A","B"),
                                 c("A","C"),
                                 c("B","C")),
              map_signif_level = T,
              test = t.test,
              y_position = c(0.48,0.62,0.55),
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0)),
              size=0.8,color="black")
p3

# ggpubr::ggarrange()函数对图进行拼接
ggarrange(p3, NULL, p1, p2, widths = c(5,2), heights = c(2,4), align = "hv")

