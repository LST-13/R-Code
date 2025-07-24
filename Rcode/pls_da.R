rm(list=ls())#clear Global Environment
setwd('D:\\R\\be')#设置工作路径
#加载包
#if (!require("BiocManager", quietly = TRUE))
#install.packages("BiocManager")

#BiocManager::install("mixOmics")
library(mixOmics)#用于偏最小二乘判别分析的包
library(ggplot2)#绘图包
otu_raw<- read.csv(file="yy_Flat.csv",header=T,check.names=FALSE ,row.names=1)
head(otu_raw)
#分组数据
group <- read.csv("design.csv", header=T)
head(group)
#由于排序分析函数所需数据格式原因，需要对数据进行转置
otu <- t(otu_raw)
#计算PLS-DA
df_plsda <- plsda(otu, group$GROUP, ncomp = 2)

#简单绘图
plotIndiv(df_plsda , comp = c(1,2),
          group = group$GROUP, style = 'ggplot2',ellipse = T, 
          size.xlabel = 20, size.ylabel = 20, size.axis = 20, pch = 16, cex = 5)
df <- unclass(df_plsda)
#提取坐标值
df1 = as.data.frame(df$variates$X)
df1$sample <- rownames(df1)
attach(df1)
df1 <- merge(df1,group)
#提取解释度
explain = df$prop_expl_var$X
x_lable <- round(explain[1],digits=3)
y_lable <- round(explain[2],digits=3)
#绘图
col=c("#1597A5","#FFC24B","#FEB3AE","#B1A4C0","#479E9B","#BB2BA0")
p1<-ggplot(df1,aes(x=comp1,y=comp2,color=GROUP))+#指定数据、X轴、Y轴，颜色
  theme_bw()+#主题设置
  geom_point(size=1.8)+#绘制点图并设定大小
  theme(panel.grid = element_blank())+
  geom_vline(xintercept = 0,lty="dashed")+
  geom_hline(yintercept = 0,lty="dashed")+#图中虚线
  geom_text(aes(label=sample,y=comp2+0.4,x=comp1+0.5, vjust=0,size=3.5))+#添加数据点的标签
  # guides(color=guide_legend(title=NULL))+#去除图例标题
  labs(x=paste0("P1 (",x_lable*100,"%)"),
       y=paste0("P2 (",y_lable*100,"%)"))+#将x、y轴标题改为贡献度
  stat_ellipse(data=df1,
               geom = "polygon",level = 0.95,
               linetype = 2,size=0.5,
               aes(fill=GROUP),
               alpha=0.2,
               show.legend = T)+
  scale_color_manual(values = col) +#点的颜色设置
  scale_fill_manual(values = c("#1597A5","#FFC24B","#FEB3AE","#B1A4C0","#479E9B","#BB2BA0"))+
  theme(axis.title.x=element_text(size=12),#修改X轴标题文本
        axis.title.y=element_text(size=12,angle=90),#修改y轴标题文本
        axis.text.y=element_text(size=10),#修改x轴刻度标签文本
        axis.text.x=element_text(size=10),#修改y轴刻度标签文本
        panel.grid=element_blank())#隐藏网格线
p1


rm(list=ls())#clear Global Environment
setwd('D:\\桌面\\PLSDA分析')#设置工作路径
#加载包
library(ropls)#用于偏最小二乘判别分析的包
library(ggplot2)#绘图包
library(ggforce)

otu_raw <- read.table(file="otu.txt",sep="\t",header=T,check.names=FALSE ,row.names=1)
head(otu_raw)
#分组数据
group <- read.table(file="group.txt",sep="\t",header=T,check.names=FALSE ,row.names=1)
head(group)
#由于排序分析函数所需数据格式原因，需要对数据进行转置
otu <- t(otu_raw)
df1_plsda <- opls(otu, group$group, orthoI = 0)#不指定或orthoI = 0时，执行PLS
#提取坐标值
data <- as.data.frame(df1_plsda@scoreMN)
data$group = group$group
data$samples = rownames(data)
#提取解释度
x_lab <- df1_plsda@modelDF[1, "R2X"] * 100
y_lab <- df1_plsda@modelDF[2, "R2X"] * 100
#绘图
col=c("#1597A5","#FFC24B","#FEB3AE")
p2 <- ggplot(data,aes(x=p1,y=p2,
                      color=group,shape=group))+#指定数据、X轴、Y轴，颜色
  theme_bw()+#主题设置
  geom_ellipse(aes(x0 = 0, y0 = 0, a = 10, b = 6, angle = 0),color="grey",size=0.5) +
  geom_ellipse(aes(x0 = 0, y0 = 0, a = 8, b = 4, angle = 0),color="grey",size=0.5)+
  geom_ellipse(aes(x0 = 0, y0 = 0, a = 6, b = 2, angle = 0),color="grey",size=0.5)+
  coord_fixed()+#图中椭圆的绘制代码，不需要可删除
  geom_point(size=1.8)+#绘制点图并设定大小
  theme(panel.grid = element_blank())+
  geom_vline(xintercept = 0,lty="dashed",color="red")+
  geom_hline(yintercept = 0,lty="dashed",color="red")+#图中虚线
  geom_text(aes(label=samples, y=p2+0.4,x=p1+0.5,  vjust=0),size=3.5)+#添加数据点的标签
  # guides(color=guide_legend(title=NULL))+#去除图例标题
  labs(x=paste0("P1 (",x_lab,"%)"),
       y=paste0("P2 (",y_lab,"%)"))+#将x、y轴标题改为贡献度
  stat_ellipse(data=data,
               geom = "polygon",level = 0.95,
               linetype = 2,size=0.5,
               aes(fill=group),
               alpha=0.2,
               show.legend = T)+
  scale_color_manual(values = col) +#点的颜色设置
  scale_fill_manual(values = c("#1597A5","#FFC24B","#FEB3AE"))+
  theme(axis.title.x=element_text(size=12),#修改X轴标题文本
        axis.title.y=element_text(size=12,angle=90),#修改y轴标题文本
        axis.text.y=element_text(size=10),#修改x轴刻度标签文本
        axis.text.x=element_text(size=10),#修改y轴刻度标签文本
        panel.grid=element_blank())#隐藏网格线

p2
#提取VIP值
data_VIP <- df1_plsda@vipVn
data_VIP_select <- data_VIP[data_VIP > 1] #阈值通常设为1
#将VIP值与原始数据合并
data_VIP_select <- cbind(otu_raw[names(data_VIP_select), ], data_VIP_select)
names(data_VIP_select)[13] <- "VIP"
#排序
data_VIP_select <- data_VIP_select[order(data_VIP_select$VIP, decreasing = TRUE), ]
# plot(df1_plsda, typeVc = "x-loading") #展示前10个
head(data_VIP_select) 
