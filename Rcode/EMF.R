setwd("D:/R/LH")

#调用R包
library(tidyverse)
#读取数据
data <- read.csv("lhsx.csv",header = T,row.names = 1)
#先进行归一化处理
mydata <- scale(data)
head(mydata)
#Z分数的计算
Zcore <- (mydata-mean(mydata))/sd(mydata)
head(Zcore)
#保存Zcore
#write.csv(Zcore,"Zcore.csv")
#计算EMF
Zcore <- as.data.frame(Zcore)
var <- c("VW","VWC","TN","TC", "C/N","TP", "NH4-N", "NO3-N","pH","EC","URE","SC","PHO","MY","Clay","Silt","Sand")
Zcore$emf <- apply(select(Zcore,var),1,mean)
head(Zcore)
#保存EMF
write.csv(Zcore,"EMF计算结果.csv")
#绘制箱线图
library(ggplot2)
#设置新罗马字体
windowsFonts(A=windowsFont("Times New Roman"),
             B=windowsFont("Arial"))
#读取绘图数据
Group<- read.csv("group.csv",header = T,row.names = 1)
# Default plot
e <- ggplot(Group, aes(x = Group, y = emf))
# Change fill color by group (dose))
e + geom_boxplot(aes(fill = Group),alpha=0.5) +
  scale_fill_manual(values = c("#E64B35FF", "#4DBBD5FF", "#00A087FF","#F39B7FFF","#91D1C2FF", "#8491B4FF"))+#定义填充颜色
  geom_jitter(aes(colour=Group),
              position = position_jitter(0.25),size=5,alpha=0.8)+
  scale_colour_manual(values = c("#E64B35FF", "#4DBBD5FF", "#00A087FF","#F39B7FFF","#91D1C2FF", "#8491B4FF"))+
  theme_bw()+scale_y_continuous(limits = c(-3,2),expand = c(0,0))+
  theme(axis.ticks.length=unit(-0.25, "cm"), 
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")) )+
  theme(text=element_text(family="A",size=20))
