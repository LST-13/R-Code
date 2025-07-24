#PCOA
rm(list=ls())
setwd('D:/R/NC')
#安装所需R包
install.packages("vegan")
install.packages("ggplot2")
install.packages("ggprism")
#加载包
library(vegan)#计算距离时需要的包
library(ggplot2)
library(ggprism)
#数据处理及PCoA分析
otu <- read.csv(file="yyg.csv",header=T,check.names=FALSE ,row.names=1)
otu <- t(otu)
otu.distance <- vegdist(otu)
PCoA <- cmdscale (otu.distance,eig=TRUE)
pc12 <- PCoA$points[,1:2]
pc <- round(PCoA$eig/sum(PCoA$eig)*100,digits=2)#解释度
pc12 <- as.data.frame(pc12)
pc12$samples <- row.names(pc12)
group <- read.csv("designg3.csv", header=T)
colnames(group) <- c("samples","GROUP")
df <- merge(pc12,group,by="samples")
head(df)
#绘图
ggplot(df,aes(V1, V2,color=GROUP,size=3))+#指定数据、X轴、Y轴
  geom_point()+
  theme_bw()


#使用vegan包中的anosim函数进行anosim分析
df_anosim <- anosim(otu.distance,df$GROUP,permutations = 999)#数据也可以是原始otu数据
#df_anosim <- anosim(otu,df$group,permutations = 999)
#整理出作图数据
df1<-data.frame(
  x=df_anosim$class.vec,
  y=df_anosim$dis.rank
)
#绘图
ggplot(df1,aes(x=x,y=y))+
  stat_boxplot(geom = "errorbar", width=0.1,size=0.8)+#添加误差线,注意位置，放到最后则这条先不会被箱体覆盖
  geom_boxplot(aes(fill=x), 
               outlier.colour="white",size=0.5)+
  theme(panel.background =element_blank(), 
        axis.line=element_line(),
        legend.position="none",plot.title = element_text(size=14))+
  scale_fill_manual(values=c("#C9C9C9","#808080","#1597A5","#FFC24B","#99CCFF","#FEB3AE","#333333","#99CCCE","#CC9999","#FFF6F6","#FF7B7B","#EEDFCC","#EEE9BF","#EED5D2","#BCD2EE","#C1FFC1","red","#1597A5","#FFC24B","#FEB3AE","#77FFEE"))+ #指定颜色
  ggtitle("Bray-Curtis Anosim")+
  theme_prism(palette = "candy_bright",
              base_fontface = "plain",
              base_family = "serif", 
              base_size = 14,  
              base_line_size = 0.8, 
              axis_text_angle = 45)+
  theme(legend.position = 'none')+
  labs(x = paste("R=",df_anosim$statistic,", ","p=", df_anosim$signif),
       y = "Rank of Distance (Bray_Curtis)")
#MRPP
MRPP <- mrpp(otu.distance,df$GROUP,permutations = 999)
MRPP
#Adonis
Adonis <- adonis2(otu.distance~GROUP,data=df,
                  distance = "bray",
                  permutations = 999)
Adonis

