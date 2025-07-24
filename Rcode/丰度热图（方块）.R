#丰度热图
otu_tax<-read.csv("D://R/Nre/hand32.csv",row.names = 1)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(dplyr)
pp<-otu_tax[1:15,1:5]
rownames(pp)<-c(paste("gene",1:15))
io<-pp%>% mutate(pp=row.names(.))%>%melt()#转化为ggplot画图需要的长列表
#设置颜色
color <- colorRampPalette(brewer.pal(8,"YlGn"))#设置连续颜色
#绘制热图
ggplot(io,aes(variable,pp,fill=value))+
  geom_tile(color="grey32",fill="white",size=0.7)+
  geom_point(pch=22,size=22)+
  scale_fill_gradientn(colours =colorRampPalette(c("gray92","#D35800"))(111))+
  labs(x = NULL,y = NULL,fill="Gene")+
  theme_minimal()+
  theme(axis.text=element_text(colour='black',size=9))+
  theme(axis.text.x =element_text(angle =90,hjust =0.5,vjust = 0.5,size=8))

