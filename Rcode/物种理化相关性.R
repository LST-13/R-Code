setwd("D:/R/LH")
#读取数据
library(ggplot2)
library(ggthemes)
df1<- read.csv("HXBYYsx.csv", row.names = 1, header=T,check.names = F)
df1<-data.frame(t(df))
#环境因子数据
df2<-read.csv("lhsx.csv", row.names = 1,  header=T, check.names = F)

#计算相关性
library(psych)
cor.result<-corr.test(df1,df2,method = "spearman")
cor.result$p
cor.result$r
#提取P值
library(tidyverse)
cor.result$p %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  pivot_longer(!rowname) %>% 
  mutate(p_value=case_when(
    value > 0.05 ~ "A",
    value >0.01 & value <= 0.05 ~ "B",
    value > 0.001 & value <= 0.01 ~ "D",
    value <= 0.001 ~ "E"
  )) -> new_df1
#绘图
ggplot()+
  geom_tile(data=new_df1,
            aes(x=rowname,y=name,fill=p_value))+
  scale_fill_manual(values = c("white","#c0c0c0",
                               "#808080","#3f3f3f"))+
  theme(legend.key = element_rect(colour="black"),
        axis.text.x = element_text(angle = 90,hjust=1,vjust=0.5))+
  coord_equal()
#叠加相关系数
cor.result$r %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  pivot_longer(!rowname) %>% 
  mutate(abs_cor=abs(value)) -> new_df2

library(paletteer)
ggplot()+
  geom_tile(data=new_df1,
            aes(x=rowname,y=name,fill=p_value))+
  scale_fill_manual(values = c("white","#c0c0c0",
                               "#808080","#3f3f3f"))+
  theme(legend.key = element_rect(colour="black"),
        axis.text.x = element_text(angle = 90,hjust=1,vjust=0.5))+
  coord_equal()+
  geom_point(data=new_df2,
             aes(x=rowname,y=name,
                 size=abs_cor*40,
                 color=value))+
  scale_color_paletteer_c(palette = "ggthemes::Classic Red-Blue")
#加网格线
ggplot()+
  geom_tile(data=new_df1,
            aes(x=rowname,y=name,fill=p_value,alpha=p_value))+
  scale_fill_manual(values = c("white","#c0c0c0",
                               "#808080","#3f3f3f"),
                    label=c(">0.05",
                            "0.01~0.05",
                            "0.001~0.01",
                            "<0.01"))+
  scale_alpha_manual(values = c(0,1,1,1))+
  guides(alpha=F)+
  theme_bw()+
  theme(legend.key = element_rect(colour="black"),
        axis.text.x = element_text(angle = 90,
                                   hjust=1,
                                   vjust=0.5),)+
  coord_equal()+
  geom_point(data=new_df2,
             aes(x=rowname,y=name,
                 size=abs_cor,
                 color=value))+
  scale_color_paletteer_c(palette = "ggthemes::Classic Red-Blue")
