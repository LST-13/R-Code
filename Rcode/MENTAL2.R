rm(list=ls())#clear Global Environment
setwd("D:/R/LH")
#安装包
# install.packages("ggplot2")
# install.packages("vegan")
# install.packages("dplyr")
#install.packages("devtools")
#devtools::install_github("houyunhuang/ggcor")

#加载包
library(vegan)
library(dplyr)
library(ggcor)
#安装devtools
#install.packages("devtools")
#安装ggcor
#devtools::install_local("D:/R/R-4.1.0/library/ggcor_master.zip")

library(ggplot2)
#OTU表格
df<- read.csv("HXBYY.csv", row.names = 1, header=T,check.names = F)
df <-data.frame(t(df))
#环境因子数据
env<-read.csv("lhjg.csv", row.names = 1,  header=T, check.names = F)
head(df)
head(env)


quickcor(env, type = "lower",method = "spearman") +
  geom_square()+
  scale_fill_gradient2( high = 'orange', mid = 'white',low = 'navyblue')  #颜色设置
  

df_mantel <- mantel_test(df, env, mantel.fun = 'mantel',
                         spec.dist.method = 'bray', 
                         env.dist.method = 'euclidean',
                         spec.select = list(A = 1:4))#将群落数据按组进行分开
df_mantel <- df_mantel %>%
  mutate(df_r = cut(r, breaks = c(-Inf, 0.1, 0.2, 0.4, Inf),
                    labels = c("< 0.1", "0.1 - 0.2", "0.2 - 0.4", ">= 0.4")),#定义Mantel的R值范围标签
         df_p = cut(p.value, breaks = c(-Inf, 0.01, 0.05, Inf),
                    labels = c("< 0.01", "0.01 - 0.05", ">= 0.05")))#定义Mantel的P值范围标签
quickcor(env,method = "spearman", type = "upper", cor.test = T, cluster.type = "all") +#环境因子之间的相关性热图
  geom_square() +#相关性显示形式
  geom_mark(r = NA,sig.thres = 0.05, size = 3.5, colour = "black")+#显著性标签
  scale_fill_gradient2(high = '#DC1623', mid = 'white',low = '#2D6DB1') + #颜色设置
  anno_link(df_mantel, aes(color = df_p,
                           size = df_r))+
  scale_size_manual(values = c(0.5, 1, 1.5, 2))+#连线粗细设置
  scale_color_manual(values = c("red","green","grey"))+#线条颜色设置
  guides(fill = guide_colorbar(title = "correlation", order = 1),#图例相关设置
         size = guide_legend(title = "Mantel's r",order = 2),
         color = guide_legend(title = "Mantel's p", order = 3),
         linetype = "none")
write.csv(df_mantel,file = "df_mantel.CSV")#写出重要性数据到默认路径
