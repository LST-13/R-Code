setwd("D:/R/LH")
library(psych)#加载R包
library(reshape2)#加载R包
library(ggplot2) #加载R包
library(randomForest)#加载随机森林R
library(patchwork)#加载拼图R包
#install.packages("psych")  #以psych包为例如果没有安装，则先在R中先安装
myro <- as.data.frame((read.csv("D:/R/LH/lhyyY.csv", header=TRUE)))
spearman <- corr.test(myro[,2:13], myro[,14:17], method = 'spearman', adjust = 'none')#取相应的列并计算相关系数
spearman #查看相关系数矩阵
#整理结果以便于作图
r <- data.frame(spearman$r)  #获取相关系数R方
p<-data.frame(spearman$p)
write.csv(p,file = "YSp.CSV")#写出重要性数据到默认路径
r$myro <- rownames(r)#定义
r <- melt(r, id = 'myro')
spearman <- cbind(r)
spearman
write.csv(spearman,file = "YSspearmanf.CSV")#写出重要性数据到默认路径
#查看重新用r值组建的新数据框
#以上代码为将相关系数矩阵处理为3列数据框
p1 <- ggplot() +
  geom_tile(data = spearman, aes(x = variable, y = myro, fill = value)) +
  scale_fill_gradientn(colors = c('#2D6DB1', 'white', '#DC1623'), limit = c(-1, 1)) +
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black'), legend.key = element_blank(), 
        axis.text.x = element_text(color = 'black', angle =45, hjust = 1, vjust = 1), axis.text.y = element_text(color = 'black'), axis.ticks = element_line(color = 'black')) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(y = '', x = '', fill = 'Correlation')

p1#查看上面绘制的相关热图

#逐个计算理化性质的重要值，如果目的是想知道微生物对理化性质的贡献，下面的回归等式需要反过来逐个计算
set.seed(123)
bacillus_forest <- randomForest(g__Bacillus~VWC+VW+URE+TP+TN+TC+SC+PHO+pH+NO3.N+NH4.N+MY+KW+HGW+EC+CAT+C.N+AP, data =myro, importance = TRUE, ntree = 500)
Pseudomonas_forest <- randomForest(g__Pseudomonas~VWC+VW+URE+TP+TN+TC+SC+PHO+pH+NO3.N+NH4.N+MY+KW+HGW+EC+CAT+C.N+AP, data =myro, importance = TRUE, ntree = 500)
Enterobacter_forest <- randomForest(g__Enterobacter~VWC+VW+URE+TP+TN+TC+SC+PHO+pH+NO3.N+NH4.N+MY+KW+HGW+EC+CAT+C.N+AP, data =myro, importance = TRUE, ntree = 500)
Brevibacillus_forest <- randomForest(g__Brevibacillus~VWC+VW+URE+TP+TN+TC+SC+PHO+pH+NO3.N+NH4.N+MY+KW+HGW+EC+CAT+C.N+AP, data =myro, importance = TRUE, ntree = 500)
Stenotrophomonas_forest <- randomForest(g__Stenotrophomonas~VWC+VW+URE+TP+TN+TC+SC+PHO+pH+NO3.N+NH4.N+MY+KW+HGW+EC+CAT+C.N+AP, data =myro, importance = TRUE, ntree = 500)
Herbaspirillum_forest <- randomForest(g__Herbaspirillum~VWC+VW+URE+TP+TN+TC+SC+PHO+pH+NO3.N+NH4.N+MY+KW+HGW+EC+CAT+C.N+AP, data =myro, importance = TRUE, ntree = 500)
Ochrobactrum_forest <- randomForest(g__Ochrobactrum~VWC+VW+URE+TP+TN+TC+SC+PHO+pH+NO3.N+NH4.N+MY+KW+HGW+EC+CAT+C.N+AP, data =myro, importance = TRUE, ntree = 500)
NPK_forest <- randomForest(NPK~VWC+VW+URE+TP+TN+TC+SC+PHO+pH+NO3.N+NH4.N+MY+KW+HGW+EC+CAT+C.N+AP, data =myro, importance = TRUE, ntree = 500)
PA_forest <- randomForest(PA~VWC+VW+URE+TP+TN+TC+SC+PHO+pH+NO3.N+NH4.N+MY+KW+HGW+EC+CAT+C.N+AP, data =myro, importance = TRUE, ntree = 500)
DR_forest <- randomForest(DR~VWC+VW+URE+TP+TN+TC+SC+PHO+pH+NO3.N+NH4.N+MY+KW+HGW+EC+CAT+C.N+AP, data =myro, importance = TRUE, ntree = 500)
MP_forest <- randomForest(MP~VWC+VW+URE+TP+TN+TC+SC+PHO+pH+NO3.N+NH4.N+MY+KW+HGW+EC+CAT+C.N+AP, data =myro, importance = TRUE, ntree = 500)

NPK_forest <- randomForest(NPK~Zn+Si+Pb+Mn+Mg+Fe2.+Fe+Cu+Co+Ca+B+Al, data =myro, importance = TRUE, ntree = 500)
PA_forest <- randomForest(PA~Zn+Si+Pb+Mn+Mg+Fe2.+Fe+Cu+Co+Ca+B+Al, data =myro, importance = TRUE, ntree = 500)
DR_forest <- randomForest(DR~Zn+Si+Pb+Mn+Mg+Fe2.+Fe+Cu+Co+Ca+B+Al, data =myro, importance = TRUE, ntree = 500)
MP_forest <- randomForest(MP~Zn+Si+Pb+Mn+Mg+Fe2.+Fe+Cu+Co+Ca+B+Al, data =myro, importance = TRUE, ntree = 500)



bacillus_forest#以OTU1_forest为例查看OTU_1的回归计算结果
Pseudomonas_forest
Enterobacter_forest
Brevibacillus_forest
Stenotrophomonas_forest
Herbaspirillum_forest
Ochrobactrum_forest
NPK_forest
PA_forest
DR_forest
MP_forest

#使用函数 importance() 查看表示每个预测变量（细菌 OTU）重要性的得分（标准化后的得分）
bacillus<- data.frame(importance(bacillus_forest, scale = TRUE), check.names = FALSE)
Pseudomonas<- data.frame(importance(Pseudomonas_forest, scale = TRUE), check.names = FALSE)
Enterobacter<- data.frame(importance(Enterobacter_forest, scale = TRUE), check.names = FALSE)
Brevibacillus <- data.frame(importance(Brevibacillus_forest, scale = TRUE), check.names = FALSE)
Stenotrophomonas <- data.frame(importance(Stenotrophomonas_forest , scale = TRUE), check.names = FALSE)
Herbaspirillum<- data.frame(importance(Herbaspirillum_forest, scale = TRUE), check.names = FALSE)
Ochrobactrum <- data.frame(importance(Ochrobactrum_forest, scale = TRUE), check.names = FALSE)
NPK <- data.frame(importance(NPK_forest , scale = TRUE), check.names = FALSE)
PA<- data.frame(importance(PA_forest, scale = TRUE), check.names = FALSE)
DR <- data.frame(importance(DR_forest, scale = TRUE), check.names = FALSE)
MP <- data.frame(importance(MP_forest, scale = TRUE), check.names = FALSE)

#查看重要值构成
bacillus
Pseudomonas
Enterobacter
Brevibacillus
Stenotrophomonas
Herbaspirillum
Ochrobactrum
NPK
PA
DR
MP

IMportance_t <- data.frame(cbind(bacillus$`%IncMSE`, Pseudomonas$`%IncMSE`,Enterobacter$`%IncMSE`,Brevibacillus$`%IncMSE`,Stenotrophomonas$`%IncMSE`,Herbaspirillum$`%IncMSE`,Ochrobactrum$`%IncMSE`))
IMportance_t <- data.frame(cbind(NPK$`%IncMSE`, PA$`%IncMSE`,DR$`%IncMSE`,MP$`%IncMSE`))

colnames(IMportance_t) <- c('bacillus', 'Pseudomonas', 'Enterobacter', 'Brevibacillus',"Stenotrophomonas","Herbaspirillum","Ochrobactrum")#命名行名
colnames(IMportance_t) <- c('NPK', 'PA', 'DR', 'MP')#命名行名

rownames(IMportance_t) <- c('Zn','Si','Pb','Mn','Mg','Fe2.','Fe','Cu','Co','Ca','B','Al')#命名列名
rownames(IMportance_t) <- c('VWC', 'VW', 'URE', 'TP',"TN","TC","SC","PHO","pH","NO3.N","NH4.N","MY","KW","HGW","EC","CAT","C.N","AP")#命名列名

IMportance_t[IMportance_t<0] <- 0#将重要性低于0的全部过滤
IMportance_t[is.na(IMportance_t)] <- 0#将空值赋值为0
write.csv(IMportance_t,file = "IMportance_YS.CSV")#写出重要性数据到默认路径

impdata <- read.csv("IMportance_YS.CSV",header = TRUE)#将重新处理果的数据写入R中
measure_name=setdiff(colnames(impdata),
                     c('Items'))#按第一列来一对多的赋值
#结果整理以便于作图
data1=melt(impdata ,
           id.vars='Items', 
           measure.vars=measure_name,
           variable.name = "sample", 
           value.name = "expr")
p <- p1+geom_point(data = data1, aes(x = sample, y = Items, size = expr*10), shape = 1) +
  scale_size_continuous(range = c(0,6)) +#控制圆圈大小
  labs(size = 'Importance (%)')#在热图上加上圆圈
p#查看绘图结果

exp <- read.csv("expfY.CSV",header = TRUE)#读如整理的总解释量值数据框，这里需要自己手动统计相关数据
exp
p2 <- ggplot(exp, aes(锘縢enus, values)) +
  geom_col(fill = "steelblue") +
  theme_bw() +
  theme_classic()+
  theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())+coord_cartesian(ylim = c(-120, 50))+
  ylab("Explained variation(%)")
p2 +p +  plot_layout(ncol= 1, widths = c(2, 1))

