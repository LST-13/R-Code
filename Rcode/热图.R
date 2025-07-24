exp<- read.csv("cy.csv", row.names = 1, header=T,check.names = F)
chem<- read.csv("lhsx.csv", row.names = 1,  header=T, check.names = F)
#对数据框进行转置；
expt <- t(exp[1:4,])
chemt <- t(chem)
#预览转置后的数据；
expt[1:9,1:6]
chemt[1:9,1:10]
#安装psych包；
#install.packages("psych")
#载入R包；
library(psych)
#计算基因表达量之间的pearson相关性；
ct1 <- corr.test(expt,chem,method = "pearson")
#提取相关性系数矩阵；
r1 <- ct1$r
#提取pvalue值矩阵；
p1 <- round(ct1$p,3)

#预览转置后的相关性系数矩阵和pvalue矩阵；
r2 <- t(r1)
p2 <- t(p1)
#使用显著性星号标记进行替换；
p2[p2>=0 & p2 < 0.001] <- "***"
p2[p2>=0.001 & p2 < 0.01] <- "**"
p2[p2>=0.01 & p2 < 0.05] <- "*"
p2[p2>=0.05 & p2 <= 1] <- ""

# 删掉标准差为0的行
#df = df[apply(df, 1, function(x) sd(x)!=0),] 

# 删掉标准差为0的列
#df = df[,apply(df, 2, function(x) sd(x)!=0)] 

#载入pheatmap包；
library(pheatmap)
#自定义颜色；
mycol<-colorRampPalette(c("#2D6DB1", "white", "#DC1623"))(200)
#绘制热图；
pheatmap(r2,scale = "none",
         border_color ="white",
         number_color="white",
         fontsize_number=14,
         fontsize_row=8,
         fontsize_col=9,
         cellwidth=15,
         cellheight=15,
         cluster_rows=T,
         cluster_cols=T,
         color = mycol,
         display_numbers = p2,
         show_rownames=T)

