setwd("D:/R/be")
rm(list=ls())
#读入文件
index <- read.csv('diversity.cpyy.csv',header=T,check.names = F,row.names = 1)
##figure:take shannon for example
index$samples <- rownames(index)#将样本名写到文件中
#读入分组文件
groups <- read.csv('design.csv',header = T, stringsAsFactors = F)
colnames(groups)[1:2] <- c('samples','group')#改列名
#合并分组信息与多样性指数
df2 <- merge(index,groups,by = 'samples')
#Shannon
p1 <- ggplot(df2,aes(x=group,y=Shannon))+#指定数据
  stat_boxplot(geom = "errorbar", width=0.1,size=0.8)+#添加误差线,注意位置，放到最后则这条线不会被箱体覆盖
  geom_boxplot(aes(fill=group), #绘制箱线图函数
               outlier.colour="white",size=0.8)+#异常点去除
  theme(panel.background =element_blank(), #背景
        axis.line=element_line(),#坐标轴的线设为显示
        plot.title = element_text(size=14))+#图例位置
  # scale_fill_manual(values=c("#ffc000","#a68dc8","blue"))+#指定颜色
  geom_jitter(width = 0.2)+#添加抖动点
  geom_signif(comparisons = list(c("A","B"),
                                 c("A","C"),
                                 c("B","C")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              y_position = c(3,3.5,3.25),#图中横线位置 设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size=0.8,color="black")+
  theme_prism(palette = "candy_bright",
              base_fontface = "plain", # 字体样式，可选 bold, plain, italic
              base_family = "serif", # 字体格式，可选 serif, sans, mono, Arial等
              base_size = 16,  # 图形的字体大小
              base_line_size = 0.8, # 坐标轴的粗细
              axis_text_angle = 45)+ # 可选值有 0，45，90，270
  scale_fill_prism(palette = "candy_bright")+
  theme(legend.position = 'none')#去除图例
p1











Shannon<- P[,c(1,2)]
#Shannon$Shannon = as.numeric(Shannon$Shannon)#####数据分析与转换+可视化########是否符合正态分布#######
attach(Shannon)
qqPlot(lm(Shannon ~group, data=Shannon), 
       simulate=TRUE, main="Q-Q Plot", lables=FALSE)
##正态检验
shapiro.test(Shannon$Shannon)
#无论Bartlett检验还是Levene检验，两者的P值都大于0.05，#因此接受原假设：样本之间的方差是相同的。因此可以接着做方差分析了。
# Bartlett检验
bartlett.test(Shannon ~ group, data=Shannon)
# Levene检验,对原始数据的正态性不敏感
leveneTest(Shannon ~ group, data=Shannon)
#######方差检验
model<-aov(Shannon ~ group, data=Shannon)
#进行多重比较，不矫正P值
p<- LSD.test(model,"group", p.adj="none")#结果显示：标记字母法otu$group
grou<- group_by(Shannon,group)
bar_data <- summarise(grou,sd(Shannon,na.rm = T))
bar_data2 = merge(bar_data,p$group,by.x="group",by.y = "row.names",all = F)#colnames(bar_data2) = c("group", "SD","TN","label");bar_data2
colnames(bar_data2) = c("group","SD",'Shannon','label')
######画一张图看看
bar_data2$group <- factor(bar_data2$group,levels=c('CK','GM','N1','GMN1','GMN2','GMN3'))
library(ggplot2)
a = max(bar_data2$Shannon)*1.5
mi=c("#1B9E77" ,"#D95F02", "#7570B3","#E7298A")
p = ggplot(bar_data2 , aes(x = group, y = Shannon)) +  geom_bar(stat = "identity", width = 0.4,position = "dodge",colour="black",fill="#E6AB02") + 
  geom_errorbar(aes(ymin=Shannon - SD,
                    ymax=Shannon + SD),
                colour="black",width=0.1,size=1)+
  geom_text(aes(label = label ,y= Shannon + SD, x = group),vjust = -0.3,size = 6)+
  scale_y_continuous(expand = c(0,0),limits = c(0,a))+
  labs(x="TN of all group",
       y="group",
       title = "")
pp1=p+theme_bw()+
  geom_hline(aes(yintercept=mean(Shannon + SD)), colour="black", linetype=2) +
  geom_vline(aes(xintercept=0), colour="black", linetype="dashed") +
  scale_fill_manual(values = mi, guide = guide_legend(title = NULL))+
  theme(
    panel.