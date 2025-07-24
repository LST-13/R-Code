#调用R包，好几个用不上，懒得筛选
library(multifunc)
library(ggplot2)
library(patchwork)
library(tidyr)
library(dplyr)
library(purrr)
library(forcats)
library(car)

#读取数据
setwd("D:/R/EMF")
germany<-read.csv("lhyy.csv",row.names = 1)
head(germany)
#选择变量
allVars<-c("VW","VWC","TN","TC","C.N","TP","NH4.N","NO3.N","pH","EC","URE","SC","PHO","MY","Clay","Silt","Sand")
#筛选那些三分之二以上没NA值的数据
german_vars <- whichVars(germany, allVars)

#分析阈值
germanyThresh<-getFuncsMaxed(germany, german_vars, 
                             threshmin=0.05, threshmax=0.99,
                             prepend=c("plot","Streptomyces"), maxN=7)

germanyThresh

#我们只看阈值0.8以上的
mfuncGermanyLinear08<-glm(funcMaxed ~ Streptomyces, 
                          data=subset(germanyThresh, germanyThresh$thresholds=="0.8"), 
                          family=quasipoisson(link="identity"))
mfuncGermanyLinear08

#看看多样性对EMF变化影响是否显著
Anova(mfuncGermanyLinear08, test.statistic="F")

#查看整体统计信息

summary(mfuncGermanyLinear08)

#先绘制4个阈值的
gcPlot<-subset(germanyThresh, 
               germanyThresh$thresholds %in% qw(0.2, 0.4, 0.6, 0.8)) 

gcPlot$percent<-paste(100*gcPlot$thresholds, "%", sep="")

#绘4个阈值的图
qplot(Streptomyces, funcMaxed, data=gcPlot, facets=~percent) +
  stat_smooth(method="glm", 
              method.args = list(family=quasipoisson(link="identity")),
              colour="red", lwd=1.2) +
  ylab(expression("Number of Functions" >= Threshold)) +
  xlab("Species Richness") +
  theme_bw(base_size=14) +
  geom_text(data=data.frame(percent = unique(gcPlot$percent),
                            lab = paste(letters[1:4], ")", sep=""),
                            Bacillus=2,
                            funcMaxed=6
  ), mapping=aes(x=Bacillus, y=funcMaxed, label=lab))


#整个阈值范围的变化，我们前边只是使用单阈值看了0.8的，现在可以看看整体的变化
#也是我们论文中常见的图，不过后续的才是完整版
germanyThresh$percent <- 100*germanyThresh$thresholds
ggplot(data=germanyThresh, aes(x=Bacillus, y=funcMaxed, group=percent)) +
  ylab(expression("Number of Functions" >= Threshold)) +
  xlab("Species Richness") +
  stat_smooth(method="glm", 
              method.args = list(family=quasipoisson(link="identity")), 
              lwd=0.8, fill=NA, aes(color=percent)) +
  theme_bw(base_size=14) +
  scale_color_gradient(name="Percent of \nMaximum", low="blue", high="red")


#多阈值法

germanyLinearSlopes<-getCoefTab(funcMaxed ~ Diversity,
                                data = germanyThresh, 
                                coefVar = "Bacillus",
                                family = quasipoisson(link="identity"))


######
# Plot the values of the diversity slope at
# different levels of the threshold
######
germanSlopes <- ggplot(germanyLinearSlopes, aes(x=thresholds*100,
                                                y = estimate,
                                                ymax = estimate + 1.96 * std.error,
                                                ymin = estimate - 1.96 * std.error)) +
  geom_ribbon(fill="grey50") +
  geom_point() +
  ylab("Change in Number of Functions per Addition of 1 Species\n") +
  xlab("\nThreshold (%)") +
  geom_abline(intercept=0, slope=0, lwd=1, linetype=2) +
  theme_bw(base_size=14)

germanSlopes

#这一步是那几个关键参数
germanIDX <- getIndices(germanyLinearSlopes, germanyThresh, funcMaxed ~ Diversity)
germanIDX

#这个是包迹线中的最大值
germanyLinearSlopes$estimate[which(germanyLinearSlopes$thresholds==germanIDX$Tmde)]

#绘制多阈值图
germanyThresh$IDX <- 0
germanyThresh$IDX [which(germanyThresh$thresholds %in% 
                           c(germanIDX$Tmin, germanIDX$Tmax, germanIDX$Tmde))] <- 1


#最终版的多阈值图
p1 <- ggplot(data=germanyThresh, aes(x=Diversity, y=funcMaxed, group=percent)) +
  ylab(expression("Number of Functions" >= Threshold)) +
  xlab("Species Richness") +
  geom_smooth(method="glm", 
              method.args = list(family=quasipoisson(link="identity")), 
              fill=NA, aes(color=percent, lwd=IDX)) +
  theme_bw(base_size=14) +
  scale_color_gradient(name="Percent of \nMaximum", low="blue", high="red") +
  scale_size(range=c(0.3,5), guide="none") +
  annotate(geom="text", x=0, y=c(0.2,2,4.6), label=c("Tmax", "Tmde", "Tmin")) +
  annotate(geom="text", x=16.7, y=c(germanIDX$Mmin, germanIDX$Mmax, germanIDX$Mmde), label=c("Mmin", "Mmax", "Mmde"))
p1
#最终版的包迹线图
p1+germanSlopes + annotate(geom="text", 
                           y=c(-0.01, -0.01, -0.01, 
                               germanIDX$Rmde.linear+0.02),
                           x=c(germanIDX$Tmin*100, 
                               germanIDX$Tmde*100, 
                               germanIDX$Tmax*100, 
                               germanIDX$Tmde*100),  
                           label=c("Tmin", "Tmde", "Tmax", "Rmde"),
                           color="black")
