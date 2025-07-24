setwd('D:/R/AMF')
library(tidyverse)
library(performance)
library(stargazer)

AMF<-read.csv(file="AMF.csv",header=T,check.names=FALSE ,row.names=1)
data=AMF%>%
select(AMF,NHN)  #挑指标
#直线拟合（一次）
data%>%
  ggplot(aes(x=NHN,y=AMF))+
  geom_point()+
  geom_smooth(method='lm')
#曲线拟合（二次）
data%>%
  ggplot(aes(x=NHN,y=AMF))+
  geom_point()+
  geom_smooth(method='lm',formula = y~poly(x,2))
#线性回归模型
model1=lm(AMF~NHN,data=data)
model2=lm(AMF~poly(NHN,2),data=data)
#模型对比
stargazer(model1,model2,type='text')
#模型诊断
library(patchwork)
check_model(model1)
check_model(model2)
