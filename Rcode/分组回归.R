setwd('D:/R/AMF')
library(tidyverse)
library(performance)
library(stargazer)
library(ggpubr)
AMF<-read.csv(file="AMF.csv",header=T,check.names=FALSE ,row.names=1)
p1 <- ggplot(AMF,aes(x=AMF$NHN,y=AMF$AMF))+
  geom_point(size=4,aes(color=AMF$group))+
  scale_color_manual(values=c("#DD5F60","#FFDAB9","#B4EEB4","#8FBC8F","#FFD39B"))+
  geom_smooth(method=lm,level=0.95,color="gray4")+
  stat_cor(method = "pearson",label.x.npc ="left",label.y.npc = 0.02)+
  theme_classic() +
  labs(x="NH4-N mg/kg",y="AMF",color="group")+
  theme(axis.text=element_text(colour='black',size=9))
p1
