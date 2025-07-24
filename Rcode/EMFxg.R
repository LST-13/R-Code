setwd("D:/R/LH")

library(devtools)
#install.packages("multifunc")
#调用R包
library(multifunc)
library(ggplot2)
library(patchwork)
library(tidyr)
library(dplyr)
library(purrr)
library(forcats)
library(car)
all_biodepth<- read.csv("EMFxg.csv",header = T,row.names = 1)
#筛选需要的变量
allVars<-c("TN","TC")
varIdx<-which(names(all_biodepth) %in% allVars)
#数据集包含多个国家，我们只提取德国的数据
germany<- all_biodepth %>%
  filter(location=="Germany")
#筛选三分之二以上没有NA的数据
german_vars <- whichVars(germany, allVars)
# 查看物种名以及非0数据
species <- relevantSp(germany,17:ncol(germany))
spIDX <- which(names(germany) %in% species) 
#单功能法
germanyForPlotting <- germany %>%
  select(EMF, german_vars) %>%
  pivot_longer(cols = german_vars,
               names_to = "variable",
               values_to = "value") %>%
  mutate(variable = factor(variable))

#将数据整理成易于展示的形式
levels(germanyForPlotting$variable) <- c("TN","TC")
germanyLabels <- germanyForPlotting %>%
  group_by(variable) %>%
  nest() %>%
  mutate(fits = map(data, ~lm(value ~ EMF, data=.x)),
         stats = map(fits, broom::glance)) %>%
  unnest(stats) %>%
  ungroup() %>%
  mutate(labels = paste0("p = ", round(p.value, 3), ", ", 
                         expression(R^2), " = ", round(r.squared, 2)),
         labels =  gsub("p = 0 ", "p < 0.001 ", labels),
         EMF = 1, 
         value=c(25, 5))
#绘图
ggplot(aes(x=EMF, y=value),data=germanyForPlotting) +
  geom_point(size=3)+
  facet_wrap(~variable, scales="free") +
  theme_bw(base_size=15)+
  stat_smooth(method="lm", colour="black", size=2) + 
  xlab("Species Richness") +
  ylab("Value of Function") +
  geom_text(data=germanyLabels,
            aes(label=labels)) +
  theme(panel.grid = element_blank())
