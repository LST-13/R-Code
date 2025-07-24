rm(list=ls())
library(linkET)
library(dplyr)
library(ggplot2)
setwd("D:/R")
otu<- read.csv("N1-NR.csv", row.names = 1, header=T,check.names = F)
env<- read.csv("ymlh.csv", row.names = 1,  header=T, check.names = F)
otu<-as.data.frame(t(otu[,1:9]))
property <- env[,c(1:12)]
P_parament <- env[,c(13:20)]
env <- env[,c(1:13)]

mantel <- mantel_test(otu, env,spec_select = list(N = 1:5027
)) %>% 
  mutate(rd = cut(r, breaks = c(-Inf, 0.2, 0.4, Inf),
                  labels = c("< 0.2", "0.2 - 0.4", ">= 0.4")),
         pd = cut(p, breaks = c(-Inf, 0.01, 0.05, Inf),
                  labels = c("< 0.01", "0.01 - 0.05", ">= 0.05")))
qcorrplot(correlate(env), type = "lower",diag = FALSE)+
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu"),
                       limits = c(-1, 1),
                       breaks = seq(-1,1,0.5)) +
  geom_square() +
  geom_couple(aes(colour = pd, size = rd), 
              data = mantel, 
              curvature = nice_curvature()) +
  scale_size_manual(values = c(0.5, 1, 2)) +
  scale_colour_manual(values = color_pal(3)) +
  guides(size = guide_legend(title = "Mantel's r",
                             override.aes = list(colour = "grey35"), 
                             order = 2),
         colour = guide_legend(title = "Mantel's p", 
                               override.aes = list(size = 3), 
                               order = 1),
         fill = guide_colorbar(title = "Pearson's r", order = 3))