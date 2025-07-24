setwd("D:/R/WZ/yy")
otu<-read.csv("AMF.csv",row.names = 1)

design<-read.csv("designg3.csv")

tax<-read.csv("tax.csv",row.names = 1)

otu_tax<-cbind(otu,tax)

########### 对top 10 phylum基因进行统计

phyla<-aggregate(otu_tax[,1:15],by=list(otu_tax$Genus),FUN=sum)

rownames(phyla)<-phyla[,1]

mean<-phyla[,-1]

a<-as.numeric(length(rownames(phyla)))

#筛选top 10丰度物种

mean<-mean[order(rowSums(mean),decreasing=T),]

top_phyla<-rbind(colSums(mean[5:a,]),mean[6:1,])

rownames(top_phyla)[1]<-"Others"

rownames(top_phyla)[2]<-"Unassigned"

bb<-cbind(t(mean),design)
#求处理直接丰度的平均值

taxonomy<-aggregate(bb[,1:5],by=list(bb$GROUP),FUN=mean)

rownames(taxonomy)<-taxonomy[,1]

tax<-as.data.frame(taxonomy)

library(ggprism)

library(ggplot2)

library(reshape2)

dat <- reshape::melt(tax, id = 'Group.1')

color<-c("gray34","#4169B2","#B1A4C0","#479E9B","#BB2BA0","#DDA0DD",
         
         "#BC8F8F","#DD5F60","#FFDAB9","#B4EEB4","#8FBC8F","#FFD39B","#8EE5EE","#B4EEB4","#D1D1D1","gray34","#4169B2","#B1A4C0","#479E9B")

library(ggalluvial)

a<-ggplot(dat, aes(x =Group.1, y = value, fill =variable,stratum =variable, alluvium =variable)) +geom_stratum(width = 0.5,alpha=0.7) +geom_flow(alpha = 0.3) +
  
  scale_fill_manual(values=color)+theme_prism(base_size = 11)+
  
  labs(x = '', y = 'Abundance',fill="Taxonomy",title=" ")
a
#截断
library("ggbreak")

a<-a+scale_y_break(breaks=c(1200,6000),scales=500,ticklabels = seq(6000,8000,1000))

a

#
p5<-a+scale_y_break(c(1200, 6000),scales = "fixed",
                         expand=expansion(add = c(0,500)))
p5

write.csv(cbind(sample=c(rownames(mean)),mean),'zcg.csv',row.names = F)
