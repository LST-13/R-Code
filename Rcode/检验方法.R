#物种差异检验方法
rm(list=ls())
getwd()
setwd('D:/my data/R-practise/significant difference test')
data <- read.csv('genus_abundance.csv',header=T,check.names = F)
kruskalmc_compare1 <- function(data,group,compare,value){
  library(multcompView)
  library(pgirmess)##多组两两比较函数用到的R包
  library(multcomp)
  a <- data.frame(stringsAsFactors = F)
  type <- unique(data[,group])
  for (i in type)
  {
    g1=compare
    sub_dat <- data[data[,group]==i,]
    names(sub_dat)[names(sub_dat)==compare] <- 'g1'
    names(sub_dat)[names(sub_dat)==value] <- 'value'
    sub_dat$g1 <- factor(sub_dat$g1)
    options(warn = -1)
    
    k <- kruskalmc(value ~ g1, data=sub_dat, probs=0.05)
    dif <- k$dif.com[['difference']]
    names(dif) <- rownames(k$dif.com)
    difL <- multcompLetters(dif)
    label <- data.frame(difL['Letters'], stringsAsFactors = FALSE)
    label$compare = rownames(label)
    label$type <- i
    
    mean_sd <- merge(aggregate(sub_dat[['value']],by=list(sub_dat[,'g1']),FUN=sd),
                     aggregate(sub_dat[['value']],by=list(sub_dat[,'g1']),FUN=mean),by="Group.1"
    )
    names(mean_sd) <- c('compare','std','mean')
    a <- rbind(a,merge(mean_sd,label,by='compare'))
  }
  names(a) <- c(compare,'std','mean','Letters',group)
  return(a)
}
library(tidyr)
data <- gather(data,abundance,v,-(sample:group))
df1 <- kruskalmc_compare1(data,'abundance','group','v')
data$group <- factor(data$group,levels=c('HVRP','HVGP','CKP'))
p1 = ggplot(data)+geom_boxplot(aes(x=GROUP,y=v,fill=GROUP))+
  geom_text(data=df2,aes(x=group,y=mean+1.3*std,label=Letters))+
  facet_wrap(~abundance,scales = "free_y")+ labs(x='group',y='Abundance')+
  ggprism::theme_prism()+theme(axis.text.x = element_text(angle = 45))
p1

