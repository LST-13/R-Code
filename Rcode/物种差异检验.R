setwd('D:/R/be')
rm(list=ls()) 
library(tidyverse)#数据整理与数据转换包，用了一些更好用更易懂的函数
library(ggprism)
library(vegan)
otu <- read.csv('cpyy.csv',row.names = 1)
head(otu, n = 3)
otu <-decostand(otu,'total',2)#按列标准化otu
colSums(otu)#若想后面做成相对丰度的差异比较，可把这两行释放出来即可
tax <- read.csv('tax.csv',row.names = 1)
head(tax, n = 3)
metadata<- read.csv('design.csv')
head(metadata, n = 3)
dat <- merge(x=otu,y=tax,by='row.names')
head(dat, n = 3)
dat =dplyr::rename(dat,OTUID = sample)
head(dat, n = 3)
##按Phylum水平分组汇总(根据自己需求更改要展示的物种水平)
aa<-aggregate(dat[,2:ncol(otu)],by=list(dat$Genus),FUN=sum)
head(aa)
########################三种排序方法，任选其一
#1
# aa<- mutate(aa,v=apply(aa[,c(2:ncol(aa))],1,sum))
# cc<- arrange(aa,desc(v))        
# head(cc)
# cc<-select(cc,-v)
# head(cc)
# row.names(cc)=cc$Phylum
# head(cc)
# cc<-select(cc,-Phylum)
# head(cc)
#2
# row.names(aa)=aa$Phylum    
# head(aa)
# aa<-select(aa,-Phylum)
# head(aa)
# cc<-aa[order(rowSums(aa),decreasing=T),]   
#3
row.names(aa)=aa$Group.1   
head(aa)
aa<-dplyr::select(aa,-Group.1)
head(aa, n = 3)
#根据行求和结果对数据排序
order<-sort(rowSums(aa[,2:ncol(aa)]),index.return=TRUE,decreasing=T)   
#根据列求和结果对表格排序
cc<-aa[order$ix,]
head(cc, n = 3)
##只展示排名前10的物种，之后的算作Others(根据需求改数字)
dd<-rbind(colSums(cc[11:as.numeric(length(rownames(cc))),]),cc[10:1,])
head(dd, n = 3)
rownames(dd)[1]<-"Others"
head(dd, n = 3)
##再与metadata合并
bb<-merge(t(dd),dplyr::select(metadata,sample,GROUP),
          by.x = "row.names",by.y ="sample")
head(bb, n = 3)
##宽数据变长数据
kk<-tidyr::gather(bb,Genus,Abundance,-c(GROUP,Row.names))
#将未注释到的Unassigned也改为Others,你也可以不改，有以下两种方式
kk$Genus<-ifelse(kk$Genus=='Unassigned','Others',kk$Genus)#1      
#kk[kk$Phylum=='Unassigned','Phylum']='Others'               #2
##根据Group,Phylum分组运算
hh <- kk %>%
  group_by(GROUP,Genus) %>%
  dplyr :: summarise(Abundance=sum(Abundance))
head(hh, n = 3)
##更改因子向量的levels
hh$Genus = factor(hh$Genus,order = T,levels = row.names(dd))
yanse <-c("#999999","#F781BF","#A65628","#FFFF33","#FF7F00","#984EA3",
          "#4DAF4A","#377EB8","#74D944","#E41A1C","#DA5724","#CE50CA", 
          "#D3D93E","#C0717C","#CBD588","#D7C1B1","#5F7FC7","#673770", 
          "#3F4921","#CD9BCD","#38333E","#689030","#AD6F3B")#要确保颜色够用，否则会报错
##按物种丰度排序好的堆积柱形图
p1 <- ggplot(hh,aes(x = GROUP,y = Abundance,fill = Genus)) + 
  geom_bar(position="fill",stat = "identity",width = 0.5) +
  scale_fill_manual(values = yanse) +
  labs(x='Group',y='Abundance(%)')+
  scale_x_discrete(limits = c("CK","CPG","CPR","SPG","SPR"))+
  guides(fill=guide_legend(reverse = TRUE))+
  ggprism::theme_prism()+
  scale_y_continuous(expand = c(0,0))
p1#由于把Unassigned也算成了Others，所以只显示9个物种

###进行处理间各物种非参数检验的多组比较
#数据整理与转换
head(bb,n = 3)
cc =dplyr::select(bb,Row.names,GROUP,everything(),-c(Others))
head(cc,n = 3)
library("tidyr")
dat <- gather(cc,Genus,v,-(Row.names:GROUP))
head(dat,n = 3)
(参见之前推送，一共写了五种多重比较的方法，总有一款适合你)
data <- read.csv('genus.data.csv',row.names =1,check.names = T)
kruskalmc_compare1 <- function(data,GROUP,compare,value){
  library(multcompView)
  library(pgirmess)##多组两两比较函数用到的R包
  library(multcomp)
  a <- data.frame(stringsAsFactors = F)
  type <- unique(data[,GROUP])
  for (i in type)
  {
    g1=compare
    sub_dat <- data[data[,GROUP]==i,]
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
  names(a) <- c(compare,'std','mean','Letters',GROUP)
  return(a)
}
library(tidyr)
library(rstatix)
df1 <- kruskalmc_compare1(data,'Genus','GROUP','v')
row.names(data) <- data$Row.names
data$group <- factor(data$group,levels=c('HVRP','HVGP','CKP'))
p1 = ggplot(data)+geom_boxplot(aes(x=group,y=v,fill=group))+
  geom_text(data=df1,aes(x=group,y=mean+1.3*std,label=Letters))+
  facet_wrap(~property,scales = "free_y")+ labs(x='',y='mg/kg')+
  ggprism::theme_prism()+theme(axis.text.x = element_text(angle = 45))
p1


##非参数检验的多组比较函数
PMCMR_compare1 <- function(data,group,compare,value){
  library(multcompView)
  library(multcomp)
  library(PMCMRplus)
  library(PMCMR)
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
    
    k <- PMCMRplus::kwAllPairsNemenyiTest(value ~ g1,data=sub_dat)
    n <- as.data.frame(k$p.value)
    h <- n %>%
      mutate(compare=rownames(n)) %>%
      gather(group,p,-compare,na.rm = TRUE) %>%
      unite(compare,group,col="G",sep="-")
    dif <- h$p
    names(dif) <- h$G
    dif
    difL <- multcompLetters(dif)
    K.labels <- data.frame(difL['Letters'], stringsAsFactors = FALSE)
    K.labels$compare = rownames(K.labels)
    K.labels$type <- i
    
    mean_sd <- merge(aggregate(sub_dat[['value']],by=list(sub_dat[,'g1']),FUN=sd),
                     aggregate(sub_dat[['value']],by=list(sub_dat[,'g1']),FUN=mean),by="Group.1"
    )
    names(mean_sd) <- c('compare','std','mean')
    a <- rbind(a,merge(mean_sd,K.labels,by='compare'))
  }
  names(a) <- c(compare,'std','mean','Letters',group)
  return(a)
}
##################################用函数运行输入的数据
library(pgirmess)
df2<-kruskalmc_compare1 (dat,'Genus','GROUP','v')

df2 <- PMCMR_compare1(dat,'Genus','GROUP','v')
df2########字母标正着标(a>b>c)(之后跑自己的数据可能出现相反的顺序，不影响结果，可在AI或PDF编辑器中调)

df2<- read.csv('df3.csv',row.names =1,check.names = T)
p2 = ggplot(data,aes(GROUP,v))+geom_boxplot(aes(color=GROUP))+
  geom_text(data=df2,aes(x=group,y=mean+2*std,label=Letters))+
  geom_jitter(aes(fill=GROUP),position = position_jitter(0.2),shape=21,size=1,color="black")+
  facet_wrap(~abundance,scales = "free_y")+ labs(x='group',y='Genus')+
  ggprism::theme_prism()+theme(axis.text.x = element_text(angle = 45))
p2

