install.packages("devtools")
devtools::install_github("Hy4m/linkET", force = TRUE)
packageVersion("linkET")

library(linkET)
library(igraph)
library(dplyr)
setwd("D:/R/WZ")
network<-read.csv("zcg150.csv",row.names = 1)
tax<-read.csv("tax.csv",row.names = 1)

#边
edges0<-network %>%
  correlate(method="spearman")%>%
  as_md_tbl(type="upper",diag=FALSE)%>%
  dplyr::filter(abs(r)>0.6,p<0.05)
edges<-edges0%>%
  dplyr::mutate(Type="Undirected",
                ID=seq_len(dplyr::n()),
                Label="",
                sign=ifelse(r>0,"P","N"),
                abs_r=abs(r))%>%
  dplyr::rename(Source=.rownames,
                Target=.colnames)%>%
  dplyr::select(Source,Target,Type,ID,Label,r,p,sign,abs_r)
#保存边文件
write.table(edges,"150edges.csv",row.names = FALSE,col.names = TRUE,sep=",")


#节点
nodes<-as.igraph(edges0) %>%
igraph::as_data_frame("vertices")

#节点文件注释门
tax_map<-rlang::set_names(tax$Phylum,tax$Genus)
nodes$Phlyum<-tax_map[nodes$name]