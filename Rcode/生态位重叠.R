setwd("D:/R/NPK")
rm(list = ls())
#-----------import data----------------
library(tidyverse)
library(readxl)
library(spaa)
bac<-read.csv("zcgt.csv",row.names = 1)
bac
#---------------process data----------------
bac %>%
  data.frame() ->bac
rownames(bac) <- bac$otuid
bac2 <- bac[,-1]
tbac <- data.frame(t(bac2))
head(tbac[,1:6])
#---------------calculate niche width--------------
calNiche <- function(comun,method = "levins"){
  require(spaa)
  require(tidyverse)
  comun<-comun[,colSums(comun)>0] #Delete all columns whose values are 0
  B<-niche.width(comun,method= method)#select levins, detail see "?niche.width"
  names <- vector()
  niche_value <- vector()
  for(i in 1:ncol(comun)){
    names[[i]] <- names(B[i])
    niche_value[[i]] <- B[[i]]
    B_com <- data.frame(names,niche_value) %>%
      as_tibble() %>%
      arrange(desc(niche_value))
  }
  return(B_com)
}
a<-calNiche(bac,method = "levins")#"shannon"
write.csv(cbind(sample=c(rownames(a)),a),'stwNPKg2.csv',row.names = F)


#-------------------calculate niche overlap-----------------

bac[,colSums(bac) > 0] %>%
  niche.overlap(method = "levins") %>%
  data.matrix() %>%
  as_tibble(rownames = "otuid") ->bac_overlap


bac_overlap[upper.tri(bac_overlap,diag=FALSE)]=NA
bac_overlap %>%
  reshape2::melt(id.vars = "otuid") %>%
  filter(value!="NA") %>%
  mutate(paired = str_c(otuid,variable,sep = "_") ) -> bac_overlap2
write.csv(cbind(sample=c(rownames( bac_overlap2)), bac_overlap2),'stwNPKgcd2.csv',row.names = F)


#--------calculate niche overlap for each otu/species-------------
bac[,colSums(bac) > 0] %>%
  as_tibble() %>%
  colnames() -> bac_names

calculate_scores <- function(data, predict = bac_names) {
  x <- list()
  i = 0
  while (i < length(predict)) {
    i = i+1
    fam = predict[[i]]
    x[[i]] <- data %>%
      filter(str_detect(paired,fam)) %>%
      summarize(sum(value))
  }
  dt <- matrix(unlist(x),ncol = 1,nrow=length(predict))
  rownames(dt) <- predict
  colnames(dt) <- "Total_niche_overlap"
  dt %>%
    as_tibble(rownames = "otuid") %>%
    arrange(desc(Total_niche_overlap))
}

c<-calculate_scores(bac_overlap2)
