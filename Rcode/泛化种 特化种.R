library(EcolUtils)
#devtools::install_github("GuillemSalazar/EcolUtils")
setwd("D:/R/WZ/YY")
dune<-read.csv("yyspec.csv",row.names = 1)
#在 R 中，可使用 spaa 包的函数 niche.width() 计算生态位宽度指数，详情加载 spaa 包后 ?niche.width
library(spaa)

#这里以 Levins 生态位宽度指数为例，Shannon 生态位宽度指数可通过 method 参数修改
niche_width <- niche.width(dune, method = 'levins')
niche_width

#画图展示分布
boxplot(unlist(niche_width), ylab = 'niche breadth index')

#niche.width.method = 'levins'，基于 Levins（1968）的公式计算生态位宽度指数；若要计算 Shannon 生态位宽度指数可修改此参数
#n = 1000，随机化重排 1000 次
#probs = c(0.025, 0.975)，计算双侧 95% 置信区间为准划分
set.seed(123)
spec_gen <- spec.gen(dune, niche.width.method = 'levins', perm.method = 'quasiswap', n = 1000, probs = c(0.025, 0.975))
tail(spec_gen)

#输出 Specialist/Generalist species 的划分
write.table(spec_gen, 'spec_genyys.txt', sep = '\t', col.names = NA, quote = FALSE)
