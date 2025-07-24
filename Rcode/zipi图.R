##igraph 包计算网络模块

library(igraph)
setwd("D:/R/WZ/YY")

#输入数据示例，邻接矩阵
#这是一个微生物相关性网络，数值表示了微生物 OTU 之间的相关系数（正负相关以及相关性的大小）
adjacency_weight<-read.csv("yygcor.csv",row.names = 1)
#adjacency_weight <- read.delim('150genus_corr.matrix.txt', row.names = 1, sep = '\t', check.names = FALSE)
head(adjacency_weight)[1:6]    #邻接矩阵类型的网络文件

#邻接矩阵 -> igraph 的邻接列表，获得含权的无向网络
igraph = graph_from_adjacency_matrix(as.matrix(adjacency_weight), mode = 'undirected', weighted = TRUE, diag = FALSE)
igraph    #igraph 的邻接列表

#上述将相关系数转化为边的权重
#由于相关系数有负值，而权重正常都应为正值，所以对由相关系数得到的权重取绝对值
#新生成一列边属性记录相关系数
E(igraph)$corr <- E(igraph)$weight
E(igraph)$weight <- abs(E(igraph)$weight)

##节点特征
#节点数量
length(V(igraph)$name)
#或
vcount(igraph)

#节点度（Degree）
#由于本示例是个无向网络，故无出度和入度之分
V(igraph)$degree <- degree(igraph)
V(igraph)$degree

#查看度分布
#可观察到微生物相关网络通常服从幂律分布，这个下节再讲怎样通过计算验证
degree_dist <- degree.distribution(igraph)[-1]
degree_num <- 1:max(V(igraph)$degree)

par(mfrow = c(1, 2))
hist(V(igraph)$degree, xlab = 'Degree', ylab = 'Frequency',
     main = 'Degree distribution')
plot(degree_num, degree_dist, log = 'xy', xlab = 'Log-degree',
     ylab = 'Log-intensity', main = 'Log-log degree distribution')

#查看节点度与其“邻居”的平均度的关系
#微生物网络中高度值的节点更倾向连接在一起，是普遍现象吗？
neighbor_degree <- graph.knn(igraph, V(igraph))$knn
plot(V(igraph)$degree, neighbor_degree, log = 'xy',
     xlab = 'Log degree', ylab = 'Log average neighbor degree')

#加权度（Weighted degree）
V(igraph)$weight_degree <- strength(igraph)
V(igraph)$weight_degree

#接近中心性（Closeness centrality）
V(igraph)$closeness_centrality <- closeness(igraph)
V(igraph)$closeness_centrality

#介数中心性（Betweenness centrality）
V(igraph)$betweenness_centrality <- betweenness(igraph)
V(igraph)$betweenness_centrality

#特征向量中心性（Eigenvector centrality）
V(igraph)$eigenvector_centrality <- evcent(igraph)$vector
V(igraph)$eigenvector_centrality

#探索三种描述节点中心性的特征的关系
library(car)
library(rgl)

scatter3d(V(igraph)$closeness_centrality, V(igraph)$betweenness_centrality, V(igraph)$eigenvector_centrality,
          xlab =  'Closeness centrality', ylab = 'Betweenness centrality', zlab = 'Eigenvector centrality',
          surface = FALSE)

#探索节点度和节点中心性的关系，如与特征向量中心性的关系
plot(V(igraph)$degree, V(igraph)$eigenvector_centrality,
     xlab = 'Degree', ylab = 'Eigenvector centrality')

#输出列表
node_list <- data.frame(
  node_id = V(igraph)$name,
  degree = V(igraph)$degree,
  weight_degree = V(igraph)$weight_degree,
  closeness_centrality = V(igraph)$closeness_centrality,
  betweenness_centrality = V(igraph)$betweenness_centrality,
  eigenvector_centrality = V(igraph)$eigenvector_centrality)

head(node_list)
write.table(node_list, 'node_listyyg.txt', sep = '\t', row.names = FALSE, quote = FALSE)

##边特征
#边的数量
ecount(igraph)

#权重（Weighted），已在数据读入时转化获得
E(igraph)$weight

#边介数中心性（Edge betweenness centrality）
E(igraph)$betweenness_centrality <- edge.betweenness(igraph)
E(igraph)$betweenness_centrality

#输出列表
edge <- data.frame(as_edgelist(igraph))    #igraph 的邻接列表转为边列表

edge_list <- data.frame(
  source = edge[[1]],
  target = edge[[2]],
  weight = E(igraph)$weight,
  correlation = E(igraph)$corr,
  betweenness_centrality = E(igraph)$betweenness_centrality
)
head(edge_list)

write.table(edge_list, 'edge_listyyg.txt', sep = '\t', row.names = FALSE, quote = FALSE)

library(igraph)

#输入数据示例，邻接矩阵
#这是一个微生物互作网络，数值“1”表示微生物 OTU 之间存在互作，“0”表示无互作
adjacency_unweight <- read.csv("unyyg.csv",row.names = 1)
#adjacency_unweight <- read.delim('un150genus_corr.matrix.txt', row.names = 1, sep = '\t', check.names = FALSE)
head(adjacency_unweight)[1:56]    #邻接矩阵类型的网络文件

#邻接矩阵 -> igraph 的邻接列表，获得非含权的无向网络
igraph = graph_from_adjacency_matrix(as.matrix(adjacency_unweight), mode = 'undirected', weighted = NULL, diag = FALSE)
igraph    #igraph 的邻接列表

##子图与普查（电脑带不动）
#所有尺寸的团的普查可以提供一个快照，将显示各尺寸的团的数量
#census <- table(sapply(cliques(igraph), length))
#census
#plot(census)

#k 核
cores <- graph.coreness(igraph)
cores
sna::gplot.target(adjacency_unweight, cores, usearrows = FALSE, vertex.col = cores)

#二元组（dyad）和三元组（triad）
dyad.census(simplify(igraph))
triad.census(simplify(igraph))

#节点数量（number of nodes）和边数量（number of edges）
nodes_num <- length(V(igraph))
nodes_num

edges_num <- length(E(igraph))
edges_num

#平均度（average degree）
average_degree <- mean(degree(igraph))
#或者，2x边数量/节点数量
average_degree <- 2*edges_num/nodes_num
average_degree

#平均加权度（average weighted degree），仅适用于含权网络
average_weight_degree <- mean(strength(igraph))

#节点和边连通度（connectivity）
nodes_connectivity <- vertex.connectivity(igraph)
nodes_connectivity

edges_connectivity <- edge.connectivity(igraph)
edges_connectivity

#平均路径长度（average path length）
average_path_length <- average.path.length(igraph, directed = FALSE)
average_path_length

#网络直径（diameter）
graph_diameter <- diameter(igraph, directed = FALSE)
graph_diameter

#图密度（density）
graph_density <- graph.density(igraph)
graph_density

#聚类系数（clustering coefficient）
clustering_coefficient <- transitivity(igraph)
clustering_coefficient

#介数中心性（betweenness centralization)
betweenness_centralization <- centralization.betweenness(igraph)$centralization
betweenness_centralization

#度中心性（degree centralization）
degree_centralization <- centralization.degree(igraph)$centralization
degree_centralization

#模块性（modularity），详见 ?cluster_fast_greedy，?modularity，有多种模型
fc <- cluster_fast_greedy(igraph)
modularity <- modularity(igraph, membership(fc))

#同配混合（assortative mixing），例如
otu_class <- read.delim('node_attribute.txt', row.names = 1, stringsAsFactors = FALSE)
V(igraph)$group <- otu_class[V(igraph)$name,'group']
assortativity.nominal(igraph, (V(igraph)$group == 'class2')+1, directed = FALSE)

#互惠性（reciprocity），仅适用于有向网络
#reciprocity(igraph, mode = 'default')
#reciprocity(igraph, mode = 'ratio')

#选择部分做个汇总输出
igraph_character <- data.frame(
  nodes_num,    #节点数量（number of nodes）
  edges_num,    #边数量（number of edges）
  average_degree,    #平均度（average degree)
  nodes_connectivity,    #节点连通度（vertex connectivity）
  edges_connectivity,    #边连通度（edges connectivity）
  average_path_length,    #平均路径长度（average path length）
  graph_diameter,    #网络直径（diameter）
  graph_density,    #图密度（density）
  clustering_coefficient,    #聚类系数（clustering coefficient）
  betweenness_centralization,    #介数中心性（betweenness centralization)
  degree_centralization,    #度中心性
  modularity    #模块性（modularity）
)
igraph_character

write.table(igraph_character, 'igraph_characteryyg.txt', sep = '\t', row.names = FALSE, quote = FALSE)



#输入数据示例，邻接矩阵
#这是一个微生物互作网络，数值“1”表示微生物 OTU 之间存在互作，“0”表示无互作
adjacency_unweight <- read.delim('adjacency_unweight.txt', row.names = 1, sep = '\t', check.names = FALSE)
head(adjacency_unweight)[1:6]    #邻接矩阵类型的网络文件

#邻接矩阵 -> igraph 的邻接列表，获得非含权的无向网络
igraph <- graph_from_adjacency_matrix(as.matrix(adjacency_unweight), mode = 'undirected', weighted = NULL, diag = FALSE)
igraph    #igraph 的邻接列表

#计算节点度
V(igraph)$degree <- degree(igraph)

#模块划分，详情 ?cluster_fast_greedy，有多种模型
set.seed(123)
V(igraph)$modularity <- membership(cluster_fast_greedy(igraph))


#输出各节点（微生物 OTU）名称、节点度、及其所划分的模块的列表
#nodes_list <- data.frame(
  #nodes_id = V(igraph)$name, 
  #degree = V(igraph)$degree,
  #modularity = V(igraph)$modularity
#)
#head(nodes_list)    #节点列表，包含节点名称、节点度、及其所划分的模块

a<-as.character(V(igraph)$modularity)
nodes_list <- data.frame(
nodes_id=V(igraph)$name,
degree=V(igraph)$degree,
modularity=a)

write.table(nodes_list, 'monodes_listyyg.txt', sep = '\t', row.names = FALSE, quote = FALSE)

##计算模块内连通度（Zi）和模块间连通度（Pi）
source('zi.pi')

#上述的邻接矩阵类型的网络文件
adjacency_unweight <- read.delim('adjacency_unweight.txt', row.names = 1, sep = '\t', check.names = FALSE)

#节点属性列表，包含节点所划分的模块
nodes_list <- read.delim('monodes_listyyg.txt', row.names = 1, sep = '\t', check.names = FALSE)

#两个文件的节点顺序要一致
nodes_list <- nodes_list[rownames(adjacency_unweight), ]

#计算模块内连通度（Zi）和模块间连通度（Pi）
#指定邻接矩阵、节点列表、节点列表中节点度和模块度的列名称
zi_pi <- zi.pi(nodes_list, adjacency_unweight, degree = 'degree', modularity_class = 'modularity')
head(zi_pi)

write.table(zi_pi, 'zi_pi_resultyyg.txt', sep = '\t', row.names = FALSE, quote = FALSE)

##可再根据阈值对节点划分为 4 种类型，并作图展示其分布
library(ggplot2)

zi_pi <- na.omit(zi_pi)   #NA 值最好去掉，不要当 0 处理
zi_pi[which(zi_pi$within_module_connectivities < 2.5 & zi_pi$among_module_connectivities < 0.62),'type'] <- 'Peripherals'
zi_pi[which(zi_pi$within_module_connectivities < 2.5 & zi_pi$among_module_connectivities > 0.62),'type'] <- 'Connectors'
zi_pi[which(zi_pi$within_module_connectivities > 2.5 & zi_pi$among_module_connectivities < 0.62),'type'] <- 'Module hubs'
zi_pi[which(zi_pi$within_module_connectivities > 2.5 & zi_pi$among_module_connectivities > 0.62),'type'] <- 'Network hubs'

ggplot(zi_pi, aes(among_module_connectivities, within_module_connectivities)) +
  geom_point(aes(color = type), alpha = 0.5, size = 2) +
  scale_color_manual(values = c('gray','red','blue','purple'),
                     limits = c('Peripherals', 'Connectors', 'Module hubs', 'Network hubs'))+
  theme(panel.grid = element_blank(), axis.line = element_line(colour = 'black'),
        panel.background = element_blank(), legend.key = element_blank()) +
  labs(x = 'Among-module connectivities', y = 'Within-module connectivities', color = '') +
  geom_vline(xintercept = 0.62) +
  geom_hline(yintercept = 2.5)

