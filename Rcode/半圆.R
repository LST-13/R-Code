# 基于netET和ggraph展示微生物与环境因子网络相关性
# Time：2022.12.11
# Author：LingC.TONG
#########################################################
## install.packages("devtools")
# 建议全部更新,但是容易报错
#devtools::install_github("Hy4m/netET", force = TRUE)
#加载包
library(ggraph)
library(tidygraph)
library(netET)
setwd("D:/R/LH")
# 读取数据
varespec <- read.csv("lhsx.csv",header = T,row.names = 1)
varechem <- read.csv("hx.csv",header = T,row.names = 1)


p1 <- correlate(varespec, varechem, method = "pearson") |> 
  as_tbl_graph(abs(r) > 0.5, p < 0.05) |>
  mutate(Degree = centrality_degree())
xy <- layout_on_circle(p1)
ggraph(p1, xy) +
  #geom_edge_fan(aes(colour = r > 0), width = 0.75, linetype="dashed") + #width 改变线条粗细
  geom_edge_fan(aes(colour = r > 0), width = 0.75) +
  geom_node_point(aes(size = Degree), colour = "#fa8c35") +
  scale_edge_colour_manual(values = c("TRUE" = "#c93756", "FALSE" = "#21a675"),#R>0,为TRUE
                           labels = c("Negative", "Positive")) +
  geom_node_text(aes(x = 1.07 * x,
                     y = 1.07 * y,
                     label = name,
                     angle = node_angle(x, y)),
                 hjust = "outward",
                 data = function(data) dplyr::filter(data, Degree > 0)) +
  expand_limits(x = c(-1.5, 1.5), y = c(-1.5, 1.5)) + #
  coord_fixed(clip = "off") + 
  theme(panel.background = element_blank()) + 
  labs(edge_colour = "Pearson's r")
ggsave("hxp1.png", width = 6, height = 6)
p2 <- correlate(varechem, varespec, method = "pearson") |> 
  as_tbl_graph(abs(r) > 0.5, p < 0.05) |>
  mutate(Degree = centrality_degree()) |>
  as_bipartite_circular(outer_nodes = names(varespec))
ggraph(p2, layout_bipartite_circular(p2)) +
  annotate_arc_rect(0, 180, 
                    fill = "#e0eee8", 
                    r0 = 0.55, 
                    r1 = 1.05) +
  geom_edge_circular(aes(colour = r > 0), edge_width = 0.75, edge_alpha = 0.8) +
  geom_node_point(aes(size = Degree, colour = Degree == 0)) +
  geom_node_text_circular(expand = 0.08) +
  scale_colour_manual(values = c("TRUE" = "grey55","FALSE" = "#065279"),
                      guide = "none") +
  scale_edge_colour_manual(values = c("TRUE" = "#c93756", "FALSE" = "#21a675"),
                           labels = c("Negative", "Positive")) +
  coord_fixed(clip = "off", xlim = c(-1.2, 1.2), ylim = c(0, 1.1)) +
  theme(panel.background = element_blank()) +
  guides(edge_colour = guide_legend(override.aes = list(edge_width = 1))) +
  labs(edge_colour = "pearson's r")
ggsave("p2.png", width = 8, height = 8)