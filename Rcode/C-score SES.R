#链接到 github 安装 EcoSimR 包
library(devtools)
install_github('GotelliLab/EcoSimR')

#加载 EcoSimR 包
library(EcoSimR)
library(MASS)
#读取示例数据
setwd('D:/R/YYZ')
comm<- read.csv('YYSPE.csv',header=T,row.names = 1,check.names = F)

#根据 EcoSimR 包的方法，计算 C-score，需转为 0-1 丰度矩阵
comm[comm>0] <- 1

#按分组拆分为 ENV1 和 ENV2 两组数据
comm_ENV1 <- comm[ ,1:6]
comm_ENV2 <- comm[ ,7:12]
comm_ENV3 <- comm[ ,13:18]

#计算基于 C-score 的零模型，详情 ?cooc_null_model
#本示例基于 1000 次随机置换模拟获得 C-score 零分布（实际使用时可能要设置高一些，如 Mo 等（2021）的 30000 次随机，不过可能会比较慢）
set.seed(123)
Cscore_ENV1 <- cooc_null_model(comm_ENV1, algo = 'sim9', metric = 'c_score', nReps = 1000, 
                               saveSeed = FALSE, burn_in = 500, algoOpts = list(), metricOpts = list(), suppressProg = FALSE)

summary(Cscore_ENV1)  #ENV1 群落的 C-score 和 SES
plot(Cscore_ENV1, type = 'hist')  #C-score 观测值（经验 C-score）和 C-score 模拟值（C-score 零分布）的比较

set.seed(123)
Cscore_ENV2 <- cooc_null_model(comm_ENV2, algo = 'sim9', metric = 'c_score', nReps = 1000, 
                               saveSeed = FALSE, burn_in = 500, algoOpts = list(), metricOpts = list(), suppressProg = FALSE)

summary(Cscore_ENV2)  #ENV2 群落的 C-score 和 SES
plot(Cscore_ENV2, type = 'hist')  #C-score 观测值（经验 C-score）和 C-score 模拟值（C-score 零分布）的比较

set.seed(123)
Cscore_ENV3 <- cooc_null_model(comm_ENV3, algo = 'sim9', metric = 'c_score', nReps = 1000, 
                               saveSeed = FALSE, burn_in = 500, algoOpts = list(), metricOpts = list(), suppressProg = FALSE)

summary(Cscore_ENV3)  #ENV1 群落的 C-score 和 SES
plot(Cscore_ENV3, type = 'hist')  #C-score 观测值（经验 C-score）和 C-score 模拟值（C-score 零分布）的比较

#根据上述统计好的 C-score 观测值（经验 C-score）、C-score 模拟值（C-score 零分布）以及 SES 等，作个手动记录输出
result <- data.frame(group = c('ENV1', 'ENV2','ENV3'), 
                     Cscore_obs = c(0.43137, 0.27273),  #C-score 观测值（经验 C-score）
                     Cscore_sim = c(0.41345 , 0.29293),  #C-score 模拟值（C-score 零分布）的均值
                     SES = c(1.5487 , -1.1464)  #标准化效应大小（SES）
)
write.table(result, 'result.txt', row.names = FALSE, sep = '\t', quote = FALSE)

#仿照 Mo 等（2021）的可视化方法，绘制三 Y 坐标图展示 C-score 观测值（经验 C-score）、C-score 模拟值（C-score 零分布）以及 SES
library(ggplot2)

p.Cscore_obs <- ggplot(data = result, aes(x = group, y = Cscore_obs)) +
  geom_col(fill = 'gray30', width = 0.6) + 
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = NA, color = 'gray30')) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(result$Cscore_obs, result$Cscore_sim)*1.2)) +
  labs(x = '', y = 'C-score obs')

p.Cscore_obs

p.Cscore_sim <- ggplot(data = result, aes(x = group, y = Cscore_sim)) +
  geom_col(fill = 'blue', width = 0.4) + 
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = NA, color = NA),  
        axis.text.y = element_text(color = 'blue'), axis.ticks.y = element_line(color = 'blue'), 
        axis.title.y = element_text(color = 'blue'), axis.line.y = element_line(color = 'blue')) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(result$Cscore_obs, result$Cscore_sim)*1.2)) +
  labs(x = '', y = 'C-score sim\n')

p.Cscore_sim

p.SES <- ggplot(data = result, aes(x = group, y = SES)) +
  geom_point(color = 'red', shape = 15, size = 5) + 
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = NA, color = NA),  
        axis.text.y = element_text(color = 'red'), axis.ticks.y = element_line(color = 'red'), 
        axis.title.y = element_text(color = 'red'), axis.line.y = element_line(color = 'red')) +
  labs(x = '', y = 'Standardized Effect Size (SES)')

p.SES

#备注：自定义函数 y3_plot() 的来源：https://mp.weixin.qq.com/s/Wl01G8_6-e0GgBLnbrK74A
y3_plot(gp1 = p.Cscore_obs, gp2 = p.Cscore_sim, gp3 = p.SES)
