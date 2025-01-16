library(ggplot2)
library(dplyr)
library(cluster)
#加载必要的库

data <- read.csv("C:\\Users\\niebo\\OneDrive\\Desktop\\euro_summary.csv")
#读取数据

if("result" %in% names(data)) {
  #如果列存在，进行数据清洗
  clean_data <- data %>%
    filter(!is.na(result)) #过滤掉score列中的NA值
  print("数据已成功清洗。")
} else {
  #如果列不存在，输出错误信息
  stop("错误：数据中不存在名为 'result' 的列。请检查列名。")
}
#查看清洗后的数据
head(clean_data)
#检查列名并进行数据清洗

set.seed(123) #设置随机种子，确保结果可复现
k <- kmeans(clean_data[, c("matches", "goals")], centers = 3) #这里我们把k的值设为3(Here we set the value of k to 3)
clean_data$cluster <- k$cluster
#查看聚类结果
print(k$centers) #打印聚类中心
#聚类分析

ggplot(clean_data, aes(x = result, y = goals, color = factor(cluster))) +
  geom_point() +
  labs(title = "Cluster analysis of team skills and tactics", x = "Score", y = "GoalS") +
  theme_minimal()
#使用ggplot2进行可视化