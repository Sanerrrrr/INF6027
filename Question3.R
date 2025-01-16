#加载必要的库
library(dplyr)
library(ggplot2)

#读取数据
lineups <- read.csv("C:/Users/niebo/OneDrive/Desktop/euro_lineups.csv", header = TRUE)
summary <- read.csv("C:/Users/niebo/OneDrive/Desktop/euro_summary.csv", header = TRUE)

#清洗 summary 数据
clean_summary <- summary %>%
  select(year, goals) %>%
  group_by(year) %>%
  summarise(total_goals = sum(goals))  # 汇总每年的总进球数
#清洗 lineups 数据
clean_lineups <- lineups %>%
  select(name, position_field, year) %>%
  filter(!is.na(position_field))

#合并数据集，使用年份作为连接
merged_data <- clean_lineups %>%
  left_join(clean_summary, by = "year")

#构建线性回归模型，预测球员进球数基于年份和位置
model <- lm(total_goals ~ position_field + year, data = merged_data)
#查看线性回归模型的基本信息
summary(model)

#可视化模型预测结果
ggplot(merged_data, aes(x = year, y = total_goals, color = position_field)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "球员位置和年份对总进球数的影响", x = "年份", y = "总进球数") +
  theme_minimal()
