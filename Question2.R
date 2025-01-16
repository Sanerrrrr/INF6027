# 加载必要的库
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)

#读取数据
coaches <- read.csv("C:/Users/niebo/OneDrive/Desktop/euro_coaches.csv", header = TRUE)
lineups <- read.csv("C:/Users/niebo/OneDrive/Desktop/euro_lineups.csv", header = TRUE)
summary <- read.csv("C:/Users/niebo/OneDrive/Desktop/euro_summary.csv", header = TRUE)

#数据清洗
clean_coaches <- coaches %>%
  select(country, name, year) %>%
  filter(!is.na(name))

clean_summary <- summary %>%
  mutate(is_final = ifelse(grepl("Spain - England|England - Spain", final), "Yes", "No")) %>%
  filter(is_final == "Yes") %>%
  select(year, matches, goals) 
#确保只选择需要的列

clean_lineups <- lineups %>%
  select(country_code, name, position_field, year, id_match) %>%
  filter(!is.na(position_field))

#数据合并
merged_data <- clean_summary %>%
  inner_join(clean_coaches, by = "year") %>%
  left_join(clean_lineups, by = c("name" = "name", "year" = "year"))

#确保数据已正确合并
print(merged_data)

#分组和汇总
grouped_data <- merged_data %>%
  group_by(name) %>%
  summarise(total_goals = sum(goals, na.rm = TRUE), matches = n(), .groups = 'drop')

#构建决策树模型
tree_model <- rpart(total_goals ~ matches + name, data = grouped_data, method = "anova", model = TRUE)

#可视化决策树模型
rpart.plot(tree_model, main = "Decision tree for the effect of coach experience on team scoring", type = 4, extra = 101, roundint = FALSE)

