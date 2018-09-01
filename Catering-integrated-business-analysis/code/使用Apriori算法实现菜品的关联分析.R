#构建购物篮信息
info <- read.csv("E:/results/info_clear.csv", stringsAsFactors = F)
detail <- read.csv("E:/results/detail_clear.csv", stringsAsFactors = F)

#建立aliment列表，每个列表代表一个订单的菜品
order.id <- unique(detail$order_id)     #对ID去重

aliment <- list()
for(i in 1:length(order.id)){
  aliment[[i]] <- detail[which(detail$order_id == order.id[i]), 4]
  aliment[[i]] <- unique(aliment[[i]])   #去掉重复的事物
}

#导出购物篮数据
require(plyr)
aliment1 <- ldply(aliment,rbind)
row.names(aliment1) <- as.character(order.id)
write.csv(aliment1, "E:/results/aliment.csv")


#构建二元矩阵和Apriori模型
#创建购物篮的二元矩阵
col <- levels(as.factor(unlist(aliment)))
ruleData <- matrix(FALSE, length(aliment), length(col))
colnames(ruleData) <- col
row.names(ruleData) <- as.character(order.id)

#将每一个订单中所包含的菜改为1
for(i in 1:length(aliment)){
  ruleData[i, match(aliment[[i]],col)] <- TRUE
}
write.csv(ruleData,"E:/results/ruleData.csv",row.names = FALSE)

#构建关联规则模型
#install.packages("arules")
library(arules)
# trans <- read.csv("E:/results/ruleData.csv", stringsAsFactors = F)
trans <- as(aliment, "transactions")

inspect(trans[1:5])

#生成关联规则
rules <- apriori(trans,parameter = list(support = 0.01, confidence = 0.5))
summary(rules)
inspect(sort(rules, by = list('support'))[1:10])

#绝对数量显示
itemFrequencyPlot(trans,type = 'absolute', topN = 10, horiz = T)

#查看前项为"芹菜炒腰花"的规则
item <- subset(rules,subset = rhs %in% "芹菜炒腰花")
inspect(sort(item, by="support"))

write(item, "E:/results/item.csv", sep = ",", row.names = F)
write(rules, "E:/results/rules.csv", sep = ",", row.names = F)


#计算综合评分
#提取规则的前项和后项
result <- read.csv("E:/results/rules.csv", stringsAsFactors = F)
meal.recom <- strsplit(result$rules,"=>")

#去掉中括号
lhs <- 0
rhs <- 0
for (i in 1:length(meal.recom)){
  lhs[i] <- gsub("[{|} + \n]|\\s", " ", meal.recom[[i]][1])
  rhs[i] <- gsub("[{|} + \n]|\\s", " ", meal.recom[[i]][2])
}

rules.new <- data.frame(lhs = lhs, rhs = rhs, support = result$support,
                        confidence = result$confidence, lift = result$lift)
write.csv(rules.new, "E:/results/rules_new.csv", row.names = F)

#进行综合评分
rules.new <- read.csv("E:/results/rules_new.csv", stringsAsFactors = F)
sales_volume <- read.csv("E:/results/sales_volume.csv", stringsAsFactors = F)
profit <- read.csv("E:/results/profit.csv", stringsAsFactors = F)
dish <- read.csv("E:/data/data-1/meal_dishes_detail.csv", stringsAsFactors = F)

#统计前项
rules.count <- as.data.frame(table(rules.new$lhs))
rules.count <- rules.count[order(rules.count$Freq, decreasing = TRUE), ]

#提取前项为"芹菜炒腰花、孑然羊排"的数据，对推荐的菜品进行综合分析
#计算每个菜所推荐的菜的综合评分
#设置A的权重，a1 = 1.5, a2 = 2.5, a3 = 2, a4 = 4
A <- matrix(c(0,2.5,2,4,
              1.5,0,2,4,
              1.5,2.5,0,4,
              1.5,2.5,2,0), 4, 4, byrow = T)
E <- c(1, 1, 1, 1)

#初始化
rules.new$sales <- 0
rules.new$recommendation <- 0
rules.new$profit <- 0
rules.new$mark <- 0

for (i in 1:nrow(rules.new)){
  #找到对应的热销度
  sales.num <- which(sales_volume$dishes_name == rules.new$rhs[i])
  rules.new$sales[i] <- sales_volume$sales_hot[sales.num]
  
  #找到对应的毛利率和主推度
  profit.num <- which(profit$dishes_name == rules.new$rhs[i])
  rules.new$profit[i] <- profit$rate[profit.num]
  rules.new$recommendation[i] <- profit$recommend_percent[profit.num]
  
  #计算综合得分
  Y <- c(rules.new$sales[i],rules.new$recommendation[i],
         rules.new$profit[i],rules.new$confidence[i])
  rules.new$mark[i] <- round((E - Y) %*% A %*% t(t(Y)), 3)
}

#对综合评分进行排序
rules.new <- rules.new[order(rules.new$mark, decreasing = TRUE), ]
write.csv(rules.new, "E:/results/recommend.csv", row.names = F)

#选取后项为"芹菜炒腰花"的数据
rules.item <- rules.new[which(rules.new$rhs == "芹菜炒腰花"), ]
write.csv(rules.item,"E:/results/rules_item.csv", row.names = F)





