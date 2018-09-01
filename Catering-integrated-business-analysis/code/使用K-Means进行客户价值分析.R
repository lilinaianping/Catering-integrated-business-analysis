info <- read.csv("E:/data/data-1/meal_order_info.csv", stringsAsFactors = FALSE)
users <- read.csv("E:/data/data-1/users.csv", stringsAsFactors = FALSE)


# 数据预处理
info <- info[which(info$order_status == 1), ]  # 提取有效订单

# 对info的时间列按用户的ID去重
info_time <- info[, c("emp_id", "use_start_time")]
library(plyr)
info_time <- ddply(info_time, .(emp_id), tail, n = 1)

# 匹配用户的最后一次用餐时间
for (i in 1:nrow(info)) {
  num <- which(users$USER_ID == info$emp_id[i])
  users[num, "LAST_VISITS"] <- info$use_start_time[i]
}

user <- users[-which(users$LAST_VISITS == ""), c(1, 3, 13, 15)]  # 特征选取


# 构建RFM特征
# 构建F特征
user.value1 <- as.data.frame(table(info$emp_id))  # 统计每个人的用餐次数
colnames(user.value1) <- c("USER_ID", "F")  # 修改列名

# 构建M特征
user.value2 <- aggregate(info[, "expenditure"], list(info$emp_id), FUN = 'sum')
colnames(user.value2) <- c("USER_ID", "M")
user.value <- merge(user.value1, user.value2, by = c("USER_ID"))  # 合并两个表

# 构建R特征
user.value <- merge(user.value, user, by = c("USER_ID"))  # 合并两个表
# 转换时间格式
last_time <- as.Date(user.value$LAST_VISITS, "%Y/%m/%d")
finally <- as.Date("2016-8-31")  # 观测窗口结束时间

user.value$R <- as.numeric(difftime(finally, last_time, units = "days"))

user.value <- user.value[, c(1,4,7,2,3)]  # 特征提取
write.csv(user.value, "E:/results/user_value.csv", row.names = FALSE)


# 确定类数
user.value <- read.csv("E:/results/user_value.csv", stringsAsFactors = FALSE)
USER_ID <- user.value$USER_ID
ACCOUNT <- user.value$ACCOUNT
user.value <- user.value[, -c(1,2)]

# 标准化数据
standard <- scale(user.value)  # 数据标准化
write.csv(standard, "E:/results/standard.csv", row.names = FALSE)  # 写出数据


# 求组间距离平方和与总体距离平方和的比值（betweenss / totss，越接近1越好）
BT <- 0
for(i in 1:10){
  model <- kmeans(user.value, centers = i)
  BT[i] <- model$betweenss / model$totss
}
plot(1:10, BT, type = "b", xlab = "聚类数", ylab = "组间平方和/总体距离平方和")



# 构建模型
set.seed(123)
result <- kmeans(standard, 3)

result$center  # 查看聚类中心值
result$size  # 查看每一类的个数


# 导出聚类后的数据
users.class <- cbind(USER_ID, ACCOUNT, user.value, class = result$cluster)
write.csv(users.class, "E:/results/users_class.csv", row.names = FALSE)


# 每一簇各指标的关系程度--雷达图
#install.packages("fmsb")
library(fmsb)
max <- apply(result$centers, 2, max)
min <- apply(result$centers, 2, min)
radar <- data.frame(rbind(max, min, result$centers))
radarchart(radar, pty = 32, plty = c(1:3), plwd = 4, vlcex = 1.2)  # 画雷达图
# 给雷达图加图例
L <- 1.2
for(i in 1:3){
  legend(1.0, L, legend = paste("客户群", i), lty = i, lwd = 3, col = i, bty = "n")
  L <- L - 0.3
}







































































































