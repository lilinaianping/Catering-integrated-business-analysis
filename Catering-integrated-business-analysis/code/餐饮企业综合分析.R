# 1. 统计每日用餐人数和销售额
info <- read.csv("E:/data/data-1/meal_order_info.csv")  #导入数据

info$use_start_time <- as.Date(info$use_start_time)   #转换时间格式
table(info$order_status)      #查看订单状态
info <- info[which(info$order_status == 1), ]         #提取订单状态为1的数据

#统计每日用餐人数与销售额
sale <- aggregate(info[, c(3,9)], list(info$use_start_time), sum)
colnames(sale) <- c("date","number","saleroom")         #修改列名

write.csv(sale,"E:/results/sale_day.csv",row.names = FALSE) #导出每日的用餐人数和销售额

# 每日用餐人数折线图
sale$date <- as.POSIXct(sale$date)     $ 转换date字段时间格式
plot(sale$date, sale$number, col= "blue", type= "b",
     xlab = "日期", ylab = "用餐人数")

#每日营业额的折线图
plot(sale$date, sale$saleroom, col = "blue", type = "o",
     xlab = "日期", ylab = "营业额")


#2. 计算菜品热销度
detail <- read.csv("E:/data/data-1/meal_order_detail.csv", stringsAsFactors = FALSE)

head(detail$dishes_name)      #查看前6个菜品名称
detail$dishes_name <- gsub("\\s|\\n+"," ",detail$dishes_name)    #删除菜品名称中的回车符和空格

#求出每个菜品的销售量
sales_volume <- aggregate(detail$counts, list(detail$dishes_name), sum)
colnames(sales_volume) <- c("dishes_name","counts")

#求出每个菜品的热销度
sales_formula <- (sales_volume$count - min(sales_volume$count)) /
  (max(sales_volume$count) - min(sales_volume$count))
sales_volume$sales_hot <- round(sales_formula, 3)

#查看热销度最高和最低的菜品
sales_volume[which(sales_volume$sales_hot == max(sales_volume$sales_hot)), ]
sales_volume[which(sales_volume$sales_hot == min(sales_volume$sales_hot)), ]

#绘制出热销度最高的前10个菜品的条形图
sales_volume <- sales_volume[order(sales_volume$sales_hot, decreasing = TRUE), ]
barplot(sales_volume[1:10, 3], names.arg = sales_volume[1:10, 1],
        xlab = "菜品名称", ylab = "热销度", col = "blue")

write.csv(sales_volume, "E:/results/sales_volume.csv",row.names = FALSE)  #导出数据


#3. 计算菜品毛利率
dish <- read.csv("E:/data/data-1/meal_dishes_detail.csv", stringsAsFactors = FALSE)
dish <- dish[, c(1, 3, 4, 8, 14)]   #特征选取

#删除菜品名称中的回车符
head(dish$dishes_name)
dish$dishes_name <- gsub("\\s|\\n+"," ", dish$dishes_name)

#得出毛利润
dish$rate <- round((dish$price - dish$cost) / (dish$price), 2)

#找出毛利润最高和最低的菜品
dish[which(dish$rate == max(dish$rate)), ]
dish[which(dish$rate == min(dish$rate)), ]

write.csv(dish, "E:/results/profit.csv",row.names = FALSE)


#使用ARIMA算法预测销售额
#1. 检测平稳性和纯随机性
sale <- read.csv("E:/results/sale_day.csv")

saleroom <- ts(sale[1:28, 3])
plot(saleroom, xlab = "时间", ylab = "销售额")      # 绘制时序图
acf(saleroom, lag.max = 30)     #绘制ACF图

#差分
saleroom.diff <- diff(saleroom, differences = 1, lag = 7)   #进行差分
acf(saleroom.diff, lag.max = 30)    #绘制差分后序列的ACF图

#单位根检验
library(tseries)
acf.test(saleroom.diff)

Box.test(saleroom, type = "Ljung-Box")     #纯随机性检验


#2. 构建模型
install.packages("TSA")
library(TSA)

#原序列定阶
saleroom.BIC <- armasubsets(y = saleroom, nar = 5, nma = 5)
plot(saleroom.BIC)

#差分后的序列定阶
saleroom.diff.BIC <- armasubsets(y = saleroom.diff, nar = 5, nma = 5)
plot(saleroom.diff.BIC)


install.packages("forecast")
library(forecast)
#初始化
checkout <- data.frame(p = 0, d = 0, q = 0, P = 0, D = 0, Q = 0,
                       "残差P值" = 0, "平均误差" = 0, "AIC" = 0, "BIC" = 0)
test_checkout <- data.frame(p = 0, d = 0, q = 0, P = 0, D = 0, Q = 0,
                            "残差P值" = 0, "平均误差" = 0, "AIC" = 0, "BIC" = 0)
j <- 1
test_model <- function(p, q, P, Q){
model <- Arima(saleroom, order = c(p, 0, q), seasonal = list(order = c(P, 1, Q), period = 7))
result <- Box.test(model$residuals, type = "Ljung-Box")

#预测
sale.forecast <- forecast(model, h =3, level = c(99.5))

#计算平均误差
error <- abs(as.numeric(sale.forecast[[4]]) - sale[29:31, 3]) / sale[29:31, 3]
p.value <- round(result$p.value, 4)  
print(paste('p=', p, ';q=', q, ';P=',P,',Q=',Q,';残差P值:',
            p.value,';平均误差:',mean(error),collapse = " ",
            'AIC: ', model$aic,'BIC: ',model$bic))
test_checkout[1,1] <- p
test_checkout[1,2] <- 0
test_checkout[1,3] <- q  
test_checkout[1,4] <- P  
test_checkout[1,5] <- 1  
test_checkout[1,6] <- Q
test_checkout[1,7] <- round(result$p.value, 4)
test_checkout[1,8] <- mean(error)
test_checkout[1,9] <- model$aic
test_checkout[1,10] <- model$bic
return(test_checkout)
}

for (p in c(0,2,3,4,5)){
  for (q in 1:5){
    if (q == 1){
      for (P in c(0,1)){
        for (Q in c(2,3,5)){
          test_checkout <- test_model(p,q,P,Q)
          checkout[j, ] <- test_checkout[1, ]
          j <- j +1
        }
      }
    }else {
      for (P in c(0)){
        for (Q in c(2,3,5)){
          test_checkout <- test_model(p,q,P,Q)
          checkout[j, ] <- test_checkout[1, ]
          j <- j + 1
        }
      }
    }
  }
}
write.csv(checkout, "E:/results/checkout.csv",row.names = F)

#取最优模型预测
#选取最佳模型
saleroom.arima <- auto.arima(saleroom, d = 2, trace = TRUE)
summary(saleroom.arima)

#构建模型
model <- Arima(saleroom,order = c(2,0,1),
               seasonal = list(order = c(0,1,3), period = 7))
summary(model)

Box.test(model$residuals, type = "Ljung-Box")  #纯随机性检验

#预测未来三天的销售额
sale.forecast <- forecast(model, h = 3, level = c(99.5))
plot(sale.forecast)

#计算平均误差
error <- abs(as.numeric(sale.forecast[[4]]) - sale[29:31, 3]) / sale[29:31, 3]
mean(error)










