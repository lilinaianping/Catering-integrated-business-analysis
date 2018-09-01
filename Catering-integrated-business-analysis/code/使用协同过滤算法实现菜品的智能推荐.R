#1. 特征选取
# 对订单表和订单详情表进行特征选取
info <- read.csv("E:/data/data-1/meal_order_info.csv", stringsAsFactors = F)
detail <- read.csv("E:/data/data-1/meal_order_detail.csv", stringsAsFactors = F)

#数据预处理
head(detail$dishes_name)
detail$dishes_name <- gsub("\\s|\\n+", " ", detail$dishes_name)

#删除白饭的记录
detail <- detail[-which(detail$dishes_name == "白饭/小碗" | detail$dishes_name == "白饭/大碗"), ]

table(info$order_status)     #查看订单状态

#统计订单状态为0或2的订单占比
info.id <- info[which(info$order_status == 0 | info$order_status == 2), "info_id"]
proportion <- length(info.id) / nrow(info)
proportion

detail <- detail[-which(detail$order_id %in% info.id), ]  #删除detail数据中无意义的订单
info <- info[which(info$order_status == 1), ]    #提取订单状态为1的数据

#特征选取
info <- info[, c(1:3, 8:10, 12, 19:21)]
detail <- detail[, c(1:3, 6, 8, 9, 11, 19)]

#写出数据规约后的订单表和订单详情
write.csv(info, "E:/results/info_clear.csv",row.names = F)
write.csv(detail, "E:/results/detail_clear.csv",row.names = F)


#2. 构建模型
#构建两个推荐模型
install.packages("recommenderlab")
require(recommenderlab)

#将用户ID和菜品名称转换为0-1二元型数据，即模型的输入数据集
dishes <- as(detail[, c(8, 4)], "binaryRatingMatrix")
write.csv(as(dishes,"matrix"), "E:/results/dishes_matrix.csv")

model.IBCF <- Recommender(dishes, method = "IBCF")   # 建模

#导出相似度矩阵
dishes.model.sim <- as(model.IBCF@model$sim, "matrix")
write.csv(dishes.model.sim, "E:/results/dishes_model_sim.csv")

#利用模型对原始数据集进行预测，并获得推荐长度为30的结果
recommend.IBCF <- predict(model.IBCF, dishes, n =30)
as(recommend.IBCF, "list")[1:5]

#对list型结果采用sink与print命令将其保存
sink("E:/results/recommend_IBCF.txt")
print(as(recommend.IBCF, "list"))
sink()

#基于用户的协同过滤
model.UBCF <- Recommender(dishes, method = "UBCF")    #建模

recommend.UBCF <- predict(model.UBCF, dishes, n = 30)
as(recommend.UBCF,"list")[1:5]

sink("E:/results/recommend_UBCF.txt")
print(as(recommend.UBCF, "list"))
sink()


#3. 模型评价
#评价两个推荐模型
algorithms <- list("ItemCF" = list(name = "IBCF",param = NULL),
                   "UserCF" = list(name = "UBCF",param = NULL))

#将数据以交叉检验划分成10份，其中9份训练，1份测试
dishes.es <- evaluationScheme(dishes,method = "cross-validation",k=10,given = 1) 

#采用算法列表对数据进行模型预测与评价
results <- evaluate(dishes.es, algorithms, n = c(15,20,25,30,35))

#绘制出评价结果的图形
plot(results,"prec/rec",legend = "topleft",cex = 0.67)

#构建F1评价指标
fvalue <- function(p,r){
  return (2 * p * r / (p + r))
}

#求两个模型的各个评价指标的均值，并将其转换为数据框的形式
library(plyr)
index <- ldply(avg(results))

index[, 1] <- paste(index[, 1], c(15,20,25,30,35))  #重命名指标第一列中模型的名字

#计算两个模型的F1指标，并将所有指标综合
F1 <- fvalue(index[, 6], index[, 7])   
dishes.Fvalue <- cbind(index, F1)

#对评价指标值只保留3位小数
for (i in 2:ncol(dishes.Fvalue)){
  dishes.Fvalue[, i] <- round(dishes.Fvalue[, i], 3)
}
write.csv(dishes.Fvalue, "E:/results/dishes_predict_index.csv", row.names = F)







