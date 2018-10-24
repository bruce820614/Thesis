library(e1071)
# data(iris)
# x <- subset(iris, select = -Species)
# y <- iris$Species
# model <- svm(x, y)
# model <- svm(Species ~ ., data = iris)
# pred_result <- predict(model, x)
# table(pred_result,y)
# 
# plot(model, iris, Petal.Width ~ Petal.Length,
#      slice = list(Sepal.Width = 3, Sepal.Length = 4),color.palette = terrain.colors)
# 
# plot(model, iris, Sepal.Width ~ Petal.Width,
#      slice = list(Sepal.Length = 3, Petal.Length = 4),color.palette = terrain.colors)

x <- subset(data, select = -y)
y <- data$y
model_ma <- svm(x,y)
pred_result <- predict(model_ma, x)
table(pred_result,y)

x <- subset(ma.train, select = -y)
y <- as.factor(ma.train$y)
set.seed(820614)
model_ma <- svm(x,y,kernel = "radial",cross= 5,gamma = 0.0009765625,cost = 2048)#class.weights= c("0" = 1, "1" = 2)
summary(model_ma)
pred_result <- predict(model_ma, x)
table(pred_result,y)
x.test <- subset(ma.test, select = -y)
y.test <- as.factor(ma.test$y)
test_result <- predict(model_ma, x.test)
table(test_result,y.test)
set.seed(820614)
obj <- tune.svm(as.factor(y)~.,data = ma.train,kernal = "radial", sampling = "fix", gamma = 2^c(-10:10), cost = 2^c(0:20))
plot(obj, transform.x = log2, transform.y = log2)
plot(obj, type = "perspective", theta = 120, phi = 45)

obj 
# rm(list=ls(all=TRUE))
# library(MASS)
# data(cats)
# library(e1071)
# 
# SVM_RBF_Model <- svm(Sex~., data = cats)
# plot(SVM_RBF_Model,data=cats,color.palette = topo.colors)
# 
# SVM_Linear_Model <- svm(Sex~., data = cats,kernel="linear")
# train.result <- predict(SVM_Linear_Model,cats[,-1])
# table(train.result,cats$Sex)
# plot(SVM_Linear_Model,data=cats,color.palette = topo.colors)
x <- subset(fin.train, select = -y)
y <- as.factor(fin.train$y)
model_ma <- svm(x,y,kernel = "linear",cross= 5)#class.weights= c("0" = 1, "1" = 2)
summary(model_ma)
pred_result <- predict(model_ma, x)
table(pred_result,y)
x.test <- subset(fin.test, select = -y)
y.test <- as.factor(fin.test$y)
test_result <- predict(model_ma, x.test)
table(test_result,y.test)