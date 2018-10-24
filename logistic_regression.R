data <- read.csv("D:\\論文\\Data_originalx.csv")
#scale(data$degree_centrality)
x <- data$x9
normalized_x = (x-min(x))/(max(x)-min(x))
y <- data$x11
normalized_y = (y-min(y))/(max(y)-min(y))
data[,12] = normalized_x
data[,14] = normalized_y
data <- data[,c(-1,-2,-16)]
table(data$y)
write.csv(data,"D:\\論文\\Data_normalizex.csv")
## try http:// if https:// URLs are not supported
# source("https://bioconductor.org/biocLite.R")
# biocLite("logicFS")
# library(logicFS)
attach(data)
y <- as.factor(data$y)
#logic.bagging(y~., data=data, recdom = F)
ma.train <- data[1:24,]
ma.test <- data[25:33,]
nrow(ma.train)
attach(ma.train)
train.result <- glm(as.factor(y)~.,data = ma.train,family = binomial(link = "logit"))
summary(train.result)
print(train.result)
plot(exp(train.result$coefficients))
coeff <- train.result$coefficients
odds <- exp(train.result$coefficients)
write.csv(coeff,file = "D:\\論文\\logit_coefficient_final.csv")
write.csv(odds,file = "D:\\論文\\logit_oddsratio_final.csv")
library(boot)
k    <- 5
kfCV <- cv.glm(data=ma.train, glmfit=train.result, K=k)
kfCV$delta
pred <- predict(train.result,newdata = ma.test,type = "response")
pred <- (pred>0.66) * 1
#pred <- round(pred)
pred
write.csv(pred,file = "D:\\論文\\logit_output.csv")
library(caret)
library(e1071)
set.seed(1)
ctrl <- trainControl(method = "repeatedcv", number = 5, savePredictions = TRUE)
attach(ma.train)
mod_fit <- train(as.factor(y) ~ .,  data=ma.train, method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 1)
#"bayesglm","LogitBoost","logicBag"
pred = predict(mod_fit, newdata=ma.test)
cm <- confusionMatrix(data=pred, as.factor(ma.test$y))
true <- factor(c(1,0,0,1,0,0,1,0,0))
sensitivity(pred,true)
specificity(perd,true)

library(adabag)
#bagging()
#bagging.cv()

fin_data <- data[,-10:-13]
fin.train <- fin_data[1:24,]
fin.test <- fin_data[25:33,]
nrow(fin.train)
attach(fin.train)
library(caret)
library(e1071)
set.seed(7)
ctrl <- trainControl(method = "repeatedcv", number = 5, savePredictions = TRUE)
attach(fin.train)
as.factor(fin.train$y)
as.factor(fin.test$y)
mod_fit_fa <- train(as.factor(y) ~ .,  data=fin.train, method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 1)
#"logicBag","bayesglm","LogitBoost"
pred_fa = predict(mod_fit_fa, newdata=fin.test)
cm_fa <- confusionMatrix(data=pred_fa, as.factor(fin.test$y))
true <- factor(c(1,0,0,1,0,0,1,0,0))
sensitivity(pred_fa,true)
specificity(perd_fa,true)

fa_train.result <- glm(as.factor(y)~.,data = fin.train,family = binomial(link = "logit"))
coeff_fa <- fa_train.result$coefficients
write.csv(coeff_fa,file = "D:\\論文\\logit_coefficient_fa_final.csv")
odds_fa <- exp(fa_train.result$coefficients)
write.csv(odds_fa,file = "D:\\論文\\logit_odds_fa_final.csv")
# library(caretEnsemble)
# library(hybridEnsemble)
# library(randomGLM)
# randomGLM::randomGLM()
# randomGLM::predict.randomGLM()
library(rms)
r2_all <- lrm(as.factor(y)~.,data = ma.train)
r2_fin <- lrm(as.factor(y)~.,data = fin.train)
