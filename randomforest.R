library(randomForest)
set.seed(820614)
fit.rf <- randomForest(x,y,importance=T,proximity=T,ntree=500)
print(fit.rf)
importance(fit.rf)
varImpPlot(fit.rf)
n = ncol(ma.train)-1
rate = 1
for(i in 1:n){
  set.seed(1)
  model <- randomForest(as.factor(y)~.,data = ma.train,mtry=i,importance=T,ntree=1000)
  rate[i] =mean(model$err.rate)
  #print(model)
}
set.seed(1)
model <- randomForest(as.factor(y)~.,data = ma.train,mtry=3,importance=T,ntree=1000)
plot(model,col = 1:1)
set.seed(1)
model <- randomForest(as.factor(y)~.,data = ma.train,mtry=3,importance=T,proximity=T,ntree=400)
print(model)
hist(treesize(model))
#MDSplot(model,ma.train$y,palette=rep(1,2),pch = as.numeric(ma.train$y))
rf_result <-predict(model, ma.test, type="response",norm.votes=F, predict.all=FALSE, proximity=FALSE, nodes=FALSE)#, cutoff = c(2/3,1/3))
# rd_pred <- rf_result[,1]
# for(j in 1:9){
#   if(rd_pred[j]>0.66)rd_pred[j]=0
#   else rd_pred[j]=1
# }
table(rf_result,y.test)

n = ncol(fin.train)-1
rate = 1
for(i in 1:n){
  set.seed(1)
  model <- randomForest(as.factor(y)~.,data = fin.train,mtry=i,importance=T,ntree=1000)
  rate[i] =mean(model$err.rate)
  #print(model)
}
set.seed(1)
model <- randomForest(as.factor(y)~.,data = fin.train,mtry=5,importance=T,ntree=1000)
plot(model,col = 1:1)
set.seed(1)
model <- randomForest(as.factor(y)~.,data = fin.train,mtry=5,importance=T,proximity=T,ntree=200)
print(model)
hist(treesize(model))
#MDSplot(model,ma.train$y,palette=rep(1,2),pch = as.numeric(ma.train$y))
rf_result <-predict(model, ma.test, type="response",norm.votes=F, predict.all=FALSE, proximity=FALSE, nodes=FALSE,cutoff = c(2/3,1/3))#cutoff = c(2/3,1/3)
table(rf_result,y.test)
