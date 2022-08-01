library(e1071)
set.seed(2049)

x<-matrix(rnorm(2*20), ncol=2)
y<-c(rep(F, 10), rep(T, 10))
x[y,]=x[y,]+1
plot(x=x[,2], y=x[,1], col=3-y, xlab='x.2', ylab='x.1')

# Support Vector Classifier
dat<-data.frame(x=x, y=as.factor(y))
model<-svm(y~., data=dat, kernel='linear', cost=10, scale=F)
plot(model, dat)
# support vectors
model$index
summary(model)

model<-svm(y~., data=dat, kernel='linear', cost=0.2, scale=F)
plot(model, dat)
model$index

# 10-fold cross validation
model.tune<-tune(svm, y~., data=dat, kernel='linear', ranges=list(cost=10^(-3:2)))
summary(model.tune)
model.best<-model.tune$best.model
summary(model.best)

# test
x.test<-matrix(rnorm(2*20), ncol=2)
y.test<-sample(c(F, T), 20, rep=T)
x.test[y.test,]<-x.test[y.test,] + 1
dat.test<-data.frame(x=x.test, y=as.factor(y.test))
y.pred<-predict(model.best, dat.test)
table(predict=y.pred, actual=dat.test$y)

model<-svm(y~., data=dat, kernel='linear', cost=.05, scale=F)
y.pred<-predict(model, dat.test)
table(predict=y.pred, actual=dat.test$y)

# Maximal Margin Classifier
model<-svm(y~., data=dat, kernel='linear', cost=.Machine$integer.max, scale=F)
plot(model, dat)
model$index
y.pred<-predict(model, dat.test)
table(predict=y.pred, actual=dat.test$y)

# Support Vector Machine
x<-matrix(rnorm(2*200), ncol=2)
x[1:100,]<-x[1:100,]+2
x[101:150,]<-x[101:150,]-2
y<-c(rep(T, 150), rep(F, 50))
dat<-data.frame(x=x, y=as.factor(y))
plot(x=x[,2], y=x[,1], col=3-y, xlab='x.2', ylab='x.1')

partition<-sample(200, 100)
dat.train<-dat[partition,]
dat.test<-dat[-partition,]
model<-svm(y~., data=dat.train, kernel='radial', gamma=1, cost=1)
plot(model, dat.train)
summary(model)

model<-svm(y~., data=dat.train, kernel='radial', gamma=1, cost=1e6)
plot(model, dat.train)
model.tune<-tune(svm, y~., data=dat.train, kernel='radial',
                 ranges=list(cost=10^(-1:3), gamma=c(.5, 1:4)), tunecontrol=tune.control(cross=5))
summary(model.tune)
table(predict=predict(model.tune$best.model, dat.test), actual=dat.test$y)

suppressPackageStartupMessages(library(ROCR))
rocplot<-function(score, label, ...) {
  predob<-prediction(score, label)
  perf<-performance(predob, 'tpr', 'fpr')
  plot(perf, ...)
  sprintf('AUC=%2.2f%%', 100*unlist(performance(predob, 'auc')@y.values))
}

model<-svm(y~., data=dat.train, kernel='radial', decision.values=T, gamma=model.tune$best.model$gamma, cost=model.tune$best.model$cost)
fitted<-attr(predict(model, dat.train, decision.values=T), 'decision.values')
par(mfrow=c(1, 2))
rocplot(fitted, dat.train$y, main='Training set')

model.flex<-svm(y~., data=dat.train, kernel='radial', decision.values=T, gamma=4, cost=model.tune$best.model$cost)
fitted<-attr(predict(model.flex, dat.train, decision.values=T), 'decision.values')
rocplot(fitted, dat.train$y, col='blue', add=T)

# Test
fitted<-attr(predict(model, dat.test, decision.values=T), 'decision.values')
rocplot(fitted, dat.test$y, main='Test set')
fitted<-attr(predict(model.flex, dat.test, decision.values=T), 'decision.values')
rocplot(fitted, dat.test$y, col='blue', add=T)

# Multiclass
x<-rbind(x, matrix(rnorm(2*50), ncol=2))
y<-c(y, rep('neither', 50))
x[201:250,]<-x[201:250,]+3
dat<-data.frame(x=x, y=as.factor(y))
par(mfrow=c(1, 1))
plot(x, col=as.factor(y))
model<-svm(y~., data=dat, kernel='radial', cost=10, gamma=1)
plot(model, dat)

# Support Vector Regression
autodane<-read.table('/usr/miswdm/autodane.csv', header = TRUE, sep = ',', dec = '.')
model<-svm(Cena~., data=autodane, kernel='linear', cost=2, eps=.2, scale=T)
print(predict(model, data.frame(Wiek=8, Przebieg=60)))
