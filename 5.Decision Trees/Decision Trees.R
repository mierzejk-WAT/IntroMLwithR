suppressPackageStartupMessages(library(caret))
set.seed(10)

# A simulated data set containing sales of child car seats at 400 different stores.
data<-ISLR::Carseats
data$High<-as.factor(ifelse(data$Sales>7, 'Yes', 'No')) # > 7 000
partition<-createDataPartition(data$High, p=.5, list=F)
data.test<-data[-partition,]

# Classification tree 1
library(tree)
tree.carseats<-tree(High~.-Sales, data, split='deviance', subset=partition)
summary(tree.carseats)
dev.off()
plot(tree.carseats)
text(tree.carseats, cex=0.75)
print(tree.carseats)

tree.pred<-predict(tree.carseats, data.test, type='class')
cm<-table(tree.pred, data.test$High, dnn=c('Predicted', 'Actual'))
print(cm)
sprintf("Accuracy: %.2f%%", 100 * sum(diag(cm))/sum(cm))

# Cross-validation -> pruning
cv.carseats<-cv.tree(tree.carseats, FUN=prune.misclass, K=20) # FUN=prune.tree -> method == deviance
print(cv.carseats)
plot(cv.carseats$size ,cv.carseats$dev, type="b")

prune.carseats<-prune.misclass(tree.carseats, best=11)
plot(prune.carseats)
text(prune.carseats, cex=0.75)

tree.pred<-predict(prune.carseats, data.test, type='class')
cm<-table(tree.pred, data.test$High, dnn=c('Predicted', 'Actual'))
print(cm)
sprintf("Accuracy: %.2f%%", 100 * sum(diag(cm))/sum(cm))

# Classification tree 2
set.seed(5)
library(rpart)
library(rpart.plot)
tree.carseats<-rpart(High~.-Sales, data[partition,], method='class')
print(tree.carseats)
dev.off()
#plot(tree.carseats)
#text(tree.carseats, cex=0.75)
rpart.plot(tree.carseats, cex=0.75)

tree.pred<-predict(tree.carseats, data.test, type='class')
cm<-table(tree.pred, data.test$High, dnn=c('Predicted', 'Actual'))
print(cm)
sprintf("Accuracy: %.2f%%", 100 * sum(diag(cm))/sum(cm))

# Cross-validation -> pruning
printcp(tree.carseats)
plotcp(tree.carseats)
cp<-tree.carseats$cptable[which.min(tree.carseats$cptable[,"xerror"]),"CP"]
prune.carseats<-prune(tree.carseats, cp)
rpart.plot(prune.carseats, cex=0.75)

tree.pred<-predict(prune.carseats, data.test, type='class')
cm<-table(tree.pred, data.test$High, dnn=c('Predicted', 'Actual'))
print(cm)
sprintf("Accuracy: %.2f%%", 100 * sum(diag(cm))/sum(cm))

# Regression Tree
data <- read.table('/usr/miswdm/cadata.dat', head=TRUE)
tree.value<-tree(log(MedianHouseValue)~Longitude+Latitude, data=data)
dev.off()
plot(tree.value)
text(tree.value, cex=0.75)

price.deciles<-quantile(data$MedianHouseValue, 0:10/10)
cut.prices<-cut(data$MedianHouseValue, price.deciles, include.lowest=T)
# Recursive Binary Splitting
plot(data$Longitude, data$Latitude, col=grey(10:2/11)[cut.prices], pch=20, xlab="Longitude", ylab="Latitude")
partition.tree(tree.value, ordvars=c('Longitude', 'Latitude'), add=TRUE)
print(tree.value)

tree.value<-tree(log(MedianHouseValue)~Longitude+Latitude, data=data, mindev=0.02) # default mindev=0.01
dev.off()
plot(tree.value)
text(tree.value, cex=0.75)
predict(tree.value, newdata=list(Latitude=33.49, Longitude=-117.65), type='tree')
exp(predict(tree.value, newdata=list(Latitude=33.49, Longitude=-117.65), type='vector'))
