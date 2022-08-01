library(ISLR)
Hitters<-na.omit(Hitters)
#Hitters<-Hitters[complete.cases(Hitters),]
colnames(Hitters)
ncol(Hitters)
set.seed(30112)

library(leaps)
# target function == RSS; ref -> leaps
summary(regsubsets(Salary~., Hitters)) # method = 'exhaustive search', nvmax=8
regfit.full<-regsubsets(Salary~., Hitters, nvmax=19)
reg.summary<-summary(regfit.full)
names(reg.summary)
# R**2
print(reg.summary$rsq)

par(mfrow=c(2, 2))
plot(reg.summary$rss, xlab='|Variables|', ylab='RSS', type='l')
plot(reg.summary$adjr2, xlab='|Variables|', ylab='Adj R^2', type='l') +
  points(which.max(reg.summary$adjr2), reg.summary$adjr2[which.max(reg.summary$adjr2)], col='red', cex=2, pch=20)
plot(reg.summary$cp, xlab='|Variables|', ylab='Cp', type='l') +
  points(which.min(reg.summary$cp), reg.summary$cp[which.min(reg.summary$cp)], col='red', cex=2, pch=20)
plot(reg.summary$bic, xlab='|Variables|', ylab='BIC', type='l') +
  points(which.min(reg.summary$bic), reg.summary$bic[which.min(reg.summary$bic)], col='red', cex=2, pch=20)

plot(regfit.full, scale='r2')
plot(regfit.full, scale='adjr2')
plot(regfit.full, scale='Cp')
plot(regfit.full, scale='bic')

# lowest BIC
coef(regfit.full, which.min(reg.summary$bic))

# forward / backward stepwise selection
regfit.fwd<-regsubsets(Salary~., Hitters, nvmax=19, method='forward')
summary(regfit.fwd)
regfit.bck<-regsubsets(Salary~., Hitters, nvmax=19, method='backward')
summary(regfit.bck)

library(stats)
# PCA applies only to numerical variables
httrs<-Hitters[,-c(14, 15, 20)]
pca<-princomp(as.formula(paste('~', paste(colnames(httrs[,-17]), collapse= "+"), sep='')),
              data=httrs, cor=T) # correlation matrix
#pca<-prcomp(as.formula(paste('~', paste(colnames(httrs[,-17]), collapse= "+"), sep='')), data=httrs, scale=TRUE)
summary(pca)
# columns of eigenvectors
loadings(pca)
dev.off()
plot(pca)
screeplot(pca, type='l')
biplot(pca, cex=0.5, col=c('gray50', 'black'))

library(e1071)
components.1.2 <- pca$loadings[,c(1,2)]
scores <- as.matrix(httrs[,-17]) %*% components.1.2

httrs$Salary <- httrs[,17] > median(httrs$Salary)
model<-naiveBayes(httrs[,-17], httrs$Salary)
table(predict(model, httrs[,1:16]), httrs[,17], dnn=c("Predicted","Actual"))
model.pca<-naiveBayes(scores, httrs[,17])
table(predict(model.pca, scores), httrs$Salary, dnn=c("Predicted","Actual"))
