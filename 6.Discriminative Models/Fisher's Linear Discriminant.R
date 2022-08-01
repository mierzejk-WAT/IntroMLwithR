data<-data.frame(
  x1=c(2.95, 2.53, 3.57, 3.16, 2.58, 2.16, 3.27),
  x2=c(6.63, 7.79, 5.65, 5.47, 4.46, 6.22, 3.52),
  y=c(1, 1, 1, 1, 0, 0, 0))

data.0<-data[data$y==0,1:2]
data.0.mean<-sapply(data.0, mean)
data.1<-data[data$y==1,1:2]
data.1.mean<-sapply(data.1, mean)

# maximizes S = ver.between / var.within
# when cov(data.0)~cov(data.1) -> LDA
w<-solve(cov(data.0)+cov(data.1))%*%(data.1.mean-data.0.mean) # vector normal to separating hyperplane

# Assuming same distributions of points projections for true and false classes
c<-t(w)%*%((data.1.mean+data.0.mean)/2)
#c<-sum(w*((data.1.mean+data.0.mean)/2))
discriminant<-function(x) {
  dotProduct<-sum(w*x);
  result<-dotProduct>c;
  attr(result, "dotProduct")<-dotProduct;
  result
}
apply(data[,1:2], 1, discriminant)

plot(data$x1, data$x2, col=ifelse(data$y, "red", "blue"), pch=16, asp=1)
arrows(2,4,2+w[1]/7,4+w[2]/7)
abline(drop(c)/w[2], -w[1]/w[2], lty=2, lwd=2, col="dark green")
