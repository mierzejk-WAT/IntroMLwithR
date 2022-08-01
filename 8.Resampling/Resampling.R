set.seed(30112)
data(mtcars)
n <- nrow(mtcars)

# Hold-out validation set
train <- sample(n, n/2)
lm.fit <- lm(mpg~disp, data=mtcars, subset=train) # lm.fit <- lm(mpg~poly(disp, 1), data=mtcars, subset=train)
val.set <- mtcars[-train,]
mse <- mean((val.set$mpg - predict(lm.fit, val.set))**2)
print(mse)

lm.fit2 <- lm(mpg~poly(disp, 2), data=mtcars, subset=train) # quadratic
mean((val.set$mpg - predict(lm.fit2, val.set))**2)
lm.fit3 <- lm(mpg~poly(disp, 3), data=mtcars, subset=train) # cubic
mean((val.set$mpg - predict(lm.fit3, val.set))**2)

# Leave-one-out cross validation
library(boot)
glm.fit <- glm(mpg~disp, data=mtcars)
cv.error <- cv.glm(mtcars, glm.fit) # cv.glm(mtcars, glm.fit, K=n)
cv.error$delta[1] # raw cross-validation estimate of prediction error

cv.error.1 <- rep (0, 6)
for (i in 1:6) {
  glm.fit <- glm(mpg~poly(disp, i), data=mtcars)
  cv.error.1[i] <- cv.glm(mtcars, glm.fit)$delta[1]
}
print(cv.error.1)

# k-fold cross validation
cv.error.8 <- rep (0, 6)
for (i in 1:6) {
  glm.fit <- glm(mpg~poly(disp, i), data=mtcars)
  cv.error.8[i] <- cv.glm(mtcars, glm.fit, K=8)$delta[1]
}
print(cv.error.8)

# bootstrap - parameters error
boot.fn <- function(data, indices) coef(lm(mpg~disp, data=data, subset=indices))
lm.fit <- lm(mpg~disp, data=mtcars)
print(lm.fit)
boot.fn(mtcars, 1:n)

boot.fn(mtcars, sample(n, n, replace=1))
boot(mtcars, boot.fn, 100)
summary(lm.fit)$coef

boot.fn <- function(data, indices) coefficients(lm(mpg~disp+I(disp^2)+I(disp^3), data=data, subset=indices))
summary(lm(mpg~disp+I(disp^2)+I(disp^3), data=mtcars))$coef
bootobj<-boot(mtcars, boot.fn, 100)
print(bootobj)
plot(bootobj, index=2)

# bootstrap - confidence intervals
boot.fn <- function(data, indices) summary(lm(mpg~disp+I(disp^2)+I(disp^3), data=data, subset=indices))$r.square
bootobj <- boot(mtcars, boot.fn, 100)
print(bootobj)
plot(bootobj)
boot.ci(bootobj, conf=.99, type='bca') # boot.ci(bootobj, conf=.99, type='bca', index=1)
