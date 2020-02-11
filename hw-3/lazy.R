cleaned <- read.csv("C:/Users/jaesu/OneDrive/Desktop/School_code/STAT390/hw-1/cleaned.csv")
x <- cleaned$Drink.week
y <- cleaned$Money.week

plot(sqrt(x), y)

lm.1 <- lm(y ~ x)
summary(lm.1)
abline(lm.1)

#R^2 = 0.4183 the variance of amount of money spend of week is due to the drinks per week 
#Se = 6.944 Typical error from the from the fit line is 6.944


par(mfrow=c(1,1)) 
transform_dat <- read.csv("C:/Users/jaesu/Downloads/transform_dat.txt", sep="")
x <- transform_dat$x
y <- transform_dat$y

plot(x,y)

xs <- sqrt(x)
ys <- sqrt(y)

plot(xs,ys)

lm.1 <- lm(ys ~xs)
summary(lm.1)
lines(lm.1$fitted.values)
abline(lm.1)

# 99% of transformed y is due to transformed and the typical error is 0.019

x <- transform_dat$x
y <- transform_dat$y
lm.2 <- lm(y ~ sqrt(x) + I(x))
summary(lm.2)

plot(x, lm.2$fitted.values)
plot(x, lm.1$fitted.values)


hw4 <- read.csv("~/hw4.txt", sep="")

x1 <- hw4$Depth
x2 <- hw4$Content
ys <- hw4$Strength

lm.1 <- lm(ys ~ x1 + x2 + I(x1^2) + I(x2^2) + x1 * x2)
summary(lm.1)

# b1 = -16.47521, b2 = 12.82710, b3 = 0.09555, b4 = -0.2433, b5 = 0.49864

#You cannot interpert the regression coeficients because the term x1*x2 means the change in x1 impacts x2 which means
# that the coefficents are uninterpertable 

# R^2 = .76 which means that these predictors are pretty good at determining the strength  

plot(ys - lm.1$fitted.values, lm.1$fitted.values)

# the model that was fitted is good because the plot of residuals vs the fitted values is scattered and follows no pattern

lm.2 <- lm(ys ~ x1 + x2)
summary(lm.2)

# R^2 = .447 which means the model is okay at determining the strength 

# Yes, because with only the lower order terms gives a lower R^2 which is the amount strength is determined by depth and content

# Se for model 2 is 9.01 and model 1 is 7.023 which means that on average model 1 is closer to the actual value 
# than in model 2


dat1 <- read.table("C:/Users/jaesu/Downloads/hw_3_dat1.txt", quote="\"", comment.char="", header = TRUE)

ydat1 <- dat1$y
x1dat1 <- dat1$x1
x2dat1 <- dat1$x2

plot(x1dat1, ydat1)
plot(x2dat1, ydat1)

cor(dat1)

lm.1 <- lm(ydat1 ~ x1dat1 + x2dat1)
summary(lm.1)
# R^2 = .235 Pretty weak

lm.2 <- lm(ydat1 ~ x1dat1 + x2dat1 + x1dat1:x2dat1)
summary(lm.2)
# R^2 = .9356 much stronger

lm.3 <- lm(ydat1 ~ x1dat1 + x2dat1 + I(x1dat1^2) + I(x2dat1^2) + x1dat1:x2dat1)
summary(lm.3)
# R^2 = .9358 not much stronger at all 

# ydat1 ~ x1dat1 + x2dat1 + x1dat1:x2dat1 this is probably the most accurate model, there is almost no colinearity 
# between x1 & x1 given that their correlation coefficent is about 5%, and the interaction is not over fitting 
# as the model jumped to be three times more accurate with the interaction and adding quadratic makes it 
# barely more accurate which makes it a lot more prone to overfitting. The model is definately nonlinear 

dat2 <- read.csv("C:/Users/jaesu/Downloads/hw_3_dat2.txt", sep="")

ydat2 <- dat2$y
x1dat2 <- dat2$x1
x2dat2 <- dat2$x2

plot(x1dat2, x2dat2)
plot(x1dat2, ydat2)
plot(x2dat2, ydat2)

cor(dat2)
plot(x1dat2, x2dat2)
# colinearity between x1 & x2


lm.11 <- lm(ydat2 ~ x1dat2 + I(x1dat2^2))
summary(lm.11)
# R^2 value is 0.8639

lm.12 <- lm(ydat2 ~ x2dat2 + I(x2dat2^2))
summary(lm.12)
# R^2 value is 0.8878

# The model used should be ydat2 ~ x2dat2 + I(x2dat2^2) there is colinearity between x1 and x2 which means 
# that one is unnecessary and should be removed, I chose x1 to be removed because R^2 is better for that one 


par(mfrow=c(2,2)) 
mult <- read.table("C:/Users/jaesu/Downloads/hw_3_mult_simple_dat.txt", quote="\"", comment.char="")

x1m <- mult$V1
x2m <- mult$V2
ym <- mult$V3

plot(x1m, ym)
plot(x2m, ym)

lm.1 <- lm(ym ~ x1m + x2m)
summary(lm.1)

lm.2 <- lm(ym ~ x1m)
summary(lm.2)

lm.3 <- lm(ym ~ x2m)
summary(lm.3)

# The beta for each regression model for x1 and x2 are the respective betas for the combined model. This makes 
# sense because if one x is held constant then the beta is the linear relationship between the x and the 
# predicted y 
min(x1m)
x <- seq(min(x1m), max(x1m), length =100)
y <- seq(min(x2m), max(x2m), length =100)

f <- function(x,y) {
  r <- lm.1$coefficients[1] + lm.1$coefficients[2]*x + lm.1$coefficients[3]*y
}


y.fit <- outer(x,y,f) 
y.fit

library(lattice)
cloud(y.fit, type="p")


par(mfrow=c(1,2)) 

ntrial = 5000 
xmax = numeric(ntrial)
for(trial in 1:ntrial) {
  x = rnorm(50, 0, 1)
  xmax[trial] = max(x)
}


hist(xmax, main="")

xmin = numeric(ntrial)
for(trial in 1:ntrial) {
  x = rnorm(50, 0, 1)
  xmin[trial] = min(x)
}
hist(xmin, main="")

hist(rexp(100, 2))



ntrial = 5000 
xmean = numeric(ntrial)
for(trial in 1:ntrial) {
  x = rexp(100, 2)
  xmean[trial] = mean(x)
}
hist(xmean, main="")

qqnorm(xmean, cex=0.5)

mean(xmean)
sd(xmean)
.5/sqrt(5000)

# The means are close but the predicted deviation is not close to the actual 
# actual = 0.07 predicted = 0.05







par(mfrow=c(2,2)) 
hw4.book.1 <- read.csv("~/hw4-book-1.txt", sep="")

x <- hw4.book.1$Cycfail
y <- hw4.book.1$Strampl

plot(x,y)
plot(log(x),y)
plot(log(x),log(y))
plot(1/x,1/y)

summary(lm(y ~ log(x)))

0.0197092 + -0.0012805*log(5000)


thickness <- c(220,220,220,220,370,370,370,370,440,440,440,440,680,680,680,680,860,860,860,860)
str <- c(24.0,22.0,19.1,15.5,26.3,24.6,23.1,21.2,25.2,24.0,21.7,19.2,17.0,14.9,13.0,11.8,12.2,11.2,6.6,2.8)
plot(thickness, str)
lm.6 <- lm(str ~ thickness + I(thickness^2))
lines(thickness, lm.6$fitted.values)
summary(lm(str ~ thickness + I(thickness^2)))

1.452e+01 + 4.323e-02*500 -6.001e-05 * 500^2

.3/6


