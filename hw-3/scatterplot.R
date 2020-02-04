cleaned <- read.csv("C:/Users/jaesu/OneDrive/Desktop/School_code/STAT390/hw-1/cleaned.csv")
cleaned$Drink.week
hist(cleaned$Drink.week)
hist(cleaned$Money.week)
windows.options(width=10, height=10)
plot(sqrt(cleaned$Drink.week), cleaned$Money.week, main="Drinks per week vs Money per week", pch=19)


qqnorm(cleaned$Drink.week, cex=0.5)
qqnorm(cleaned$Money.week, cex=0.5)
library(lattice)
qqmath(cleaned$Drink.week, dist=qexp, cex=0.5)
qqmath(cleaned$Money.week, dist=qexp, cex=0.5)

?qqmath


MOE <- c(29.8,33.2,33.7,35.3,35.5,36.1,36.2,36.3,37.5,37.7,38.7,38.8,39.6,41.0,42.8,42.8,43.5,45.6,46.0,46.9,48.0,49.3,51.7,62.6,69.8,79.5,80.0)
Str <- c(5.9,7.2,7.3,6.3,8.1,6.8,7.0,7.6,6.8,6.5,7.0,6.3,7.9,9.0,8.2,8.7,7.8,9.7,7.4,7.7,9.7,7.8,7.7,11.6,11.3,11.8,10.7)

sStr <- Str^2
sMOE <- MOE^2

mean(Str^2)
mean(MOE^2)


mean(Str) * mean(MOE)


sd(MOE)
sd(Str)

sum(Str*(MOE-45.10741)) / (13.267*1.66*26)

sum((Str - (0.107*MOE + 3.31))^2)



plot(MOE,Str)
boxplot(MOE)
boxplot(Str)

mean(Str)

cor(MOE, Str)

mod <- lm(Str ~ MOE)
summary(mod)