# Regression for the accidents per Bundesland and the population.
# works fine.

rm(list=ls())
setwd("~/Documents/ds/sem3/Felix/transport/mycode/data-visualisation")
bula <- read.csv('../data/bundeslaender.csv', sep=',') 

# simple linear regression

ma <- mean(bula$accidents)

lm0 <- lm(accidents~0+Einwohner31122019, data = bula)
summary(lm0)

lm1 <- lm(accidents~Einwohner31122019, data = bula)
summary(lm1)
plot(lm1$residuals, main='residuals for linear model')
abline(h=0, col='gray')

anova(lm1,lm0)
lm0$coeff
lm1$coeff
plot(bula$accidents~bula$Einwohner31122019, xlab='population' , ylab='accidents', main='population vs accidents')
abline(lm1$coeff, col='gray50')

plot(bula$accidents~0+bula$Einwohner31122019, 
     xlab='population' , ylab='accidents', 
     main='population vs accidents')

print(par())
abline(coeff=lm0$coeff*c(1,1))

plot(bula$accidents~0+bula$Einwohner31122019, 
     xlab='population', 
     ylab='accidents', 
     main='population vs accidents', 
     #asp=90, 
     xlim=c(0,18000000),
     ylim=c(2900, 60000))


plot(bula$accidents~0+bula$Einwohner31122019, 
     xlab='population (mio)', # keep the axis labels
     ylab='accidents (K)', 
     main='population vs accidents', 
     #asp=90, 
     xlim=c(0,18000000),
     ylim=c(2900, 60000),
     xaxt="n", yaxt="n", # remove the axis 
     pch=16)

xtick<-c(seq(0, 18000000, by=5000000),18000000)
xlabels<-c('0','5','10','15','18')
axis(side=1, at=xtick, labels = xlabels)

ytick<- seq(10000,60000,10000)
#  c(10000,20000,30000,40000,50000,60000)
ylabels<- seq(10,60,10)
#  c('10','20','30','40','50','60')
axis(side=2, at=ytick, labels = ylabels)

abline(lm1$coeff, col='green')

bula$apc = bula$accidents/bula$Einwohner31122019

##############################################

# boxplot(lm1$residuals)
# var(lm1$residuals)
plot(lm1$residuals, pch=16, col='red', main='residuals',
     ylab='residual', xlab='Bundesland')
abline(h=0,col='lightgray')

plot(c(-2,3), c(-1,5), type = "n", xlab = "x", ylab = "y", asp = 1)
## the x- and y-axis, and an integer grid
abline(h = 0, v = 0, col = "gray60")
text(1,0, "abline( h = 0 )", col = "gray60", adj = c(0, -.1))
abline(h = -1:5, v = -2:3, col = "lightgray", lty = 3)
abline(a = 1, b = 2, col = 2)
text(1,3, "abline( 1, 2 )", col = 2, adj = c(-.1, -.1))

