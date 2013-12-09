## Remember to change to your working directory by typing "setwd()"
# setwd("C:/kilifi_course/data")
setwd("/home/thoya/Documents/kemri/MYDATA/kilifi_course/data")
## To know your current working directory, type "getwd()"

## Your working directory should have the datasets and the R-Scripts

## Practical 16

library(epicalc)
library(lattice)

## We want to examine the relationship between the outcome(birthweight)
## and explanatory factors (mothers age)

d1<-use("birthweight1.dta")
des()
summ()
# Check bweight
attach(d1)
summ(bweight, graph=T)

par(bg="cornsilk") 
 hist(bweight, col="lavender", main="") 
title(main="Histogram:Birthweight") 

## testing normality
 
opar <- par(mfrow=c(2,2)) # set up 1x2 graphics frame
shapiro.qqnorm(od_igg)
shapiro.qqnorm(nca2)
shapiro.qqnorm(mothage)
par(opar)  # reset graphics device


##Examine the univariate statistics for several variables
summ(data.frame(mothage,od_igg, nca2))  

#plot a histogram for the mothers age
histogram(~mothage, col="green", main="Mothers age distribution",type="density")

#plot a histogram for the od.igg
histogram(~od_igg, col="green", main="od_igg",type="density")

## Plot scatter graphs
opar <- par(mfrow=c(2,2)) # set up 1x2 graphics frame
par(bg="cornsilk") 

plot(bweight, mothage, col="red",main="scatterplot (a)")
plot(bweight, nca2, col="blue", main="scatterplot (b)")
plot(bweight, od_igg, col="green", main="scatterplot (c)")

# histogram( ~ bweight, data = .data,
#           xlab = "birth weight", type = "density",
#           panel = function(x, ...) {
#               panel.histogram(x, ...)
#               panel.mathdensity(dmath = dnorm, col = "black",
#                                 args = list(mean=mean(x),sd=sd(x)))
#           } )

# check for correlation between any two variables
cor.test(nca2, od_igg)# method is pearson's product moment correlation

cor.test(bweight,nca2)
cor.test(bweight,od_igg)

## fit a linear model
lm1<-lm(bweight~mothage)
summary(lm1)

opar <- par(mfrow=c(3,2),bg="cornsilk") # set up 1x2 graphics frame

 
lm2<-lm(bweight~od_igg)
plot(od_igg, bweight)
abline(lm2, col="red")
residuals<-lm2$residuals
#fitted<-lm2$fitted.values # same as above
shapiro.qqnorm(residuals)
plot(lm2)

# Fit a multiple linear regression
lm3<-lm(bweight~mothage*od_igg);summary(lm3)

# examine its goodness of fit by plotting its residuals
opar <- par(mfrow=c(3,2)) # set up 1x2 graphics frame
par(bg="cornsilk")

residuals<-lm3$residuals

shapiro.qqnorm(residuals)
plot(lm3)


