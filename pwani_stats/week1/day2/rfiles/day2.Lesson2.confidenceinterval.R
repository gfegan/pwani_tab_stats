


##########Show that not all the times the CI covers the True population parameter
m = 50; n=20; p = .5;         # toss 20 coins 50 times
phat = rbinom(m,n,p)/n        # divide by n for proportions
SE = sqrt(phat*(1-phat)/n)    # compute SE
alpha = 0.10 #Level of significance.
zstar = qnorm(1-alpha/2)
matplot(rbind(phat - zstar*SE, phat + zstar*SE),
        rbind(1:m,1:m),type="l",lty=1)
abline(v=p)                  # draw line for p=0.5


#This are the solutions to the Quiz.
#Suppose we’ve collected a random sample of 10 recently graduated students and asked #them what their annual salary is. Imagine that this is the data we see, 44617,7066, 17594, #2726, 1178, 18898, 5033, 37151, 4514, and 4000.


#Goal: Estimate the mean salary of the graduated children. Find a 90 and 95 % confidence #interval for the mean.
#Setting 1: Assume that incomes are normally distributed with unknown mean and SD = ksh15,000.


#Create a Vector named X with the values
x<-c(44617,7066, 17594, 2726, 1178, 18898, 5033, 37151, 4514,  4000)

#90 % CI means that we have an alpha of 0.05. We can get z(alpha/2) from R.
qnorm(1-(0.1)/2)
qnorm(0.95)

#We can round this value 
round(1.644,2)


#determine the mean of x
mean(x)

#determine the margin of error
me<-1.64*(15000/sqrt(10))

#Lower limit
mean(x)-me

#Upper limit
mean(x)+me

#90% ci (6498.5, 22056.9)


###95 % CI means that we have an alpha of 0.05. We can get z(alpha/2) from R.
qnorm(1-(0.05)/2)
qnorm(0.975)

#We can round this value 
round(1.959964,2)


#determine the mean of x
mean(x)

#determine the margin of error
me<-1.96*(15000/sqrt(10))

#Lower limit
mean(x)-me

#Upper limit
xbar+me


#######Try for 90 % CI.



#Setting II:Same problem, only now we do not know the value for the SD.

#####90 % CI
#We need a t-value 
#degrees of freedom are n-1 = 10-1=9
qt(0.95,9)

#determine the margin of error
me<-qt(0.95,9)*sd(x)/sqrt(10)

#lower(limit)
mean(x)-me

#upper limit
mean(x)+me

#(5381.94, 23173.46)



#####95 % CI
#We need a t-value 
#degrees of freedom are n-1 = 10-1=9
qt(0.975,9)

#determine the margin of error
me<-qt(0.975,9)*sd(x)/sqrt(10)

#lower(limit)
mean(x)-me

#upper limit
mean(x)+me

#(3299.868,25255.53)


##########Calculation of the CI from a given dataset
library(Hmisc) #library(inspect)
library(psych) #summarizing data
library(epicalc) #epdim* packages (ci, sd, )

##reads data
#--------------------------------------------------------------------------------
#setwd("put the path to the cd")
#-------------------------------------------------------------------------

maltreat<-read.dta("data/maltreat.dta")

##describe the maltreat data set
#describe(maltreat)
str(maltreat) 

##inspect the pcv variable for missing,maximum,minimum etc
describe(maltreat$pcv)
summary(maltreat$pcv)
#inspect(maltreat$pcv) though not available for r 3.0.2

##identify values out of range(note:normal pcv=35% in children)
boxplot(maltreat$pcv, xlab="pcv" , ylab="frequency")
describe(maltreat$pcv) #fro, pysch package
##check the distribution of pcv
hist(maltreat$pcv, freq=F)
lines(density(maltreat$pcv))

##descriptive statistics for pcv
summary(maltreat$pcv)
describe(maltreat$pcv)

##se(mean) of 670 obs
#confint(maltreat$pcv,level=0.95)
ci(maltreat$pcv,ci=0.95)
mean(maltreat$pcv)

#std <- function(x) sd(x)/sqrt(length(x))
#se.pcv <- std(pcv)

##analysis of first 60 obs
maltreat.60 <- maltreat[1:60,]
ci(maltreat.60$pcv,ci=0.95)
mean(maltreat.60$pcv)







