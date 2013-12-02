#  Need to change this to R format and get comments around the original stata parts
# also need to decide upon the location where we are puting things

setwd("H:/Pwani_Collabo/tab_stats")
library(foreign)

# use G:\Continuous\maltreat.dta

maltreat <- read.dta("data/maltreat.dta") 
*dscribe the maltreat dataset
describe
*inspect the pvc variable for missing, maximum, minmum etc
codebook pcv
inspect pcv
*identifying values out of range (note normal pcv = 35% in children)
graph box pcv
sum pcv,detail

*check the distribution of pcv

#hist pcv, norm
x <- maltreat$pcv
h<-hist(x, breaks=40, col="red", xlab="PCV",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=100)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2) 


*desceptive statistics for pcv
sum pcv,detail

*se(mean) of 670 obs 
ci pcv
mean pcv

*analysis of first 60 obs
ci pcv if _n <= 60
mean pcv if _n <= 60
