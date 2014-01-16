##defines your working space to save
#setwd("H:/New folder")
library(foreign) ## reads data written in another programming data language
library(Rmisc) # for calculating CI of the mean
library(epicalc)#check gfor 

#read in the data from a stata data file 
birthweight2<-read.dta("data/birthweight2.dta")

##Practical5

##explore data variables,labels and types
str(birthweight2)
#View(birthweight2)
table(birthweight2$lbw)
str(birthweight2$lbw)

##recode data to have normal weight=0
#birthweight2$lbw2<-ordered(birthweight2$lbw,levels=c("Weight<2500","Normal 2500+"),labels=c(1,0))
#another way of doing it
birthweight2$lbw2b[birthweight2$lbw=="Weight<2500"]<- 0
birthweight2$lbw2b[birthweight2$lbw=="Normal 2500+"]<-1
#adding label to the first
birthweight2$lbw2<-factor(birthweight2$lbw2,levels=c(1,0),labels=c("normal birth weight","low birth weight"))
table(birthweight2$lbw2)
#adding labels to the second example
birthweight2$lbw2b<-factor(birthweight2$lbw2b,levels=c(1,0),labels=c("normal birth weight","low birth weight"))
table(birthweight2$lbw2)

##calculate confidence interval
testlbw <-as.numeric(birthweight2$lbw2b)-1
CI(as.numeric(birthweight2$lbw2), ci=0.95)
ci.binomial(birthweight2$lbw2b)
ci(birthweight2$lbw2b)

testv <- c(0,1,0,1,1,1,1,0,0,0)
class(testv)
ci(testv)
ci.binomial(testv)
#claculating confidence interval by gender

#Test the hypothesis ie null hypthesis =0.9
prop.test(birthweight2$lbw2,n=length(birthweight2$lbw2 ))
prop.test(birthweight2$bweight ,n=641)
class(birthweight2$bweight)
str(birthweight2$bweight)

summary.factor(birthweight2$lbw2)
prop.table(table(birthweight2$lbw2) ) #giving the proportions
prop.test(x=length(birthweight2$bweight<2500), n=641 ,p=.9)

table(birthweight2$lbw2)
prop.test(80,641,p=0.9 )
datX <- subset(birthweight2, lbw2==1)
table(birthweight2$lbw2==1) 
prop.test(length(subset(birthweight2, lbw2==1)),641,0.9)
str(datX)

#Test the hypothesis ie null hypthesis =0.9 by gender
binom.test(as.numeric(birthweight2$lbw2b))


#session 2
#Hypothesis test and CI for difference in proportions

#Practical 6
#how to compute a chi-square test
table(birthweight2$sex,birthweight2$lbw2)
#correct  logical indicating whether to apply continuity correction when computing the test statistic
chisq.test(birthweight2$sex,birthweight2$lbw2,correct = FALSE)

length(which(data$X=="a"))

