##defines your working space to save
#setwd("H:/New folder")
library(foreign) ## reads data written in another programming data language
library(Rmisc) # for calculating CI of the mean

#read in the data from a stata data file 
birthweight2<-read.dta("data/birthweight2.dta")

##Practical5

##explore data variables,labels and types
str(birthweight2)
#View(birthweight2)
table(birthweight2$lbw)
str(birthweight2$lbw)

##recode data to have normal weight=0
birthweight2$lbw2<-ordered(birthweight2$lbw,levels=c("Weight<2500","Normal 2500+"),labels=c(1,0))
#another way of doing it
birthweight2$lbw2b[birthweight2$lbw=="Weight<2500"]<- 1
birthweight2$lbw2b[birthweight2$lbw=="Normal 2500+"]<-0
#adding label to the first
birthweight2$lbw2<-factor(birthweight2$lbw2,levels=c(1,0),labels=c("normal birth weight","low birth weight"))
table(birthweight2$lbw2)
#adding labels to the second example
birthweight2$lbw2b<-factor(birthweight2$lbw2b,levels=c(1,0),labels=c("normal birth weight","low birth weight"))
table(birthweight2$lbw2)

##calculate confidence interval

#CI(as.numeric(birthweight2$lbw2), ci=0.95)
#claculating confidence interval by gender

#Test the hypothesis ie null hypthesis =0.9
prop.test(birthweight2$lbw2,n=length(birthweight2$lbw2 ))

#Test the hypothesis ie null hypthesis =0.9 by gender
binom.test(as.numeric(birthweight2$lbw2b))


#session 2
#Hypothesis test and CI for difference in proportions

#Practical 6
#how to compute a chi-square test
table(birthweight2$sex,birthweight2$lbw2)
#correct  logical indicating whether to apply continuity correction when computing the test statistic
chisq.test(birthweight2$sex,birthweight2$lbw2,correct = FALSE)


