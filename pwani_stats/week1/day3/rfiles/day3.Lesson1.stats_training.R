##defines your working space to save
setwd("/Users/lwafula/KWTRP/person_profiles&requests/Pwani_university/data")
#install.packages('data.table')
#install.packages('xtable')
library(xtable)
library(data.table)
library(foreign) ## reads data written in another programming data language
library(epicalc)
require(stats)

#--------------------------------------------------------------------------------
#setwd("put the path to the cd")
#-------------------------------------------------------------------------
#read in the data from a stata data file 
birthweight2<-read.dta("birthweight2.dta")
head(birthweight2, 10) # Veiw the first 10 observations, useful in cheking that the data has been read in correctly

#########################################################################################

### Introduction to the basics of Binomial distribution in R
# binomial distribution and tests

# Generates a sequence of numbers from 1 to 50 
x <- seq(0,50,by=1)     

# generate density ; the probability of success is 0.2
y <- dbinom(x,50,0.2)
plot(x,y, pch=16, col="red")         # most probable number of succeses in 50 trials is 10 (10/50 = 0.2!) from the plot

# if probability of success is now 0.6
y <- dbinom(x,50,0.6)
plot(x,y, pch=16, col="blue")     # the number of successes with highest density is now 30 

#cumulative probability; probability <=5 success in 10 trials with probability of success per trial= 0.5
pbinom(5,10,0.5)
   
# inverse cumulative probabilities; gives the number of success given cumulative probability 
qbinom(0.623046,10,0.5)  #the inverse for : pbinom(5,10,0.5)


### Introduction to the basics of proportions, their standard errors and hypothesis tests:
### Two-tailed hypothesis tests


#Problem
#Suppose a coin toss turns up 12 heads out of 20 trials. At .05 significance level, can one reject the null hypothesis that the coin toss is fair?

#Solution using the confidence interval

# The null hypothesis is that p = 0.5. We begin with computing the test statistic.
pbar = 12/20           # sample proportion 
p0 = .5                # hypothesized value 
n = 20                 # sample size 
z2 = (pbar-p0)/sqrt(pbar*(1-pbar)/n) 
z2                      # test statistic 


# We then compute the critical values at .05 significance level.
alpha = .05 
z.half.alpha = qnorm(1-alpha/2) 
c(-z.half.alpha, z.half.alpha) 


#Answer
#The test statistic 0.9128709 lies between the critical values -1.9600 and 1.9600. Hence, at .05 significance level, we do not reject the null hypothesis that the coin toss is fair.

#Alternative Solution 1: p_value

pval = 2*pnorm(z2, lower.tail=FALSE)  # upper tail 
pval                   # two tailed p-value 

#Alternative Solution 2: use of prop.test

prop.test(12, 20, p=0.5, correct=FALSE) 

#for a one-sided alternative test, make the necessary changes for the alpha and do the same, listed here is the calculation for prop.test
prop.test(85, 148, p=.6, alt="less", correct=FALSE) 
prop.test(85, 148, p=.6, alt="greater", correct=FALSE) 


###CONFIDENCE INTERVALS
#Interval Estimate of Population Proportion


#Problem
#Compute the margin of error and estimate interval for the female students proportion in survey at 95% confidence level.
#Solution

#We first determine the proportion point estimate
table(birthweight2$sex)
n1 = length(birthweight2$sex)    # valid responses count 
k = sum(birthweight2$sex == "Female") 
pbar = k/n1; pbar 

#Then we estimate the standard error.
SE = sqrt(pbar*(1-pbar)/n1); SE     # standard error 

#Since there are two tails of the normal distribution, the 95% confidence level would imply the 97.5th percentile of the normal distribution at the upper tail.
E = qnorm(.975)*SE; E              # margin of error 

#Combining it with the sample proportion, we obtain the confidence interval.
pbar + c(-E, E) 

#Alternative Solution
#Instead of using the textbook formula, we can apply the prop.test function in the built-in stats package.
prop.test(k, n1, correct=F) 

########################################################################################

##Practical5

##explore data variables,labels and types
names(birthweight2)
str(birthweight2)  #displays the internal structure of the object
codebook(birthweight2) #gives summary statistics for numeric variables and one-way tabs for factors

#View(birthweight2$lbw)
table(birthweight2$lbw)
summary(birthweight2$bweight)
str(birthweight2$lbw)

##recode data to have normal weight=0
birthweight2$lbw2[birthweight2$lbw=="Weight<2500"]<- 1
birthweight2$lbw2[birthweight2$lbw=="Normal 2500+"]<-0

#another way of doing it
#birthweight2$lbw2<-ordered(birthweight2$lbw,levels=c("Weight<2500","Normal 2500+"),labels=c(1,0))

table(birthweight2$lbw2)

##calculate confidence interval for the proportion (for the <2500 i.e 80/641)
ci(birthweight2$lbw2==1)

#########################################################################

#calculating confidence interval by gender
#create the male subset (35/326)
table(birthweight2$sex, birthweight2$lbw)      # crosstabulate sex and birthweight 
birthweight.male <- subset(birthweight2, birthweight2$sex=="Male") # a dataset for males 
ci(birthweight.male$lbw2==1)
#create the female subset
birthweight.female <- subset(birthweight2, birthweight2$sex=="Female")  # a dataset for males 
ci(birthweight.female$lbw2==1)

#using the dataTable library -require(data.table) # using the by loop
require(data.table) 
#data2[,sapply(propTest=prop), simplify=F]      
data <- data.table(birthweight2)     
data[,data.frame(ci(lbw2)),by=sex]
  

#Test the hypothesis ie null hypthesis =0.9


n<-sum(birthweight2$lbw2==0) # Number of normal birth weights
N<-length(birthweight2$lbw2)  # Total number of birts 

#the proportional test
prop.test(n,N,p=0.9 )

# Alternatively
#prop.test(sum(birthweight2$lbw2==0),length(birthweight2$lbw2),p=0.9 )

#Test the hypothesis ie null hypthesis =0.9 by gender
prop.test(sum(birthweight.male$lbw2==0),length(birthweight2$lbw2),p=0.9)
prop.test(sum(birthweight.female$lbw2==0),length(birthweight2$lbw2),p=0.9)

#using the dataTable library -require(data.table) #using the by loop
require(data.table)
data2 <- data.table(birthweight2)
data2[,c(propTest=prop.test(length(which(lbw2== 0)),n=length(lbw2),p=0.9 ),p=0.9 ),by=sex]


#performing an exact test of a simple binomial exp.
binom.test(sum(birthweight.male$lbw2==0),length(birthweight2$lbw2),p=0.9)   # Approximation was good due to the hight numbers
binom.test(sum(birthweight.female$lbw2==0),length(birthweight2$lbw2),p=0.9)




#session 2 (covered by the above code)

#Hypothesis test and CI for difference in proportions
#
#and this is how simple can get!! Two-Sample Proportions Test

#A random sample of 428 adults from Bofa Beach reveals 128 smokers. A random sample of 682 adults from 
#Watamu reveals 170 smokers. Is the proportion of adult smokers in Bofa Beach different from that in Watamu?

prop.test(x=c(128,170), n=c(428,682), alternative="two.sided",correct=F)


#Practical 6
#how to compute a chi-square test
mytable <- table(birthweight2$sex,birthweight2$lbw2)
mytable
summary(mytable)
#or
chisq.test(birthweight2$sex,birthweight2$lbw2,correct = FALSE)

#or
fisher.test(birthweight2$sex,birthweight2$lbw2)

mytable2 <- table(birthweight2$ht,birthweight2$lbw2)
mytable2
summary(mytable2)
#or
chisq.test(birthweight2$ht,birthweight2$lbw2,correct = FALSE)

mytable3 <- table(birthweight2$ethnic,birthweight2$lbw2)
mytable3
summary(mytable3)
#or
chisq.test(birthweight2$ethnic,birthweight2$lbw2,correct = FALSE)


#Practical 7
#measures of association - Odds ratios
#create a categorical variable from maternal age 
min.matagegp <- min(birthweight2$matage)
max.matagegp <- max(birthweight2$matage)
           
birthweight2$matagegp[birthweight2$matage <= 29]<- 1
birthweight2$matagegp[birthweight2$matage>29 & birthweight2$matage <=34]<- 2
birthweight2$matagegp[birthweight2$matage>34 & birthweight2$matage <=39]<- 3
birthweight2$matagegp[birthweight2$matage>39]<- 4

### labeling the cateorical maternal age as a factor
birthweight2$matagegp <- factor(birthweight2$matagegp,levels = c(1,2,3,4),labels = c("23 - 29", "30 - 34", "35 - 39", "40+"))
table(birthweight2$matagegp)

#**Create a categorical varible "gestwkgp" from gestation in weeks "gestwks"
br<-c(min(birthweight2$gestwks),32,36,40,max(birthweight2$gestwks))   # To define where to break the continouts variable gestwk
birthweight2$gestwkgp<-cut(birthweight2$gestwks, breaks=br, include.lowest=T)     # Use function cut to get data into bins

  
# Association between low birth weight and hypertention: OR
birthweight2$ht2 <- birthweight2$ht
birthweight2$ht2[birthweight2$ht==2] <- 0     # change coding from 1,2 to 1,0
cc(birthweight2$lbw2 , birthweight2$ht2)      # First augument is outcome , next is exposure 
cs(birthweight2$lbw2 , birthweight2$ht2)



# Association between low birth weight and gestation age: Trend
#check for trend in the "gestwkgp" categories
t<- table(birthweight2$lbw2 ,birthweight2$gestwkgp)  # Number of normal/low birth weight by gestation age 
t

x<-t[2,] # number of low birth weights
n<-apply(t,2,FUN=sum)   # sum along the columns of t to obtain the tota number of birth in each gestation age group 
x/n      # show the proportons, proportin of lbw reduces with increased gestation 

prop.trend.test(x,n)     # Trend test; Significant 

# Association between low birth weight and Matenal age
cc(birthweight2$lbw2 ,birthweight2$matagegp) 
fisher.test(birthweight2$lbw2 ,birthweight2$matagegp) 
   
# Association between hypertension and Maternal age   
cc(birthweight2$ht ,birthweight2$matagegp)    
 