
###############Sample size calculation Solutions.

library(epicalc) #package with functions for sample size calculations.


#Quiz No. 1
	#CI=95%
    #deff=0.05
    
#R as a calculator
0.5*(1-0.5)*(1.96/0.05)^2

#384 students


#Using package epicalc
args(n.for.survey)

n.for.survey(0.5, delta=0.05)


## Sample of 384 fresh year students



Quiz No. 2
##Using R as a calculator
#Subsitite in the formula given in the presentation
#subsitute values for the numerator
num<-(2*(7.7^2))*(0.84+1.96)^2

#subsitute values for the denominator
den<-5^2

n=num/den

print(n) # 37.18. Always round up to 38 as we cannot have fractions of individuals.

#The required sample size is 38. 

#Add a third of 38


#Use the epicalc package for calculating the sample size.
#use the function(n.for.2means) for a difference in two means
#view the required arguments for this function
args(n.for.2means)

#subsitute these values
n.for.2means(45,40, sd1=7.7, sd2=7.7, alpha=0.05, power=0.80)

#A sample size of 38 in each group will be sufficient to detect #a difference of 5 points on the Beck scale of suicidal #ideation, assuming a standard deviation of 7.7 points, a #power of 80%, and a significance level of 5%.




#Quiz 3.

#Using R as a calculator
    #power=80%
    #Effect size=2.5 kgs
    #Standard deviation=3.5 kgs

((2*(3.5^2))*(0.84+1.96)^2)/(2.5^2)

#31 in each group

##Using epicalc
n.for.2means(7.5,5, sd1=3.5, sd2=3.5, alpha=0.05, power=0.80)

#To determine the power in this situation
power.for.2means(7.5,5,sd1=3.5,sd2=3.5,alpha=0.05, n1=32,n2=32)








