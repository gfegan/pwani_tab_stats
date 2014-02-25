
library(epicalc)
library(forign)
library(agricolae)

################################################
##PRACTICAL ##################################

#critical values for F-distribution with 3, and , 5 df at 95% CI
qf(.95,df1=3,df2=13)
qf(.99, df1=3,df2=12)

#critical values at 99% CI
qf(.99, df1=4, df2=6)
qf(.99, df1=6, df2=22)


##oneway anova
data<-read.dta("/Users/lwafula/KWTRP/person_profiles&requests/Pwani_university/ANOVA/anova_one_way.dta", convert.dates=TRUE, convert.factors=TRUE, missing.type=TRUE)

#Response.variable ~ Explanatory.variable
#check whether the factors are indeed
is.factor(drug)
is.factor(disease)


data
data<-data.frame(data)
names(data)
data$drug<-as.factor(data$drug)
data$disease<-as.factor(data$disease)

##### factorwise means for the systolics, comparison of the means of the variable across interest groups, and below with a plot
use(data)
tapply(systolic, drug, mean)  ##calculating the mean systolic at aech drug level
tapply(systolic, disease, mean)  ##calculating the mean systolic at aech disease  level

##### drug
plot(systolic~drug, data=data, pch=16, col="grey", xlab="drug level", ylab="systolic level", main="systolic vs. drug")

#which is just as good as applying the boxplot command below

boxplot(systolic~drug, data=data, pch=16, col="grey", xlab="drug level", ylab="systolic level", main="systolic vs. drug")  #there's an outlier in drug 1, unequal varience due to 
										#unequal box sizes, and Skewness due to assymetry of the boxes, compare the means by each level

#####disease
plot(systolic~disease, data=data, pch=16, col="grey", xlab="disease type", ylab="systolic level", main="systolic vs. disease")

#which is just as good as applying the boxplot command below

boxplot(systolic~disease, data=data, pch=16, col="grey", xlab="disease type", ylab="systolic level", main="systolic vs. disease")  #there's an outlier in drug 1, unequal varience due to 
										#unequal box sizes, and Skewness due to assymetry of the boxes. compare the means by level


##################################################
### the conventional way of testing with ANOVA on regression lines would involve 
lm1<-lm(systolic~drug, data=data)
summary(lm1)
#the design matrix
model.matrix(lm1)

#fitting the model without the intercept
lm2<-lm(systolic~drug -1, data=data)
summary(lm2)

#diagnostics
qqnorm(lm1$res, pch=16)
plot(lm1$fit,lm1$res,xlab="Fitted",ylab="Residuals",main="Residual-Fitted plot")
plot(jitter(lm1$fit),lm1$res,xlab="Fitted",ylab="Residuals",main="Jittered plot") #adding a little noise to the above

###then we would check on the ANOVA table using;
anova(lm1)

####################################################
##the direct R command to perform the above is

one.way<-aov(systolic~drug)
one.way                      ##gives the degrees of freedom and the sum of squares, the estimated treatments may be of unequal replication
summary(one.way)  			##gives the full anova table. check on the F_statistic and assess whether it's significant.
							###rejecting the null indicates a significant difference in amongst the different groups

plot1<-plot(one.way, pch=16) ##gives the various plots on the residuals, normality etc


###check out on multiple comparisons, necessary if the difference among the groups is significant, to know which groups are diff.
### 1. using the pairwise test
pairwise.t.test(systolic, drug, p.adj="none")
pairwise.t.test(systolic, disease, p.adj="none")

#or specify the correction method, in this case p.adj( the correction method) is 'bonferroni'
pairwise.t.test(systolic, drug, p.adj="bonferroni")   ##gives a matrix of p-values on the diff. amongst the groups. the p-values are used to
												##determine whether the diff are significant or not in the normal way
pairwise.t.test(systolic, disease, p.adj="bonferroni")

### 2. using the Turkey's test, which creates a set of confidence intervals on the diff means with specified family-wise prob. of coverage
lm1 = aov(systolic ~ drug, data=data)
TukeyHSD(lm1, conf.level =0.95)    ##check the p.adj(pvalue) and determine whether or not the difference is significant


#homogeneity of varience using the Levene's test, since the popln. varience is assumed to be the same
summary(lm(abs(lm1$res)~data$drug))

#Since the p-value is large, we conclude that there is no evidence of a non-constant variance.

####### ASSIGNMENT
#do the same procedures for the case of disease


###############################################################################

##TWO-WAY anova
##used to compare the means of populations that are classified in two diff. ways
use (data)
data

##we fit two ANOVA using the commands; Quantitative response and categorical independent variables

# lm(Response ~ FactorA + FactorB) ; without factor interactions
 
 #lm(Response ~ FactorA + FactorB + FactorA*FactorB) ; with interaction
 
 ##  EDA
tapply(systolic, disease, mean)
tapply(systolic, drug, mean)

par(mfrow=c(2,2))
plot( systolic~drug + disease, data=data, pch=16, col="white")  #gives boxplots for each
boxplot(systolic~drug + disease, data=data, pch=16, col="white") #combines the two


## Interaction plot; used to display the levels of one factor on the X-axis, & mean response
##for each treatment on the y-axis. In addition, a line connecting the means corresponding to each level of second factor. When no interaction is present, the lines are roughly parallel

interaction.plot(data$drug,data$disease,data$systolic)
interaction.plot(data$disease,data$drug,data$systolic)

interaction.plot(data$drug,data$disease,data$systolic, type="b", col=c(1:3),leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22), xlab="drug", ylab="systolic",  main="Interaction Plot")


##to confirm that the interactions are indeed parallel or not, we fit a two-way ANOVA
lm3 <- lm(systolic~drug+disease+ drug:disease,data)

summary(lm3)
anova(lm3)   ##check whther the p-value for interaction term is significant, look for the other vars, look for the main effects significance


###after the ANOVA fit, always check the relevant model assumptions, using the QQ plots and the residual plots

qqnorm(lm3$res)  ##check how they compare with normal quantiles

plot(lm3$fitted, lm3$res, xlab="Fitted", ylab="Residuals", pch=16, col="blue")  ##no pattern expected


####note 
##the model could still be written as
lm_3 <- lm(systolic~drug+disease+ drug*disease,data)

lm_3_1 <- lm(systolic~drug*disease,data)
summary(lm_3_1)
anova(lm_3_1)

###or better still use this
lm_3_2<-aov(systolic~drug*disease)
lm_3_2
summary(lm_3_2)   ###check for the difference in  errors btwn one and two way anova
tapply(systolic, list(drug, disease),sum) 	#looking at how the interaction was obtained, the sum 											of the 4 replicates in each of the 12 combinations of 
												the factor levels

#removing non-significant parameters from the models

model2<-update(lm_3_2, ~. - drug:disease)
anova(lm_3_2, model2)  #to compare the two models, look at the analysis of varience and keep 								updating if need be, basing on the p_value

plot(lm_3_2, pch=16, col="red")  ##for all the model criticism


#an omnibus command for the same

anova(lm(systolic~drug*disease, data))
##pairwise comparisons for the main effects especially where interaction isn't significant
pairwise.t.test(systolic, drug, p.adj="none")
pairwise.t.test(systolic, disease, p.adj="none")


##Post_ANOVA tests
####tukey's test for ANOVA

lm4 <- aov(systolic~drug+disease)
lm4
summary(lm4)  ##drug significant

posthoc_1 <- TukeyHSD(x=lm4, 'drug', conf.level=0.95)   #used to assess the inter-factor difference source
posthoc_1

###or use
q_1<-HSD.test(lm4, 'drug')
q_1  	##1 and 2, and 3 and 4, are shown to belong to the same grp, thus it's only the diff btn non group 		members that's signf

#################################################################################

# Analysis of Covariance 
###combines both ANOVA and regression, covariates are related to the response variables

zap()

########################################################################################################
###Practical2
data1<-data.frame(
yields=
c(5, 7, 3,
4, 2, 6,
5, 3, 6,
5, 6, 0,
7, 4, 0,
7, 7, 0,
6, 6, 0,
4, 6, 1,
6, 4, 0,
7, 7, 0,
2, 4, 0,
5, 7, 4,
7, 5, 0,
4, 5, 0,
6, 6, 3
),
ferts = factor(rep(c("A", "B", "C"), 15)),
prover = factor(rep(1:15, rep(3, 15))))

use(data1)
data1

#the average for the different ferts yields are
tapply(yields, ferts, mean)
#tapply(yields, prover, mean)

#to test whether there are differences between ferts and provers
lmm <- lm(yields ~ ferts + prover)
summary(lmm)
anova(lmm)  ##p value of 0.8175, the provers have no significant effect on yields, unlike the fertilizer type

##the ferts have significant difference on the yields, we tets with the Tukey's tets to assess the difference comes from 
lmm1 <- aov(yields ~ ferts + prover)
posthoc <- TukeyHSD(x=lmm1, 'ferts', conf.level=0.95)
posthoc  ##the adjusted p-values indicate that the differences C/A and C/B are significant


####the same as assessment to the above can be made using the plot below
plot(lmm1)

####we can also use the following procedure to assess where the difference actually lies

q1<-HSD.test(lmm, 'ferts')
q1  ##A and B are shown to belong to the same group 'a' while C from 'b', thus it's only the diff btn C and 	A/B that's signf

