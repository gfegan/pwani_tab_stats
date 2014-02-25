
######################################################################################
## ASSIGNMENT
data<-read.dta("/Users/lwafula/KWTRP/person_profiles&requests/Pwani_university/ANOVA/anova_assignment.dta", convert.dates=T, convert.factors=T, missing.type=T)

use(data)

temperature<-as.factor(temperature)
table(temperature)

chemical<-as.factor(chemical)
table(chemical)

method<-as.factor(method)
table(method)

summ(yield)

###1. f-distribution pvalues in R; at 99% CI and df1=10, df2=14, and df1=13 df2=17
qf(.99, df1=10, df2=14)
qf(.99, df1=13, df2=17)

###2. one-way anova and diagnostics for chemical and temperature
one.way<-aov(yield~chemical)
one.way                      ##gives the degrees of freedom and the sum of squares, the estimated treatments may be of unequal replication
summary(one.way)  			##gives the full anova table. check on the F_statistic and assess whether it's significant.
							###rejecting the null indicates a significant difference in amongst the different groups



one.way1<-aov(yield~temperature)
one.way1                      ##gives the degrees of freedom and the sum of squares, the estimated treatments may be of unequal replication
summary(one.way1)  			##gives the full anova table. check on the F_statistic and assess whether it's significant.
							###rejecting the null indicates a significant difference in amongst the different groups


##post analysis tests
TukeyHSD(one.way, conf.level =0.95)    ##check the p.adj(pvalue) and determine whether or not the difference is significant


TukeyHSD(one.way1, conf.level =0.95)    ##check the p.adj(pvalue) and determine whether or not the difference is significant


##3.two way
###3.1 interaction between temperature
interaction.plot(data$chemical,data$temperature,data$yield)
interaction.plot(data$chemical,data$method,data$yield)

##3.2 two-way model
###or better still use this
lm_3_2<-aov(yield~chemical*temperature)
lm_3_2
summary(lm_3_2)   ###check for the difference in  errors btwn one and two way anova
tapply(yield, list(chemical, temperature),sum) 	#looking at how the interaction was obtained, the sum 											of the 4 replicates in each of the 12 combinations of 
												the factor levels

#removing non-significant parameters from the models

model_2<-update(lm_3_2, ~. - chemical:temperature)
anova(lm_3_2, model_2)  #to compare the two models, look at the analysis of varience and keep 								updating if need be, basing on the p_value

plot(lm_3_2, pch=16, col="red")  ##for all the model criticism

###post analysis tests

posthoc_1 <- TukeyHSD(x= lm_3_2, 'drug', conf.level=0.95)   #used to assess the inter-factor difference source
posthoc_1

###or use
q_1<-HSD.test(lm_3_2, 'drug')
q_1  	##1 and 2, and 3 and 4, are shown to belong to the same grp, thus it's only the diff btn non group 		members that's sign



###############################


###links 
http://www.stat.columbia.edu/~martin/W2024/R3.pdf 
http://www.stat.columbia.edu/~martin/W2024/R8.pdf 
