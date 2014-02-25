####
#requires the following packages 
library(lattice)
library(nlme)
####

ipomopsis <- read.delim("F:/R course material/Rose_wk1/day6/day6_data/ipomopsis.txt")
names(ipomopsis)
tapply(ipomopsis$Fruit,ipomopsis$Grazing, mean)
plot(Fruit~Root,data=ipomopsis)
plot(Fruit~Root,pch=16+(Grazing=="Grazed"),data=ipomopsis)
plot(Fruit~Root,pch=16+(Grazing=="Grazed"),col=as.numeric(Grazing),data=ipomopsis)

model<-lm(Fruit~Root*Grazing, data=ipomopsis)
summary(model)
drop1(model,test = "F")

## To draw the first line "Grazed", use the intercept of Grazed (-127.821, with the slope of the line, 23.560)
abline (-127.821,23.560)
abline (-127.821+36.1032,23.560, col="red")

#Model validation

plot(model)
EGr <- resid(model)
plot(model)
hist(EGr, xlab = "Residuals", main = "")
plot(ipomopsis$Grazing, EGr, xlab = "Grazing",ylab = "Residuals")
fligner.test(EGr, ipomopsis$Grazing)


###PRACTICE
# read in the data and exploration
Clams <- read.delim("F:/R course material/Rose_wk1/day6/day6_data/Clams.txt")
names(Clams)
Clams$fMONTH <- factor(Clams$MONTH)
Clams$LNAFD <- log(Clams$AFD)
Clams$LNLENGTH <- log(Clams$LENGTH)
coplot(LNAFD ~ LNLENGTH | fMONTH, data = Clams)
xyplot(LNAFD ~ LNLENGTH | fMONTH, data = Clams)
M1 <- lm(LNAFD ~ LNLENGTH * fMONTH, data=Clams)
summary(M1)
drop1(M1,test = "F")

### BOXCOX

library(MASS)
par(mfrow=c(1,2))
boxcox(Clams$AFD~Clams$LENGTH+Clams$MONTH)## before transoformation
boxcox(log(Clams$AFD+1)~log(Clams$LENGTH+1)+Clams$fMONTH)##after transformation
par(mfrow=c(1,1))

##drop1 compares the full model with a model in which the interaction is dropped,
##and an F-test is used to compare the residual sum of squares of both the models

##Model validation
plot(M1)
E <- resid(M1)
plot(M1, add.smooth = FALSE, which = 1)
plot(M1, add.smooth = FALSE, which = 2)
hist(E, xlab = "Residuals", main = "")
plot(Clams$LNLENGTH, E, xlab = "Log(Length)",ylab = "Residuals")
plot(Clams$fMONTH, E, xlab = "Month",ylab = "Residuals")
fligner.test(E, Clams$fMONTH)## test for homegeneity of variances between groups (months in this case)
bartlett.test(E, Clams$fMONTH)## test for homogeneity of variances (senstive to non-normality)
hist(E[Clams$MONTH == 12])##gives a histogram of the Error distribution per month 

## CONCLUSION
#The conclusion of the linear regression (or ANCOVA) model is that there is a significant
#relationship between biomass, length, and month with a weak but significant
#interaction between the length and the month. 
#However, even with a p-value of 0.02 for the interaction term, the lack of homogeneity of varaince demands one uses a different kind of model

## the order in which you put your terms affects the output! 


#requires the following packages 
library(lattice)
library(nlme)
####

### read in the data and exploration

Clams <- read.delim("F:/R course material/Rose_wk1/day6/day6_data/Clams.txt")
names(Clams)
Clams$fMONTH <- factor(Clams$MONTH)
Clams$LNAFD <- log(Clams$AFD)
Clams$LNLENGTH <- log(Clams$LENGTH)
coplot(LNAFD ~ LNLENGTH | fMONTH, data = Clams)
xyplot(LNAFD ~ LNLENGTH | fMONTH, data = Clams)
M1 <- lm(LNAFD ~ LNLENGTH * fMONTH, data=Clams)
summary(M1)
drop1(M1,test = "F")

##drop1 compares the full model with a model in which the interaction is dropped,
##and an F-test is used to compare the residual sum of squares of both the models

##Model validation
plot(M1)
E <- resid(M1)
plot(M1, add.smooth = FALSE, which = 1)
plot(M1, add.smooth = FALSE, which = 2)
hist(E, xlab = "Residuals", main = "")
plot(Clams$LNLENGTH, E, xlab = "Log(Length)",ylab = "Residuals")
plot(Clams$fMONTH, E, xlab = "Month",ylab = "Residuals")
fligner.test(E, Clams$fMONTH)## test for homegeneity of variances between groups (months in this case)
bartlett.test(E, Clams$fMONTH)## test for homogeneity of variances (senstive to non-normality)
hist(E[Clams$MONTH == 12])##gives a histogram of the Error distribution per month 

## CONCLUSION
#The conclusion of the linear regression (or ANCOVA) model is that there is a significant
#relationship between biomass, length, and month with a weak but significant
#interaction between the length and the month. 
#However, even with a p-value of 0.02 for the interaction term, the lack of homogeneity of varaince demands one uses a different kind of model

##NOTE in ANCOVA use lm function instead of aov since the order in which you put your terms affects the output

## What if we were not interested with the variation in MONTH

xyplot(LNAFD ~ LNLENGTH, data = Clams)

M2<- aov(LNAFD ~ LNLENGTH, data = Clams)
summary(M2)
plot(M2)
E2<- resid(M2)
F2<-fitted.values(M2)
plot(M2, add.smooth = FALSE, which = 1)
plot(M2, add.smooth = FALSE, which = 2)
hist(E2, xlab = "Residuals", main = "")
plot(Clams$LNLENGTH, E2, xlab = "Log(Length)",ylab = "Residuals")
fligner.test(LNAFD ~ LNLENGTH, data = Clams)

M3<- aov(LNAFD ~ LNLENGTH + Error(fMONTH), data = Clams)
summary(M3)

M4<- aov(LNAFD ~ fMONTH+LNLENGTH , data = Clams)
summary(M4)

### repeated measures ANOVA

Growth<- read.delim("F:/R course material/Rose_wk1/day6/day6_data/fertilizer.txt")
names(Growth)
GrowthB<-groupedData(root~week|plant,outer=~fertilizer,Growth)## this function groups the data to allow the use of trellis plotting
plot(GrowthB)##Grouped data with the nesting structure specified above
plot(GrowthB,outer=T)
modelG<-lme(root~fertilizer,random=~week|plant, data=Growth)
summary(modelG)
##
modelG2<-aov(root~fertilizer,subset=(week==10), data=Growth)
summary(modelG2)
summary.lm(modelG2)

###CONCLUSION
#The effect size in the lme is slightly smaller (-1.039 393
#compared to -1.2833) but the standard error is appreciably lower (0.203 415 8 compared to 0.3787), so the
#significance of the result is higher in the lme than in the aov. You get increased statistical power as a result
#of going to the trouble of fitting the mixed-effects model. And, crucially, you do not need to make potentially
#arbitrary judgements about which time period to select for the non-pseudoreplicated analysis.
