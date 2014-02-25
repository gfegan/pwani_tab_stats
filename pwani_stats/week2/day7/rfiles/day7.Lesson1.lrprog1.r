# Load the foreign packahge to be able to read a Stata .dta file
library(foreign)

# Set the working directoryy accordinglt
setwd("E:/pwaniTraining/pwani_stats/week2/data")

# assign the stat dataframe to an R dataframe object called onchall
onchall <- read.dta("onchall.dta")

# let's inspect this data frame
View(onchall)

#label up the values of the area variable 
onchall$area <-  factor(onchall$area,labels=c("Savannah","Forest")) 

# produce a contingency table of the outcome by area
with(onchall, table(mf,area))

# create a logistics regression model object caled mod1
mod1 <- glm(mf ~ area, data=onchall,family=binomial)

# show the model
summary(mod1)


confint(mod1)               # show the 95% CI for the coefficients
exp(coef(mod1))             # transform the coeffs into ORs 
exp(confint(mod1))          # and show their CIs  



# Now lets look at a a predictor with more than one level
with(onchall, table(mf,agegrp))

# Is this correct ?
mod2 <- glm(mf ~ agegrp, data=onchall, family=binomial)
summary(mod2)


# No, - why not?
onchall$agegrp = factor(onchall$agegrp,labels=c("5-9","10-19","20-39", "40+")) 
with(onchall, table(mf,agegrp)) #show table with labels
mod2 <- glm(mf ~ agegrp, data=onchall, family=binomial)

# Let- see this output
summary(mod2)

confint(mod2)
exp(coef(mod2))             # transform the coeffs into ORs 
exp(confint(mod2))          # and show their CIs  

################################################################################## 
# One can get the above info in one go by using functions from the epicalc package
# eg 
# logistic.display(mod1)
# ---> the following output
# OR lower95ci upper95ci     Pr(>|Z|)
# as.factor(agegrp)1  2.821337  1.847566  4.308341 1.570730e-06
# as.factor(agegrp)2  8.112000  5.495007 11.975335 6.048703e-26
# as.factor(agegrp)3 16.023913 10.657505 24.092486 1.504344e-40
################################################################################## 


# Likelihood ratio calculations of model fit
emptymod <- glm(mf ~ 1, data=onchall, family=binomial) # fit the empty model
summary(emptymod)
anova(emptymod,mod2,test="LRT") # compare the empty model aka "Null model" to the test model


# Lets look at a model where there is an intearction between two exposures
data("womensrole", package = "HSAUR2")
View(womensrole)
use(womensrole)
fm1 <- cbind(agree, disagree) ~ gender + education
womensrole_glm_1 <- glm(fm1, data = womensrole, 
                        family = binomial())
summary(womensrole_glm_1)
womensrole_glm_2 <- glm(cbind(agree, disagree) ~ gender * education, data = womensrole, 
                        family = binomial())
summary(womensrole_glm_2)

# certainly from the Wald test of the interaction term there would appear to be a 
# statistically signficant interaction hwowever the more 'correct' way to do this is to 
# do a Likelihood Ratio Test

anova(womensrole_glm_1,womensrole_glm_2,test="Chisq") # NB one could change the Chisq to LRT