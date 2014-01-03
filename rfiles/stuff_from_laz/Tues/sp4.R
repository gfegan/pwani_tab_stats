## Remember to change to your working directory by typing "setwd()"
# setwd("C:/kilifi_course/data")

## To know your current working directory, type "getwd()"

## Your working directory should have the datasets and the R-Scripts

## R Practical 1
getwd()
setwd("/home/thoya/Documents/kemri/MYDATA/kilifi_course/data")
dir()

library(nlme)
library(epicalc)

use("maltreat.dta")
mal<-.data
attach(mal)
search()
des(mal)
tab1(mal$ethnic)
ethnic[ethnic=="Jla"] <- "Jola"
ethnic[ethnic=="Madinka"] <- "Mandinka"
ethnic[ethnic=="Mandnka"] <- "Mandinka"
ethnic[ethnic=="Serahouli"] <- "Serahule"
ethnic[ethnic=="Wolllof"] <- "Wollof"
ethnic[ethnic=="Wolof"] <- "Wollof"
tab1(ethnic)
ethnic <- factor(ethnic)
tab1(ethnic)
label.var(ethnic, "Ethnicity")
any(is.na(ethnic)) # is there NAs?
## Qn.1 # applying oneway analysis of variance in R
# Analyse the difference in mean pcv by ethnic group (for Stata: use oneway)

summary(aov(pcv~ethnic))

keepData(subset=ethnic!="")

anova(lm(pcv ~ ethnic))
plot(ethnic,pcv,col=c(2:7), ylab="Packed Cell Volume")
library(gplots)
plotmeans(pcv ~ ethnic)

## Qn.2 # lets now use the function aov in R
class(sex); levels(sex) # sex is stored as a character(string in Stata)
tab1(sex, missing=FALSE) # It also has 4 missing values

sex2<-factor(sex, levels=c("Female", "Male"),labels=c("0","1")) 
# sex should be a factor, not string
label.var(sex2, "gender description")  # add labels and define the var sex2
class(sex2);levels(sex2) # ckeck it now, any progress?

## Apply ttest
t.test(pcv~sex2)

(m2<-aov(pcv~sex2))
summary(m2)
plot(TukeyHSD(m2))

## Qn.3
# (c)Code the age into 6 age groups using the command:
agegrp <- cut(ageyrs, br=c(0,1,2,3,4,5,99), lower=TRUE)
label.var(agegrp, "age categoties")

(m3<-aov(pcv ~ agegrp));summary(m3)
summary(m3)
plot(TukeyHSD(m3))

(m4<-aov(pcv ~ agegrp + sex2))
summary(m4)
plot(TukeyHSD(m4))

anova(lm(pcv ~ agegrp*sex2)) # compare both agegrp and sex in the same model


##Qn.4

plot(resprate, pcv, ylab=("Packed cell Volume Haemocrit"), main=("PCV_Scatter"))
lm4<-lm(pcv~resprate);summary(lm4) # Similar to regress in Stata
abline(lm4, col="red")

----------------  END ---------------------


