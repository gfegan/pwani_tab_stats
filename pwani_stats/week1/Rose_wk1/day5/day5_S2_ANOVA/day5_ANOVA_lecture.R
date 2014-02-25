onewayA <- read.delim("H:/R course material/Rose_wk1/day5/day5_data_ANOVA/onewayA.txt")
View(onewayA)
attach(onewayA)
boxplot(ozone~garden)

##the comands below show how to draw a graph illustrating the SST
plot(ozone,main="SST"); abline(mean(ozone),0)
for(i in 1:14) lines(c(i,i),c(mean(ozone),ozone[i]))

##the comands below show how to draw a graph illustrating the SSE
means<-tapply(ozone,garden,mean); means
plot(ozone, main="SSE")
lines(c(1,7.5),c(means[1],means[1]))
lines(c(7.5,14),c(means[2],means[2]))
for(i in 1:7) lines(c(i,i),c(means[1],ozone[i]))
for(i in 8:14) lines(c(i,i),c(means[2],ozone[i]))
abline(mean(ozone),0, lty=2)

##Example ANOVA for the hand calculations

onewayB <- read.delim("H:/R course material/Rose_wk1/day5/day5_data_ANOVA/onewayB.txt")
View(onewayB)
names(onewayB)

tapply(onewayB$Growth,onewayB$Photoperiod,mean)
is.factor(onewayB$Photoperiod)###to check that a variable is a factor 
fligner.test(onewayB$Growth~onewayB$Photoperiod)##homogeneity of variances
one.way<-aov(onewayB$Growth~onewayB$Photoperiod)##how to do ANOVA on R
summary(one.way)## gives you the anova table
par(mfrow=c(2,2))
plot(one.way)
######
#note plot 1 slight evidence of heteroscedasticity
#(i.e. there is some tendency for the variance to increase as the fitted values increase)
#note 2:strong signs of non-normality in the residuals (J-shape not linear)
#########
###remember always follow the rule:  Response.Variable~Exaplanatory.Variable

##TWO WAY ANOVA
tapply(twoway$Growth,twoway$Genotype,mean)
two.way<-aov(twoway$Growth~twoway$Genotype+twoway$Photoperiod)
summary(two.way)
plot(two.way)
