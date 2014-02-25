
####--------Generating the blood pressure data : SBP-systolic blood pressure:
###salt_sugar- salt and or sugar intake

library(mvtnorm)
set.seed(100)

#SBP-- Normal (124,10) &&& Age-- Normal (23,5)

###Simulating SBP and Age simultaneously:
data<- rmvnorm(n=10000,mean=c(23,124),sigma=matrix(c(5,0.7*sqrt(50),0.7*sqrt(50),10),2,2)) 
colnames(data)<-c("Age","SBP")
data<-data.frame(data)


###simulating Salt_sugar-- intake : 1-8 spoons
data$salt_sugar<-NULL
data$salt_sugar[data$SBP>mean(data$SBP)]<-sample(5:8)
data$salt_sugar[is.na(data$salt_sugar)]<-sample(1:4)
###Gender::
data$gender<-sample(1:2)
data$gender<-factor(data$gender,levels=1:2,labels=c("Male","Female"))

###Plotting histogram and density plots
par(mfrow=c(1,2))
library(ggplot2)
qplot(SBP, data=data, geom="density", alpha=I(.5), main="Distribution of SBP", xlab="SBP", 
      ylab="Density")

qplot(SBP, data=data, geom="histogram", alpha=I(.5), main="Distribution of SBP", xlab="SBP", 
      ylab="Frequency")

####Random forest

library(randomForest)

rf <- randomForest(SBP ~ ., data=data, ntree=1000,keep.forest=FALSE, importance=TRUE)
varImpPlot(rf,main="Variable importance plot")

###Average of SBP by Age
mean.SBP.age<-with(data,tapply(SBP,Age,mean))
mean.data<-cbind(names(mean.SBP.age),mean.SBP.age)
colnames(mean.data)<-c("Age","SBP")
mean.data<-data.frame(mean.data)

##Converting Age and SBP into numeric
mean.data$Age<-as.numeric(as.character(mean.data$Age))
mean.data$SBP<-as.numeric(as.character(mean.data$SBP))

###scatter plot: with trend plotted:
with(mean.data,plot(Age,SBP))
fitted.SBP<-with(mean.data,fitted(lm(SBP~Age)))
with(mean.data,lines(Age,fitted.SBP,type="l",xlab="Age",ylab="SBP"))



####Modelling:

model1<-with(data,lm(SBP~Age))
AIC(model1)
model2<-with(data,lm(SBP~gender))
AIC(model2)
model3<-with(data,lm(SBP~salt_sugar))
AIC(model3)
model4<-with(data,lm(SBP~Age+gender))

AIC(model4)
model5<-with(data,lm(SBP~Age+salt_sugar))

AIC(model5)
model6<-with(data,lm(SBP~Age+salt_sugar+gender))

AIC(model6)
model7<-with(data,lm(SBP~Age+gender+gender*Age))

AIC(model7)
model8<-with(data,lm(SBP~Age+salt_sugar+Age* salt_sugar))
AIC(model8)
model9<-with(data,lm(SBP~salt_sugar+gender+salt_sugar*gender))

AIC(model9)
model10<-with(data,lm(SBP~Age+salt_sugar+gender+gender*Age))

AIC(model10)
model11<-with(data,lm(SBP~Age+salt_sugar+gender+Age* salt_sugar))

AIC(model11)
model12<-with(data,lm(SBP~Age+salt_sugar+gender+salt_sugar*gender))

AIC(model12)
model13<-with(data,lm(SBP~Age+salt_sugar+gender+gender*Age+Age* salt_sugar))

AIC(model13)
model14<-with(data,lm(SBP~Age+salt_sugar+gender+gender*Age+salt_sugar*gender))

AIC(model14)
model15<-with(data,lm(SBP~Age+salt_sugar+gender+gender*Age+salt_sugar*gender))

AIC(model15)


###Obtaining model coefficients

summary(model5)

###Examining model diagnostics:
par(mfrow=c(2,2))
plot(model5)

####Other diagnostics:
model.dfbetas<-dfbetas(model5)
plot(model.dfbetas[,c(2,3)])

plot(hatvalues(model5))
abline(h=c(2,3)*3/length(data),lty=2)


library(car)
# Assessing Outliers
outlierTest(model5) # Bonferonni p-value for most extreme obs
qqPlot(model5, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(model5) # leverage plots


# Influential Observations
# added variable plots 
av.Plots(model5)
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(data)-length(model5$coefficients)-2)) 
plot(model5, which=4, cook.levels=cutoff)
# Influence Plot 
influencePlot(model5,   id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )


###
# Evaluate Collinearity
vif(model5) # variance inflation factors 
sqrt(vif(model5)) > 2 # problem?

# Test for Autocorrelated Errors
durbinWatsonTest(model5)


#####---------Performing Likelihood Ratio Test
library(lmtest)
lrtest(model5,model8)

###Interaction in logistic regression:

data.logistic = data.frame(gender=rep(c("Male","Female"),c(6,6)), dept=rep(LETTERS[1:6],2),yes=c(512,353,120,138,53,22,89,17,202,131,94,24),no=c(313,207,205,279,138,351,19,8,391,244,299,317))

mod.form = "cbind(yes,no) ~ gender * dept"
glm.out = glm(mod.form, family=binomial(logit), data=data.logistic )

summary(glm.out)

###Visualising the odds::
exp(coef(glm.out))



