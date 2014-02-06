##Regression
##Source Crawley Chapter  4
##Demonstrating the range of data to measure variability

#--------------------------------------------------------------------------------
#setwd("put the path to the cd")
#-------------------------------------------------------------------------

y<-c(13,7,5,12,9,15,6,11,9,7,12)
plot(y,ylim=c(0,20))

###alternate plot
plot(y,ylim=c(0,20), pch=16, cex=2, cex.axis=2, cex.lab=2)

##line of mean

abline(h=mean(y)) 

##lines for demonstrating the error sum of squares by vertical departures from mean y

for (i in 1: length(y)){
  lines(c(i,i),c(mean(y),y[i]))}

tannin2 <- read.delim("data/tannin2.txt")
View(tannin2)
attach(tannin2)
# detach(tannin2) #use this when finished with the data
# rm(tannin2)#use this when finished with the data
plot(growth~tannin,pch=16) 

plot(growth~tannin,pch=16,main="SST")
abline(mean(growth),0) #grandmean
for(i in 1:9) lines(c(tannin[i],tannin[i]),c(mean(growth),growth[i]),lty=2)

model<-lm(growth~tannin)
model

abline(model)
## abline(lm(growth~tannin)) ##alternative

yhat<-predict(model)
yhat


plot(growth~tannin,pch=16,main="SSE")
abline(model) # slope
# save fitted values in a vector 'yhat' using predict:
yhat<-predict(model); yhat
for(i in 1:9) lines(c(tannin[i],tannin[i]),c(growth[i],yhat[i]),lty=2)


plot(growth~tannin,pch=16,main="SSR")
abline(mean(growth),0,lty =2)
model<-lm(growth~tannin)
abline(model)
for(i in 1:9) lines(c(tannin[i],tannin[i]),c(mean(growth),predict(model)[i]))

##DESK WORK

##LINEAR REGRESSION

model1<-lm(growth~tannin)
summary(model1)
par(mfrow=c(2,2))
par(mfrow=c(1,1))
summary.aov(model1)

##example of non-linear data
detach(tannin2)
decay <- read.delim("data/decay.TXT")
View(decay)
attach (decay)
names (decay)
plot(x,y)
## alternate 
plot(y~x)
abline(lm(y~x))

model2<-lm(y~x)
summary(model2)

plot(model2)

model3<-lm(log(y)~x)
summary(model3)
detach(decay)



mathlsd <- read.delim("data/mathlsd.txt")
View(mathlsd)
plot(math~leveld,mathlsd)


sum(mathsc);sum(mathsc^2)

sum(leveld);sum(leveld^2)

sum(mathsc*leveld)

mySST<-19639.24-(350.61^2)/7
mySST

mySSX<-(153.8905-((30.33^2)/7))
mySSX

mySSXY<-(1316.656-(350.61*30.33)/7)
mySSXY

myb<-mySSXY/mySSX
myb

mySSR<-myb*mySSXY
mySSR

mySSE<-mySST-mySSR
mySSE

model13<-lm(mathsc~leveld,mathlsd)
summary(model13)
summary.aov(model13)
plot(model13)


