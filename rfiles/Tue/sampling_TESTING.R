##package that helps read data written in other languages
library(foreign)
##package that helps with the describe function
library(Hmisc)
#library(inspect)
library(psych) #summarizing data
library(epicalc) #epdim* packages (ci, sd, )

##reads data
maltreat<-read.dta("data/maltreat.dta")

##describe the maltreat data set
#describe(maltreat)
str(maltreat) #added by ken

##inspect the pcv variable for missing,maximum,minimum etc
describe(maltreat$pcv)
summary(maltreat$pcv)
#inspect(maltreat$pcv) though not available for r 3.0.2

##identify values out of range(note:normal pcv=35% in children)
boxplot(maltreat$pcv, xlab="pcv" , ylab="frequency")
describe(maltreat$pcv) #fro, pysch package
##check the distribution of pcv
hist(maltreat$pcv, freq=F)
lines(density(maltreat$pcv))

##descriptive statistics for pcv
summary(maltreat$pcv)
describe(maltreat$pcv)

##se(mean) of 670 obs
#confint(maltreat$pcv,level=0.95)
ci(maltreat$pcv,ci=0.95)
mean(maltreat$pcv)

#std <- function(x) sd(x)/sqrt(length(x))
#se.pcv <- std(pcv)

##analysis of first 60 obs
maltreat.60 <- maltreat[1:60,]
ci(maltreat.60$pcv,ci=0.95)
mean(maltreat.60$pcv)

data2 <-as.data.frame(read.dta("data/maltreat.dta"))
dd <- function(x,y,z) {
  pars <- match.call()[-1]
   data2 <- read.dta(x)
  data3 <- subset(data2, y=="Madinka")
  print(mean(...=data3 , z, na.rm=T))
  za <-  pars$z
  ya <- pars$y
 attach(data2)
 # ddply(data2,~ ya,summarise,mean=mean(za))
 #print(pars$z)
  #print(class(data))
  #z2 <- aggregate( d, pars$data, mean)
}
dd("data/maltreat.dta", ethnicn , pcv)

ddply(data2,~ethnicn,summarise,mean=mean(pcv))


for (i in maltreat$ethnicn) {mm <- mean(maltreat$pcv)  return(mm)}
getwd()
data2 <- read.dta("data/maltreat.dta")
class(data2$pcv)
aggregate( pcv~ethnicn, data2, mean )