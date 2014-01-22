##defines your working space to save
#setwd("H:/New folder")
library(foreign) ## reads data written in another programming data language
##library(Rmisc) # for calculating CI of the mean
library(epicalc)#check gfor 


#read in the data from a stata data file 
birthweight2<-read.dta("data/birthweight2.dta")

##Practical5

##explore data variables,labels and types
str(birthweight2)
#View(birthweight2)
table(birthweight2$lbw)
str(birthweight2$lbw)

##recode data to have normal weight=0
birthweight2$lbw2[birthweight2$lbw=="Weight<2500"]<- 1
birthweight2$lbw2[birthweight2$lbw=="Normal 2500+"]<-0

#another way of doing it
#birthweight2$lbw2<-ordered(birthweight2$lbw,levels=c("Weight<2500","Normal 2500+"),labels=c(1,0))
#adding label to the first
=
table(birthweight2$lbw2)

##calculate confidence interval
ci(birthweight2$lbw2)

#claculating confidence interval by gender
#create the male subset
birthweight.male <- subset(birthweight2, birthweight2$sex=="Male")
ci(birthweight.male$lbw2)
#create the female subset
birthweight.female <- subset(birthweight2, birthweight2$sex=="Female")
ci(birthweight.female$lbw2)
            
      #using the dataTable library -require(data.table) #using the by loop
      ##require(data.table)        data2[,sapply(propTest=prop
      ##data <- data.table(birthweight2)
      ##data[,data.frame(conf.I=ci(lbw2)),by=sex]


   
#Test the hypothesis ie null hypthesis =0.9
prop.test(length(which(birthweight2$lbw2== "normal birth weight")),n=length(birthweight2$lbw2 ),p=0.9 )

#Test the hypothesis ie null hypthesis =0.9 by gender
prop.test(length(which(birthweight.male$lbw2== "normal birth weight")),n=length(birthweight.male$lbw2),p=0.9 )
prop.test(length(which(birthweight.female$lbw2== "normal birth weight")),n=length(birthweight.female$lbw2),p=0.9 )
        #using the dataTable library -require(data.table) #using the by loop
        ##require(data.table)
      ## data2 <- data.table(birthweight2)
        ##data2[,c(propTest=prop.test(length(which(lbw2== "normal birth weight")),n=length(lbw2),p=0.9 ),p=0.9 ),by=sex]
#or
binom.test(length(which(birthweight.male$lbw2== "normal birth weight")),n=length(birthweight.male$lbw2),p=0.9 )
binom.test(length(which(birthweight.female$lbw2== "normal birth weight")),n=length(birthweight.female$lbw2),p=0.9 )


#session 2 (covered by the above code)
#Hypothesis test and CI for difference in proportions
  


#Practical 6
#how to compute a chi-square test
mytable <- table(birthweight2$sex,birthweight2$lbw2)
summary(mytable)
    #or
    #chisq.test(birthweight2$sex,birthweight2$lbw2,correct = FALSE)

mytable2 <- table(birthweight2$ht,birthweight2$lbw2)
summary(mytable2)
mytable3 <- table(birthweight2$ethnic,birthweight2$lbw2)
summary(mytable3)
#using the second method to calculate prportion test
data2 <- data.table(birthweight2)
data2[,c(propTest=prop.test(length(which(lbw2== "normal birth weight")),n=length(lbw2),p=0.9 ),p=0.9 ),by=ht]

mytable4 <- table(birthweight2$ht,birthweight2$lbw2)
summary(mytable4)


#Practical 7
#*measures of association - Odds ratios
#*create a categorical variable from maternal age 
#correct  logical indicating whether to apply continuity correction when computing the test statistic
min.matagegp <- min(birthweight2$matage)
max.matagegp <- max(birthweight2$matage)
birthweight2$matagegp[birthweight2$matage <= 29]<- 1
birthweight2$matagegp[birthweight2$matage>29 & birthweight2$matage <=34]<- 2
birthweight2$matagegp[birthweight2$matage>34 & birthweight2$matage <=39]<- 3
birthweight2$matagegp[birthweight2$matage>39]<- 4
birthweight2$matagegp <- factor(birthweight2$matagegp,levels = c(1,2,3,4),labels = c("23 - 29", "30 - 34", "35 - 39", "40+"))
table(birthweight2$matagegp)

#**Create a categorical varible "gestwkgp" from gestation in weeks "gestwks"
birthweight2$gestwkgp <- birthweight2$gestwks
birthweight2$gestwkgp[birthweight2$gestwks <= 32]<- 1
birthweight2$gestwkgp[birthweight2$gestwks>32 & birthweight2$gestwks <=36]<- 2
birthweight2$gestwkgp[birthweight2$gestwks>36 & birthweight2$gestwks <=40]<- 3
birthweight2$gestwkgp[birthweight2$gestwks>40]<- 4


birthweight2$ht2 <- birthweight2$ht
birthweight2$ht2[birthweight2$ht==2] <- 0
#compute measures of association
cs(birthweight2$lbw2 , birthweight2$ht2 )

#check for trend in the "gestwkgp" categories
birthweight2$gestwkgp <- as.numeric(birthweight2$gestwkgp)
birthweight2$lbw2 <- as.numeric(birthweight2$lbw2)
birthweight2$gestwkgp <- factor(birthweight2$gestwkgp,levels = c(1,2,3,4),labels = c("1", "2", "3", "4"))
birthweight2$gestwkgp2 <- as.numeric(birthweight2$gestwkgp)
prop.trend.test (birthweight2$lbw2 ,birthweight2$gestwkgp2 )


#tabodds cc From both functions, odds ratio and its confidence limits, chisquared test and Fisher's exact test are computed.
#cs - cs'  are for cohort and cross-sectional studies.
cc(birthweight2$lbw2 ,birthweight2$gestwkgp   ,decimal=5 )
cc(birthweight2$lbw2,  birthweight2$sex ,decimal=5 ) #Chi-squared = 1.84787, 1 d.f., P value = 0.174031 ml for max likelihood

