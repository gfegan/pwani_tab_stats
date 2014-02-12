#------------------------------------------------------#
#Question1

#Plot the age for rural people (urban==1) –Zambia Data
#load the dataset first
#Zambia 3
zambia3 <- read.dta("data/zambia3.dta", convert.dates=TRUE)
#subset the data with rural only
zambia3.Rural <- subset(zambia3, zambia3$urban==1)
#histogram of age
hist(zambia3.Rural$age)
#Add a title for the histogram that says “Age distribution of Rural People”
hist(zambia3.Rural$age, main="Age distribution of Rural People")
#Add a label to the x-axis that says “Age in Rural Areas”
hist(zambia3.Rural$age, main="Age distribution of Rural People", xlab="Age in Rural Areas")
#make the bars red
hist(zambia3.Rural$age, main="Age distribution of Rural People", xlab="Age in Rural Areas", col="red")

#------------------------------------------------------#
#Question2
#•  Plot the age for  rural and urban people  on one plotting platform – Zambia Data
#we dont load the data again since its loaded
#set the graph platform to accept 2 graphs
#2 rows, one column
par(mfrow = c(2, 1))

#since we had a subset we dont have to re - subset for rural data
zambia3.Urban <- subset(zambia3, zambia3$urban==2)
hist(zambia3.Urban$age)
hist(zambia3.Rural$age)
#make the bars red and for urban blue
hist(zambia3.Rural$age, main="Age distribution of Rural People", xlab="Age in Rural Areas", col="red")

hist(zambia3.Urban$age, main="Age distribution of Rural People", xlab="Age in Urban Areas", col="blue")

#add an abline
x <- median(zambia3.Rural$age , na.rm=TRUE)
y <- median(zambia3.Urban$age, na.rm=TRUE)

hist(zambia3.Rural$age, main="Age distribution of Rural People", xlab="Age in Rural Areas", col="red")
abline(v=x,col = "black", lwd = 2)
hist(zambia3.Urban$age, main="Age distribution of Rural People", xlab="Age in Urban Areas", col="blue")
abline(v=y,col = "black", lwd = 2)
#set to the original format
par(mfrow = c(1, 1))

#A boxplot of gest weeks by maternal age group  - bab9
#load the dataset
bab9 <- read.dta("data/bab9.dta", convert.dates=TRUE)
boxplot(bab9$gestwks ~ bab9$matagegp  )
#adding y axis label and the title
boxplot(bab9$gestwks ~ bab9$matagegp , main="Gest weeks by maternal age group" , ylab="Gest weeks")


#Do a scatter plot bweight vs age - bab9
plot(bab9$bweight, bab9$matage)
#color by low birth weight
plot(bab9$bweight, bab9$matage, col=bab9$lbw )

#change the dots
example(points) #see the different points in R
plot(bab9$bweight, bab9$matage, col=bab9$lbw ,pch=8)

##using ggplot2
library(ggplot2)
qplot(bweight, matage,data=bab9, color=lbw)
#add the regression line for each category of birthweight
qplot(bweight, matage,data=bab9, color=lbw, facets=~lbw) + geom_smooth(method="lm")



