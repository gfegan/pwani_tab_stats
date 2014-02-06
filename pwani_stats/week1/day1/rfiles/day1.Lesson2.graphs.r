#******** QUESTION  SOLUTION ******#
#load the required libraries
library(foreign) #allow reading of dta files
library(plyr) #provide summary of data

#--------------------------------------------------------------------------------
#setwd("put the path to the cd")
#-------------------------------------------------------------------------

#load the data set of Zambia 3
zambia3 <- read.dta("data/zambia3.dta", convert.dates=TRUE)

#histogram of age
hist(zambia3$age)
#adding title to the  histogram , y label and x label
hist(zambia3$age , main="The Age distribution" , ylab="Age count" ,  xlab="Age")
#adding color to the bars
hist(zambia3$age , main="The Age distribution" , ylab="Age count" ,  xlab="Age", col="Red")
#color by a factor variable and ??add a legend
hist(zambia3$age , main="The Age distribution" , ylab="Age count" ,  xlab="Age", col=zambia3$agegrp)

#Do a graph to summarize by education in urban and rural areas
#to see diff ploting symbols -- example(points)
#summarize the count of data by education and urban/rural
table <- table(zambia3$educ ,zambia3$urban )
table

#*DISTRIBUTION OF EDUCATION IN URBAN AND RURAL AREAS
barplot (table) # stacked
barplot (table,  beside=TRUE  )


#adding title and y labels
barplot (table, main="DISTRIBUTION OF EDUCATION IN URBAN AND RURAL AREAS",
          beside=TRUE , ylab="Number of respodents" ,legend.text = row.names(table), args.legend = list(x = "topright"))

#*YOU PROBABLY WANT TO ADD A TEXT LABEL TO THE CATEGORY AXIS.
#*EITHER LABEL THE URBAN VARIABLE OR:
zambia3$urban <- factor(zambia3$urban,
                         levels = c(1,2),
                         labels = c("Urban", "Rural")) 
table2 <- table( zambia3$educ ,zambia3$urban )
table2
barplot (table2, main="DISTRIBUTION OF EDUCATION IN URBAN AND RURAL AREAS",
          , beside=TRUE , ylab="Number of respodents" )

#saving a plot
#the plot will be saved in your working directory which is
getwd()
dev.copy(png,'myplot.png')
dev.off()


#*WITH MOST OF THE COLOURS CHANGED. Add color according to the number of grouping values available
barplot (table2, main="DISTRIBUTION OF EDUCATION IN URBAN AND RURAL AREAS",
         , beside=TRUE , ylab="Number of respodents" ,col=c("red","orange" ,"blue","green") ,
         legend.text = row.names(table), args.legend = list(x = "topright") )


#******** QUESTION 3 SOLUTION *******
#load the data set
bab9 <- read.dta("data/bab9.dta", convert.dates=TRUE)

#scatter plot
plot(bab9$gestwks)
plot(bab9$matage)
#scatter plot of maternal age vs gestational age
plot(bab9$gestwks ,bab9$matage )
#add yaxis label and xaxis label
plot(bab9$gestwks ,bab9$matage, ylab="Maternal Age", xlab="Gestational age" )

#color
#scatter of gestational age and birthweight
names(table(bab9$sex))
plot(bab9$bweight, bab9$gestwks , ylab="Maternal Age", xlab="Gestational age" , col=bab9$sex)
abline(lm(bab9$bweight ~ bab9$gestwks), col="blue") # regression line (y~x) 
legend('bottomright',  legend=c("Male","female"),  lwd=1, col=c('red', 'black'), bty='n', cex=.75)

#using ggplot makes it easier
library(ggplot2)
qplot(data=bab9 ,bweight, gestwks ,color=sex)
#adding a regression line
qplot(data=bab9 ,bweight, gestwks ,color=sex , geom=c("point","smooth"),method="lm")


#boxplot
boxplot(data=bab9,   bweight ~ gestcat )
#*WITH A TITLE for GRAPH
boxplot(data=bab9,   bweight ~ gestcat , main="Birth weight by gestational age")

#*AGAIN YOU CAN LABEL THE gestcat VARIABLE OR WRITE YOUR 
#*OWN LABELS ON THE X-AXIS
bab9$gestcat2 <- factor(bab9$gestcat,
                         levels = c(1,2),
                         labels = c("Premature", "Term"))#
table(bab9$gestcat2)
boxplot(data=bab9,   bweight ~ gestcat2 , main="Birth weight by gestational age")

#******** QUESTION 4 SOLUTION *******
#load the data set
zambia4 <- read.dta("data/zambia4_isingo.dta", convert.dates=TRUE)
#*PIE CHART FOR EDUCATIONAL LEVEL OF WOMEN WHO USED A CONDOM 

table3 <- table(zambia4$clastsex) 
pie(table3, labels = rownames(table3), col=c("red","orange" ))

#*AT LAST SEX AND THOSE WHO DID NOT
#summarize the count of data by education and urban/rural
#subset those who used a condom and not used
zambia4.yes <- subset(zambia4,clastsex=="Yes" )
zambia4.no <- subset(zambia4,clastsex=="No" )
#specify to output on the same platform or page - one row with 2 graphs
par(mfrow=c(1,2))
table3.yes <- table(zambia4.yes$educ) 
pie(table3.yes, labels = rownames(table3.yes), col=c("red","orange" ,"blue","green"))
table3.no <- table(zambia4.no$educ) 
pie(table3.no, labels = rownames(table3.no), col=c("red","orange" ,"blue","green") )
#return to the 1 by 1 output
par(mfrow=c(1,1))
