#******** QUESTION 2 SOLUTION ******#
#load the required libraries
library(foreign) #allow reading of dta files
#library(reshape2) 
library(plyr) #provide summary of data

#setting the working directory (cannot change working directory if you are working on a project folder)
#setwd("~/pwani_tab_stats")

#load the data set
zambia3 <- read.dta("data/zambia3.dta", convert.dates=TRUE)

#summarize the count of data by education and urban/rural
table <- table(zambia3$educ ,zambia3$urban )
#*DISTRIBUTION OF EDUCATION IN URBAN AND RURAL AREAS
barplot (table,  beside=TRUE  )

#adding title and y labels
barplot (table, main="DISTRIBUTION OF EDUCATION IN URBAN AND RURAL AREAS",
          beside=TRUE , ylab="Number of respodents" ,   legend = rownames(table))

#*YOU PROBABLY WANT TO ADD A TEXT LABEL TO THE CATEGORY AXIS.
#*EITHER LABEL THE URBAN VARIABLE OR:
zambia3$urban <- factor(zambia3$urban,
                         levels = c(1,2),
                         labels = c("Urban", "Rural")) 
table2 <- table( zambia3$educ ,zambia3$urban )
barplot (table2, main="DISTRIBUTION OF EDUCATION IN URBAN AND RURAL AREAS",
          , beside=TRUE , ylab="Number of respodents" )
#saving a plot
dev.copy(png,'myplot.png')
dev.off()

#*WITH MOST OF THE COLOURS CHANGED. Add color according to the number of grouping values available
barplot (table2, main="DISTRIBUTION OF EDUCATION IN URBAN AND RURAL AREAS",
         , beside=TRUE , ylab="Number of respodents" ,col=c("red","orange" ,"blue","green") , legend = rownames(table))

#******** QUESTION 3 SOLUTION *******
#load the data set
bab9 <- read.dta("data/bab9.dta", convert.dates=TRUE)
boxplot(data=bab9,   bweight ~ gestcat )

#*WITH A TITLE BELOW THE GRAPH
boxplot(data=bab9,   bweight ~ gestcat , main="Birth weight by gestational age")

#*AGAIN YOU CAN LABEL THE gestcat VARIABLE OR WRITE YOUR 
#*OWN LABELS ON THE X-AXIS
bab9$gestcat <- factor(bab9$gestcat,
                         levels = c(1,2),
                         labels = c("Premature", "Term"))
boxplot(data=bab9,   bweight ~ gestcat , main="Birth weight by gestational age")

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
