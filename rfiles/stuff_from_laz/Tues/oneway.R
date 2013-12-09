
setwd("~/Downloads/tuesd")
getwd()
dir()
library(epicalc)
library(MASS)

# 1.Load the file into a data frame named df1 with the read.table function. As the first line in the file contains the column names, we set the header argument as TRUE. 
df1 = read.table("oneway.txt", header=TRUE); df1
boxplot(df1, col=c(2:4), ylab="% potassium in blood serum", xlab="Analyst")

# 2.Concatenate the data rows of df1 into a single vector r . 
r = c(t(as.matrix(df1)));r # response data 

# 3.Assign new variables for the treatment levels and number of observations 
 f = c("A", "B", "C") # treatment levels 
 k = 3# number of treatment levels 
 n = 6# observations per treatment 

# 4.Create a vector of treatment factors that corresponds to each element of r in step 3 with the gl function
 tm = gl(k, 1, n*k, factor(f)) ; tm # matching treatments 
 
library(gplots)
plotmeans(r~tm, main="Mean plot with 95% CI")

# 5.Apply the function aov to a formula that describes the response r by the treatment factor tm. 
av = aov(r ~ tm)

# 6.Print out the ANOVA table with the summary function. 
summary(av)

## Use Tukey'smethod'
(m1<-TukeyHSD(av, conf.level=0.99))# at 1% significance level
(m2<-TukeyHSD(av)) # at 5% significance level

X11(height=6, width=6)
op<-par(mfrow=c(2,2))
plot(av)
par(op)
graphics.off()
shapiro.qqnorm(r)

plot(m1)
plot(m2)


