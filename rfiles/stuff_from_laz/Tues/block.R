
setwd("~/Downloads/tuesd")
getwd()
library(epicalc)
library(MASS)

rm(list=c()) # remove all existing objects
# 1.Load the file into a data frame named df2 with the read.table function. 
df2 = read.table("block.txt", header=TRUE); df2
boxplot(df2, col=c(2:6), ylab="body sizes (mean length in mm)", xlab="Genotypes")

# 2.Concatenate the data rows in df2 into a single vector r . 
r = c(t(as.matrix(df2))); r # response data 

#3.Assign new variables for the treatment levels and number of control blocks
f = c("1", "2", "3","4","5")# treatment levels 
k = 5 # number of treatment levels 
n = 3# number of control blocks 

# 4.Create a vector of treatment factors that corresponds to the each element in r of step 3 with the gl function.                     # number of control blocks 
tm = gl(k, 1, n*k, factor(f))   # matching treatment 

library(gplots)
plotmeans(r~tm, main="Mean plot with 95% CI")

# 5.Similarly, create a vector of blocking factors for each element in the response data r. 
blk = gl(n, k, k*n) ;blk            # blocking factor 

# 6.Apply the function aov to a formula that describes the response r by both the treatment factor tm and the block control blk
av = aov(r ~ tm + blk)

# 7.Print out the ANOVA table with the summary function
summary(av) 

#### Use Tukey's method to look at the differences between groups'
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
