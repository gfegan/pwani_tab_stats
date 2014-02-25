### Stats1.13.Lab.01.R ###

# Basic mathematical operations
3 + 4
5 * 5
12 / 3
5^5

# R objects

# Vector 
## Most basic object in R 
## Contains elements of the same class
## Can be: character, numeric, integer, complex, logical(True/False))

# Create a vector
v=c(1,3,5,7)
v

# List 
## (Vector with different class of objects) 
l=c("Blue", 2, 5, "Red")
l

# Create a matrix
#a matrix with values 1 to 6 of 2 rows 3 columns
m=matrix(1:6,2,3)
m
## Matrix creation is column-wise

# Create a matrix from a vector
m2=matrix(1:6)
# Then add dimensionality
dim(m2)=c(2,3)
m2

# Create a matrix by binding columns or rows
x=1:6
y=5:10
cbind(x,y) # by column
rbind(x,y) # by row

# Check the attributes
attributes(m)

# Call a particular cell in a matrix
m
m[1,2]

# Dataframes
## Different than matrices => can store different classes of objects
## Usually called with read.table()

# Create a dataframe
d=data.frame(subjectID=1:5,gender=c("M","F","F","M","F"),score=c(8,3,6,5,5))
d

# Number of rows
nrow(d)

# Number of columns
ncol(d)

# Check the attributes
attributes(d)

#saving as an .rda file
save(d, "d.rda")
# Call a particular cell in a dataframe
d[2,1]
d[1,2]

# Display dataframe
View(d)
# Edit dataframe
edit(d)

# Getting help on a function
?functionname

# Download and install packages
install.packages("psych", ) ## Need to specify CRAN the 1st time

#using R as a calcualtor
#The following are the heights in cm of a random sample of 20 Jamaican boys with homozygous sickle cell disease at age two years
height <- c(84.4,87.0,80.6,83.4,85.0,85.4,89.2,78.5,80.0,89.8,82.5,85.0,89.0,84.1,81.3,85.4,80.7,85.5,81.9,86.3)
#calculating the mean
#mean= sum(height)/n
n <- 20
sum <- sum(height)
mean <- sum/n
mean
mean(height)

#calculating the standard deviation
#The Standard Deviation is a measure of how spread out numbers are
#standard deviation is the square root of variance
deviation <- height-mean
deviation.squared  <- deviation * deviation
sum.deviation <- sum(deviation.squared)
variance.height <- sum.deviation/n
sd.height <- sqrt(variance.height)
sd(height)

#calculate the range
height.ordered  <- ordered(height)
range(height)

