## Analysis of the bwmal data
## Data found in the data folder on the CD provided
## Original Stata .do file By Jim Todd
## First Modified by Greg Fegan and Ritah Meka November 28th 2013
## Last modified by Ken Mwai
##
  
## As a practical for new R users
## This section at the beginning enables you to put comments about the do-file
###################################################3
# You can also put comments between these markers 
# clear #
rm(list = ls())

#--------------------------------------------------------------------------------
#setwd("put the path to the cd")
#-------------------------------------------------------------------------
###########################################################
# Best to ensure that you change directory, to the folder for this analysis
# Do this first, and then the log file can be saved in the same directory
################################################################
#setwd("H:/Pwani_Collabo/tab_stats")
# We need to istall necessary packages, install packages
# install.packages("packages/foreign", repos = NULL)
# install.packages("packages/psych", repos = NULL)

#  there are a number of  packages within R that can read in Stata .dta binary data files we prefer "foreign"
library(foreign) #allowing reading data from other statistal programs
#library(psych)
library(arm) #helps in drawing hist of categorical data

# lets create a dataframe object called bwmal which will read in all the dat from the stat file bwaml.dta
bwmal <- read.dta("data/bwmal.dta")

# Start by describing the variables, 11 variables, and 791 observations
str(bwmal)

# View enables you to look at the data and also edit() allows you to make changes to the data
View(bwmal)
#allowing editing of the loaded data
edit(bwmal)

## The command summarize to show means and std dev
## This can be for all variables or just for some variables
summary(bwmal)
#summarizes a specific variable
summary(bwmal$matage) 


# Listing all the data would take a lot of space
# Better to just list the first 10 observations in 1/10
head(bwmal$matage,n=10)
head(cbind(bwmal$matage, bwmal$mheight),n=10)
#viewing the last data for  bwmal$matage
tail(bwmal , n=10)
tail(bwmal$matage,n=10)

# Or listing a subset, such as those who smoke
# Note when we use logical tests ie "if we test some variable equals some value" 
# we must use the == (double equals sign) 
# Note that in the line below the R command print is assumed
#listing data where smoke==1
bwmal[bwmal$smoke=="1", ]
#assign the data
bwmal.Smoke1 <- bwmal[bwmal$smoke=="1", ]
View(bwmal.Smoke1)

# This command gives the frequency and percentage for different levels of a variable
table(bwmal$smoke)

# To obtain a histogram
#histogram matage
hist(bwmal$matage)
arm::discrete.histogram (bwmal$sex, xlab="Sex") 

## Generate a new variable, and recode it to show two categories
gestgrp <- bwmal$gestwks
gestgrp[bwmal$gestwks<=36] <- 1 
gestgrp[bwmal$gestwks>=37] <- 2
table(gestgrp)

## We can generate labels for the values in each variable
## Two steps. First define the label - smokelbl
## And then apply that label to the values in one variable
bwmal$smoke<-factor(bwmal$smoke,levels=c(0,1),labels=c("Non-smoker","Smoker"))
table(bwmal$smoke)

## Notice the difference with the label applied to the values
# Define labels for sex
bwmal$sex<-factor(bwmal$sex,levels=c(0,1),labels=c("Female","Male"))
table(bwmal$sex)


# We can also label the variable itself to make it clear what it means
##changes the label
#names(bwmal)[4]<-"The sex of the Baby"
##gives a label
table(bwmal[4])

# Create a special group for analysis
bwmal$specialgrp <- 0
bwmal$specialgrp[bwmal$sex=="Male" & bwmal$bweight>4.0 & bwmal$gestwks>40] <- 1
table(bwmal$specialgrp)

#bwmal$specialgrpB<-as.numeric(bwmal$sex=="Male" & bwmal$bweight>4.0 & bwmal$gestwks>40)
#specialgrpB<-subset(bwmal,bwmal$sex=="Male" & bwmal$bweight>4.0 & bwmal$gestwks>40)


## Then we can save the data in a new data file
## The replace option overwrites any file of the same name - BEWARE do not over write your original data
save bwmal_new , replace

## Another useful command is ?
? summarize
? save
? generate
?? generate



 

