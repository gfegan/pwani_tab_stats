#  Need to change this to R format and get comments around the original stata parts
use G:\Continuous\maltreat.dta
*dscribe the maltreat dataset
describe
*inspect the pvc variable for missing, maximum, minmum etc
codebook pcv
inspect pcv
*identifying values out of range (note normal pcv = 35% in children)
graph box pcv
sum pcv,detail

*check the distribution of pcv

hist pcv, norm

*desceptive statistics for pcv
sum pcv,detail

*se(mean) of 670 obs 
ci pcv
mean pcv

*analysis of first 60 obs
ci pcv if _n <= 60
mean pcv if _n <= 60
