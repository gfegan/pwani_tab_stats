** Analysis of the bwmal data
** Data found in C:\jimt\mwanza\nimr
** Date 8th Feb 2011
** By Jim Todd
** As a practical for new Stata users
** This section at the beginning enables you to put comments about the do-file
*************************************
                    /* You can also put comments between these markers   */
clear               /* The command clear removes the data from memory    */

*************************************
* This next command closes the log file (in case it is left open by the last program)
*******************************************************
cap log close 


************************************************************
* Best to ensure that you change directory, to the folder for this analysis
* Do this first, and then the log file can be saved in the same directory
****************************************************************
cd "C:\JimT\Mwanza\NIMR\Training\Training_committee\Research methods course\Course materials\Stats\Data"

*****************************************************
** You must close the previous log files first
** Then open the log file to record your results
******************************************
log using bwmal.log , replace


* Before starting any analysis you must get the data
* Open the data with the command 'use'
*use "C:\JimT\Mwanza\NIMR\Training\Training_committee\Research methods course\Course materials\Stats\Data\bwmal.dta"
use bwmal, replace

* Start by describing the variables, 11 variables, and 791 observations
describe

* Browse enables you to look at the data
browse

** The command summarize to show means and std dev
** This can be for all variables or just for some variables
** Note the option , detail
sum
sum matage , detail

codebook


* Listing all the data would take a lot of space
* Better to just list the first 10 observations  in 1/10
list matage mheight in 1/10

* Or listing a subset, such as those who smoke
* Note when we use if we must put the == (double equals sign)
list if smoke==1

* This command gives the frequency and percentage for different levels of a variable
tabulate smoke

* To obtain a histogram
histogram matage
histogram sex , discrete

** Generate a new variable, and recode it to show two categories
gen gestgrp=gestwks
recode gestgrp min/36=1 37/max=2

** We can generate labels for the values in each variable
** Two steps.  First define the label - smokelbl
** And then apply that label to the values in one variable 
label define smokelbl 0 "Non-smoker" 1 "smoker"
label values smoke smokelbl
tab smoke
** Notice the difference with the label applied to the values

* Define labels for sex
label define gender 0 "Female" 1 "Male"
label value sex gender
tab sex
* We can also label the variable itself to make it clear what it means
label variable sex "The sex of the Baby"
tab sex

* Create a special group for analysis
generate specialgrp=0
replace specialgrp=1 if sex==1 &  bweight>4.0 & gestwks>40


** Then we can save the data in a new data file
** The replace option overwrites any file of the same name - BEWARE do not over write your original data
save bwmal_new , replace

** Another useful command is help: 
help summarize
help save
help generate

log close

