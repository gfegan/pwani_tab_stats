set more off
clear
*cap log close
*log using "C:\Users\Data Entry\Desktop\Stats_binary_cat.log", replace
use "birthweight2.dta" 

***Practical 5
**Explore data variables, labels and types
describe
tab  lbw
codebook  lbw

**recode data to have normal weight =0
gen lbw2 = 1 if lbw ==0
replace lbw2 =0 if lbw ==1
label define bwlab 0 "normal birth weight" 1 "low birth weight"
label values lbw2 bwlab
label var lbw2 "low birth weight"

tab  lbw2
**claculating confidence interval
ci  lbw2
**claculating confidence interval by gender
bysort sex: ci lbw2, b

***Test the hypothesis ie null hypthesis =0.9
prtest  lbw2 =.9
***Test the hypothesis ie null hypthesis =0.9 by gender
prtest  lbw2 =.9 if sex ==1
prtest  lbw2 =.9 if sex ==2
**or
bysort sex: prtest lbw2=.9

bitest  lbw2 =.9
prtest  lbw2 =.9


***session 2
**Hypothesis test and CI for difference in proportions
prtest lbw2, by(sex)
bysort sex: ci lbw2, b

***Practical 6
**how to compute a chi-square test
tab  sex lbw2, col chi
tab  ht lbw2, col chi
tab  ethnic lbw2, col chi
prtest  lbw2, by(ht)
tab  ht lbw2, col chi

***Practical 7
**measures of association - Odds ratios
**create a categorical variable from maternal age 
gen matagegp=matage
recode matagegp min/29=1 30/34=2 35/39=3 40/max=4
label define matagelab 1 "23 - 29" 2 "30 - 34" 3 "35 - 39" 4 "40+"
label values matagegp matagelab
label var matagegp "maternal age group"

**Create a categorical varible "gestwkgp" from gestation in weeks "gestwks"
gen gestwkgp=gestwks
recode gestwkgp min/32=1 33/36=2 37/40=3 41/max=4

gen ht2 =ht
replace ht2 = 0 if ht ==2
**compute measures of association
cs lbw2 ht2, or

tab lbw2 matagegp, col exp chi
tab lbw2 gestwkgp, col exp chi

tab ht2 matagegp, col exp chi
**check for trend in the "gestwkgp" categories
nptrend lbw2, by(gestwkgp)
tabodds lbw2 gestwkgp

tabodds lbw2 sex
mhodds lbw2 sex


*log close
