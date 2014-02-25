*ANOVA one-way example data
use "/Users/lwafula/KWTRP/person_profiles&requests/Pwani_university/ANOVA/anova_one_way.dta", clear

anova systolic drug


*ANOVA two_way data
anova systolic drug disease

*Two-way factorial
anova systolic drug disease drug#disease

*or
 anova systolic drug##disease

*three way 
*webuse manuf
save "/Users/lwafula/KWTRP/person_profiles&requests/Pwani_university/ANOVA/anova_three_way.dta", replace
anova yield temp chem temp#chem meth temp#meth chem#meth temp#chem#meth

*or simply
anova yield temp##chem##meth
