*Correlation practical

use f:\diet.dta,clear
*we could graph these data as a scatterplot of bmi on weight
*check normality  (eclipse shape)
scatter  bmi weight
*check normality 
su  bmi weight

hist bmi,norm
hist weight,norm


*we could graph these data as a scatterplot and put on top of that the prediction from a linear regression of bmi on weight
twoway (scatter bmi weight) (lfit bmi weight)

*check for outliers
graph box bmi

graph box weight

*remove outliers
replace weight = . if weight > 110 | weight < 40
replace bmi = . if bmi > 40 | bmi < 15

corr bmi weight
