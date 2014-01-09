******** QUESTION 2 SOLUTION ******
use zambia3,clear

*DISTRIBUTION OF EDUCATION IN URBAN AND RURAL AREAS
graph bar (count) id, over(educ) over(urban)  asyvars ///
ytitle("number of respondents") ylabel(,angle(horizontal))

*YOU PROBABLY WANT TO ADD A TEXT LABEL TO THE CATEGORY AXIS.
*EITHER LABEL THE URBAN VARIABLE OR:
graph bar (count) id, over(educ) over(urban,relabel(1 "Urban" ///
2 "Rural"))  asyvars ytitle("number of respondents") ///
ylabel(,angle(horizontal))

graph export gr2b4.wmf,replace

*WITH MOST OF THE COLOURS CHANGED
graph bar (count) id,over(educ) over(urban)  asyvars ///
ytitle("number of respondents") ylabel(,grid glcolor(black)) ///
graphregion(fcolor(white)) plotregion(fcolor(gray)) ///
legend(region(fcolor(yellow)))  bar(1,bfcolor(red) blcolor(red)) ///
bar(2,bfcolor(orange) blcolor(orange)) bar(3,bfcolor(blue) ///
 blcolor(blue)) bar(4,bfcolor(green) blcolor(green)) 


******** QUESTION 3 SOLUTION *******
use bab9,clear
graph box bweight, over(gestcat) 
*WITH A TITLE BELOW THE GRAPH
graph box bweight, over(gestcat) ///
title("Birth weight by gestational age",pos(6))

*AGAIN YOU CAN LABEL THE gestcat VARIABLE OR WRITE YOUR 
*OWN LABELS ON THE X-AXIS
graph box bweight, over(gestcat,relabel(1 "Premature" 2 "Term")) ///
 title("Birth weight by gestational age",pos(6))



 
******** QUESTION 4 SOLUTION *******
use zambia4,clear
*PIE CHART FOR EDUCATIONAL LEVEL OF WOMEN WHO USED A CONDOM 
*AT LAST SEX AND THOSE WHO DID NOT
graph pie [pw=weight],over(educ) by(clastsex)
*WITH ALTERED CAPTION
graph pie [pw=weight],over(educ) by(clastsex,note("") ///
caption("Educational level of women by condom use at last sex"))
