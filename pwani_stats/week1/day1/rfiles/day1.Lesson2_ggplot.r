#******** QUESTION 2 SOLUTION ******#
#load the required libraries
library(ggplot2) #used for plotting
library(foreign) #allow reading of dta files
#--------------------------------------------------------------------------------
#setwd("put the path to the cd")
#-------------------------------------------------------------------------

#load the data set
zambia3 <- read.dta("data/zambia3.dta", convert.dates=TRUE)

#summarize the count of data by education and urban/rural
totals <- table(zambia3$educ , zambia3$urban)
totals <- as.data.frame(totals )
#*DISTRIBUTION OF EDUCATION IN URBAN AND RURAL AREAS
names(totals) <- c("educ", "urban" , "freq")
ggplot(totals , aes(as.factor(urban), fill=educ, weight=freq)) + geom_bar(position="dodge") + ylab("Number of respodents") + 
  xlab("") +  ggtitle("") 
#*YOU PROBABLY WANT TO ADD A TEXT LABEL TO THE CATEGORY AXIS.
#*EITHER LABEL THE URBAN VARIABLE OR:
totals$urban <- factor(totals$urban,
                         levels = c(1,2),
                         labels = c("Urban", "Rural")) 

ggplot(totals[!is.na(totals$urban), ] , aes(as.factor(urban), fill=educ, weight=freq)) + geom_bar(position="dodge") + ylab("Number of respodents") + 
  xlab("") +  ggtitle("")
ggsave(filename="gr2b4.wmf")
#*WITH MOST OF THE COLOURS CHANGED
ggplot(totals[!is.na(totals$urban), ] , aes(as.factor(urban), fill=educ, weight=freq)) + geom_bar(position="dodge") + ylab("Number of respodents") + 
  xlab("") +  ggtitle("") +  
  scale_fill_manual(values = c("No education" = "red", "Primary education" = "orange", "Secondary education" = "blue", "Higher education" = "green")) +
  theme(panel.background = element_rect(fill = "gray") ,panel.grid.major = element_line(colour = "black") 
        ,panel.grid.minor = element_line(colour = "red", linetype = "dotted") ,plot.background = element_rect(fill = "cyan") , legend.background = element_rect(colour = "black") ,
        legend.key = element_rect(fill = "yellow")) 


# /Another way of doing it
# require(doBy) #provide summary of data
# total.id <- summaryBy(id~educ +urban, data=zambia3, FUN=function(x)  c(count=length(x)))
# as.data.frame(total.id)
# ggplot(total.id[!is.na(total.id$urban), ] , aes(as.factor(urban), fill=educ, weight=id.count)) + geom_bar(position="dodge") + ylab("Number of respodents") + 
#   xlab("") +  ggtitle("")   
# 
#   /
# /continuation of the second way
#total.id$urban <- factor(total.id$urban,
#                          levels = c(1,2),
#                          labels = c("Urban", "Rural")) 
# ggplot(total.id[!is.na(total.id$urban), ] , aes(as.factor(urban), fill=educ, weight=id.count)) + geom_bar(position="dodge") + ylab("Number of respodents") + 
#   xlab("") +  ggtitle("")
# ggsave(filename="gr2b4.wmf")


# continuation of second way
# ggplot(total.id[!is.na(total.id$urban), ] , aes(as.factor(urban), fill=educ, weight=id.count)) + geom_bar(position="dodge") + ylab("Number of respodents") + 
#   xlab("") +  ggtitle("") +  
#   scale_fill_manual(values = c("No education" = "red", "Primary education" = "orange", "Secondary education" = "blue", "Higher education" = "green")) +
#   theme(panel.background = element_rect(fill = "gray") ,panel.grid.major = element_line(colour = "black") 
#         ,panel.grid.minor = element_line(colour = "red", linetype = "dotted") ,plot.background = element_rect(fill = "cyan") , legend.background = element_rect(colour = "black") ,
#         legend.key = element_rect(fill = "yellow"))

  

#******** QUESTION 3 SOLUTION *******
#load the data set
bab9 <- read.dta("data/bab9.dta", convert.dates=TRUE)

ggplot(bab9, aes(factor(gestcat), bweight)) + geom_boxplot()
#*WITH A TITLE BELOW THE GRAPH
ggplot(bab9, aes(factor(gestcat), bweight)) + geom_boxplot() + ggtitle("Birth weight by gestational age")  + 
  theme(plot.title=element_text(vjust=-54)) + xlab("")

#*AGAIN YOU CAN LABEL THE gestcat VARIABLE OR WRITE YOUR 
#*OWN LABELS ON THE X-AXIS
bab9$gestcat <- factor(bab9$gestcat,
                         levels = c(1,2),
                         labels = c("Premature", "Term"))
ggplot(bab9, aes(factor(gestcat), bweight)) + geom_boxplot() + ggtitle("Birth weight by gestational age")  + 
  theme(plot.title=element_text(vjust=-54)) + xlab("")



#******** QUESTION 4 SOLUTION *******
#load the data set
zambia4 <- read.dta("data/zambia4_isingo.dta", convert.dates=TRUE)
#*PIE CHART FOR EDUCATIONAL LEVEL OF WOMEN WHO USED A CONDOM 
#*AT LAST SEX AND THOSE WHO DID NOT
#summarize the count of data by education and urban/rural
ggplot(zambia4[!is.na(zambia4$clastsex), ] ,   aes(x = factor(1), fill = educ,weight=weight) )  +  coord_polar(theta = "y") +
  scale_x_discrete("") + facet_grid(facets=. ~ clastsex) +  geom_bar(width = 1,position = "fill")
#*WITH ALTERED CAPTION
ggplot(zambia4[!is.na(zambia4$clastsex), ] ,   aes(x = factor(1), fill = educ,weight=weight) )  +  coord_polar(theta = "y") +
  scale_x_discrete("") + facet_grid(facets=. ~ clastsex) +  geom_bar(width = 1,position = "fill") + 
  ggtitle("Educational level of women by condom use at last sex")

