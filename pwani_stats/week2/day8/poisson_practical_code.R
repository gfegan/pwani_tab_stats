setwd("J:\\Advanced stats_nrb\\Wednesday_poisson regression")

poisson.data<-read.dta("collapsed_mortality.dta")
poisson.data
summary(poisson.data)

fit <- glm(died2 ~, data=poisson.data, family=poisson())
summary(fit) #display results 


library(Zelig)
data(sanction)

prac.data<-subset(sanction,select=c(mil,coop,target,num))
colnames(prac.data)<-c("outcome","agegrp","SES","visits")

# Fit the statistical model
model1=glm(visits ~ SES + agegrp, family = "poisson", data = prac.data)
summary(model1)

###LRT

model2=glm(visits ~ agegrp, family = "poisson", data =  prac.data)
summary(model2)

library(lmtest)

lrtest(model2,model1)






