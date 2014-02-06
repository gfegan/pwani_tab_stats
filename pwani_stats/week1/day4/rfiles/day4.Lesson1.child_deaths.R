#--------------------------------------------------------------------------------
#setwd("put the path to the cd")
#-------------------------------------------------------------------------

library(foreign)
library(epicalc)
library(epitools)


load("data/child.deaths.RData")
use(child.deaths)
attach(child.deaths)
des()
codebook()

# generate person-years
pyo<-difftime(date_death,date_birth)
pyo<-(as.numeric(pyo))/365.25
label.var(pyo, "person years observed")
child.deaths<-data.frame(child.deaths,pyo)

# rates
child.deaths$status<-as.numeric(child.deaths$status)
total_deaths<-length(child.deaths$status[child.deaths$status==2])

total_pyo<-sum(child.deaths$pyo)
rate<-(as.numeric(total_deaths)/total_pyo)*1000


# standard error of rate
s.e.Rate<-sqrt(total_deaths)/total_pyo*1000 # +/- 2.769689
s.e.log.Rate<-1/sqrt(total_deaths)

# 95% CI for log rate
log.rate=log(rate)
lower.bound=exp(log.rate-1.96*s.e.log.Rate)
upper.bound=exp(log.rate+1.96*s.e.log.Rate)

## Comparing two rates
#males vs females

f.deaths<-length(child.deaths$status[child.deaths$status==2 & child.deaths$sex=="f"])
m.deaths<-length(child.deaths$status[child.deaths$status==2 & child.deaths$sex=="m"])


f.total_pyo<-sum(child.deaths$pyo[child.deaths$sex=="f"])
m.total_pyo<-sum(child.deaths$pyo[child.deaths$sex=="m"])

table(status,sex)
bysex<-ratetable(f.deaths,m.deaths,f.total_pyo,m.total_pyo)
dimnames(bysex)<-list(Exposure=c("females","males"), Outcome=c("Deaths","PYears"))

#
rateratio.wald(bysex,rev="r")


#######################################################################################
#home vs hospital delivery

hom.deaths<-length(child.deaths$status[child.deaths$status==2 & child.deaths$locn_birth=="Home"])
hosp.deaths<-length(child.deaths$status[child.deaths$status==2 & child.deaths$locn_birth=="Hospital"])


hom.total_pyo<-sum(child.deaths$pyo[child.deaths$locn_birth=="Home"])
hosp.total_pyo<-sum(child.deaths$pyo[child.deaths$locn_birth=="Hospital"])


table(status,locn_birth)
by.locn.birth<-ratetable(hom.deaths,hosp.deaths,hom.total_pyo,hosp.total_pyo)

dimnames(by.locn.birth)<-list(Exposure=c("Home","Hospital"), Outcome=c("Deaths","PYears"))

#
rateratio.wald(by.locn.birth,rev="r")



#######################################################################################
#mother's education. some education vs no education

no.educ.deaths<-length(child.deaths$status[child.deaths$status==2 & child.deaths$mom_educ=="no education"])
some.educ.deaths<-length(child.deaths$status[child.deaths$status==2 & child.deaths$mom_educ=="some education"])


no.educ.total_pyo<-sum(child.deaths$pyo[child.deaths$mom_educ=="no education"])
some.educ.total_pyo<-sum(child.deaths$pyo[child.deaths$mom_educ=="some education"])


table(status,mom_educ)
by.mom.educ<-ratetable(no.educ.deaths,some.educ.deaths,no.educ.total_pyo,some.educ.total_pyo)

dimnames(by.mom.educ)<-list(Exposure=c("No education","Some education"), Outcome=c("Deaths","PYears"))

#
rateratio.wald(by.mom.educ,rev="r")
