library(survival)
gender <- c("M","F","M","F","M","M","F","F","F","F")
time <- c(10,20,35,40,50,55,70,71,80,90)
status <- c(1,0,1,0,0,1,0,0,1,0)
#data=cbind(time,status,gender)
data <- data.frame(gender,time,status)
str(data)
data$status <- as.numeric(data$status)
survobj <- with(data,Surv(time,status))
survobj

fit <- survfit(survobj~1, data=data)
fit
summary(fit)
attributes(fit)

plot(fit, xlab="time to relapse (months)", ylab="Survival Function", yscale=100, main="Survival Distribution", conf.int = 0.95)
abline(h=0.5,col="blue")
abline(h=0.5,col="blue")
#kaplain-meier for the whole population
fit2 <- survfit(survobj~gender, data=data)
plot(fit2)
abline(h=0.5,col="blue")
survdiff(survobj~gender,data=data,rho=0)


