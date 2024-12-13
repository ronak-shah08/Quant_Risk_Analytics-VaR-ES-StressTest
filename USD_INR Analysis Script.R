install.packages("quantmod")
install.packages("PerformanceAnalytics")

library(quantmod)
library(PerformanceAnalytics)

usdinr = getSymbols('INR=X', src = 'yahoo', auto.assign = FALSE)
usdinr = na.omit(usdinr)
usdinr = usdinr["2020-02-07/2021-02-07"]
View(usdinr)
plot(usdinr$`INR=X.Close`)

logret = diff(log(usdinr$`INR=X.Close`))[-1]

mu = mean(logret)
sig =sd(logret)

onedayvar = qnorm(0.01,mu,sig)
round(onedayvar,6)

portfolio = 1000*onedayvar

es = mu-sig*dnorm(qnorm(0.01,0,1),0,1)/0.01


odvar = VaR(logret, p=0.99, method = 'historical')
odvar
odes = CVaR(logret, p=0.99, method = 'historical')
odes*1000
odvar1 = VaR(logret, p=0.99, method = 'gaussian')
odvar1
odes1 = ES(logret, p=0.99, method = 'gaussian')
odes1


