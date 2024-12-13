install.packages("quantmod")
install.packages("PerformanceAnalytics")

library(quantmod)
library(PerformanceAnalytics)

fromdate = "2020-02-07"
todate = "2021-02-07"

tickers = c("TATASTEEL.NS","INFY.NS","WIPRO.NS","ASIANPAINT.NS","INR=X","EURINR=X")
weights = c(0.18,0.13,0.12,0.17,0.16,0.24)

portfolio = getSymbols(tickers, src = 'yahoo', from = fromdate, to = todate, auto.assign = TRUE)
view(portfolio)

port.prices = na.omit(merge(Cl(TATASTEEL.NS),Cl(INFY.NS),Cl(WIPRO.NS),Cl(ASIANPAINT.NS),Cl(`INR=X`),Cl(`EURINR=X`)))
head(port.prices)
View(port.prices)
port.return = ROC(port.prices, type = "discrete")[-1]

head(port.return)


VaR(port.return, p=0.99, weights = weights, method = "historical", portfolio_method = "component")
ES(port.return, p=0.99, weights = weights, method = "historical", portfolio_method = "component")

VaR.gauss =VaR(port.return, p=0.99, weights = NULL, portfolio_method = "single", method = "gaussian") 
VaR.hist = VaR(port.return, p=0.99, weights = NULL, portfolio_method = "single", method = "historical")
VaR.mod = VaR(port.return, p=0.99, weights = NULL, portfolio_method = "single", method = "modified")

allvar = data.frame(rbind(VaR.gauss,VaR.hist,VaR.mod))

rownames(allvar) = c("Gaussian", "Historical", "Modified")
allvar

portvar.hist = VaR(port.return, p=0.99, weights = weights, method = "historical", portfolio_method = "component")$hVaR
portvar.gauss = VaR(port.return, p=0.99, weights = weights, method = "gaussian", portfolio_method = "component")$VaR
portvar.mod = VaR(port.return, p=0.99, weights = weights, method = "modified", portfolio_method = "component")$MVaR

allvar$portfolio = 0
allvar
allvar$portfolio = c(portvar.gauss,portvar.hist,portvar.mod)
View(allvar)

allvar$portfolioES = 0
allvar

portes.hist= ES(port.return, p=0.99, weights = weights, method = "historical", portfolio_method = "component")$`-r_exceed/c_exceed`
portes.gauss = ES(port.return, p=0.99, weights = weights, method = "gaussian", portfolio_method = "component")$ES
portes.mod = ES(port.return, p=0.99, weights = weights, method = "modified", portfolio_method = "component")$MES

allvar$portfolioES=c(portes.gauss,portes.hist,portes.mod)
allvar



