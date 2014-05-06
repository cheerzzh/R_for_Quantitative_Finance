
#handling volatility clustering
#GARCH model

#volatility forecasting for risk management
#VaR

#monthly return for Intel from Jan.1973 - Dec.2008
library(zoo)
intc = read.zoo("intc.csv",header=TRUE,
	sep=",",format="%Y-%m",FUN = as.yearmon)

#test for ARCH effects
#plot of returns
plot(intc,main="Monthly returns for Intel",
	xlab="Date",ylab="Returns in percent")

#test for ARCH effects
#1. Ljung-Box test
Box.test(coredata(intc^2),type = "Ljung-Box",lag=12)
#reject H0

#2. Lagrange Multiplier(LM) Test

library(FinTS)
ArchTest(coredata(intc)) #LM test
#reject H0

#GARCH model specification
#use package rugarch
library(rugarch)

#fit GARCH(1,1)
intc_garch11_spec = ugarchspec(variance.model=list(
	garchOrder=c(1,1)),mean.model=list(armaOrder=c(0,0)))

#Garch model estimation
intc_garch11_fit = ugarchfit(spec = intc_garch11_spec,
	data = intc)


#historical backtesting the risk model
# comapre estimated VaR with actual return 
# ugarch11 performs a historical backtest
intc_garch11_roll = ugarchroll(intc_garch11_spec,intc,
	n.start=120, refit.every = 1, refit.window = "moving",
	solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = 0.01,
	keep.coef= TRUE)

#report() examine backtesting report
report(intc_garch11_roll, type = "VaR", VaR.alpha = 0.01,
	conf.level = 0.99)

intc_VaR <- zoo(intc_garch11_roll@forecast$VaR[, 1])
index(intc_VaR) <- as.yearmon(rownames(intc_garch11_roll@forecast$VaR))
intc_actual <- zoo(intc_garch11_roll@forecast$VaR[, 2])
index(intc_actual) <-
as.yearmon(rownames(intc_garch11_roll@forecast$VaR))

plot(intc_actual, type = "b", main = "99% 1 Month VaR Backtesting",
	xlab = "Date", ylab = "Return/VaR in percent")
lines(intc_VaR, col = "red")
legend("topright",inset=0.05,c("Intel return","VaR"),
	col =c("black","red"),lty=c(1,1))

#forecasting
#produce VaR forecast

intc_garch11_fcst = ugarchforecast(intc_garch11_fit,n.ahead=12)
intc_garch11_fcst











