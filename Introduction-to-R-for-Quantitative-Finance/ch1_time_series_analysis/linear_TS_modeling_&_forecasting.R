#packages to handle TS: zoo, xts, timrSeries
#

library(zoo)

# aapl.csv
#daily closing price for Apple's stock
# ISO8601 YYYY-MM-DD


aapl = read.zoo("aapl.csv",sep=",",header=TRUE,format = "%Y-%m-%d")
plot(aapl,main="APPLE Closing Prices on NASDAQ",
	ylab = "price(USD)",xlab = "Date")

#view first and last part
head(aapl)
tail(aapl)

#highest price and that day
aapl[which.max(aapl)]

#diff to get returns which are usually stationary
#calculate simple returns in percentage
ret_simple  = diff(aapl)/lag(aapl,k=-1)*100
ret_cont = diff(log(aapl))*100 #log return in percentage term

#plot the histogram
hist(ret_simple,breaks=100,main="Histogram of simple returns",
		xlab="%")

#highest price in 2013
aapl_2013 = window(aapl,start="2013-1-1",end="2013-12-31")
aapl_2013[which.max(aapl_2013)] #545.85 at 2013-1-2

#1 day 99% VaR
quantile(ret_simple,prob=0.01)


#linear time series modeling and forecasting
#use ARIMA

# model identification- determine order
# model estimation- MLE, LS
# model diagnostic checking- check residuals

#case study- Modeling and forecasring UK house prices
#use forecast package

#library(forecast)
#not avaiable under R 3.01

hp = read.zoo("UKHP.csv",sep=",",
	header=TRUE, format = "%Y-%m",FUN = as.yearmon)
#FUN applies the given function as.yearmon()

frequency(hp)
hp_ret = diff(hp)/lag(hp,k=-1)*100 #compute simple monthly return

#model identification and estimation
#use auto.arima()
# arima()
pacf(hp_ret)
mod = arima(hp_ret,order=c(2,0,0)) #AR(2) seems resonable
confint(mod)
tsdiag(mod) #model diagnostic
#seems good fitting, since residuals no volatility cluster, no autocorr
#high p-value for Ljung-Box test

#plot the raw monthly return versus fitted value
plot(hp_ret,lty=1,main = "UK house prices: raw data vs. fitted values",
		ylab = "Returns in percent",xlab = "Date")
lines(fitted(mod),lty=2,lwd=2,col="red")