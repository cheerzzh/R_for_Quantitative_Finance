
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




















