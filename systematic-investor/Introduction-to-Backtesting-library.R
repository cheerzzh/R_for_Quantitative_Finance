
#  evaluate and analyze Trading Strategies
con = gzcon(file('sit.gz', 'rb')) #from same folder
source(con)
close(con)



load.packages('quantmod')


# data is a time series of price
# signal is a indicator vector for buy and sell
bt.simple <- function(data, signal)
{
	# lag serial
	signal <- lag(signal,1)

	# back fill
	signal <- na.locf(signal, na.rm = FALSE)
	signal[is.na(signal)] = 0

	# calculate close-to-close returns
	# ROC() : Calculate the (rate of) change of a series over n periods.
	ret <- ROC(Cl(data), type="discrete")
	ret[1] = 0

	# compute stats
	bt <- list()
	bt$ret <- ret * signal 
	bt$equity <- cumprod(1 + bt$ret)

	return(bt)
}


# Test for bt.simple functions

# load historical prices from Yahoo Finance
data <- getSymbols('SPY', src = 'yahoo', from = '1980-01-01', auto.assign = F)

# buy and hold
signal <- rep(1, nrow(data))
buy.hold <- bt.simple(data, signal)

# MA cross (moving average)
# Cl: get closing price
sma <- SMA(Cl(data), 200)
signal <- ifelse(Cl(data) > sma, 1, 0) # if price large than moving mean, buy
sma.cross <- bt.simple(data, signal)

# Create a chart showing the strategies perfromance in 2000:2009
dates <- '2000::2009'
buy.hold.equity <- buy.hold$equity[dates] / as.double(buy.hold$equity[dates][1])
sma.cross.equity <- sma.cross$equity[dates] / as.double(sma.cross$equity[dates][1])

# chartSeries() : Charting tool to create standard financial charts given a time series like object
chartSeries(buy.hold.equity, TA = c(addTA(sma.cross.equity, on=1, col='red')), 
    theme ='white', yrange = range(buy.hold.equity, sma.cross.equity) )



# sample code to implement the above strategies using the backtesting library in the Systematic Investor Toolbox:
#*****************************************************************
# Load historical data
#******************************************************************    
load.packages('quantmod')
tickers <- spl('SPY')

data <- new.env() # data is a environment

# bt.prep function merges and aligns all symbols in the data environment
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
bt.prep(data, align='keep.all', dates='1970::2011')


#*****************************************************************
# Code Strategies
#******************************************************************
prices <- data$prices  # price for SPY

# bt.run computes the equity curve of strategy specified by data$weight matrix. 
# The data$weight matrix holds weights (signals) to open/close positions

# Buy & Hold   
data$weight[] <- 1
buy.hold <- bt.run(data)


# MA Cross
#  bt.apply function applies user given function to each symbol in the data environment
sma <- bt.apply(data, function(x) { SMA(Cl(x), 200) } ) 
data$weight[] <- NA # update weights matirx
data$weight[] <- iif(prices >= sma, 1, 0)
sma.cross <- bt.run(data, trade.summary=T)   

plotbt.custom.report(sma.cross, buy.hold)
