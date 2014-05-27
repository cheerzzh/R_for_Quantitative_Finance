
#loads historical prices from Yahoo Fiance
#compares performance of 
#the Buy and Hold, Mean-Reversion, and Trend-Following strategies
# backtesting library in the Systematic Investor Toolbox:



###############################################################################
# Load Systematic Investor Toolbox (SIT)
#####################################################################

con = gzcon(file('sit.gz', 'rb'))
source(con)
close(con)

 #*****************************************************************
    # Load historical data from quantmod package
 #******************************************************************
library(quantmod)
tickers <- "SPY"

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)  
bt.prep(data, align='remove.na', dates='2000::2012')


#*****************************************************************
    # Code Strategies
#******************************************************************
prices <- data$prices  
n <- len(tickers)
nperiods <- nrow(prices)


# Buy & Hold   
data$weight[] = 1
buy.hold = bt.run(data)


rsi2 = bt.apply.matrix(prices, RSI, 2)
data$weight[] = NA
	data$weight[] = iif(rsi2 < 50, 1, -1)   
	capital = 100000
	data$weight[] = (capital / prices) * bt.exrem(data$weight)
mr = bt.run(data, type='share', capital=capital, trade.summary=T)



# Trend Following(TF) strategy - MA 50/200 crossover
sma.short = bt.apply.matrix(prices, SMA, 50)
sma.long = bt.apply.matrix(prices, SMA, 200)
data$weight[] = NA
    data$weight[] = iif(sma.short > sma.long, 1, -1)
    capital = 100000
    data$weight[] = (capital / prices) * bt.exrem(data$weight)
tf = bt.run(data, type='share', capital=capital, trade.summary=T)


#*****************************************************************
# Create Report
#******************************************************************
plotbt.custom.report.part1(mr, tf, buy.hold)



#*****************************************************************
# Regime Switching based on historical market volatility
# Classify current volatility by percentile using a 252 day look-back period
# percentrank(MA(percentrank(Stdev( diff(log(close)) ,21),252),21),250)
#******************************************************************


ret.log = bt.apply.matrix(prices, ROC, type='continuous')
hist.vol = bt.apply.matrix(ret.log, runSD, n = 21)
vol.rank = percent.rank(SMA(percent.rank(hist.vol, 252), 21), 250)


# Regime Switching Historical
data$weight[] = NA
    data$weight[] = iif(vol.rank > 0.5,
                iif(rsi2 < 50, 1, -1),
                iif(sma.short > sma.long, 1, -1)
            )
    capital = 100000
    data$weight[] = (capital / prices) * bt.exrem(data$weight)
regime.switching = bt.run(data, type='share', capital=capital, trade.summary=T)


#*****************************************************************
# Create Report
#******************************************************************
plotbt.custom.report.part1(regime.switching, mr, tf, buy.hold)



#*****************************************************************
# Benchmarking Garch algorithms
#******************************************************************
load.packages('tseries,fGarch,rbenchmark') 
temp = garchSim(n=252)
 
test1 <- function() {
    fit1=garch(temp, order = c(1, 1), control = garch.control(trace = F))
}
test2 <- function() {
    fit2=garchFit(~ garch(1,1), data = temp, include.mean=FALSE, trace=F)
}
         
benchmark(
    test1(),
    test2(),
    columns=spl('test,replications,elapsed,relative'),
    order='relative',
    replications=100
)



 #*****************************************************************
    # Forecast Volatility using Garch  
    # garch from tseries is fast, but does not consistently converge
    # garchFit from fGarch is slower, but converges consistently
    #******************************************************************
    load.packages('tseries,fGarch')
             
 #seems has bug 
 












