
# 2014-7-27


# load historical for the 10 major asset classes:
# Gold ( GLD )
# US Dollar ( UUP )
# S&P500 ( SPY )
# Nasdaq100 ( QQQ )
# Small Cap ( IWM )
# Emerging Markets ( EEM )
# International Equity ( EFA )
# Real Estate ( IYR )
# Oil ( USO )
# Treasurys ( TLT )

on = gzcon(file('sit.gz', 'rb')) #from same folder
source(con)
close(con)


#*****************************************************************
# Load historical data for ETFs
#******************************************************************


load.packages('quantmod')
 
tickers <- spl('GLD,UUP,SPY,QQQ,IWM,EEM,EFA,IYR,USO,TLT')

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1900-01-01', env = data, auto.assign = T)
    for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
# bt.prep function merges and aligns all symbols in the data environment
bt.prep(data, align='remove.na') 

# use the historical returns over the past year to compute correlations between all asset classes 
# and group assets into 4 clusters


# compute returns
ret <- data$prices / mlag(data$prices) - 1
ret <- na.omit(ret)     


# setup period and method to compute correlations
dates <- '2000::2012' # vary time period, cluster position change a lot 
method <- 'pearson'

correlation <- cor(ret[dates], method = method)  
dissimilarity <- 1 - (correlation)
distance <- as.dist(dissimilarity)


# find 4 clusters     
xy <- cmdscale(distance)
fit <- kmeans(xy, 4, iter.max=100, nstart=100)


#*****************************************************************
# Create Plot
#******************************************************************    
load.packages('cluster')
clusplot(xy, fit$cluster, color=TRUE, shade=TRUE, labels=3, lines=0, plotchar=F,
    main = paste('Major Market Clusters over', dates), sub='') 









