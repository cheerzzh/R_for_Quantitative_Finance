con = gzcon(file('sit.gz', 'rb')) #from same folder
source(con)
close(con)

load.packages('quantmod')
 
tickers = spl('GLD,UUP,SPY,QQQ,IWM,EEM,EFA,IYR,USO,TLT')

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1900-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
     
bt.prep(data, align='remove.na')

# Use following 3 methods to determine number of clusters
# * Minimum number of clusters that explain at least 90% of variance
#   cluster.group.kmeans.90
# * Elbow method
#   cluster.group.kmeans.elbow
# * Hierarchical clustering tree cut at 1/3 height
#   cluster.group.hclust
#******************************************************************

# on our data set every week with 250 days look-back to compute correlations.
# helper function to compute portfolio allocation additional stats
portfolio.allocation.custom.stats.clusters <- function(x,ia) {
    return(list(
        ncluster.90 = max(cluster.group.kmeans.90(ia)),
        ncluster.elbow = max(cluster.group.kmeans.elbow(ia)),
        ncluster.hclust = max(cluster.group.hclust(ia))
    ))
}
 
#*****************************************************************
# Compute # Clusters
#******************************************************************         
periodicity = 'weeks'
lookback.len = 250
     
obj = portfolio.allocation.helper(data$prices, 
    periodicity = periodicity, lookback.len = lookback.len,
    min.risk.fns = list(EW=equal.weight.portfolio),
    custom.stats.fn = portfolio.allocation.custom.stats.clusters
) 

#  historical number of cluster time series plots for each method
#*****************************************************************
# Create Reports
#******************************************************************         
temp = list(ncluster.90 = 'Kmeans 90% variance',
     ncluster.elbow = 'Kmeans Elbow',
     ncluster.hclust = 'Hierarchical clustering at 1/3 height') 
 
for(i in 1:len(temp)) {
    hist.cluster = obj[[ names(temp)[i] ]]
    title = temp[[ i ]]
 
    plota(hist.cluster, type='l', col='gray', main=title)
        plota.lines(SMA(hist.cluster,10), type='l', col='red',lwd=5)
    plota.legend('Number of Clusters,10 period moving average', 'gray,red', x = 'bottomleft')           
}

