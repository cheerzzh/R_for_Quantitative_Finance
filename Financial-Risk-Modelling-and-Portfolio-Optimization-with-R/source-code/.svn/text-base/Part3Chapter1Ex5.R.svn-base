## Loading of packages
library(FRAPO)
library(PerformanceAnalytics)
library(quadprog)
library(rrcov)
library(zoo)
## Loading data, calculate returns
data(StockIndex)
pzoo <- zoo(StockIndex, order.by = rownames(StockIndex))
rzoo <- (pzoo / lag(pzoo, k = -1) - 1) * 100
## boxplot and descriptive statistics
boxplot(coredata(rzoo))
rstats <- rbind(apply(rzoo, 2, summary),
                 skewness(rzoo),
                 kurtosis(rzoo)
                 )
