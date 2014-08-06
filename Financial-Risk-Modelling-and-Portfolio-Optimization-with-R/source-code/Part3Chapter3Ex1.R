library(FRAPO)
library(fPortfolio)
## Retrieving data and calculating returns
data(StockIndex)
StockReturn <- na.omit(timeSeries(returnseries(StockIndex,
                                  method = "discrete"),
                                  charvec = rownames(StockIndex)))
## Specifying portfolio
pspec <- portfolioSpec()
gmv <-  pspec
cvar <- pspec
setType(cvar) <- "CVaR"
setAlpha(cvar) <- 0.1
setSolver(cvar) <- "solveRglpk"
## Conducting back-test
end <- time(StockReturn)[60:239]
from <- time(StockReturn)[1:length(end)]
wGMV <- matrix(NA, ncol = ncol(StockReturn), nrow = length(end))
wCVAR <- wGMV
for(i in 1:length(end)){
  series <- window(StockReturn, start = from[i], end = end[i])
  gmvpf <- minvariancePortfolio(data = series, spec = gmv,
                                constraints = "LongOnly")
  wGMV[i, ] <- c(getWeights(gmvpf))
  cvarpf <- minriskPortfolio(data = series, spec = cvar,
                             constraints = "LongOnly")
  wCVAR[i, ] <- c(getWeights(cvarpf))
}
