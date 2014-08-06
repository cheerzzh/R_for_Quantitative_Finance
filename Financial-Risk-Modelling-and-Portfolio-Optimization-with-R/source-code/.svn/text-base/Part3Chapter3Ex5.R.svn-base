library(FRAPO)
library(fPortfolio)
library(PerformanceAnalytics)
## Loading of data set
data(EuroStoxx50)
## Creating timeSeries of prices and returns
pr <- timeSeries(EuroStoxx50, charvec = rownames(EuroStoxx50))
NAssets <- ncol(pr)
RDP <- na.omit((pr / lag(pr, k = 1) - 1) * 100)
## Backtest of GMV vs. CDaR
## Start and end dates
to <- time(RDP)[208:nrow(RDP)]
from <- rep(start(RDP), length(to))
## Portfolio specifications
## CDaR portfolio
DDbound <- 0.10
DDalpha <- 0.95
## GMV portfolio
mvspec <- portfolioSpec()
BoxC <- c("minsumW[1:NAssets] = 0.0", "maxsumW[1:NAssets] = 1.0")
## Initialising weight matrices
wMV <- wCD <- matrix(NA, ncol = ncol(RDP), nrow = length(to))
## Conducting backtest
for(i in 1:length(to)){
  series <- window(RDP, start = from[i], end = to[i])
  prices <- window(pr, start = from[i], end = to[i])
  mv <- minvariancePortfolio(data = series,
                             spec = mvspec,
                             constraints = BoxC)
  cd <- PCDaR(prices, alpha = DDalpha, bound = DDbound,
              softBudget = TRUE) 
  wMV[i, ] <- c(getWeights(mv))
  wCD[i, ] <- Weights(cd)
}
## Lagging optimal weights and sub-sample of returns
wMV <- rbind(rep(NA, ncol(RDP)), wMV[-nrow(wMV), ])
wMVL1 <- timeSeries(wMV, charvec = to)
colnames(wMVL1) <- colnames(RDP)
wCD <- rbind(rep(NA, ncol(RDP)), wCD[-nrow(wCD), ])
wCDL1 <- timeSeries(wCD, charvec = to)
colnames(wCDL1) <- colnames(RDP)
RDPback <- RDP[to,]
colnames(RDPback) <- colnames(RDP)
