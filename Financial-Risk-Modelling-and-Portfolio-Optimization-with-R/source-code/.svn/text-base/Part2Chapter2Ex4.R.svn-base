library(fExtremes)
library(fEcofin)
data(nyse)
NYSELevel <- timeSeries(nyse[, 2],
                        charvec = as.character(nyse[, 1]))
NYSELoss <- na.omit(-1.0 * diff(log(NYSELevel)) * 100)
colnames(NYSELoss) <- "NYSELoss"
## Point process data
NYSEPP <- pointProcess(x = NYSELoss, u = quantile(NYSELoss, 0.95))
## Declustering
DC05 <- deCluster(x = NYSEPP, run = 5, doplot = FALSE)
DC10 <- deCluster(x = NYSEPP, run = 10, doplot = FALSE)
DC20 <- deCluster(x = NYSEPP, run = 20, doplot = FALSE)
DC40 <- deCluster(x = NYSEPP, run = 40, doplot = FALSE)
DC60 <- deCluster(x = NYSEPP, run = 60, doplot = FALSE)
DC120 <- deCluster(x = NYSEPP, run = 120, doplot = FALSE)
## Fit of declustered data
DC05Fit <- gpdFit(DC05, u = min(DC05))
DC10Fit <- gpdFit(DC10, u = min(DC10))
DC20Fit <- gpdFit(DC20, u = min(DC20))
DC40Fit <- gpdFit(DC40, u = min(DC40))
DC60Fit <- gpdFit(DC60, u = min(DC60))
DC120Fit <- gpdFit(DC120, u = min(DC40))
