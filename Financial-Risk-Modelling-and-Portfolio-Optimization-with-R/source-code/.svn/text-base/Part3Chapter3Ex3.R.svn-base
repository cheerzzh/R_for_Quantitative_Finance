library(fPortfolio)
library(FRAPO)
library(PerformanceAnalytics)
data(MultiAsset)
## Return calculation
Rets <- returnseries(MultiAsset, method = "discrete",
                     percentage = FALSE, trim = TRUE)
Rets <- timeSeries(Rets, charvec = rownames(Rets))
## Benchmark portfolio: GMV
gmvspec <- portfolioSpec()
GMV <- minvariancePortfolio(data = Rets, spec = gmvspec,
                            constraints = "LongOnly")
GMVret <- timeSeries(Rets %*% getWeights(GMV),
                     charvec = time(Rets))
GMVDD <- Drawdowns(GMVret)
## Plot of draw downs for GMV
ylims <- c(-6, 0)
plot(GMVDD * 100, xlab = "", ylab = "Draw Downs (percentage)",
     main = "Draw Downs of Global Minimum Variance", ylim = ylims)
abline(h = 0, col = "grey")
grid()
## Max DD of GMV
GMVMaxDD <- max(-1.0 * GMVDD)
## Draw Down Portfolios
MaxDD <- PMaxDD(MultiAsset, MaxDD = GMVMaxDD)
AveDD <- PAveDD(MultiAsset, AveDD = GMVMaxDD)
CDaR95 <- PCDaR(MultiAsset, alpha = 0.95, bound = GMVMaxDD)
CDaRMin95 <- PCDaR(MultiAsset, alpha = 0.95)
## Plot of draw downs
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
plot(AveDD, main = "(a) AveDD")
plot(MaxDD, ylim = ylims, main = "(b) MaxDD")
plot(CDaR95, ylim = ylims, main = "(c) CDaR")
plot(CDaRMin95, ylim = ylims, main = "(d) Minimum CDaR")
par(oldpar)
