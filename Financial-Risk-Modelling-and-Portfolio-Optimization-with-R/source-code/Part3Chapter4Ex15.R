## Equal-weighted long-only strategy
EwRetfac <- 1 + rowMeans(Returns)
EwRetfac[1] <- 100
EW <- timeSeries(cumprod(EwRetfac), epoints)
## Plot of portfolio wealth
ylims <- range(cbind(LO, EW))
plot(LO, ylim = ylims, xlab = "", ylab = "Index")
lines(EW, col = "grey", lty = 2)
legend("topleft",
       legend = c("TAA long-only", "EW long-only"),
       lty = 1:2, col = c("black", "grey"))
## Portfolio analytics
library(PerformanceAnalytics)
## Portfolio returns
LORet <- returns(LO, method = "discrete", percentage = FALSE,
                 trim = TRUE)
EWRet <- returns(EW, method = "discrete", percentage = FALSE,
                 trim = TRUE)
## VaR
LOVAR <- -100 * VaR(LORet, p = 0.95, method = "gaussian")
EWVAR <- -100 * VaR(EWRet, p = 0.95, method = "gaussian")
## ES
LOES <- -100 * ES(LORet, p = 0.95, method = "gaussian")
EWES <- -100 * ES(EWRet, p = 0.95, method = "gaussian")
## Sharpe
LOSR <- SharpeRatio(LORet)
EWSR <- SharpeRatio(EWRet)
## Annualised returns
LORA <- Return.annualized(LORet, scale = 52)
EWRA <- Return.annualized(EWRet, scale = 52)
