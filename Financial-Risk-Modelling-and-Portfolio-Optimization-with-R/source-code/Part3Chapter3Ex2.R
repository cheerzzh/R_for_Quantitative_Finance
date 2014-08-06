## Compute Portfolio values (subsequent returns)
wGMVL1 <- lag(timeSeries(wGMV, charvec = end), k = 1)
colnames(wGMVL1) <- colnames(StockReturn)
wCVARL1 <- lag(timeSeries(wCVAR, charvec = end), k = 1)
colnames(wCVARL1) <- colnames(StockReturn)
## Return factors and portfolio values  
GMVRetFac <- 1 + rowSums(wGMVL1 *
                         StockReturn[time(wGMVL1), ]) / 100  
GMVRetFac[1] <- 100
GMVPort <- timeSeries(cumprod(GMVRetFac),
                      charvec = names(GMVRetFac))
CVARRetFac <- 1 + rowSums(wCVARL1 *
                          StockReturn[time(wCVARL1), ]) / 100  
CVARRetFac[1] <- 100
CVARPort <- timeSeries(cumprod(CVARRetFac),
                       charvec = names(CVARRetFac))
## Plotting of portfolio values 
ylims <- range(cbind(GMVPort, CVARPort))
plot(GMVPort, ylim = ylims, xlab = "",
     ylab = "Portfolio Value (Index)")
lines(CVARPort, col = "blue")
legend("topleft",
       legend = c("Minimum-Variance", "Minimum-CVaR"),
       col = c("black", "blue"), lty = 1)
## Relative performance
RelOutPerf <- (CVARPort - GMVPort) / GMVPort * 100
plot(RelOutPerf, type = "h", col = "blue", xlab = "",
     ylab = "Percent",
     main = "Relative Out-Performance Min-CVaR vs. Min-Variance")
abline(h = 0, col = "grey")
