## Loading of packages
library(lmomco)
library(FRAPO)
## Data loading
data(SP500)
Idx <- SP500[, "QCOM"]
L <- -1 * returnseries(Idx, method = "discrete", trim = TRUE)
## Computing VaR (Normal & GLD) 99%, moving window 
ep <- 104:length(L)
sp <- 1:length(ep)
level <- 0.99
VaR <- matrix(NA, ncol = 2, nrow = length(ep))
for(i in 1:length(sp)){
  x <- L[sp[i]:ep[i]]
  lmom <- lmom.ub(x)
  fit <- pargld(lmom)
  VaRGld <- quagld(level, fit)
  VaRNor <- qnorm(level, mean(x), sd(x))
  VaR[i, ] <- c(VaRGld, VaRNor)
  print(paste("Result for", ep[i], ":", VaRGld, "and", VaRNor)) 
}
## Summarising results
Res <- cbind(L[105:length(L)], VaR[-nrow(VaR), ])
colnames(Res) <- c("Loss", "VaRGld", "VaRNor")
## Plot of backtest results
plot(Res[, "Loss"], type = "p", xlab = "Time Index",
     ylab = "Losses in percent", pch = 19, cex = 0.5,
     ylim = c(-15, max(Res)))
abline(h = 0, col = "grey")
lines(Res[, "VaRGld"], col = "blue", lwd = 2)
lines(Res[, "VaRNor"], col = "red", lwd = 2)
legend("bottomleft", legend = c("Losses", "VaR GLD", "VaR Normal"),
       col = c("black", "blue", "red"),
       lty = c(NA, 1, 1), pch = c(19, NA, NA), bty = "n")
