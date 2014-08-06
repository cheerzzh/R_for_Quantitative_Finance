library(ghyp)
library(timeSeries)
library(fEcofin)
## Return calculation
data(DowJones30)
y <- timeSeries(DowJones30[, "HWP"], charvec =
                as.character(DowJones30[, 1])) 
yret <- na.omit(diff(log(y)) * 100)
## Fitting
ef <- density(yret)
ghdfit <- fit.ghypuv(yret, symmetric = FALSE,
                     control = list(maxit = 1000))
hypfit <- fit.hypuv(yret, symmetric = FALSE,
                    control = list(maxit = 1000))
nigfit <- fit.NIGuv(yret, symmetric = FALSE,
                    control = list(maxit = 1000))
## Densities
ghddens <- dghyp(ef$x, ghdfit)
hypdens <- dghyp(ef$x, hypfit)
nigdens <- dghyp(ef$x, nigfit)
nordens <- dnorm(ef$x, mean = mean(yret), sd = sd(c(yret[, 1])))
col.def <- c("black", "red", "blue", "green", "orange")
plot(ef, xlab = "", ylab = expression(f(x)), ylim = c(0, 0.25))
lines(ef$x, ghddens, col = "red")
lines(ef$x, hypdens, col = "blue")
lines(ef$x, nigdens, col = "green")
lines(ef$x, nordens, col = "orange")
legend("topleft",
       legend = c("empirical", "GHD", "HYP", "NIG", "NORM"),
       col = col.def, lty = 1)
## QQ-Plots
qqghyp(ghdfit, line = TRUE, ghyp.col = "red", plot.legend = FALSE,
       gaussian = FALSE, main = "", cex = 0.8)
qqghyp(hypfit, add = TRUE, ghyp.pch = 2, ghyp.col = "blue",
       gaussian = FALSE, line = FALSE, cex = 0.8)
qqghyp(nigfit, add = TRUE, ghyp.pch = 3, ghyp.col = "green",
       gaussian = FALSE, line = FALSE, cex = 0.8)
legend("topleft", legend = c("GHD", "HYP", "NIG"),
       col = col.def[-c(1,5)], pch = 1:3)
## Diagnostics
AIC <- stepAIC.ghyp(yret, dist = c("ghyp", "hyp", "NIG"),
                    symmetric = FALSE,
                    control = list(maxit = 1000))
LRghdnig <- lik.ratio.test(ghdfit, nigfit)
LRghdhyp <- lik.ratio.test(ghdfit, hypfit)
