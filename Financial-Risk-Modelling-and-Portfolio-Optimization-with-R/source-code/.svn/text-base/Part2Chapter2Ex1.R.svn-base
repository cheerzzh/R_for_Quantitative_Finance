library(evir)
data(siemens)
## Losses
SieLoss <- -100.0 * siemens
## package evir:
SieGEV <- gev(SieLoss, block = "semester")
SieGEV
plot(SieGEV$data, type = "h", col = "blue", xlab = "",
     ylab = "Block Maxima",
     main = "Maximum Biannual Losses of Siemens")
## package ismev:
library(ismev)
SieGEV2 <- gev.fit(SieGEV$data)
SieGEV2
gev.diag(SieGEV2)
par(mfrow = c(2, 1))
gev.prof(SieGEV2, m = 20, xlow = 5, xup = 16, conf = 0.95)
gev.profxi(SieGEV2, xlow = 0.0, xup = 0.7, conf = 0.95)
mLoss <- max(SieGEV$data)
mYears <- 1 / (1 - pgev(mLoss, mu = SieGEV2$mle[1],
                        sigma = SieGEV2$mle[2],
                        xi = SieGEV2$mle[3])) / 2
## package fExtremes:
library(fExtremes)
SieGEV3 <-  gevFit(SieGEV$data, type = "pwm")
SieGEV3
