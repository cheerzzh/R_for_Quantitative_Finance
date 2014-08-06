library(FRAPO)
library(nacopula)
## S&P 500 
data(INDTRACK6)
## Market and Asset Returns
RM <- returnseries(INDTRACK6[1:260, 1], method = "discrete",
                   trim = TRUE)
RA <- returnseries(INDTRACK6[1:260, -1], method = "discrete",
                   trim = TRUE)
Beta <- apply(RA, 2, function(x) cov(x, RM) / var(RM))
Tau <- apply(RA, 2, function(x) cor(x, RM, method = "kendall"))
## Clayton Copula: Lower Tail Dependence 
ThetaC <- copClayton@tauInv(Tau)
LambdaL <- copClayton@lambdaL(ThetaC)
## Selecting Stocks below median; inverse log-weighted and scaled
IdxBeta <- Beta < median(Beta)
WBeta <- -1 * log(abs(Beta[IdxBeta]))
WBeta <- WBeta / sum(WBeta) * 100
## TD
IdxTD <- LambdaL < median(LambdaL)
WTD <- -1 * log(LambdaL[IdxTD])
WTD <- WTD / sum(WTD) * 100
Intersection <- sum(names(WTD) %in% names(WBeta)) /
                    length(WBeta) * 100
## Out-of-Sample Performance
RMo <- returnseries(INDTRACK6[260:290, 1], method = "discrete",
                   percentage = FALSE) + 1
RAo <- returnseries(INDTRACK6[260:290, -1], method = "discrete",
                   percentage = FALSE) + 1
## Benchmark
RMo[1] <- 100
RMEquity <- cumprod(RMo)
## Low Beta
LBEquity <- RAo[, IdxBeta]
LBEquity[1, ] <- WBeta
LBEquity <- rowSums(apply(LBEquity, 2, cumprod))
## TD 
TDEquity <- RAo[, IdxTD]
TDEquity[1, ] <- WTD
TDEquity <- rowSums(apply(TDEquity, 2, cumprod))
## Collecting results
y <- cbind(RMEquity, LBEquity, TDEquity)
