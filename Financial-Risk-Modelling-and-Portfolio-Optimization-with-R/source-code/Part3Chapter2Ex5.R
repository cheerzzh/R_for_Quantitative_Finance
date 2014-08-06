library(PerformanceAnalytics)
## Key measures (ex post)
RAdec <- RA / 100
RALB <- RAdec[, names(WBeta)]
RATD <- RAdec[, names(WTD)]
LbStd <- StdDev(rowSums(RALB * WBeta / 100)) * 100
TdStd <- StdDev(rowSums(RATD * WTD / 100)) * 100
LbES95 <- abs(ES(R = rowSums(RALB * WBeta / 100),
                 method = "gaussian")) * 100
TdES95 <- abs(ES(R = rowSums(RATD * WTD / 100),
                 method = "gaussian")) * 100
LbDr <- dr(WBeta, Sigma = cov(RALB))
TdDr <- dr(WTD, Sigma = cov(RATD))
LbCr <- cr(WBeta, Sigma = cov(RALB))
TdCr <- cr(WTD, Sigma = cov(RATD))
## Key measure (ex ante)
LbRetO <- returnseries(LBEquity, percent = FALSE, trim = TRUE)
LbRetO <- timeSeries(LbRetO, charvec = 1:30)
TdRetO <- returnseries(TDEquity, percent = FALSE, trim = TRUE)
TdRetO <- timeSeries(TdRetO, charvec = 1:30)
BmRetO <- timeSeries(RMo[-1] - 1, charvec = 1:30)
km <- function(pf, bm, scale){
  ra <- Return.annualized(pf, scale = scale) * 100
  ir <- InformationRatio(pf, bm, scale = scale)
  upr <- UpDownRatios(pf, bm, method = "Capture", side = "Up")
  dnr <- UpDownRatios(pf, bm, method = "Capture", side = "Down")
  res <- c(ra, ir, upr, dnr)
  names(res) <- c("Return", "IR", "UpRatio", "DownRatio")
  return(res)
}
LbKM <- km(LbRetO, BmRetO, scale = 52)
TdKM <- km(TdRetO, BmRetO, scale = 52)
     
