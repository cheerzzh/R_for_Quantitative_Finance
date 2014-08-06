## Conduct back test
EST <- c("CovClassic", "CovMcd", "CovMest", "CovMMest", "CovMve",
         "CovOgk", "CovSde", "CovSest")
## Function for back test
PfBack <- function(x, method = c("CovClassic", "CovMcd", "CovMest",
                   "CovMMest", "CovMve", "CovOgk", "CovSde",
                   "CovSest"), ...){
  cov <- Moments(x, method = method)
  return(PortMinVar(cov))
}
## Conducting back test
PfWeights <- lapply(EST, function(x)
                    rollapply(rzoo, width = 120, FUN = PfBack,
                              method = x, by.column = FALSE,
                              align = "right"))

periods <- as.Date(index(PfWeights[[1]]))
## Calculate portfolio returns / relative performance
PfReturns <- lapply(PfWeights, function(x)
                    rowSums(lag(x, k = -1) * rzoo))
PfReturns <- zoo(matrix(unlist(PfReturns),
                        ncol = length(PfReturns)), periods)
colnames(PfReturns) <- EST
PortOut <- (PfReturns[, -1] - PfReturns[, 1])
## plot relative peformance
plot(PortOut, type = "h",
     xlab = "",
     ylab = EST[-1],
     main = "Relative Performance",
     ylim = range(PortOut))
## statistics on relative performance
PortRelStats <- rbind(apply(PortOut, 2, summary),
                      skewness(PortOut)
                      )
