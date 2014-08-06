## Back-test function
BT <- function(DataSub, BLR){
  DataSub <- as.timeSeries(DataSub)
  PPrior <- tangencyPortfolio(data = DataSub, spec = MSPrior,
                              constraints = BoxC)
  PBl <- tangencyPortfolio(data = DataSub, spec = MSBl,
                           constraints = BoxC)
  Weights <- rbind(getWeights(PPrior), getWeights(PBl))
  colnames(Weights) <- ANames
  rownames(Weights) <- c("Prior", "BL")
  return(Weights)
}
## Conducting back-test
Backtest <- list()
for(i in 1:length(idx)){
  DataSub <- window(R, start = start(AssetsM), end = idx[i])
  BLR <- PostDist[[i]]
  Backtest[[i]] <- BT(DataSub, BLR)
}
