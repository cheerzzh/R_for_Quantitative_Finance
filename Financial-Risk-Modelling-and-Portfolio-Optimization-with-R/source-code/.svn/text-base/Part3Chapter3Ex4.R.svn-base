## Portfolio Names
Pnames <- c("GMV", "MaxDD", "AveDD", "CDaR95", "CDaRMin95")
## Portfolio allocations
WeightMatrix <- cbind(getWeights(GMV),
                      Weights(MaxDD),
                      Weights(AveDD),
                      Weights(CDaR95),
                      Weights(CDaRMin95))
colnames(WeightMatrix) <- Pnames
## Expected Shortfall and components
tmp <- apply(WeightMatrix, 2, function(x) ES(Rets, weights = x,
             method = "gaussian", portfolio_method = "component"))
## ES 95%
PES <- unlist(lapply(tmp, function(x) x[[1]])) * 100
## Marginal Contributions to ES
PMES <- matrix(unlist(lapply(tmp, function(x) x[[3]])),
               nrow = ncol(Rets)) * 100
rownames(PMES) <- colnames(Rets)
colnames(PMES) <- Pnames
## Marginal Contributions to StdDev
V <- cov(Rets)
PMRC <- apply(WeightMatrix, 2, mrc, Sigma = V)
rownames(PMRC) <- colnames(Rets)
## Diversification ratio
PDR <- apply(WeightMatrix, 2, dr, Sigma = V)
