## Defining portfolio specifications
SSTPrior <- function(x, spec = NULL, ...){
  list(mu = c(MSTfit@fit$beta), Sigma = MSTfit@fit$Omega)
}
BlCopPost <- function(x, spec = NULL, ...){
  Sim <- CopPost@posteriorSims
  list(mu = colMeans(Sim), Sigma = cov(Sim))
}
## Skewed Student's t
MSPriorSST <-  portfolioSpec()
setEstimator(MSPriorSST) <- "SSTPrior"
## BLCOP specification
MSBlCop <-  portfolioSpec()
setEstimator(MSBlCop) <- "BlCopPost"
## Tangency portfolios
R <- as.timeSeries(R)
BLR <- PostDist[[27]]
PSpecs <- list(MSPrior, MSBl, MSPriorSST, MSBlCop)
POpt <- lapply(PSpecs, function(x)
               tangencyPortfolio(data = R, spec = x,
                                 constraints = BoxC)
               )
PWeights <- unlist(lapply(POpt, getWeights))
Weights <- matrix(PWeights, ncol = NAssets, nrow = 4,
                  byrow = TRUE) * 100
colnames(Weights) <- ANames
rownames(Weights) <- c("Gauss", "Skewed Student's t",
                       "BL", "BLCop")
Weights

