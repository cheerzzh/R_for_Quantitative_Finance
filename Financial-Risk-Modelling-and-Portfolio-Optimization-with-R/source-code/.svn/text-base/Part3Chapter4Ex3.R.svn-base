library(BLCOP)
library(fPortfolio)
## Computing returns
R <- (AssetsM / lag(AssetsM, k = -1) -1.0) * 100
## Pick matrix for directional views
P <- diag(NAssets)
colnames(P) <- ANames
## Function for BL posterior distribution
BL <- function(x, tau, kappa){
  q <- qv[time(qv) == x, ]
  q <- c(coredata(q))
  Rw <- window(R, start = start(R), end = x)
  mu <- colMeans(Rw)
  cov <- cov(Rw)
  clevel <- rep(1, NAssets)
  views <- BLViews(P = P, q = q, confidences = clevel,
                   assetNames = ANames)
  post <- posteriorEst(views, mu = mu, tau = tau,
                       sigma = cov, kappa = kappa)
  return(post)
}
PostDist <- lapply(idx, BL, tau = 1, kappa = 1)
## Defining portfolio specifications
EstPrior <- function(x, spec = NULL, ...){
  list(mu = BLR@priorMean, Sigma = BLR@priorCovar)
}
EstBL <- function(x, spec = NULL, ...){
  list(mu = BLR@posteriorMean, Sigma = BLR@posteriorCovar)
}
## Prior specificication
MSPrior <-  portfolioSpec()
setEstimator(MSPrior) <- "EstPrior"
## BL specification
MSBl <-  portfolioSpec()
setEstimator(MSBl) <- "EstBL"
## Constraints
BoxC <- c("minW[1:NAssets] = -0.8", "maxW[1:NAssets] = 0.8")
