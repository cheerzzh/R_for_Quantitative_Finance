library(urca)
library(vars)
## Loading data set and converting to zoo
data(EuStockMarkets)
Assets <- as.zoo(EuStockMarkets)
## Aggregating as month's-end series
AssetsM <- aggregate(Assets, as.yearmon, tail, 1)
head(AssetsM)
## Applying unit root tests for sub-sample
AssetsMsub <- window(AssetsM, start = start(AssetsM),
                     end = "Jun 1996")
## Levels
ADF <- lapply(AssetsMsub, ur.df, type = "drift",
              selectlags = "AIC")
ERS <- lapply(AssetsMsub, ur.ers)
## Differences
DADF <- lapply(diff(AssetsMsub), ur.df, selectlags = "AIC")
DERS <- lapply(diff(AssetsMsub), ur.ers)
## VECM
VEC <- ca.jo(AssetsMsub, ecdet = "none", spec = "transitory")
summary(VEC)
