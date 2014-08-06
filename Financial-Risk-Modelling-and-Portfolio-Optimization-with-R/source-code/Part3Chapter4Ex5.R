## Extracting weights based on prior
WPrior <- matrix(unlist(lapply(Backtest, function(x) x[1, ])),
                 ncol = NAssets, byrow = TRUE)
WPrior <- zoo(WPrior, order.by = idx)
WPriorL1 <- lag(WPrior, k = -1, na.pad = TRUE)
## Extracting weights based on BL
WBl <- matrix(unlist(lapply(Backtest, function(x) x[2, ])),
                 ncol = NAssets, byrow = TRUE)
WBl <- zoo(WBl, order.by = idx)
WBlL1 <- lag(WBl, k = -1, na.pad = TRUE)
## Compute portfolio equities
Rsub <- R[time(WBlL1), ] / 100
RetFacPrior <- rowSums(Rsub * WPriorL1) + 1
RetFacPrior[1] <- 100
RetFacBl <- rowSums(Rsub * WBlL1) + 1
RetFacBl[1] <- 100
EquityPrior <- zoo(cumprod(RetFacPrior), index(Rsub))
EquityBL <- zoo(cumprod(RetFacBl), index(Rsub))
## Equal-weight strategy
EW <- matrix(1 / NAssets, ncol = NAssets, nrow = nrow(WBlL1))
RetFacEw <- rowSums(Rsub * EW) + 1
RetFacEw[1] <- 100
EquityEw <- zoo(cumprod(RetFacEw), index(Rsub))
