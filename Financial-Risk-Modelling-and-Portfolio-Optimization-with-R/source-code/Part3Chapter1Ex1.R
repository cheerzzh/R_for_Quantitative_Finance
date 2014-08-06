## Loading of packages
library(copula)
library(quadprog)
library(rrcov)
## Creating copula objects
ncop <- normalCopula(param = 0.5, dim = 5)
tcop <- tCopula(param = 0.5, dim = 5, df = 5, df.fixed = TRUE)
## Creating DGPs
NcopMargN <- mvdc(ncop, margins = "norm",
                  paramMargins = list(list(mean = 0, sd = 1)),
                  marginsIdentical = TRUE)
NcopMargT <- mvdc(ncop, margins = "t",
                  paramMargins = list(df = 5),
                  marginsIdentical = TRUE)
TcopMargT <- mvdc(tcop, margins = "t",
                  paramMargins = list(df = 5),
                  marginsIdentical = TRUE)
## Initialising list objects for DGP
Lobj <- list()
length(Lobj) <- 1000
## Setting a seed
set.seed(12345)
## Generating random samples
rNcopMargN <- lapply(Lobj, function(x) rmvdc(NcopMargN, 240))
rNcopMargT <- lapply(Lobj, function(x) rmvdc(NcopMargT, 240))
rTcopMargT <- lapply(Lobj, function(x) rmvdc(TcopMargT, 240))
