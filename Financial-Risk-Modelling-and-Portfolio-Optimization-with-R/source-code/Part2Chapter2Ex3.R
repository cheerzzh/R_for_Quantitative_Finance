## Loading of packages
library(fEcofin)
library(fExtremes)
## Data handling
data(DowJones30)
DJ <- timeSeries(DowJones30[, -1],
                 charvec = as.character(DowJones30[, 1]))
BALoss <- -1.0 * returns(DJ[, "BA"], percentage = TRUE,
                         trim = TRUE)
## MRL-plot
mrlPlot(BALoss, umin = -10, umax = 10)
## GPD
BAFit <- gpdFit(BALoss, u = 3)
## Diagnostic plots
plot(BAFit)
## Risk measures
gpdRiskMeasures(BAFit, prob = c(0.95, 0.99, 0.995))

