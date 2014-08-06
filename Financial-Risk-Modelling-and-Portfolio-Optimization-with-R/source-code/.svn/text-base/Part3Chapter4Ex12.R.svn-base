## Computing market risk measures
## Long/Short risk 
RiskFac <- -1.0 * sign(RE)
ES <- matrix(0, ncol = NAssets, nrow = size)
GpdEs <- function(x, nextremes = 30, method = "pwm", level = 0.95,
                  RiskFac){
  x <- RiskFac * x
  mod <- gpd(data = x, nextremes = nextremes, method = method)
  GpdEs <- riskmeasures(mod, level)[3]
  GpdEs
}
for(i in idx){
    DatSub <- window(REDWeekly, start = spoints[i],
                     end = epoints[i])
    FacSub <- window(RiskFac, start = epoints[i],
                     end = epoints[i])
    for(j in 1:6){
    x <- na.omit(DatSub[, j])
    ES[i, j] <- GpdEs(x, RiskFac = c(FacSub[, j]))
  }
}
ES <- timeSeries(ES, charvec = epoints)
colnames(ES) <- colnames(REDWeekly)
head(ES)
tail(ES)
