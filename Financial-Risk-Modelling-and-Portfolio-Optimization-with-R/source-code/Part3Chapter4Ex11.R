## Producing one-step ahead forecasts
Forecast <- function(x, order = c(1, 1, 1), ...){
  mod <- arima(x, order = order, ...)
  f1 <- forecast(mod, h = 1)$mean
  re <- (f1 - tail(x, 1)) * 100
  re
}
RetExp <- function(x, order = c(1, 1, 1), ...){
  ans <- apply(x, 2, Forecast, order = order, ...)
  ans
}
## Computing return forecasts
RE <- fapply(PELWeekly, from = spoints, to = epoints, FUN = RetExp)
head(RE)
tail(RE)
