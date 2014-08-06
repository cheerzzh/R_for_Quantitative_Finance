## Recursive back-test
## Initialising variables
LO <- timeSeries(rep(NA, size), charvec = epoints)
LO[1] <- 100
PLevel <- 0.9
FLO <- LO[1, ] * PLevel
Returns <- REDWeekly[epoints, ]
MoneyRate <- 1.01^(1/52) - 1
## Simulation
for(i in 2:size){
  BLO <- c(PLevel * LO[i - 1, ])
  if(BLO < FLO){
    LO[i, ] <- LO[i - 1, ] * (1 + MoneyRate)
  } else {
    re <- c(RE[i -1, ])
    if(all(re <= 0)){
      LO[i, ] <- LO[i - 1, ] * (1 + MoneyRate)
    } else {
      es <- c(ES[i -1, ])
      r <- c(Returns[i, ])
      B <- c(LO[i - 1, ]) / c(FLO) - 1
      ans <- Lp(RE = re, ES = es, Buffer = B, ub = 0.4)
      w <- ans$solution
      LO[i, ] <- LO[i - 1, ] * (1 + t(w) %*% c(r))
    }
  }
}
