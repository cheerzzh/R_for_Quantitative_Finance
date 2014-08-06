## Index of time stamps in back-test (extending window)
idx <- index(AssetsM)[-c(1:60)]
ANames <- colnames(AssetsM)
NAssets <- ncol(AssetsM)
## Function for return expectations
f1 <- function(x, ci, percent = TRUE){
  data <- window(AssetsM, start = start(AssetsM), end = x)
  Lobs <- t(tail(data, 1))
  vec <- ca.jo(data, ecdet = "none", spec = "transitory")
  m <- vec2var(vec, r = 1)
  fcst <- predict(m, n.ahead = 1, ci = ci)
  LU <- matrix(unlist(fcst$fcst),
               ncol = 4, byrow = TRUE)[, c(2, 3)]
  RE <- rep(0, NAssets)
  PView <- LU[, 1] > Lobs
  NView <- LU[, 2] < Lobs
  RE[PView] <- (LU[PView, 1] / Lobs[PView, 1] - 1)
  RE[NView] <- (LU[NView, 1] / Lobs[NView, 1] - 1)
  names(RE) <- ANames
  if(percent) RE <- RE * 100
  return(RE)               
}
ReturnEst <- lapply(idx, f1, ci = 0.5)
qv <- zoo(matrix(unlist(ReturnEst),
                 ncol = NAssets, byrow = TRUE), idx)
colnames(qv) <- ANames
tail(qv)
