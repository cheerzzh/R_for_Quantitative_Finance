PR2 <- function(SRoot, PRoot, mu, SigMax, lambda, delta, ...){
  Nassets <- nrow(SRoot)
  Nvar <- Nassets + 1
  ra <- 1- lambda
  f <- c(rep(0, Nassets), -1)
  C1 <- matrix(c(mu, -1) / delta * lambda, nrow = 1) ## cone constraint for \hat{\mu}
  C2 <- matrix(rep(0, Nvar), nrow = 1) ## quadratic constraint for risk
  C3 <- cbind(diag(Nassets), rep(0, Nassets)) ## ## non-negativity constraint
  C4 <- matrix(c(rep(-1, Nassets), 0), nrow = 1) ## budget constraint
  C <- rbind(C1,
             C2,
             C3,
             C4)
  ZeroM <- matrix(0, nrow = Nassets + 1, ncol = Nvar)
  A1 <- cbind(PRoot, rep(0, Nassets))
  A1 <- rbind(A1,
              c(rep(0, Nassets), 0))
  A2 <- cbind(SRoot, rep(0, Nassets))
  A2 <- rbind(A2,
              c(rep(0, Nvar)))
  A <- rbind(A1,
             A2,
             ZeroM)
  b <- rep(0, nrow(A))
  d <- c(0, SigMax * ra, rep(0, Nassets), 1)
  ans <- Socp(f = f, A = A, b = b, C = C, d = d, N = c(nrow(A1), nrow(A2), rep(1, nrow(ZeroM))), ...)
  return(ans)
}
