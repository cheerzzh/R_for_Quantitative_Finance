PMV <- function(SRoot, mu, SigMax, lambda, ...){
  Nassets <- length(mu)
  ra <- 1 - lambda
  f <- -mu 
  C1 <- matrix(rep(0, Nassets), nrow = 1)
  ## no cone, but quadratic constraint
  C2 <- diag(Nassets)
  ## non-negativity constraint
  C3 <- matrix(rep(-1, Nassets), nrow = 1)
  ## budget constraint
  C <- rbind(C1,
           C2,
           C3)
  ZeroM <- matrix(0, nrow = Nassets + 1, ncol = Nassets)
  A <- rbind(SRoot,
             ZeroM)
  b <- rep(0, nrow(A))
  d <- c(SigMax * ra, rep(0, Nassets), 1)
  ans <- Socp(f = f, A = A, b = b, C = C, d = d,
              N = c(nrow(SRoot), rep(1, nrow(ZeroM))), ...)
  return(ans)
}
