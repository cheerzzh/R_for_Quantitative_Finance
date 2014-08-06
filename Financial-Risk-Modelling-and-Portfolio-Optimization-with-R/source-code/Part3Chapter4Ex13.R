## Creating LP
Lp <- function(RE, ES, Buffer, ub = 0.4){
  obj <- as.vector(RE)
  ## Initialise LHS matrix and RHS vector
  nvar <- length(obj)
  ## Wealth constraint
  a1 <- rep(1, nvar) 
  b1 <- 1
  d1 <- "<="
  ## Risk constraint
  a2 <- as.vector(ES)
  b2 <- Buffer
  d2 <- "<="
  ## Upper bound
  a3 <- diag(nvar)
  b3 <- rep(ub, nvar)
  d3 <- rep("<=", nvar)
  ## Combining
  A <- rbind(a1, a2, a3)
  b <- c(b1, b2, b3)
  d <- c(d1, d2, d3)
  ans <- Rglpk_solve_LP(obj, mat = A, dir = d, rhs = b, max = TRUE)
  ans
}
