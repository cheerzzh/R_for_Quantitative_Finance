## Setting of parameters
Nassets <- ncol(rzoo)
Nobs <- nrow(rzoo)
S <- cov(rzoo)
SR <- sqrm(S)
P <- S / Nobs
PR <- sqrm(P)
SigMax <- max(sqrt(diag(S)))
mu <- colMeans(rzoo)
delta <- sqrt(qchisq(0.9, Nassets))
lambda <- seq(0.3, 0.9, by = 0.1)
## Determining efficient frontier for MV-portfolio
PmvAns <- matrix(NA, nrow = length(lambda), ncol = Nassets + 2)
for(i in 1:length(lambda)){
  obj <- PMV(SRoot = SR, mu = mu, SigMax = SigMax,
             lambda = lambda[i])
  w <- obj$x
  PmvAns[i, ] <- c(sqrt(t(w) %*% S %*% w), t(mu) %*% w, w)
}
## Determining maximum return portfolio
PmrAns <- c(sd(rzoo[, "HSI"]), mean(rzoo[, "HSI"]))
## Determining robust portfolio
RobAns <- matrix(NA, nrow = length(lambda), ncol = Nassets + 2)
for(i in 1:length(lambda)){
  obj <- PR1(SRoot = SR, PRoot = PR, mu = mu, SigMax = SigMax,
             lambda = lambda[i], delta = delta)
  w <- obj$x[1:ncol(SR)]
  RobAns[i, ] <- c(sqrt(t(w) %*% S %*% w), t(mu) %*% w, w)
}
## Equivalent parameter for theta = 0.3
theta2lambda <- function(theta, Nassets, Nobs, level){
  delta <- sqrt(qchisq(level, Nassets))
  lambda <- theta / (1 + theta * (delta / sqrt(Nobs)))
  return(lambda)
}
lstar <- theta2lambda(0.3, Nassets = Nassets, Nobs = Nobs, level = 0.9)
wstar <- PMV(SRoot = SR, mu = mu, SigMax = SigMax, lambda = lstar)$x
mveq <- c(sqrt(t(wstar) %*% S %*% wstar), t(mu) %*% wstar)
