## Function for Moment Estimation
Moments <- function(x, method = c("CovClassic", "CovMcd",
                    "CovMest", "CovMMest", "CovMve", "CovOgk",
                    "CovSde", "CovSest"), ...){
  method <- match.arg(method)
  ans <- do.call(method, list(x = x, ...))
  return(getCov(ans))
}
