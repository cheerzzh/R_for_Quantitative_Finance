rd <- c(1, 5, 10, 20, 40)
yrets <- na.omit(matrix(unlist(lapply(rd,
                 function(x) diff(log(y), lag = x))), ncol = 5))
## Function for xi/chi coefficients
xichi <- function(x){
  param <- coef(x, type = "alpha.delta")
  rho <- param[["beta"]] / param[["alpha"]]
  zeta <- param[["delta"]] * sqrt(param[["alpha"]]^2 -
                                  param[["beta"]]^2)
  xi <- 1 / sqrt(1 + zeta)
  chi <- xi * rho
  result <- c(chi, xi)
  names(result) <- c("chi", "xi")
  return(result)
}
## HYP Fitting
hypfits <- apply(yrets, 2, fit.hypuv, symmetric = FALSE)
points <- matrix(unlist(lapply(hypfits, xichi)),
                 ncol = 2, byrow = TRUE)
## Shape triangle
col.def <- c("black", "blue", "red", "green", "orange")
leg.def <- paste(rd, rep("day return", 5))
plot(points, ylim = c(-0.2, 1.2), xlim = c(-1.2, 1.2),
     col = col.def, pch = 16, ylab = expression(xi),
     xlab = expression(chi))
lines(x = c(0, -1), y = c(0, 1))
lines(x = c(0, 1), y = c(0, 1))
lines(x = c(-1, 1), y = c(1, 1))
legend("bottomright", legend = leg.def, col = col.def, pch = 16)
text(x = 0.0, y = 1.05, label = "Laplace", srt = 0)
text(x = -1.0, y = 1.05, label = "Exponential", srt = 0)
text(x = 1.0, y = 1.05, label = "Exponential", srt = 0)
text(x = 0.0, y = -0.1, label = "Normal", srt = 0)
text(x = -0.6, y = 0.5, label = "Hyperbolic, left skewed",
     srt = 302)
text(x = 0.6, y = 0.5, label = "Hyperbolic, right skewed",
     srt = 57)
