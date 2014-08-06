library(AER)
library(fGarch)
data(NYSESW)
NYSELOSS <- timeSeries(-1.0 * diff(log(NYSESW)) * 100,
                       char.vec = time(NYSESW))
## Function for ES of t-GARCH
ESgarch <- function(y, p = 0.99){
  gfit <- garchFit(formula = ~garch(1, 1), data = y,
                   cond.dist = "std", trace = FALSE)
  sigma <-  predict(gfit, n.ahead = 1)[3]
  df <- coef(gfit)["shape"]
  ES <- sigma * (dt(qt(p, df), df)/(1 - p)) *
        ((df + (qt(p, df))^2)/(df - 1))
  return(ES)
}
## Date vectors for backtest
from <- time(NYSELOSS)[-c((nrow(NYSELOSS) - 999) : nrow(NYSELOSS))]
to <- time(NYSELOSS)[-c(1:1000)]
NYSEES <- fapply(NYSELOSS, from = from, to = to, FUN = ESgarch)
NYSEESL1 <- lag(NYSEES, k = 1)
res <- na.omit(cbind(NYSELOSS, NYSEESL1))
colnames(res) <- c("NYSELOSS", "ES99")
plot(res[, 2], col = "red", ylim = range(res),
     main = "NYSE: t-GARCH(1,1) ES 99%",
     ylab = "percentages", xlab = "")
points(res[, 1], type = "p", cex = 0.2, pch = 19, col = "blue")
legend("topleft", legend = c("Loss", "ES"),
       col = c("blue", "red"), lty = c(NA, 1), pch = c(19, NA))
