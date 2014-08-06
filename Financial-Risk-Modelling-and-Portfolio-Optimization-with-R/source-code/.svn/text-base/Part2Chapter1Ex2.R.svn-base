## Probabilities
p <- seq(0.001, 0.05, 0.001)
## VaR
ghd.VaR <- abs(qghyp(p, ghdfit))
hyp.VaR <- abs(qghyp(p, hypfit))
nig.VaR <- abs(qghyp(p, nigfit))
nor.VaR <- abs(qnorm(p, mean = mean(yret), sd = sd(c(yret[, 1])))) 
emp.VaR <- abs(quantile(x = yret, probs = p)) 
# Plot of VaR
plot(emp.VaR, type = "l", xlab = "", ylab = "VaR", axes = FALSE,
     ylim = range(c(hyp.VaR, nig.VaR, ghd.VaR, nor.VaR, emp.VaR)))
box()
axis(1, at = seq(along = p), labels = names(emp.VaR), tick = FALSE)
axis(2, at = pretty(range(emp.VaR, ghd.VaR, hyp.VaR,
                    nig.VaR, nor.VaR)))
lines(seq(along = p), ghd.VaR, col = "red")
lines(seq(along = p), hyp.VaR, col = "blue")
lines(seq(along = p), nig.VaR, col = "green")
lines(seq(along = p), nor.VaR, col = "orange")
legend("topright",
       legend = c("Empirical", "GHD", "HYP", "NIG", "Normal"),
       col = col.def, lty = 1)
## ES
ghd.ES <- abs(ESghyp(p, ghdfit))
hyp.ES <- abs(ESghyp(p, hypfit))
nig.ES <- abs(ESghyp(p, nigfit))
nor.ES <- abs(mean(yret) - sd(c(yret[, 1])) *
              dnorm(qnorm(1 - p)) / p)
obs.p <- ceiling(p * length(yret))
emp.ES <- sapply(obs.p, function(x) abs(mean(sort(c(yret))[1:x])))
## Plot of ES
plot(emp.ES, type = "l", xlab = "", ylab = "ES", axes = FALSE,
     ylim = range(c(hyp.ES, nig.ES, ghd.ES, nor.ES, emp.ES)))
box()
axis(1, at = 1:length(p), labels = names(emp.VaR), tick = FALSE)
axis(2, at = pretty(range(emp.ES, ghd.ES, hyp.ES, nig.ES, nor.ES)))
lines(1:length(p), ghd.ES, col = "red")
lines(1:length(p), hyp.ES, col = "blue")
lines(1:length(p), nig.ES, col = "green")
lines(1:length(p), nor.ES, col = "orange")
legend("topright",
       legend = c("Empirical", "GHD", "HYP", "NIG", "Normal"),
       col = col.def, lty = 1)
