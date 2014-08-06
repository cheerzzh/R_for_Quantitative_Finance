## Efficient Frontier
EF <- rbind(PmrAns,
            PmvAns[, c(1, 2)])
plot(EF, type = "l", xlim = c(0, 8), ylim = c(0, 1.5),
     ylab = "Portfolio Return", xlab = "Portfolio Risk", lwd = 2)
points(RobAns[, c(1,2)], col = "blue", pch = 19, cex = 1.2)
points(mveq[1], mveq[2], col = "red", pch = 19, cex = 1.2)
legend("topleft", legend = c("Efficient Frontier",
       "Robust Portfolio Solutions", "Equivalent MV-Portfolio"),
       lty = c(1, NA, NA), pch = c(NA, 19, 19),
       col = c("black", "blue", "red"), lwd = 2)
