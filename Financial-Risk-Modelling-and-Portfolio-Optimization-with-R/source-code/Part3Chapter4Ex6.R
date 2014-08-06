## Graphical display of back-test result
par(mfrow = c(2, 1))
## Plotting of equity curves
plot(EquityBL, main = "Wealth Progression",
     ylab = "Value", xlab = "")
lines(EquityPrior, col = "darkgrey", lty = 2)
lines(EquityEw, col = "grey", lty = 3)
legend("topleft",
       legend = c("Black-Litterman", "Prior", "Equal-Weight"),
       col = c("black", "darkgrey", "grey"), lty = 1:3)
## Relative performance
RelOut <- cbind((EquityBL / EquityPrior - 1) * 100, 
                (EquityBL / EquityEw - 1) * 100)
barplot(RelOut, xlab = "", ylab = "Percentages", 
        main = "Relative Performance",
        ylim = c(0, 20), beside = TRUE,
        legend.text = c("Against Prior", "Against EW"),
        args.legend = list(x = "topleft")) 
box()
## Boxplots of weights
par(mfrow = c(2, 1))
boxPR <- coredata(WPriorL1) 
colnames(boxPR) <- ANames
boxplot(boxPR, ylim = c(-0.8, 0.8),
        main = "Based on Prior Distribution")
abline(h = 0, col = "grey")
boxBL <- coredata(WBlL1) 
colnames(boxBL) <- ANames
boxplot(boxBL, ylim = c(-0.8, 0.8),
        main = "Based on Posterior Distribution")
abline(h = 0, col = "grey")
