## Plotting of densities
CopPriorSim <- sampleFrom(CopPost@marketDist, NumSim)
CopPostSim <- CopPost@posteriorSims
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
for(i in 1:NAssets){
  plot(density(CopPostSim[, i]), main = ANames[i],
       ylab = "density", ylim = c(0, 0.12), xlab = "")
  lines(density(CopPriorSim[, i]), col = "grey", lty = 2)
  abline(v = mean(CopPostSim[, i]))
  abline(v = mean(CopPriorSim[, i]), col = "grey", lty = 2)
  legend("topleft", legend = c("Posterior", "Prior"), 
        lty = c(1, 2), col = c("black", "grey"), bty = "n") 
}
par(oldpar)
