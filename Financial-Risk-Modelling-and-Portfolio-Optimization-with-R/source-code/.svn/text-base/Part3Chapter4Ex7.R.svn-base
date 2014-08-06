## Prior distribution
## Fitting of skewed Student's t distribution 
MSTfit <- mvFit(R, method = "st")
mu <- c(MSTfit@fit[["beta"]])
S <- MSTfit@fit[["Omega"]]
skew <- c(MSTfit@fit[["alpha"]])
df <- MSTfit@fit[["df"]]
CopPrior <- mvdistribution("mvst", dim = NAssets, mu = mu,
                           Omega = S, alpha = skew, df = df)
## Pick matrix and view distributions for last forecast
RetEstCop <- ReturnEst[[27]]
RetEstCop
PCop <- matrix(0, ncol = NAssets, nrow = 3)
colnames(PCop) <- ANames
PCop[1, ANames[1]] <- 1
PCop[2, ANames[2]] <- 1
PCop[3, ANames[4]] <- 1
Sds <- apply(R, 2, sd)
RetViews <- list(distribution("norm", mean = RetEstCop[1],
                              sd = Sds[1]),
                 distribution("norm", mean = RetEstCop[2],
                              sd = Sds[2]),
                 distribution("norm", mean = RetEstCop[4],
                              sd = Sds[4])
                 )
CopViews <- COPViews(pick = PCop, viewDist = RetViews,
                     confidences = rep(0.5, 3),
                     assetNames = ANames)
## Simulation of posterior
NumSim <- 10000
CopPost <- COPPosterior(CopPrior, CopViews,
                        numSimulations = NumSim)
slotNames(CopPost)
