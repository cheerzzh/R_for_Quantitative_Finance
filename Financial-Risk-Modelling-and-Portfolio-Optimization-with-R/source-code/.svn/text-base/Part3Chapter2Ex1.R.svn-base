library(FRAPO)
library(fPortfolio)
library(lattice)
## Loading data and calculating returns
data(SPISECTOR)
Idx <- interpNA(SPISECTOR[, -1], method = "before")
R <- returnseries(Idx, method = "discrete", trim = TRUE)
V <- cov(R)
## Portfolio Optimizations
GMVw <- Weights(PGMV(R))
MDPw <- Weights(PMD(R))
MTDw <- Weights(PMTD(R))
ERCw <- Weights(PERC(V))
## Combining solutions
W <- cbind(GMVw, MDPw, MTDw, ERCw)
MRC <- apply(W, 2, mrc, Sigma = V)
rownames(MRC) <- colnames(Idx)
colnames(MRC) <- c("GMV", "MDP", "MTD", "ERC")
## Plot of allocations
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
dotchart(GMVw, xlim = c(0, 40), main = "GMV Allocation", pch = 19)
dotchart(MDPw - GMVw, xlim = c(-20, 20), main = "MDP vs. GMV",
         pch = 19)
abline(v = 0, col = "grey")
dotchart(MTDw - GMVw, xlim = c(-20, 20), main = "MTD vs. GMV",
         pch = 19)
abline(v = 0, col = "grey")
dotchart(ERCw - GMVw, xlim = c(-20, 20), main = "ERC vs. GMV",
         pch = 19)
abline(v = 0, col = "grey")
par(oldpar)
## lattice plots of MRC
Sector <- factor(rep(rownames(MRC), 4),
                 levels = sort(rownames(MRC)))
Port <- factor(rep(colnames(MRC), each = 9),
               levels = colnames(MRC))
MRCdf <- data.frame(MRC = c(MRC), Port, Sector)
dotplot(Sector ~ MRC | Port, groups = Port, data = MRCdf,
        xlab = "Percentages",
        main = "MR Contributions by Sector per Portfolio",
        col = "black", pch = 19)
dotplot(Port ~ MRC | Sector, groups = Sector, data = MRCdf,
        xlab = "Percentages",
        main = "MR Contributions by Portfolio per Sector",
        col = "black", pch = 19)
