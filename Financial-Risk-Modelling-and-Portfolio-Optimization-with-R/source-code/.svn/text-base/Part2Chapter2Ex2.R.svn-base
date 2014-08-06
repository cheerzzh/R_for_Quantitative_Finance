## Loading of packages
library(evir)
library(ismev)
## Order statistics
data(bmw)
BmwLoss <- -1.0 * bmw * 100 
Years <- format(attr(BmwLoss, "time"), "%Y")
attr(BmwLoss, "years") <- Years
Yearu <- unique(Years)
idx <- 1:length(Yearu)
r <- 2
BmwOrder <- t(sapply(idx, function(x)
              head(sort(BmwLoss[attr(BmwLoss, "years") ==
                                Yearu[x]], decreasing = TRUE), r)))
rownames(BmwOrder) <- Yearu
colnames(BmwOrder) <- paste("r", 1:r, sep = "")
## Plot of order data
plot(Yearu, BmwOrder[, 1], col = "black", ylim = range(BmwOrder),
     ylab = "Losses BMW (percentages)", xlab = "",
     pch = 21, bg = "black")
points(Yearu, BmwOrder[, 2], col = "grey", pch = 23, bg = "grey")
## Fit and diagnostics
BmwOrderFit <- rlarg.fit(BmwOrder)
rlarg.diag(BmwOrderFit)
