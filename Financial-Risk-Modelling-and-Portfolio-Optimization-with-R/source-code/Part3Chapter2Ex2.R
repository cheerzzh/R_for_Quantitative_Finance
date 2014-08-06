library(PerformanceAnalytics)
## Portfolio Risk Measures and Characteristics
Rdec <- R / 100
Pret <- apply(W, 2, function(x) Rdec %*% x / 100)
SD <- apply(Pret, 2, sd) * 100
ES95 <- apply(Pret, 2, function(x)
              abs(ES(R = x, method = "modified") * 100))
DR <- apply(W, 2, dr, Sigma = V)
CR <- apply(W, 2, cr, Sigma = V)
## Summarising results
Res <- rbind(SD, ES95, DR, CR)
