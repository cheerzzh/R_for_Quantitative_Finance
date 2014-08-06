library(FRAPO)
library(PortfolioAnalytics)
## Loading data and computing returns
data(MultiAsset)
R <- returnseries(MultiAsset, percentage = FALSE, trim = TRUE)
N <- ncol(R)
## Defining constraints and objective for CVaR budget
C1 <-  constraint(assets = colnames(R), min = rep(0, N),
                  max = rep(1, N), min_sum = 1, max_sum = 1)
ObjCVaR <- add.objective(constraints = C1, type = "risk",
                         name = "ES", arguments = list(p = 0.95),
                         enabled = TRUE)
ObjCVaRBudget <- add.objective(constraints = ObjCVaR,
                               type = "risk_budget",
                               name = "ES", max_prisk = 0.2,
                               arguments = list(p = 0.95),
                               enabled = TRUE)
SolCVaRBudget <- optimize.portfolio(R = R,
                                    constraints = ObjCVaRBudget,
                                    optimize_method = "DEoptim",
                                    itermax = 50,
                                    search_size = 20000,
                                    trace = TRUE)
WCVaRBudget <- SolCVaRBudget$weights
CVaRBudget <- ES(R, weights = WCVaRBudget, p = 0.95,
                 portfolio_method = "component")
## Minimum CVaR concentration portfolio
ObjCVaRMinCon <- add.objective(constraints = ObjCVaR,
                               type = "risk_budget",
                               name = "ES",
                               min_concentration= TRUE,
                               arguments = list(p = 0.95),
                               enabled = TRUE)
SolCVaRMinCon <- optimize.portfolio(R = R,
                                    constraints = ObjCVaRMinCon,
                                    optimize_method = "DEoptim",
                                    itermax = 50,
                                    search_size = 20000,
                                    trace = TRUE)
WCVaRMinCon <- SolCVaRMinCon$weights
CVaRMinCon <- ES(R, weights = WCVaRMinCon, p = 0.95,
                 portfolio_method = "component")
