library(copula)
library(FRAPO)
library(QRM)
## Retrieving data and edf
data(DJ.df)
Data <- DJ.df[, c("GM", "UTX")]
R <- returnseries(Data, method = "discrete", trim = TRUE)
U <- apply(R, 2,edf)
## Initialising copula objects
copC <- claytonCopula(2)
copG <- gumbelCopula(2)
## Objective function
LLCG <- function(params, x, copC, copG){
  slot(copC, "parameters") <- params[1]
  slot(copG, "parameters") <- params[2]
  pi <- params[3]
  opt <- sum(log(pi * dcopula(copC, x) + (1 - pi) *
                 dcopula(copG, x)))
  opt
}
## Parameter bounds & initialisation
lower <- c(copC@param.lowbnd, copG@param.lowbnd, 0)
upper <- c(copC@param.upbnd, copG@param.upbnd, 1)
par1 <- copula:::fitCopula.itau(copC, U)@estimate
par2 <- copula:::fitCopula.itau(copG, U)@estimate
par3 <- 0.5
## Optimisation
opt <- optim(c(par1, par2, par3), LLCG, x = U, copC = copC,
             copG = copG, lower = lower, upper = upper,
             method = "L-BFGS-B",
             control = list(fnscale = -1, trace = 2),
             hessian = TRUE)
## Variance-Covariance 
varcov <- round(solve(-opt$hessian), 4)
