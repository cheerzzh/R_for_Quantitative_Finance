
# topic to be covered:

#   * Maximum Loss, MAD, CVaR, CDaR, Omega Risk Measures
#   * 130:30 Long/Short portfolios and Cardinality Constraints
#   * Arithmetic and Geometric Efficient Frontiers

# how to create and visualize input assumptions, 
# set constraints, and create Markowitz mean-variance efficient frontier.

con = gzcon(file('sit.gz', 'rb')) #from same folder
source(con)
close(con)

library(quantmod)

# load historical prices from Yahoo Finance
symbols = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')
symbol.names = spl('S&P 500,Nasdaq 100,Emerging Markets,Russell 2000,EAFE,20 Year
Treasury,U.S. Real Estate,Gold')
getSymbols(symbols, from = '1980-01-01', auto.assign = TRUE)

# align dates for all symbols & convert to monthly
hist.prices = merge(SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD)
    month.ends = endpoints(hist.prices, 'months')
    hist.prices = Cl(hist.prices)[month.ends, ]
    colnames(hist.prices) = symbols
 

# remove any missing data
hist.prices = na.omit(hist.prices['1995::2010'])

# compute simple returns
hist.returns = na.omit( ROC(hist.prices, type = 'discrete') )

# compute mean, standard deviation, and Pearson correlation using historical monthly returns:

# compute historical returns, risk, and correlation
ia = list()
ia$expected.return = apply(hist.returns, 2, mean, na.rm = T)
ia$risk = apply(hist.returns, 2, sd, na.rm = T)
ia$correlation = cor(hist.returns, use = 'complete.obs', method = 'pearson')

ia$symbols = symbols
ia$symbol.names = symbol.names
ia$n = len(symbols)
ia$hist.returns = hist.returns

# convert to annual, year = 12 months
annual.factor = 12 #return*12, sd* sqr(12)
ia$expected.return = annual.factor * ia$expected.return
ia$risk = sqrt(annual.factor) * ia$risk

# compute covariance matrix
ia$risk = iif(ia$risk == 0, 0.000001, ia$risk)
ia$cov = ia$cor * (ia$risk %*% t(ia$risk)) #recover covariance matrix

# visualize input assumptions:
plot.ia(ia)
# display each asset in the Risk - Return plot
layout(1)
par(mar = c(4,4,2,1), cex = 0.8)
x = 100 * ia$risk
y = 100 * ia$expected.return
 
plot(x, y, xlim = range(c(0, x)), ylim = range(c(0, y)),
    xlab='Risk', ylab='Return', main='Risk vs Return', col='black')
grid();
text(x, y, symbols, col = 'blue', adj = c(1,1), cex = 0.8)

#
# problems with these input assumptions, to name a few:

#   * historical mean might not be a good proxy for expected returns
#   * weighted historical mean maybe a better choice because it puts more weight on the recent observations
#   * correlations are not stable
#   * volatility tends to cluster
#   * input assumptions for cash and bonds are better approximated by current yields and short-term variations

# create efficient frontier, 
#  portfolios with allocations to any asset class ranging between 0% and 80% and total portfolio weight equal to 100%


# Create Efficient Frontier
n = ia$n
 
# 0 <= x.i <= 0.8
# lower bound: 0%, upper bound: 80%
constraints = new.constraints(n, lb = 0, ub = 0.8)
 
# SUM x.i = 1 ( total portfolio weight = 100%)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
 
# create efficient frontier consisting of 50 portfolios
ef = portopt(ia, constraints, 100, 'Sample Efficient Frontier')
 
# plot efficient frontier
plot.ef(ia, list(ef)) 
# plot the effcient frontier on the risk-return plot
# as well as the transtition map for efficient frontier
# Transition Map displays portfolio weights as we move along the efficient frontier


#logic of “portopt” function that creates efficient frontier for us. 
#The first step to create efficient frontier is to find the top,right (maximum return portfolio) and bottom,left (minimum risk portfolio).
# Next, I divide the return space between minimum risk portfolio and maximum return portfolio into nportfolios equally spaced points. 
#For each point, I find minimum risk portfolio with additional constraint that portfolio return has to be equal target return for this point. 
#The last step is to compute returns and risks for portfolio on efficient frontier.




# function to compute efficient frontier
portopt <- function
(
    ia,             # Input Assumptions
    constraints = NULL,     # Constraints
    nportfolios = 50,       # Number of portfolios
    name = 'Risk',          # Name
    min.risk.fn = min.risk.portfolio    # Risk Measure
)
{
    # set up output 
    out = list(weight = matrix(NA, nportfolios, ia$n))
        colnames(out$weight) = ia$symbols       
     
    # find maximum return portfolio 
    out$weight[1, ] = max.return.portfolio(ia, constraints)
 
    # find minimum risk portfolio
    out$weight[nportfolios, ] = match.fun(min.risk.fn)(ia, constraints) 
 
    # find points on efficient frontier
    out$return = portfolio.return(out$weight, ia)
    target = seq(out$return[1], out$return[nportfolios], length.out = nportfolios)
 
    constraints = add.constraints(ia$expected.return, target[1], type = '=', constraints)
             
    for(i in 2:(nportfolios - 1) ) {
        constraints$b[1] = target[i]
        out$weight[i, ] = match.fun(min.risk.fn)(ia, constraints)
    }
     
    # compute risk / return
    out$return = portfolio.return(out$weight, ia)
    out$risk = portfolio.risk(out$weight, ia)
    out$name = name
     
    return(out)         
}















