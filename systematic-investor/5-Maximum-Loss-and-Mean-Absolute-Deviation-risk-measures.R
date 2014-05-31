
# 通常来说  risk -> SD of return
# During construction of typical efficient frontier, 
# risk is usually measured by the standard deviation of the portfolio’s return

# ========== alternative measures of risk =================#
# Maximum Loss and Mean-Absolute Deviation

# estimate efficient frontier based on Maximun Loss can be formulated as a linear programming problem


min.maxloss.portfolio <- function
(
    ia,     # input assumptions
    constraints # constraints
)
{
	n = ia$n # number of possible instruments in portfolio
    nt = nrow(ia$hist.returns) # number of observations


	# objective : maximum loss, w
	f.obj = c( rep(0, n), 1)

	# adjust constraints, add w
    constraints = add.variables(1, constraints)


 	#  - [ SUM <over i> r.ij * x.i ] < w, for each j = 1,...,T
    a = rbind( matrix(0, n, nt), 1)
        a[1 : n, ] = t(ia$hist.returns)
    constraints = add.constraints(a, rep(0, nt), '>=', constraints)
 
    # setup linear programming 
    f.con = constraints$A
    f.dir = c(rep('=', constraints$meq), rep('>=', len(constraints$b) - constraints$meq))
    f.rhs = constraints$b
 
    # find optimal solution
    x = NA
    sol = try(solve.LP.bounds('min', f.obj, t(f.con), f.dir, f.rhs,
                            lb = constraints$lb, ub = constraints$ub), TRUE)
 
    if(!inherits(sol, 'try-error')) {
        x = sol$solution[1:n]
 
    }
 
    return( x )
 
}


# ======  Mean-absolute Deviation (MAD) =========#
# refer to blog


min.mad.portfolio <- function
(
    ia,     # input assumptions
    constraints # constraints
)
{
    n = ia$n
    nt = nrow(ia$hist.returns)
 
    # objective : Mean-Absolute Deviation (MAD)
    # 1/T * [ SUM  (u+.j + u-.j) ]
    f.obj = c( rep(0, n), (1/nt) * rep(1, 2 * nt) )
 
    # adjust constraints, add u+.j, u-.j
    constraints = add.variables(2 * nt, constraints, lb = 0)
     
    # [ SUM <over i> r.ij * x.i ] - 1/T * [ SUM <over j> [ SUM <over i> r.ij * x.i ] ] = u+.j - u-.j , for each j = 1,...,T
    a = rbind( matrix(0, n, nt), -diag(nt), diag(nt))
        a[1 : n, ] = t(ia$hist.returns) - repmat(colMeans(ia$hist.returns), 1, nt)
    constraints = add.constraints(a, rep(0, nt), '=', constraints)         
     
    # setup linear programming 
    f.con = constraints$A
    f.dir = c(rep('=', constraints$meq), rep('>=', len(constraints$b) - constraints$meq))
    f.rhs = constraints$b
 
    # find optimal solution
    x = NA
    sol = try(solve.LP.bounds('min', f.obj, t(f.con), f.dir, f.rhs,
                            lb = constraints$lb, ub = constraints$ub), TRUE)
 
    if(!inherits(sol, 'try-error')) {
        x = sol$solution[1:n]
    }
 
    return( x )
}


# =========== set data and plot risk-return plot
con = gzcon(file('sit.gz', 'rb')) #from same folder
source(con)
close(con)

library(quantmod)

# load historical prices from Yahoo Finance
symbols = spl('DJIA,SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')
symbol.names = spl('Dow Jones,S&P 500,Nasdaq 100,Emerging Markets,Russell 2000,EAFE,20 Year
Treasury,U.S. Real Estate,Gold')
getSymbols(symbols, from = '1980-01-01', auto.assign = TRUE)

# align dates for all symbols & convert to monthly
hist.prices = merge(DJIA,SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD)
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


###############################################################################
# Create Efficient Frontier
###############################################################################
n = ia$n 

# 0 <= x.i <= 0.8
constraints = new.constraints(n, lb = 0, ub = 0.8)

# SUM x.i = 1
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)

# create efficient frontier(s)
ef.risk = portopt(ia, constraints, 50, 'Risk')
ef.maxloss = portopt(ia, constraints, 50, 'Max Loss', min.maxloss.portfolio)
ef.mad = portopt(ia, constraints, 50, 'MAD', min.mad.portfolio)

# Plot multiple Efficient Frontiers
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk, ef.maxloss, ef.mad), portfolio.risk, F)
plot.ef(ia, list(ef.risk, ef.maxloss, ef.mad), portfolio.maxloss, F)
plot.ef(ia, list(ef.risk, ef.maxloss, ef.mad), portfolio.mad, F)

# Plot multiple Transition Maps
layout( matrix(1:4, nrow = 2) )
plot.transition.map(ef.risk)
plot.transition.map(ef.maxloss)
plot.transition.map(ef.mad)

# mean-absolute deviation & SD result similar frontier







