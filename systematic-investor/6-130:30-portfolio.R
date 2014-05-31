
# 130/30 fund
# for each 100 dollar
# long $130
# short $30

# =========== set data and plot risk-return plot
con = gzcon(file('sit.gz', 'rb')) #from same folder
source(con)
close(con)

#--------------------------------------------------------------------------
# Create Efficient Frontier
#--------------------------------------------------------------------------
ia = aa.test.create.ia()
n = ia$n
 
# -0.5 <= x.i <= 0.8
constraints = new.constraints(n, lb = -0.5, ub = 0.8)
 
# SUM x.i = 1
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
 
# create efficient frontier(s)
ef.risk = portopt(ia, constraints, 50, 'Risk')
ef.mad = portopt(ia, constraints, 50, 'MAD', min.mad.portfolio)
 
# Plot multiple Efficient Frontiers & Transition Maps
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)
plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)
 
plot.transition.map(ef.risk)
plot.transition.map(ef.mad)

# Looking at the Transition Maps, the use of leverage increases as the portfolio’s risk and return increase
# want all portfolios on the efficient frontier to have 130% allocation to longs and 30% allocation to shorts


# ===============   method 1 ================#
#--------------------------------------------------------------------------
# Create 130:30
# -v.i <= x.i <= v.i, v.i>0, SUM(v.i) = 1.6
#-------------------------------------------------------------------------

# -0.5 <= x.i <= 0.8
constraints = new.constraints(n, lb = -0.5, ub = 0.8)
     
# SUM x.i = 1
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)       
 
# adjust prior constraints, add v.i
constraints = add.variables(n, constraints)

# -v.i <= x.i <= v.i
#   x.i + v.i >= 0
constraints = add.constraints(rbind(diag(n), diag(n)), rep(0, n), type = '>=', constraints)
 
#   x.i - v.i <= 0
constraints = add.constraints(rbind(diag(n), -diag(n)), rep(0, n), type = '<=', constraints)
     
# SUM(v.i) = 1.6
constraints = add.constraints(c(rep(0, n), rep(1, n)), 1.6, type = '=', constraints)
 
# create efficient frontier(s)
ef.risk = portopt(ia, constraints, 50, 'Risk')
    # keep only portfolio weights
    ef.risk$weight = ef.risk$weight[,(1:n)]        
         
ef.mad = portopt(ia, constraints, 50, 'MAD', min.mad.portfolio)
    ef.mad$weight = ef.mad$weight[,(1:n)]
         
# Plot multiple Efficient Frontiers & Transition Maps
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)  
plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)   
 
plot.transition.map(ef.risk)
plot.transition.map(ef.mad)

 #==========   method 2 ================#

#--------------------------------------------------------------------------
# Create 130:30
# Split x into x.long and x.short, x.long and x.short >= 0
# SUM(x.long) - SUM(x.short) = 1.6
#--------------------------------------------------------------------------
# Split Input Assumptions for x into x.long and x.short
ia.ls = aa.test.ia.add.short(ia)
     
# x.long and x.short >= 0
# x.long <= 0.8
# x.short <= 0.5
constraints = new.constraints(2*n, lb = 0, ub = c(rep(0.8,n),rep(0.5,n)))
     
# SUM (x.long - x.short) = 1
constraints = add.constraints(c(rep(1,n), -rep(1,n)), 1, type = '=', constraints)      
 
# SUM (x.long + x.short) = 1.6
constraints = add.constraints(c(rep(1,n), rep(1,n)), 1.6, type = '=', constraints)     
 
# create efficient frontier(s)
ef.risk = portopt(ia.ls, constraints, 50, 'Risk')
    # compute x
    ef.risk$weight = ef.risk$weight[, 1:n] - ef.risk$weight[, (n+1):(2*n)]
         
ef.mad = portopt(ia.ls, constraints, 50, 'MAD', min.mad.portfolio)
    ef.mad$weight = ef.mad$weight[, 1:n] - ef.mad$weight[, (n+1):(2*n)]
         
# Plot multiple Efficient Frontiers & Transition Maps
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)  
plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)   
 
plot.transition.map(ef.risk)
plot.transition.map(ef.mad)

# 200/100 portfolio
# optimizer does not use all the leverage at the lower risk region because optimal portfolios exist at the lower leverage levels
ia.ls = aa.test.ia.add.short(ia)
     
# x.long and x.short >= 0
# x.long <= 0.8
# x.short <= 0.5
constraints = new.constraints(2*n, lb = 0, ub = c(rep(0.8,n),rep(0.5,n)))
     
# SUM (x.long - x.short) = 1
constraints = add.constraints(c(rep(1,n), -rep(1,n)), 1, type = '=', constraints)      
 
# SUM (x.long + x.short) = 1.6
constraints = add.constraints(c(rep(1,n), rep(1,n)), 3, type = '=', constraints)     
 
# create efficient frontier(s)
ef.risk = portopt(ia.ls, constraints, 50, 'Risk')
    # compute x
    ef.risk$weight = ef.risk$weight[, 1:n] - ef.risk$weight[, (n+1):(2*n)]
         
ef.mad = portopt(ia.ls, constraints, 50, 'MAD', min.mad.portfolio)
    ef.mad$weight = ef.mad$weight[, 1:n] - ef.mad$weight[, (n+1):(2*n)]
         
# Plot multiple Efficient Frontiers & Transition Maps
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)  
plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)   
 
plot.transition.map(ef.risk)
plot.transition.map(ef.mad)






#--------------------------------------------------------------------------
# Create 200:100 using binary[0/1] variables and Branch and Bound algorithm
# Split x into x.long and x.short, x.long and x.short >= 0
# SUM(x.long) - SUM(x.short) = 3
#
# Solve using branch and bound: add a binary var b.i, x.long.i < b.i, x.short.i < (1-b.i)
#--------------------------------------------------------------------------
         
# x.long and x.short >= 0
# x.long <= 0.8 
# x.short <= 0.5 
constraints = new.constraints(2*n, lb = 0, ub = c(rep(0.8,n),rep(0.5,n)))
     
# SUM (x.long - x.short) = 1
constraints = add.constraints(c(rep(1,n), -rep(1,n)), 1, type = '=', constraints)       
 
# SUM (x.long + x.short) = 3
constraints = add.constraints(c(rep(1,n), rep(1,n)), 3, type = '=', constraints)        
                 
# NEW add binary constraint 
# adjust prior constraints: add b.i
constraints = add.variables(n, constraints)
     
# index of binary variables b.i
constraints$binary.index = (2*n+1):(3*n)
     
# binary variable b.i, x.long.i < b.i, x.short.i < (1-b.i)
# x.long.i < b.i
constraints = add.constraints(rbind(diag(n), 0*diag(n), -diag(n)), rep(0, n), type = '<=', constraints)
 
# x.short.i < (1-b.i)
constraints = add.constraints(rbind(0*diag(n), diag(n), diag(n)), rep(1, n), type = '<=', constraints)
     
# create efficient frontier(s)
ef.risk = portopt(ia.ls, constraints, 50, 'Risk')
    # compute x
    ef.risk$weight = ef.risk$weight[, 1:n] - ef.risk$weight[, (n+1):(2*n)]
         
ef.mad = portopt(ia.ls, constraints, 50, 'MAD', min.mad.portfolio)
    ef.mad$weight = ef.mad$weight[, 1:n] - ef.mad$weight[, (n+1):(2*n)]     
 
# Plot multiple Efficient Frontiers & Transition Maps
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)   
plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)    
 
plot.transition.map(ef.risk)
plot.transition.map(ef.mad)


