con = gzcon(file('sit.gz', 'rb')) #from same folder
source(con)
close(con)



#  how to simulate asset price paths given the expected returns and covariances
#  Assumption: prices follow the Geometric Brownian Motion.

asset.path <- function(s0, mu, sigma,
	nsims = 10000,
	periods = c(0,1))
{
	s0 <- as.vector(s0)
	nsteps <- len(periods)
	dt <- c(periods[1],diff(periods))

	if( len(s0) == 1 ) {
        drift = mu - 0.5 * sigma^2
        if( nsteps == 1 ) {
            s0 * exp(drift * dt + sigma * sqrt(dt) * rnorm(nsims))
        } else {
            temp = matrix(exp(drift * dt + sigma * sqrt(dt) * rnorm(nsteps * nsims)), nc=nsims)
            for(i in 2:nsteps) temp[i,] = temp[i,] * temp[(i-1),]
            s0 * temp
        }
    } else {
        require(MASS)
        drift = mu - 0.5 * diag(sigma)
        n = len(mu)
         
        if( nsteps == 1 ) {
            s0 * exp(drift * dt + sqrt(dt) * t(mvrnorm(nsims, rep(0, n), sigma)))
        } else {
            temp = array(exp(as.vector(drift %*% t(dt)) + t(sqrt(dt) * mvrnorm(nsteps * nsims, rep(0, n), sigma))), c(n, nsteps, nsims))
            for(i in 2:nsteps) temp[,i,] = temp[,i,] * temp[,(i-1),]
            s0 * temp
        }
    }

}


 #*****************************************************************
    # Plot some price paths
#******************************************************************  
S = c(100,105)
X = 98
Time = 0.5
r = 0.05
sigma = c(0.11,0.16)
rho = 0.63
N = 10000

# Single Asset for 10 years
periods = 0:10
prices = asset.paths(S[1], r, sigma[1], N, periods = periods)
 
# plot
matplot(prices[,1:100], type='l', xlab='Years', ylab='Prices',
    main='Selected Price Paths')
 
     
# Multiple Assets for 10 years
periods = 0:10
cov.matrix = sigma%*%t(sigma) * matrix(c(1,rho,rho,1),2,2)
prices = asset.paths(S, c(r,r), cov.matrix, N, periods = periods)

# plot
layout(1:2)
matplot(prices[1,,1:100], type='l', xlab='Years', ylab='Prices',
    main='Selected Price Paths for Asset 1')
matplot(prices[2,,1:100], type='l', xlab='Years', ylab='Prices',
    main='Selected Price Paths for Asset 2')



