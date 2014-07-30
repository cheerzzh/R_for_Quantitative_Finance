
set.seed(123)
library("stochvol")
data("exrates")
ret <- logret(exrates$USD, demean =TRUE)
par(mfrow=c(2,1), mar = c(1.9,1.9,1.9,0.5),mgp = c(2,0.6,0))
plot(exrates$date, exrates$USD, type = "l",main = "Price of 1 EUR in USD")
plot(exrates$date[-1],ret, type="l",main="Demeaned log-returns")



# built-in data generator: svsim
# produce realizations of an sv process 
# return a object of svsim class, has its own print, summary, plot methods
sim <- svsim(500, mu=-0.9, phi = 0.99, sigma = 0.1)
par(mfrow=c(2,1))
plot(sim)


# svsample()
# a R wrapper for sampler code in C
res <- svsample(ret, priormu = c(-10,1), priorphi = c(20,1.1), priorsigma = 0.1)

# assessing the output and displaying the results
summary(res, showlatent = FALSE)

# by default, display 5%, 50%, 95% quantiles
volplot(res, forecast = 100, dates = exrates$date[-1])

# display different posterior quantiles
res <- updatesummary(res, quantiles = c(0.01,0.1,0.5,0.9,0.99))
volplot(res, forecast = 100, dates = exrates$date[-1])

# paratraceplot()
# trace plots for parameters, burn-in has already been discarded
par(mfrow=c(3,1))
paratraceplot(res)

# paradensplot()
# kernal density estimate for parameters
# with posterior and prior density
par(mfrow=c(1,3))
paradensplot(res)

# combine all plot
plot(res)

# residuals
myresid <- resid(res)
# by default, mean-standardized residuals
plot(myresid,ret)

