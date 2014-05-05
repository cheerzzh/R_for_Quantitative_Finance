


# issue of cointegration
# find a linear combination between non-stationary TS that
#results in a stationary series
# to detect stable long-run relationships between non-stationary TS
# for example: price

#case study: cross hedging jet fuel
# since liquid OTS instrument for jet fuel is absent
#airline use related exchange traded futures contracts(eg. heating oil)
#for heding purpose
# goal: derive the optimal hedging ratio

library(urca) #for unit root test and estimate cointegration relationships

#import monthly price for jet fuel and heating oil
prices = read.zoo("JetFuelHedging.csv",sep=",",
	FUN=as.yearmon,format="%Y-%m",header=TRUE)

#fit a linear model that explain jet fuel price change by heating oil price change
# set intercept to be 0 -> no cash holding
simple_mod=  lm(diff(prices$JetFuel)~diff(prices$HeatingOil)+0) 
summary(simple_mod)
#obtain hedge ratio = beta = 0.89059
#resulting error = 0.0846
#improve this ratio by using an existing long-run relationships
# levels of jet fuel and heating oil futures prices

#plot two price
#amlost overlapping
plot(prices$JetFuel, main="Jet Fuel and Heating Oil Prices",
	xlab = "Date",ylab="USD")
lines(prices$HeatingOil,col="red")

#Use Engle & Granger's 2-step estimation 
# 1. test 2 TS for a unit root using Dickey-Fuller Test
# Null hypo: non-stationary
jf_adf = ur.df(prices$JetFuel,type = "drift")
summary(jf_adf)
ho_adf = ur.df(prices$HeatingOil,type="drift")
summary(ho_adf)
#accept Null hypo, non-stationary

#2. estimate static equilibrium model and test the residual
model_static = summary(lm(prices$JetFuel~prices$HeatingOil))
error = residuals(model_static)
error_cadf = ur.df(error,type="none")
summary(error_cadf)
#reject the null hypo of non-stationary

#finally obtain a Error-Correction Model(ECM)
djf = diff(prices$JetFuel)
dho = diff(prices$HeatingOil)
error_lag = lag(error,k=-1)
mod_ecm = lm(djf~dho+error_lag)
summary(mod_ecm)
#result:(get improved)
# higher heding ratio: 0.9002
#lower standard error: 0.06875
#coefficient of error term is negative (-0.65540): 
#large deviations between the two prices are going to be corrected 
#and prices move closer to their long-run stable relationship












