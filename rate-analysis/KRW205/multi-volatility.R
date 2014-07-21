library(MTS)

# sample to test multi-volatility
zt <- matrix(rnorm(2000),400,5) # generate sample that are iid, no time-varing variance
MarchTest(zt) #  4 test

# for KRW205
library(xts)
library(xtsExtra)

#currency <- read.csv('INR266.csv',stringsAsFactors = FALSE)
index <- read.csv('KRW205_currency.csv',stringsAsFactors = FALSE)

data <- index
dates <- data$Date
rates <- data[,-1]
dates <- as.POSIXct(dates,format ="%m/%d/%Y")
origin <- as.xts(rates, order.by = dates)
#t <-  (origin/lag(origin,1) - 1)[-1,]
t <-  log(origin/lag(origin,1))[-1,] # compute log return
# remove sample means
at <- scale(t,center=T,scale=FALSE)
at <- at[,1:3]
# try on first 5 columns first
MarchTest(at) # all 4 p-values are 0, strong evidence of heteroscedasticity

# apply EWMA method
# first fit a mean equation, VARIMA? 
# have to refer to previous chapter
m1 <- VAR(at,1)
re <- m1$residuals
MarchTest(re) # residial still heteroscedasticity

# fir EWMA to resifuals
m2 <- EWMAvol(re,lambda = -0.1)
(Sigma.t <- m2$Sigma.t)
m3 <- MCHdiag(re,Sigma.t)


# BEKK(1,1)
mla <- BEKK11(at)

# return seems serally uncorrelated but has heteroscedasticity
# ========   important ==============#
library(fGarch)
size <- 36
p <- 2
m <- MCholV(data.frame(t)[,1:6],size = size, p=p)
at <- scale(t[(size+p+1):nrow(t),1:6],center=T,scale=F)
Sigma.t <- m$Sigma.t
MCHdiag(at,Sigma.t)


# convert the sigma.t into a list of cov matrix
summary(m$Vol)
i <- 6
x <- t[(size+p+1):nrow(t),i]
k <- 6
sd <- sqrt(m$Vol[,i])
plot.xts(x=cbind(x,up=k*sd,lower=-k*sd), screens = factor(1, 1), auto.legend = TRUE)
sum(abs(x) > k*sd)

