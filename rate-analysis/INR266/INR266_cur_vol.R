library(xts)
library(xtsExtra)
library(MTS)
library(fGarch)

currency <- read.csv('INR266.csv',stringsAsFactors = FALSE)
index <- read.csv('NSERO_index.csv',stringsAsFactors = FALSE)

# for currency first
data <- currency
dates <- data$Date
rates <- data[,-1]
dates <- as.POSIXct(dates,format ="%m/%d/%Y")
origin <- as.xts(rates, order.by = dates)
#t <-  (origin/lag(origin,1) - 1)[-1,]
t <-  log(origin/lag(origin,1))[-1,] # compute log return

# remove sample means - mean scale to 0
at <- scale(t,center=T,scale=FALSE)
# at <- at[,1:3]
MarchTest(at)  
# all 4 p-values are 0, strong evidence of heteroscedasticity
# the volatility invloves with time


# 1m ~ 1Y\

## !!!!!!!! problemn: need to use de-mean series?
short_tenor <- t[,1:6]
size <- 72
p <- 2
m <- MCholV(data.frame(short_tenor),size = size, p=p)
at <- scale(short_tenor[(size+p+1):nrow(short_tenor),],center=T,scale=F)
Sigma.t <- m$Sigma.t
MCHdiag(at,Sigma.t,5)


# plot the oroginal return series and 
summary(m$Vol)

jpeg(file = "INR266 currency short vol %d.jpeg",quality=100,width = 1200,height = 800,units = 'px', pointsize = 12)

for(i in 1:ncol(short_tenor))
{
	x <- short_tenor[(size+p+1):nrow(short_tenor),i]
	k <- 6
	sd <- sqrt(m$Vol[,i])
	num_out <- sum(abs(x) > k*sd)
	ratio <- num_out / nrow(t)

	custom.panel <- function(index,x,...) {
	  default.panel(index,x,...)
	  #abline(v=index(indi[indi == 1]),col=rgb(1,0,0,0.2),lwd=0.7)
	  usr <- par( "usr" )
	  text( usr[ 2 ], usr[ 4 ], paste("number of outliers: ",num_out,"\n","ratio: ",format(ratio,digits=4)),  adj = c( 1, 1 ), col = "blue",,cex=1.2 )

	}
	plot.xts(x=cbind(x,up=k*sd,lower=-k*sd), panel = custom.panel, screens = factor(1, 1), auto.legend = TRUE)
}
dev.off()

# for longer tenor 2Y ~12Y

long_tenor <- t[,7:12]
size <- 72
p <- 2
m <- MCholV(data.frame(long_tenor),size = size, p=p)
at <- scale(long_tenor[(size+p+1):nrow(long_tenor),],center=T,scale=F)
Sigma.t <- m$Sigma.t
MCHdiag(at,Sigma.t,10)


# plot the oroginal return series and 
summary(m$Vol)

jpeg(file = "INR266 currency long vol %d.jpeg",quality=100,width = 1200,height = 800,units = 'px', pointsize = 12)

for(i in 1:ncol(long_tenor))
{
	x <- long_tenor[(size+p+1):nrow(long_tenor),i]
	k <- 6
	sd <- sqrt(m$Vol[,i])
	num_out <- sum(abs(x) > k*sd)
	ratio <- num_out / nrow(t)

	custom.panel <- function(index,x,...) {
	  default.panel(index,x,...)
	  #abline(v=index(indi[indi == 1]),col=rgb(1,0,0,0.2),lwd=0.7)
	  usr <- par( "usr" )
	  text( usr[ 2 ], usr[ 4 ], paste("number of outliers: ",num_out,"\n","ratio: ",format(ratio,digits=4)),  adj = c( 1, 1 ), col = "blue",,cex=1.2 )

	}
	plot.xts(x=cbind(x,up=k*sd,lower=-k*sd), panel = custom.panel, screens = factor(1, 1), auto.legend = TRUE)
}
dev.off()


# compare with moving sd 
library(Quandl)
rv <- rollapply(t[,7],50,sd)
s <- cbind(sqrt(m$Vol[,1]),rv[-(1:74)])
ts.plot(s)


# ===================
# try on stochastic volatility model for a single series
# load raw data again
library(stochvol)
currency <- read.csv('INR266.csv',stringsAsFactors = FALSE)
# for currency first
data <- currency
dates <- data$Date
rates <- data[,-1]
dates <- as.POSIXct(dates,format ="%m/%d/%Y")
origin <- as.xts(rates, order.by = dates)

t <-  log(origin/lag(origin,1))[-1,] # compute log return
# remove sample means - mean scale to 0
at <- scale(t,center=T,scale=FALSE)

ret <- coredata(at[,1])
names(ret) <- "demeaned log return"
plot.xts(x=cbind(origin[-1,1],ret), auto.legend = TRUE,main = "INR266 1M")

res <- svsample(ret, priormu = c(0,1), priorphi = c(20,1.1), priorsigma = 0.1)
# assessing the output and displaying the results
summary(res, showlatent = FALSE)

res <- updatesummary(res, quantiles = c(0.01,0.1,0.5,0.9,0.99))
volplot(res, forecast = 100)

# trace plots for parameters, burn-in has already been discarded
par(mfrow=c(3,1))
paratraceplot(res)

# kernal density estimate for parameters
# with posterior and prior density
par(mfrow=c(1,3))
paradensplot(res)

# combine all plot
par(mfrow=c(1,1))
plot(res)

# residuals
myresid <- resid(res)
# by default, mean-standardized residuals
plot(myresid,ret)