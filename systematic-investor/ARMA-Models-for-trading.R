
# packages used:
# fArma:  nice wrapper with extended functionality around the arima function from the stats package

library(quantmod)
library(fArma)

# Get S&P 500
getSymbols( "^GSPC", from="2000-01-01" )

# Compute the daily returns
gspcRets <- diff( log( Cl( GSPC ) ) )
 
# Use only the last two years of returns
gspcTail = as.ts( tail( gspcRets, 500 ) )

# Fit the model
gspcArma = armaFit( formula=~arma(2,2), data=gspcTail )
gspcArma@fit$aic

# notice:
# 1. model the daily returns instead of the prices- make stationary
# 2. use log return


# =========   walk-forward backtesting   ==============#
# While walking the series day by day, 
# we will use history of certain length to find the best model.
# Then we will use this model to predict the next day’s return. 
# If the prediction is negative, we assume short position, 
# otherwise we assume a long position.

# ============  choose a good model =============#
# In the case of ARMA, there are two parameters
# strategy:  a loop to go through all parameter combinations we deem reasonable,
# finally pick the model with the lowest AIC or some other statistic


# === function to fit the best ARMA(p,q) model ======#
# since  armaFit fails to find a fit and returns an error,
#  using the tryCatch function to catch any error or warning and return a logical value (FALSE) instead of interrupting 

armaSearch = function( # default parameter
   xx, 
   minOrder=c(0,0), 
   maxOrder=c(5,5),
   trace=FALSE )
{
   bestAic = 1e9 # initial aic
   len = NROW( xx )
   # search through all p, q order combination
   for( p in minOrder[1]:maxOrder[1] ) for( q in minOrder[2]:maxOrder[2] ) 
   {
      if( p == 0 && q == 0 )
      {   
         next
      }   
 
      formula = as.formula( paste( sep="", "xx ~ arma(", p, ",", q, ")" ) ) # prepare formula to call
 
      fit = tryCatch( armaFit( formula, data=xx ),
                      error=function( err ) FALSE,
                      warning=function( warn ) FALSE )
      if( !is.logical( fit ) ) # if fit succesfully
      {   
         fitAic = fit@fit$aic
         if( fitAic < bestAic ) # update if this model attain better aic
         {   
            bestAic = fitAic
            bestFit = fit
            bestModel = c( p, q )
         }   
 
         if( trace )  # print fitting trace
         {   
            ss = paste( sep="", "(", p, ",", q, "): AIC = ", fitAic )
            print( ss )
         }   
      }   
      else
      {   
         if( trace )
         {   
            ss = paste( sep="", "(", p, ",", q, "): None" )
            print( ss )
         }   
      }   
   }
 
   if( bestAic < 1e9 )
   {
      return( list( aic=bestAic, fit=bestFit, model=bestModel ) )
   }
 
   return( FALSE )
}

#===============================================================#

# ==============  forecasting =====================#
getSymbols( "SPY", from="1900-01-01" )
spyRets = diff( log( Cl( SPY )["/2012-05-29"] ) )
spyArma = armaFit( ~arma(0, 2), data=as.ts( tail( spyRets, 500 ) ) ) # fit ARMA(0,2) -> MA(2)
as.numeric( predict( spyArma, n.ahead=1, doplot=F )$pred )

# build an indicator for the back testing
# walk the daily return series and at each point perform the steps we covered so far. 

# ======   algorithm ===========#
# currentIndex is the index of the day we are making a forcast for
# xx is the return series
# history is look-back period to consider at each point


repeat
{
   nextIndex = currentIndex + 1
 
   # lags is how many days behind is the data, the default is 1,
   # meaning use data up to yesterdays close
   forecastLength = nextIndex - currentIndex + lags - 1
 
   # Get the history series
   yy = xx[index(xx)[(currentIndex-history-lags+1):(currentIndex-lags)]]
 
   # Find the best fit
   bestFit = armaSearch(
                  yy, 
                  minOrder,
                  maxOrder,
                  withForecast=TRUE,   # we want the model to have a valid forecast
                  forecastLength=forecastLength,   # 1 for a dialy forecast
                  trace=trace,
                  cores=cores )   # the number of cores to use
 
   if( !is.null( bestFit ) )
   {
      # Forecast
      fore = tryCatch( predict( bestFit, n.ahead=forecastLength, doplot=FALSE ),
                       error=function( err ) FALSE,
                       warning=function( warn ) FALSE )
      if( !is.logical( fore ) )
      {   
         # Save the forecast
         forecasts[currentIndex] = tail( fore$pred, 1 )
 
         # Save the model order
         ars[currentIndex] = order[1]
         mas[currentIndex] = order[2]
 
         forecasts[currentIndex] = 0
      }
 
      if( nextIndex > len ) break
      currentIndex = nextIndex
   }
}


# ==============  improving performance =============#
# quite computational intensive
# 10 years = 2520 days
# each days at least 35 ARMA model
# resulting more than 88K model fiting
# one solution: parallelize model selection
# see computeArmaForcasts.R

# ===========  model volatility using GARCH ============#
# extending the ARMA forecasting with a GARCH model. 

library(fGarch)

getSymbols("SPY", from="1900-01-01")
spyRets = diff(log(Ad(SPY)))
spyGarch = garchFit(~arma(0, 2) + garch(1, 1), data=as.ts(tail(spyRets, 500)))
predict(spyGarch, n.ahead=1, doplot=F)
# the actual forecasts are predict(spyGarch, n.ahead=1, doplot=F)[,1]

# see garchAuto.R


# ==================   example on S&P 500 ================#
library(quantmod)
library(lattice)
library(timeSeries)

getSymbols("^GSPC", from="1900-01-01")
 
gspcRets = Ad(GSPC) / lag(Ad(GSPC)) - 1
gspcRets[as.character(head(index(Ad(GSPC)),1))] = 0 # zero return for first day

# The maximum draw down- highest 10 draw down 
head(drawdownsStats(as.timeSeries(gspcRets)),10)

# The largest dropdawn is:
#         From     Trough         To      Depth Length ToTrough Recovery
# 1 2007-10-10 2009-03-09 2012-09-28 -0.5677539   1255      355       NA

# Load the ARMA indicator- computed using ARMA + GARCH prediction?
gspcArmaInd = as.xts( read.zoo(file="gspcInd3.csv", format="%Y-%m-%d", header=T, sep=",") )

# Filter out only the common indexes
mm = merge( gspcArmaInd[,1], gspcRets, all=F )
gspcArmaRets = mm[,1] * mm[,2] # daily return  according to strategy


# The maximum draw down
head(drawdownsStats(as.timeSeries(gspcArmaRets)),10)
# The largest dropdawn is:
#          From     Trough         To      Depth Length ToTrough Recovery
# 1  1987-10-26 1992-10-09 1997-10-27 -0.5592633   2531     1255     1276

# growth according to ARMA strategy
gspcArmaGrowth = log( cumprod( 1 + gspcArmaRets ) )

# actual growth
gspcBHGrowth = log( cumprod( 1 + mm[,2] ) )

gspcAllGrowth = merge( gspcArmaGrowth, gspcBHGrowth, all=F )

# plot two growth series
xyplot( gspcAllGrowth,
        superpose=T,
        col=c("darkgreen", "darkblue"),
        lwd=2,
        key=list( x=.01,
                  y=0.95,
                  text=list(c("ARMA", "Buy-and-Hold")),
                  lines=list(lwd=2, col=c("darkgreen", "darkblue"))))




