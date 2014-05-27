
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














