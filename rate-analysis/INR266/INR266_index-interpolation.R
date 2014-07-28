
# INY266 index
# potenital outlier interpolation


# read in data
library(xts)
library(xtsExtra)

index <- read.csv('NSERO_index.csv',stringsAsFactors = FALSE)

# for currency first
data <- index
dates <- data$Date
rates <- data[,-1]
dates <- as.POSIXct(dates,format ="%m/%d/%Y")
t <- as.xts(rates, order.by = dates)
names(t) <- "NSERO"
rate_name <- names(t)
span <- 0.08



# get the label for outlier
tsoutliers <- function(x,plot=TRUE,span=0.1,name="",percentile = c(0.25,0.75),k=3,range = c(-1.1))
{
    # x <- as.ts(x)
    if(frequency(x)>1)
        resid <- stl(x,s.window="periodic",robust=TRUE)$time.series[,3]
    else
    {
        tt <- 1:length(x)
        #resid <- residuals(loess(x ~ tt,span=span))

        loe <- loess(x~tt, span = span)
    pred <- predict(loe,tt,se=TRUE)
    resid <- loe$residuals
    }


    # can adjust the parameters to control the outlier 
    resid.q <- quantile(resid,prob= percentile)
    iqr <- diff(resid.q)
    limits <- resid.q + k*iqr*range
    score <- abs(pmin((resid-limits[1])/iqr,0) + pmax((resid - limits[2])/iqr,0))
    indicator <- ifelse(score>0,1,0)
    indi <- cbind(indicator,x)[,1]

    if(plot)
    {
        custom.panel <- function(index,x,...) {
        default.panel(index,x,...)
        points(x=index(indi[indi == 1]),
          y=x[,1][index(indi[indi == 1])],cex=0.9,pch=19,
          col="blue")
          #abline(v=index(indi[indi == 1]),col="grey")
   		}

      newlist <- list(score = score, resid = resid,indicator = indicator)
    plot.xts(x=cbind(x,fitted=loe$fitted,residual=loe$residuals),panel = custom.panel, screens = factor(1, 1), auto.legend = TRUE, main = paste("LOESS plot",rate_name[i],sep=" "))

        cat("Number of outliers for ",name," is ", sum(indicator),"\n")
        return(invisible(newlist))
    }
    else
        return(list(score,resid,indicator))
}

indi <- rep(0,nrow(t)) # initialize the indicator vector
i <- 1
a <- tsoutliers(t[,i],name = rate_name[i],span=span, percentile = c(0.1,0.9),k=3,range=c(-0.85,1))
# if any col in one raw has 1, label this row as 1
indi <- indi | a$indicator


# convert into data frame with index and value, outlier removed
data <- data.frame(index = 1:nrow(t),rate = coredata(t))
# remove the 
data_rm <- data[!indi,] # discard 36

# apply loess 
(span <- 30/nrow(t))
loe <- with(data_rm, loess(NSERO ~ index, span = span, degree = 2))
pred <- predict(loe,(1:nrow(t)),se=TRUE)

# back to time series
fitline <- cbind(pred$fit,t)[,1][!indi]
interpolated <- cbind(pred$fit,t)[,1][indi]

# combine rhe result
result <- data.frame("date" = index(interpolated),"original"= coredata(t[indi]),"interpolated" = coredata(interpolated))
names(result) <- c("Date","Original","Interpolated")
result

# plot the result
custom.panel <- function(index,x,...) {
        default.panel(index,x,...)
        # add origin point
        points(x=index(indi[indi == 1]),
          y=t[,1][index(indi[indi == 1])],cex=0.9,pch=19,
          col="green")
        # add interpolated points
         points(x=index(indi[indi == 1]),
          y=interpolated,cex=0.9,pch=19,
          col="blue")
          #abline(v=index(indi[indi == 1]),col="grey")
   		}

     
plot.xts(x=cbind(t,fitted=pred$fit),panel = custom.panel, screens = factor(1, 1), auto.legend = TRUE, main = "Interpolation by LOESS")



#i <- 1
#span <- 100/nrow(t)
#a <- tsoutliers(t[,i],name = rate_name[i],span=span, percentile = c(0.1,0.9),k=3,range=c(-0.85,1))
