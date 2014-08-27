library(xts)
library(xtsExtra)

# read in the indicator matirx 
indicator <-  read.csv('MYR209_currency_indicator.csv',stringsAsFactors = FALSE)

data <- indicator
dates <- data$date
rates <- data[,-1]
dates <- as.POSIXct(dates,format ="%m/%d/%Y")
indicator <- as.xts(rates, order.by = dates)
#currency <- read.csv('INR266.csv',stringsAsFactors = FALSE)
index <- read.csv('MYR209_currency.csv',stringsAsFactors = FALSE)

data <- index
dates <- data$Date
rates <- data[,-1]
dates <- as.POSIXct(dates,format ="%m/%d/%Y")
series <- as.xts(rates, order.by = dates)

data_after <- NULL 
result <- data.frame(Date=NA, Tenor=NA,Original=NA, Interpolated=NA)[numeric(0), ]

# prepare data and indicator for each tenor
for(i in 1: ncol(series)){
	t <- series[,i]
	indi <- indicator[,i]

	if(sum(indi) > 0)
	# convert into data frame with index and value, outlier removed
	{data <- data.frame(index = 1:nrow(t),rate = coredata(t))
	# remove the 
	data_rm <- data[!indi,] # discard 36

	# apply loess 
	(span <- 30/nrow(t))
	loe <- loess(data_rm[,2] ~ data_rm[,1], span = span, degree = 2)
	pred <- predict(loe,(1:nrow(t)),se=TRUE)

	# back to time series
	fitline <- cbind(pred$fit,t)[,1][!indi]
	interpolated <- cbind(pred$fit,t)[,1][indi==1]
	tenor_name <- names(t)

	# combine rhe result
	temp <- data.frame(index(interpolated),tenor_name,coredata(t[indi==1]),coredata(interpolated))
	names(temp) <- c("Date","Tenor","Original","Interpolated")
	result <- rbind(result, temp) # update result


	# combine interpolated series
	temp <- t
	temp[indi==1] <- interpolated
	data_after <- cbind(data_after,temp)

	# plot the result
	if(0){
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
	}

	} else
	{
		data_after <- cbind(data_after,series[,i])
	}
}

# save csv files
print(result)

write.csv(as.data.frame(data_after), file = "MYR209_currency_interpolated.csv",row.names=TRUE)
write.csv(result, file = "MYR209_currency_result_table.csv",row.names=TRUE)

