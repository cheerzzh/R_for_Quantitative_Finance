library(xts)
library(xtsExtra)

#currency <- read.csv('INR266.csv',stringsAsFactors = FALSE)
index <- read.csv('KRW205_currency.csv',stringsAsFactors = FALSE)

data <- index
dates <- data$Date
rates <- data[,-1]
dates <- as.POSIXct(dates,format ="%m/%d/%Y")
origin <- as.xts(rates, order.by = dates)
t <-  (origin/lag(origin,1) - 1)[-1,]

mul <- apply(t, 1, prod) 