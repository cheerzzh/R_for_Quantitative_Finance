
#packages to handle TS: zoo, xts, timrSeries
#

library(zoo)

# app1.csv
#daily closing price for Apple's stock
# ISO8601 YYYY-MM-DD


app1 = read.zoo("app1.csv",sep=",",header=TRUE,format = "%Y-%m-%d")

