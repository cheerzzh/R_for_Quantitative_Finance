
#PloTA
# time series & technical plots


#
#   * plota – main plot method
#   * plota2Y – add second Y axis to existing plot

#   * plota.lines – plot lines
#   * plota.candle – plot Candle
#   * plota.ohlc – plot Open/High/Low/Close
#   * plota.hl – plot High/Low
#   * plota.volume – plot Volume
#   * plota.scale.volume – scale Volume

#   * plota.grid – add grid
#   * plota.legend – plot legend
#   * plota.layout – specify plot layout

#   * plota.theme.blue.red – color theme
#  * plota.theme.green.orange – color theme
#  * plota.theme.gray.orange – color theme



###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
con = gzcon(file('sit.gz', 'rb')) #from same folder
source(con)
close(con)

#load quantmod package and download price history for SPY and IBM:
library(quantmod)
#download sample data from Yahoo
data.spy = getSymbols('SPY', from = '1980-01-01', auto.assign = FALSE)
data.ibm = getSymbols('IBM', from = '1980-01-01', auto.assign = FALSE)


#create a simple chart of SPY:
y = data.spy['2010:12:01::2011:02:01']
highlight = which(Cl(y) < 126) #Cl() get close price

png(filename = 'plot1-SPY.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(c(1,1,2))
plota(y, type = 'candle', main = 'SPY', plotX = F, x.highlight = highlight)
y = plota.scale.volume(y)
plota(y, type = 'volume', x.highlight = highlight) #add volumn plot

dev.off()


#create a simple chart of SPY with RSI and Legend:
#Relative Strength Index

y = data.spy['2010:01:01::2011:02:01'] #select date range
 
png(filename = 'plot2-SPY-wiht-RSI.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')

layout(c(1,1,2,3))
plota(y, type = 'candle', plotX = F) 
    plota.legend('SPY', 'blue', y)
 
y = plota.scale.volume(y) #scale to million 
plota(y, type = 'volume', plotX = F)
    plota.legend('Volume', 'blue', Vo(y))
 
rsi = RSI(Cl(y),2) #compute RSI
plota(rsi, type = 'l', y.highlight = c(c(Inf,80),c(20,-Inf)))
    abline(h = 20, col = 'red')
    abline(h = 80, col = 'red')
    plota.legend('RSI(2)', 'black', rsi)
dev.off()

#create a chart with second Y axis:

y = data.spy['2010:01:01::2011:02:01']
 
# to plot second Y axis, free some space on left side
# e.g. set LeftMargin=3
png(filename = 'plot3-second-Y-axis.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')
plota(y, type = 'ohlc', LeftMargin=3)
 
y0 = y;
y = data.ibm['2010:01:01::2011:02:01']
plota2Y(y, ylim = range(OHLC(y)),las=1, col='red', col.axis = 'red')#add second Y column
    plota.ohlc(y, col = 'red')
    plota.legend('SPY(rhs),IBM(lhs)', 'blue,red', list(y0,y))

dev.off()


#plot daily and monthly on the same plot:

y = data.spy['2010:01:01::2011:02:01']

png(filename = 'plot4-daily-monthly.png', width = 800, height = 500, units = 'px', pointsize = 12, bg = 'white')
plota(y, type = 'candle')
    y1 = to.monthly(y)
        index(y1) = as.Date(index(y1))
    plota.ohlc(y1, col = 'pink')
    plota.candle(y)
    plota.legend('Daily,Monthly', 'red,pink')
dev.off()

#plot daily, weekly and monthly:

y = data.spy['2010:01:01::2011']

png(filename = 'plot5-daily-weekly-monthly.png', width = 1000, height = 1200, units = 'px', pointsize = 12, bg = 'white')
layout(c(1,2,3))
plota(y, type = 'candle', plotX = F)
    plota.legend('Daily', 'blue', y)
 
plota(y, ylim = range(OHLC(y)), plotX = F)
    y1 = to.weekly(y)
        index(y1) = as.Date(index(y1))
    plota.candle(y1)
    plota.legend('Weekly', 'blue', y1)
 
plota(y, ylim = range(OHLC(y)))
    y1 = to.monthly(y)
        index(y1) = as.Date(index(y1))
    plota.candle(y1)
    plota.legend('Monthly', 'blue', y1)
dev.off()


