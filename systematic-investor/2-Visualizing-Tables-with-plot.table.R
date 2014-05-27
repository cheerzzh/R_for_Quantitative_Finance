
# plot.table function in the Systematic Investor Toolbox is a flexible table drawing routine. 
# plot.table has a simple interface and takes following parameters


# parameters:

#   * plot.matrix – matrix with data you want to plot
#   * smain – text to draw in (top, left) cell; default value is blank string
#   * highlight – Either TRUE/FALSE to indicate if you want to color each cell based on its numeric value Or a matrix with colors for each cell
#   * colorbar – TRUE/FALSE flag to indicate if you want to draw colorbar




###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
con = gzcon(file('sit.gz', 'rb')) #from same folder
source(con)
close(con)


#========== Example 1=============================
# define row and column titles

(mrownames = spl('row one,row two,row 3'))
(mcolnames = spl('col 1,col 2,col 3,col 4'))

# create temp matrix with data you want to plot
temp = matrix(NA, len(mrownames), len(mcolnames))
	rownames(temp) = mrownames #fill in the data matrix
    colnames(temp) = mcolnames
    temp[,] = matrix(1:12,3,4)

# plot temp, display current date in (top, left) cell
plot.table(temp, format(as.Date(Sys.time()), '%d %b %Y'))
#============================================

#To create plot.table with colorbar:

# generate 1,000 random numbers from Normal(0,1) distribution
data =  matrix(rnorm(1000), nc=10)
    colnames(data) = paste('data', 1:10, sep='')

# compute Pearson correlation of data and format it nicely
temp = compute.cor(data, 'pearson')
    temp[] = plota.format(100 * temp, 0, '', '%') #convert to xx%

# create a report page that will display a chart of IBM for 2010:2011 
#and a table with Valuation Measures from Key Statistics Yahoo Finance webpage.
library(quantmod)

Symbol = 'IBM'
 
# download IBM price history from Yahoo
data = getSymbols(Symbol, from = '1980-01-01', auto.assign = FALSE)

# download Key Statistics from yahoo
url = paste('http://finance.yahoo.com/q/ks?s=', Symbol, sep = '')
txt = join(readLines(url))  #get the text from the website

# extract Valuation Measures table from this page
temp = extract.table.from.webpage(txt, 'Market Cap', hasHeader = F)
    temp = rbind(c('', Symbol), temp)   # add header row

# prepare IBM data for 2010:2011 and compute 50 days moving average
y = data['2010::2011']
sma50 = SMA(Cl(y), 50) #for daily closing price

# plote candles and volume and table
layout(c(1,1,2,3,3))
 
plota(y, type = 'candle', main = Symbol, plotX = F) #plot for stock price
	plota.lines(sma50, col='blue') # add 50 day moving average price line
    plota.legend(c(Symbol,'SMA 50'), 'green,blue', list(y,sma50)) # add legends

y = plota.scale.volume(y) # add volumn plot
plota(y, type = 'volume')

plot.table(temp)














