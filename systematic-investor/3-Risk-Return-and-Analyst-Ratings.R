# goal: discuss a connection between Risk, Return and Analyst Ratings.



###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
con = gzcon(file('sit.gz', 'rb')) #from same folder
source(con)
close(con)

# our universe of stocks : 30 stocks from Dow Jones Industrial Average (^DJI) index
# For each stock:
# I will compute the number of Upgrades and Downgrades, Risk, and Return in 2010:2011
# run a linear regression and compute correlation between the number of Upgrades and Downgrades and Risk and Return.

library(quantmod,car)

# download Dow Jones Components
url = 'http://finance.yahoo.com/q/cp?s=%5EDJI+Components'
txt = join(readLines(url))
 
# extract table from this page
temp = extract.table.from.webpage(txt, 'Symbol', hasHeader = T)

# Symbols
Symbols = temp[, 'Symbol']



