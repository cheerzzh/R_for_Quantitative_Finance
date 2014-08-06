
library(igraph)
data <- read.csv2("networktable.csv")
str(data)

#21314 * 7

size <- read.csv2("vertices.csv")
bignetwork <- graph.data.frame(data,vertices=size)

is.connected(bignetwork)
table(is.multiple(bignetwork)) #check whether the network has multiple edges:
str(is.loop(bignetwork))
# leave out all the loops and summarize the multiple edges
snetwork <- simplify(bignetwork,edge.attr.comb="sum")
plot(snetwork,edge.arrow.size = 0.4)

#detect community (densely connected subgraphs)
communities <- walktrap.community(snetwork)
communities

#market structure evolve in time
#high stability

#compare network topology before and after 
#the default of Lehman Brothers in 2008

monthlynetwork <- subset(data,(Year == 2008)&(Month == 9))

#sum of each month of each year
mAmount <- with(data,
	aggregate(Amount, by = list(Month = Month, Year = Year),
		FUN = sum))
#plot monthly time series
plot(ts(mAmount$x,start=c(2007,1),frequency=12),
	ylab = "Amount")
#we counld find a massive struture change after Lehman-fall
#inner structure also change
#plot evolution of graph density
ds <- sapply(2007:2010,function(year){
	sapply(1:12,function(month){
		mdata <- subset(data,(Year == year)&(Month == month))
		graph.density(graph.data.frame(mdata))
		})
	})
plot(ts(as.vector(ds),start = c(2007,1),frequency=12),xlab="Time",
	ylab = "Graph densities")
#could see  after Lehman-fall, density dropped
#transactions concentrated on fewer banks











