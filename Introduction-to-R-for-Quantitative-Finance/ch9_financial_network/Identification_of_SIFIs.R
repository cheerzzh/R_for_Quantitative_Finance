
#issue of complex system
#systemic risk
#identify systemically important financial institutions
#(SIFIs), proposed by BCBS(2011)

#five factors contributing to systemic risk
#1. size
#2. interconnectedness
#3. lack of substitues
#4. cross-jurisdictional
#5. complexity of activeities

# aim to measure interconnectedness here
# degree/betweeness/closeness/eigenvector

library(igraph)
data <- read.csv2("networktable.csv")
g <- graph.data.frame(data)

degree <- degree(g,normalized = TRUE)
between <- betweenness(g,normalized =TRUE)
closeness <- closeness(g,normalized=TRUE)
eigenv <- evcent(g,directed = TRUE)$vector

norm <- function(x) x/mean(x)
#equaly weight 4 centrality measures 
index <- (norm(degree)+norm(between)+
	norm(closeness)+norm(eigenv))/4

index
#plot distribution of index 
#select banks with highest index value
hist(index)
index[which(index>2.5)]


