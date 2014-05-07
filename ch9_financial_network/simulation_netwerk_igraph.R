
#financial network

#goal:
#1. detect topology changes
#2. idenfity systemically important players by centrality measures
library(igraph)

#simmulate a pure-random networ
set.seed(7)
e <- erdos.renyi.game(100,0.1)
plot(e)

#calculate main characteristic
graph.density(e)
transitivity(e)
average.path.length(e)

#simulate small world networks 
set.seed(592)
w <- watts.strogatz.game(1,100,5,0.05)
plot(w)

graph.density(w)
transitivity(w)
average.path.length(w)

we <- get.edgelist(w)
head(we,5)
get.adjacency(w)

