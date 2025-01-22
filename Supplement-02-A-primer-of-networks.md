# Interacting with networks in R with igraph
Previously we went over the notion of functions in programming. 
In R, there are things called "libraries" and these are collections of functions written by various people across the world.
Chances are, if you want to do something in R, there is already a library that can do it.
In this case, we will use one of the most popular libraries for manipulating and visualizing networks: "igraph"

```r
## Install the igraph library if you never have before
install.packages("igraph")
```
If you've never installed igraph before, you'll need to do that first. This will take a little bit of time, to download the pieces of software from the web and install it on your computer.
After this is done, to load the library we just need to run:


```r
## Load the library
library("igraph")
```

Now we want to visualize a network. The problem is, what network will we choose? Let's start by creating a random network from scratch.


```r
## First we will make a graph from scratch
## To do this, we first need to define an adjacency matrix
## Let's first define the dimensions of the matrix, i.e. how many nodes we have, which we'll call "N"
N = 50
```

I have indicated here that we will start with N = 50 nodes in our network. 
We want to encode our network as an adjacency matrix where 0's in the matrix mean there is no link, 
and nonzero numbers mean there is a link. The value of this nonzero number corresponds to the strength (weight) of that link.
Thus, this is a weighted adjacency matrix.

```r
## Now we will populate our matrix with 50 rows and 50 columns with random numbers.
## This means we need 50^2 (N^2) random numbers. We can choose any distribution to draw random numbers from. 
## I will choose a log-normal distribution (a bell curve on the log scale).
randomNumberList = rlnorm(N^2)
A = matrix(randomNumberList, nrow = N)

## Verify that I have the right size matrix
dim(A)
```

Right now we have a matrix R full of random numbers.
Let's turn this into an adjacency matrix by simply defining a cutoff.
Any of our random numbers that are above this cutoff, we'll set to 1.
Any of our random numbers that are below the cutoff, we'll set to 0.
```r
## Random numbers below this cutoff will be 1s, and above will be 0s
cutoff        = 1
A[A < cutoff] = 0 ## this line is saying take A, grab all of the entries where A < my cutoff, and set those to 0

## We also will ignore loops for now, which we can do by making all diagonal entries 0
diag(A) = 0

## Let's also give some names to our nodes, like "Node_1", "Node_2", etc.
rownames(A) = colnames(A) = paste('Node', 1:N, sep = '_')
```

Now that we have a matrix that represents some random network, we can pass it to `igraph`

```r
## I can turn my adjacency matrix into an igraph object like this:
G = graph_from_adjacency_matrix(A, mode = 'undirected', weighted = TRUE)

## We can extract the node set and link set like this:
V(G)
E(G)
```

Now we want to look at our network. Try to evaluate these one by one and figure out what each new new addition (argument) is doing

```r
## Plot it
plot(G)

## A few extra bits
plot(G, vertex.label = NA)
plot(G, vertex.label = NA, vertex.size = 5)
plot(G, vertex.label = NA, vertex.size = 5, vertex.color = '#2f9253')
plot(G, vertex.label = NA, vertex.size = 5, vertex.color = '#2f9253', edge.width = 2)
plot(G, vertex.label = NA, vertex.size = 5, vertex.color = '#2f9253', edge.width = 0.25*E(G)$weight)
plot(G, vertex.label = NA, vertex.size = 5, vertex.color = '#2f9253', edge.width = 0.25*E(G)$weight, edge.color = '#000000')
plot(G, vertex.label = NA, vertex.size = 5, vertex.color = '#2f9253', edge.width = 0.25*E(G)$weight, edge.color = '#000000', layout = layout_on_grid(G))
plot(G, vertex.label = NA, vertex.size = 5, vertex.color = '#2f9253', edge.width = 0.25*E(G)$weight, edge.color = '#000000', layout = layout_with_kk(G))
plot(G, vertex.label = NA, vertex.size = 5, vertex.color = '#2f9253', edge.width = 0.25*E(G)$weight, edge.color = '#000000', layout = layout_with_fr(G))
```

Oh yeah. I told you that anything to the right of a # symbol will be ignored (comments). The one exception to this are colors, 
which are encoded as a # sign, followed by 6-digit hex code, all wrapped in quotes. For instance, the color black is "#000000".

Finally, deploying the algorithms we have learned in class so far is very easy once you have an igraph object (G).

```r
## The function `distances` returns a matrix where the entries are walk lengths between pairs of nodes
D = distances(G, algorithm = 'dijkstra', weights = NA)

## Which we can visualize using a heatmap
heatmap(D)

## The minimum spanning tree function is calles `mst()`
tree = mst(G, weights = E(G)$weight)
plot(tree)

## Let's compare the full network to the tree. First we need to fix the positions of our nodes so they don't move around between plots.
fixedLayout = layout_with_fr(G)

## Visualize the full graph and the tree
plot(G, vertex.label = NA, vertex.size = 5, vertex.color = '#2f9253',
     edge.width = 0.05*E(G)$weight, edge.color = '#000000',
     layout = fixedLayout)

plot(tree, vertex.label = NA, vertex.size = 5, vertex.color = '#2f9253',
     edge.width = 0.25*E(G)$weight, edge.color = '#000000',
     layout = fixedLayout)
```
