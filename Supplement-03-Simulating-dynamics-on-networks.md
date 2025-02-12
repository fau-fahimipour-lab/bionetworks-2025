# Simulating SIS dynamics on a real social network
First, we need to download some real data. I suggest browsing Mark Newman's website for some cleaned-up networks: 
[http://www-personal.umich.edu/~mejn/netdata/](http://www-personal.umich.edu/~mejn/netdata/)

I have selected the Zachary's Karate Club social network. I can load this into R's `igraph` like this (**Note:** You must point to the correct location of the file on your computer!):

```r
## Load the igraph library
library('igraph')

## Load Karate Club dataset by pointing to it's location. Mine goes to my "Downloads" folder
G = read.graph("~/Downloads/karate/karate.gml", format = "gml")
```

I can take a quick look to make sure nothing is wrong by plotting the network.

```r
## Let's assign a layout and keep it, so the network doesn't move each time we plot it
myLayout = layout_with_kk(G)

## Make a quick plot
plot(G, 
  vertex.label = NA,            ## Suppress node labels
  vertex.size = 10,             ## Set node size to 10
  vertex.color = '#2f9253',     ## Set node color to this nice green
  edge.width = 0.5,             ## Set link width to half-size
  edge.color = '#000000',       ## Set link color to black
  layout = myLayout)            ## Place the nodes based on my fixed layout
```

This looks good.

Now I want to confirm a few things. I'll start by extracting the adjacency matrix from the igraph object:

```r
## Extract adjacency matrix
A = as.matrix(get.adjacency(G))
```

Note that I can extract the degree of each node from both the igraph object and the adjacency matrix:

```r
## Extract node degrees from igraph object
degrees = degree(G)

## Extract node degrees from the adjacency matrix
rowSums(A)
```

I can also compute the mean degree (z) in a few ways. Here's one...

```r
z = mean(degree(G))
```

Okay, now we're ready for some simulations. First we need to make a vector that will keep track of the states of our nodes.

```r
## Let's assign a random state to each node to start
nodeStates = runif(nrow(A))
nodeStates = ifelse(nodeStates < 0.35, 1, 0) ## This says: if the first statement is true (nodeStates < X) then put a 1, else put a 0

## Let's look at our initial state
plot(G, vertex.label = NA, vertex.size = 10, vertex.color = nodeStates,
     edge.width = 0.5, edge.color = '#000000',
     layout = myLayout)
```

Now we need a rule for updating our states in each time step. A common one is the SIS model we have already studied.

```r
## Update my states for -- one time step --
updateStates = function(A, nodeStates, p, r){
  ## A blank vector to fill in my next states
  newStates = rep(NA, length(nodeStates))
  ## Loop through each node in the network
  for(i in 1:nrow(A)){
    ## Infections
    if(nodeStates[i] == 0){
      neighbors      = which(A[i, ] == 1)
      neighborStates = nodeStates[neighbors]
      coinFlips      = runif(sum(neighborStates))
      infected       = any(coinFlips < p)
      newStates[i]   = ifelse(infected, 1, 0)
    } else{
      ## Recover at some rate r
      recovered      = runif(1) < r
      newStates[i]   = ifelse(recovered, 0, 1)
    }
  }
  ## Return my new states
  newStates
}
```

I can manually update the network by running this block of code a few times (try highlighting and evaluating repeatedly):

```r
## Update once
nodeStates = updateStates(A, nodeStates)

## Plot the update
plot(G, vertex.label = NA, vertex.size = 10, vertex.color = nodeStates,
     edge.width = 0.5, edge.color = '#000000', edge.arrow.size = 0.15,
     layout = myLayout)
```

So to simulate this model, I need to repeat this updating step many times. I can write a function to do this.

```r
## --------
## Let's repeat this simulation some number of
## times and extract meaningful summary statistics
## --------
simulateOneNetwork = function(A, initialFractionInfected, simulationTime){
  ## Let's assign a random state to each node to start
  nodeStates = runif(nrow(A))
  nodeStates = ifelse(nodeStates < initialFractionInfected, 1, 0)
  ## Update the states in time
  for(t in 1:simulationTime){
    ## Update once
    nodeStates = updateStates(A, nodeStates)
  }
  fractionInfectedNodes = mean(nodeStates)
  fractionInfectedNodes
}

## Test my function
simulateOneNetwork(A, initialFractionInfected = 0.5, simulationTime = 10)

## It works!
```

Now I need to wrap this all in a function that repeats many simulations.

```r
## First it is always good practice to allocate an empty
## array that you will fill in
nSimulations = 15
results = data.frame(
  'initialFraction' = rep(seq(0.1, 0.9, length.out = nSimulations), 10),
  'finalFraction'   = rep(NA))

## Fill in my results DF
for(r in 1:nrow(results)){
  results$finalFraction[r] = simulateOneNetwork(A, initialFractionInfected = results$initialFraction[r], simulationTime = 20)
}

## Quick plot of results
plot(finalFraction ~ initialFraction, data = results, cex = 0.5)

## This is nasty! Let's summarize the results as averages instead.
## Note: you will need to install 2 other data science libraries for this bit
library(dplyr)
library(magrittr)
summaryResults = results %>%
  group_by(initialFraction) %>%
  summarise('meanFinalFraction' = mean(finalFraction)) %>%
  as.data.frame()

## Plot the summary statistics
plot(meanFinalFraction ~ initialFraction, data = summaryResults, cex = 2)
```
