# Centrality
The following code computes various centrality measures and plots them as colors on a network. You can change which measure is being plotted by tweaking line 21.

```r
## Load igraph
library(igraph)

## Make a random geometric graph
G  = grg.game(nodes = 200, radius = 0.2)
lo = layout_with_fr(G)

## Let's look at it
plot(G, vertex.label = NA, vertex.size = 8, vertex.color = '#f7f7f7', edge.width = 0.25, edge.color = '#000000', layout = lo)

## Centrality measures
dc = centr_degree(G)$res
cc = centr_clo(G)$res
bc = centr_betw(G)$res

# Set up resolution and palette.
my_resolution = 11
my_palette    = colorRampPalette(c('blue','red'))

# This gives you the colors you want for every point.
curr = bc
my_vector = curr / max(curr, na.rm = TRUE)
my_colors = my_palette(my_resolution)[as.numeric(cut(curr, breaks = my_resolution))]

## Let's look at it
plot(G, vertex.label = NA, vertex.size = 8, vertex.color = my_colors, edge.width = 0.25, edge.color = '#000000', layout = lo)
```
