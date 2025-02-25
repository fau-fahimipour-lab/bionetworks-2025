# Model of a zombie outbreak in deer

The model we designed together in class on 2/25 is below.

```r
## Libraries for solving ODEs in R and plotting
## (note: install these once, if you haven't before!)
library(deSolve)
library(ggplot2)
library(reshape2)

## Define my model function here
zombie_model = function(t, state, parameters) {
  ## Default things for ODEs in R... (don't change!)
  with(as.list(c(state, parameters)), {
    ## Your equations go here
    dS = -b*S*Z + g*Z - a*S + h_S*G*S
    dZ = b*S*Z - d*Z - g*Z + h_Z*G*Z
    dG = r*G*(1 - G) - h_S*G*S - h_Z*G*Z
    ## Return the state variables
    list(c(dS, dZ, dG))
  })
}

## Define initial conditions (abundances at time 0)
## Note, these names need to match the variable names in your
## model function (S, Z, G)!
init = c(S = 0.01, Z = 0.01, G = 0.1)  

## Parameters
## Note, the parameter names have to match the names
## in your model function!
params = c(b = 0.25, 
           a = 0.25, 
           d = 0.25, 
           g = 0.05,
           r = 1,
           h_S = 0.5,
           h_Z = 0.1)

## Define the time sequence to simulate
times = seq(0, 250, by = 0.1)

## Solve the ODEs by passing all of this to deSolve
out = ode(y = init, times = times, func = zombie_model, parms = params, method = 'lsoda')

## Convert the simulation output to a data frame
out_df = as.data.frame(out)

## Flatten this data frame so it is in the right format for plotting
out_long = melt(out_df, id.vars = "time")

## Plot my time series, color each species a different color
ggplot(data = out_long, aes(x = time, y = value, group = variable, color = variable)) +
  theme_classic() +
  xlab('Time') +
  ylab('Biomass') +
  ylim(0, 2) +
  geom_path(linewidth = 2) +
  scale_color_brewer(palette = 'Dark2', direction = -1)
```
