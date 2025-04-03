# A script to perform the diffusion map

Note: you will need to find some data to use, since the data we used in class is not available publicly. In it's current form, it generates a random dataset and then performs the analysis on it. Simply replace the dummy dataset with a real one to test it out!

First, load the functions that do the diffusion mapping
```r
## Install and load some libraries
if(!require(tidyverse)) install.packages('tidyverse'); library(tidyverse)
if(!require(magrittr)) install.packages('magrittr'); library(magrittr)
if(!require(reshape2)) install.packages('reshape2'); library(reshape2)
if(!require(spatstat)) install.packages('spatstat'); library(spatstat)
if(!require(RColorBrewer)) install.packages('RColorBrewer'); library(RColorBrewer)
if(!require(viridis)) install.packages('viridis'); library(viridis)
if(!require(parallelDist)) install.packages('parallelDist'); library(parallelDist)
if(!require(RSpectra)) install.packages('RSpectra'); library(RSpectra)

## Standardize columns
norm.mat <- function(mat){
  ms <- apply(mat, 2, function(col) (col - mean(col)) / sd(col))
  ms
}

## Compute euclid dist and standrdize to similarities
get.euc <- function(mat, n.threads = 1, alt = 'euclidean'){
  if(n.threads > 1){
    eu <- parDist(mat, method = alt, threads = n.threads) %>% as.matrix()
    ieu <- 1 / eu
    diag(ieu) <- 0
  }
  if(n.threads == 1){
    eu <- dist(mat, method = alt) %>% as.matrix()
    ieu <- 1 / eu
    diag(ieu) <- 0
  }
  ieu
}

## Function to threshold the normalized distance mat
threshold <- function(mat, top_k = 10){
  thr <- mat
  tnr <- nrow(thr)
  ## set similarities outside of the top k to 0
  for(i in 1:tnr){
    ## rank entries in each row in reverse order (so largest value == rank 1), 
    ## and set entries that are outside of 1:k to 0
    thr[i, !rank(-thr[i, ], ties.method = 'random') %in% 1:top_k] <- 0
  }
  for(i in 1:tnr){
    for(j in 1:tnr){
      if(thr[i, j] == 0 & thr[j, i] != 0){
        thr[i, j] <- thr[j, i]
      }
    }
  }
  thr
}

## function to calculate norm laplacian
get.laplac <- function(mat){
  L <- -mat
  S <- rowSums(mat)
  nL <- L / S
  diag(nL) <- 1
  nL
}
```

Once you've run that block, pass your data through each one like this:

```r
## Make a dummy data
m = matrix(rexp(200, rate = 0.1), ncol = 20)
rownames(m) = paste0('Sample_', 1:nrow(m))
colnames(m) = paste0('Variable_', 1:ncol(m))

## Perform each step of the diffusion map
nm <- m %>% norm.mat()

## Make affinity matrix for input
aff <- nm %>%
  get.euc(., n.threads = 1, alt = 'canberra') %>% 
  threshold(., top_k = 7)

## Make Laplacian
Lij <- aff %>% 
  get.laplac()

## Compute spectrum
eig <- eigen(Lij)

## Get eigenvalues
evl <- eig$values %>%
  Re() %>%
  round(., 10)
evl

## How many of the smallest eigenvalues do I want to keep?
keig <- 5 + 1

## Get eigenvectors
evc <- eig$vectors %>%
  Re() %>%
  round(., 10)

## Create mapping data array
for(d in 1:length(evl)){ assign(paste('dim', d, sep = '.'), Re(evc[, rank(evl, ties.method = 'random') == (d + 1)])) }

## Merge into a data array
meta <- do.call(mapply, c(FUN = cbind, mget(paste0("dim.", 1:(keig - 1))))) %>%
  t() %>%
  as.data.frame()

## Add labels
colnames(meta) <- paste(paste('dim', 1:(keig - 1), sep = '.'))
rownames(meta) <- rownames(Lij)

## Finalize the data
dph <- meta

##
## Plot function continuous
##
dph$plot.var <- dph$dim.1
ggplot(data = dph, aes(x = dim.1, 
                       y = dim.2,
                       fill = plot.var,
                       linewidth = abs(plot.var))
       ) +
  theme_minimal() +
  xlab('') +
  ylab('') +
  geom_hline(yintercept = 0, linewidth = 0.5, alpha = 1, linetype = 1, colour = '#bdbdbd') +
  geom_vline(xintercept = 0, linewidth = 0.5, alpha = 1, linetype = 1, colour = '#bdbdbd') +
  geom_point(shape = 21, 
             # fill = '#000000',
             size = 6,
             stroke = 0.5) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  scale_alpha_continuous(range = c(0.01, 1), limits = c(0, 1) * max(abs(dph$plot.var), na.rm = T), guide = F) +
  scale_size_continuous(range = c(0.1, 5), limits = c(0, 1) * max(abs(dph$plot.var), na.rm = T), guide = F) +
  scale_fill_gradientn(colours = brewer.pal(11, 'RdBu'),
                       limits = c(-1, 1) * max(abs(dph$plot.var), na.rm = T),
                       guide = 'none',
                       trans = 'identity')
```
