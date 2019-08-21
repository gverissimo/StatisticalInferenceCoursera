## clear the console
## click on the console and type ctrl-l

## clear the workspace
ls() # show workspace
rm(list=ls()) # clear workspace
ls() # show empty workspace


## ******************************** 
library(ggplot2)
library(manipulate)
library(UsingR)
library(reshape)

data(galton)

myHist <- function(mu){
  g <- ggplot(galton, aes(x = child))
  g <- g + geom_histogram(fill = "salmon", 
                          binwidth=1, 
                          aes(y = ..density..), 
                          colour = "black"
                          ) 
  g <- g + geom_density(size = 2)
  g <- g + geom_vline(xintercept = mu, size = 2)
  mse <- round(mean((galton$child - mu)^2), 3)
  g <- g + labs(title = paste('mu = ', mu, ' MSE = ', mse))
  g
}

manipulate(myHist(mu), 
           mu = slider(62, 74, step = 0.5)
           )

long <- melt(galton)

g <- ggplot(long, aes(x = value, fill = variable))
g <- g + geom_histogram(colour = "black", binwidth=1)
g <- g + facet_grid(. ~ variable)
g






