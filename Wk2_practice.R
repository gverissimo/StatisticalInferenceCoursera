## clear the console
## click on the console and type ctrl-l

## clear the workspace
ls() # show workspace
rm(list=ls()) # clear workspace
ls() # show empty workspace


## ******************************** 
library(R.utils)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)


numberOfSimulations <- 10000
n <- 10

## generate 1000 trials of 10 samples each and
## pulled from a normal distribution, ie - rnorm
str(rnorm(numberOfSimulations*n))
str(matrix(rnorm(numberOfSimulations*n), numberOfSimulations))

## calculate the mean of each of the 1000 trials (ie - rows)
par(mfrow=c(1,1))
trials <- matrix(rnorm(numberOfSimulations*n), numberOfSimulations)
means <- apply(trials, 1, mean)
plot(means, pch=20, col=rgb(.2,.5,1,alpha=0.15))

## calculate the standard deviation of the means of those 1000 trials
stdDev = sd(means)
stdDev
meanOfMeans <- mean(means)
abline(h=meanOfMeans, col="purple")
abline(h=meanOfMeans+stdDev, col="coral")
abline(h=meanOfMeans-stdDev, col="coral")

## now do the same for a uniform distribution, ie - runif()
par(mfrow=c(1,1))
trials <- matrix(runif(numberOfSimulations*n), numberOfSimulations)
means <- apply(trials, 1, mean)
plot(means, pch=20, col=rgb(.2,.5,1,alpha=0.15))

## calculate the standard deviation of the means of those 1000 trials
stdDev <- sd(means)
stdDev
meanOfMeans <- mean(means)
abline(h=meanOfMeans, col="purple")
abline(h=meanOfMeans+stdDev, col="coral")
abline(h=meanOfMeans-stdDev, col="coral")

## and again for Poisson distribution, ie - rpois()
par(mfrow=c(1,1))
trials <- matrix(rpois(numberOfSimulations*n, 4), numberOfSimulations)
means <- apply(trials, 1, mean)
plot(means, pch=20, col=rgb(.2,.5,1,alpha=0.15))

## calculate the standard deviation of the means of those 1000 trials
stdDev <- sd(means)
stdDev
meanOfMeans <- mean(means)
abline(h=meanOfMeans, col="purple")
abline(h=meanOfMeans+stdDev, col="coral")
abline(h=meanOfMeans-stdDev, col="coral")

## and one more time using coin tosses (ie - sample(0:1))
par(mfrow=c(1,1))
trials <- matrix(sample(0:1, numberOfSimulations*n, replace=TRUE), numberOfSimulations)
means <- apply(trials, 1, mean)
plot(means, pch=20, col=rgb(.2,.5,1,alpha=0.15))

## calculate the standard deviation of the means of those 1000 trials
stdDev <- sd(means)
stdDev
meanOfMeans <- mean(means)
abline(h=meanOfMeans, col="purple")
abline(h=meanOfMeans+stdDev, col="coral")
abline(h=meanOfMeans-stdDev, col="coral")

## ****************
library(UsingR)
data(father.son)

## father.son$fheight : father's height
## father.son$sheight : son's height
x <- father.son$sheight
n <- length(x) ; n
hist(x, freq = FALSE, breaks=20, col="cornflowerblue")

round( c(var(x), 
         var(x)/n, 
         sd(x), 
         sd(x)/sqrt(n)
         ), 
       2
       )


## ****************
choose(8,7) * 0.5^8 + choose(8,8) * 0.5^8

pbinom(6, size=8, prob=0.5, lower.tail=FALSE)

## ****************
pnorm(1160, mean=1120, sd=50, lower.tail=FALSE)

pnorm(2.8, lower.tail=FALSE)

pnorm(1160, mean=1120, sd=50, lower.tail=FALSE)

pnorm(2.8, lower.tail=FALSE)

## qnorm() : quantile function
qnorm(0.75, mean=1020, sd=50)

## ppois() : poisson distribution
ppois(3, lambda = 2.5 * 4)

## comparing poisson distribution for binomial where n is large and p is small
pbinom(2, size=500, prob=0.01)
ppois(2, lambda=500*0.01)

pbinom(2, size=1000, prob=0.005)
ppois(2, lambda=1000*0.005)

## look at cumulative mean drawing (1000x) from a normal distribution
n <- 1000
means <- cumsum(rnorm(n))/(1:n)
plot(means, pch=3, col=rgb(.2,.5,1,alpha=0.15))

## repeat simulating flipping a coin
##   note: sample(0:1, n, replace=TRUE) flips a fair coin (either 0 or 1
##         with equal probability) n times
n <- 1000
means <- cumsum(sample(0:1, n, replace=TRUE))/(1:n)
plot(means, pch=3, col=rgb(.2,.5,1,alpha=0.15))

library(UsingR)
data(father.son)
x <- father.son$sheight
(mean(x) + c(-1,1) * qnorm(0.975) * sd(x)/sqrt(length(x)))/12

round(1/sqrt(10^(1:6)), 3)

0.56 + c(-1, 1) * qnorm(0.975) * sqrt(0.56 * 0.44/100)

binom.test(56, 100)$conf.int

n <- 20
pvals <- seq(0.1, 0.9, by = 0.05)
numSim <- 100000
coverage <- sapply(pvals, function(p) {
  p_hats <- rbinom(numSim, prob=p, size=n)/n
  lowerLimit <- p_hats - qnorm(0.975) * sqrt(p_hats * (1 - p_hats)/n)
  upperLimit <- p_hats + qnorm(0.975) * sqrt(p_hats * (1 - p_hats)/n)
  mean(lowerLimit < p & upperLimit > p)
  } )
plot(coverage ~ pvals, 
     type="l",
     ylim=c(0.7, 1.0),
     col=rgb(.2,.5,1)
     )
abline(h=.95, col="violet")

n <- 100
pvals <- seq(0.1, 0.9, by = 0.05)
numSim <- 100000
coverage <- sapply(pvals, function(p) {
  p_hats <- rbinom(numSim, prob=p, size=n)/n
  lowerLimit <- p_hats - qnorm(0.975) * sqrt(p_hats * (1 - p_hats)/n)
  upperLimit <- p_hats + qnorm(0.975) * sqrt(p_hats * (1 - p_hats)/n)
  mean(lowerLimit < p & upperLimit > p)
} )
plot(coverage ~ pvals, 
     type="l", 
     ylim=c(0.7, 1.0),
     col=rgb(.2,.5,1)
     )
abline(h=.95, col="violet")

n <- 20
pvals <- seq(0.1, 0.9, by = 0.05)
numSim <- 100000
coverage <- sapply(pvals, function(p) {
  p_hats <- (rbinom(numSim, prob=p, size=n) + 2) / (n + 2 + 2)
  lowerLimit <- p_hats - qnorm(0.975) * sqrt(p_hats * (1 - p_hats)/n)
  upperLimit <- p_hats + qnorm(0.975) * sqrt(p_hats * (1 - p_hats)/n)
  mean(lowerLimit < p & upperLimit > p)
} )
plot(coverage ~ pvals, 
     type="l",
     ylim=c(0.7, 1.0),
     col=rgb(.2,.5,1)
     )
abline(h=.95, col="violet")

x <- 5
t <- 94.32
lambda <- x/t
round(lambda + c(-1, 1) * qnorm(0.975) * sqrt(lambda/t), 3)

poisson.test(x, T=94.32)$conf

lambdaVals <- seq(0.005, 0.1, by=0.01)
numSim <- 100000
t <- 100
coverage <- sapply(lambdaVals, function(lambda) {
  l_hats <- rpois(numSim, lambda = lambda*t)/t
  lowerLimit <- l_hats - qnorm(0.975) * sqrt(l_hats/t)
  upperLimit <- l_hats + qnorm(0.975) * sqrt(l_hats/t)
  mean(lowerLimit < lambda & upperLimit>lambda)
})
plot(coverage ~ lambdaVals, 
     type="l", 
     ylim=c(0.7, 1.0),
     col=rgb(.2,.5,1)
     )
abline(h=.95, col="violet")

lambdaVals <- seq(0.005, 0.1, by=0.01)
numSim <- 100000
t <- 1000
coverage <- sapply(lambdaVals, function(lambda) {
  l_hats <- rpois(numSim, lambda = lambda*t)/t
  lowerLimit <- l_hats - qnorm(0.975) * sqrt(l_hats/t)
  upperLimit <- l_hats + qnorm(0.975) * sqrt(l_hats/t)
  mean(lowerLimit < lambda & upperLimit>lambda)
})
plot(coverage ~ lambdaVals, 
     type="l", 
     ylim=c(0.7, 1.0),
     col=rgb(.2,.5,1)
)
abline(h=.95, col="violet")


