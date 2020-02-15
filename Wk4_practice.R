## clear the console
## click on the console and type ctrl-l
12345678901234567890123456789012345678901234567890123456789012345678901234567890

## clear the workspace
ls() # show workspace
rm(list=ls()) # clear workspace
ls() # show empty workspace


## ******************************** 
library(R.utils)
library(lubridate)
library(dplyr)
library(purrr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(RColorBrewer)


mu0 <- 30
mua <- 32
sigma <- 4
n <- 16
alpha <- .05
z <- qnorm(1 - alpha)

pnorm(mu0 + z * sigma/sqrt(n), 
      mean = mu0,  ## if we plug in mu0, we should get back alpha
      sd = sigma/sqrt(n), 
      lower.tail = FALSE
      )
## [1] 0.05

pnorm(mu0 + z * sigma/sqrt(n), 
      mean = mua, ## now plug in mua
      sd = sigma/sqrt(n), 
      lower.tail = FALSE
      )
## [1] 0.63876


library(manipulate)

mu0 <- 30
myplot <- function(sigma, mua, n, alpha) {
  g <- ggplot(data.frame(mu = c(27, 36)), aes(x = mu))
  g <- g + stat_function(fun = dnorm, 
                        geom = "line", 
                        args = list(mean = mu0, sd = sigma/sqrt(n)), 
                        size = 2, 
                        col = "red"
                        )
  g <- g + stat_function(fun = dnorm, 
                        geom = "line", 
                        args = list(mean = mua, sd = sigma/sqrt(n)), 
                        size = 2, 
                        col = "blue") 
  xitc <- mu0 + qnorm(1 - alpha) * sigma/sqrt(n)
  g <- g + geom_vline(xintercept = xitc, size = 3) 
  g
}

manipulate(
  myplot(sigma, mua, n, alpha), 
  sigma = slider(1, 10, step = 1, initial = 4),
  mua = slider(30, 35, step = 1, initial = 32), 
  n = slider(1, 50, step = 1, initial = 16), 
  alpha = slider(0.01, 0.1, step = 0.01, initial = 0.05)
  )


power.t.test(n = 16, 
             delta = 2/4, 
             sd = 1, 
             type = "one.sample", 
             alt = "one.sided"
             )$power
##  [1] 0.6040329

power.t.test(n = 16, 
             delta = 2, 
             sd = 4, 
             type = "one.sample", 
             alt = "one.sided"
             )$power
##  [1] 0.6040329

power.t.test(n = 16, 
             delta = 100, 
             sd = 200, 
             type = "one.sample", 
             alt = "one.sided"
             )$power
##  [1] 0.6040329

power.t.test(power = 0.8, 
             delta = 2/4, 
             sd = 1, 
             type = "one.sample", 
             alt = "one.sided"
             )$n
##  [1] 26.13751

power.t.test(power = 0.8, 
             delta = 2, 
             sd = 4, 
             type = "one.sample", 
             alt = "one.sided"
             )$n
##  [1] 26.13751

power.t.test(power = 0.8, 
             delta = 100, 
             sd = 200, 
             type = "one.sample", 
             alt = "one.sided"
             )$n
##  [1] 26.13751


set.seed(1010093) 

pValues <- rep(NA, 1000) 
for (i in 1:1000) {
  y <- rnorm(20)
  x <- rnorm(20)
  pValues[i] <- summary(lm(y ~ x))$coeff[2, 4]
}

# Controls false positive rate
sum(pValues < 0.05)

# Controls FWER
sum(p.adjust(pValues, method = "bonferroni") < 0.05)

# Controls FDR
sum(p.adjust(pValues, method = "BH") < 0.05)


set.seed(1010093) 

pValues <- rep(NA, 1000) 
for (i in 1:1000) {
  x <- rnorm(20)
  # First 500 beta=0, last 500 beta=2 
  if(i<=500){
    y <- rnorm(20) 
  }else{
    y<-rnorm(20,mean=2*x) 
  }
  pValues[i] <- summary(lm(y ~ x))$coeff[2, 4] 
}

trueStatus <- rep(c("zero", "not zero"), each = 500) 
table(pValues < 0.05, trueStatus)

# Controls FWER
table(p.adjust(pValues, method = "bonferroni") < 0.05, trueStatus)

# Controls FDR
table(p.adjust(pValues, method = "BH") < 0.05, trueStatus)

par(mfrow = c(1, 2))
plot(pValues, p.adjust(pValues, method = "bonferroni"), pch = 19) 
plot(pValues, p.adjust(pValues, method = "BH"), pch = 19)


library(UsingR)

data(father.son)
x <- father.son$sheight
n <- length(x)
theta <- median(x)

jk <- sapply(1:n, function(i) median(x[-i])) 
thetaBar <- mean(jk)
biasEst <- (n - 1) * (thetaBar - theta)
seEst <- sqrt((n - 1) * mean((jk - thetaBar)^2))
c(biasEst, seEst)

library(bootstrap)
temp <- jackknife(x, median) 
c(temp$jack.bias, temp$jack.se)


B <- 10000
resamples <- matrix(sample(x, n * B, replace = TRUE), B, n) 

medians <- apply(resamples, 1, median)
sd(medians)

quantile(medians, c(0.025, 0.975))
par(mfrow = c(1, 1))
hist(medians)

g = ggplot(data.frame(medians= medians), aes(x=medians))
g = g + geom_histogram(color="black", fill="lightblue", binwidth=0.05)
g

data(InsectSprays)
boxplot(count ~ spray, data = InsectSprays)


subdata <- InsectSprays[InsectSprays$spray %in% c("B", "C"), ] 
y <- subdata$count
group <- as.character(subdata$spray)

testStat <- function(w, g) mean(w[g == "B"]) - mean(w[g == "C"]) 

observedStat <- testStat(y, group)
permutations <- sapply(1:10000, function(i) testStat(y, sample(group))) 
observedStat

mean(permutations > observedStat)



