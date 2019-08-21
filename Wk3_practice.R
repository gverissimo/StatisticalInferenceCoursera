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
library(reshape2)
library(ggplot2)
library(RColorBrewer)


library(manipulate)
data(sleep)
head(sleep)

k <- 1000
xvals <- seq(-5, 5, length = k) 
myplot <- function(df) { 
  d <- data.frame(y = c(dnorm(xvals), dt(xvals, df) ), 
                  x = xvals, 
                  dist = factor( rep(c("Normal", "T"), c(k,k) ) ) ) 
  g <- ggplot(d, aes(x = x, y = y))
  g <- g + geom_line(size = 2, aes(colour = dist))
  g 
}
manipulate(myplot(mu), mu = slider(1, 20, step = 1))


sp <- sqrt((7*15.34^2+20*18.23^2)/(8+21-2)) 
(132.86-127.44) + c(-1,1) * qt(0.975,27)*sp*(1/8 + 1/21)^0.5

data(sleep)
g1 <- sleep$extra[1 : 10]
g2 <- sleep$extra[11 : 20] 
n1 <- length(g1)
n2 <- length(g2)

sp <- sqrt( ((n1 - 1) * sd(x1)^2 + (n2-1) * sd(x2)^2) / (n1 + n2-2)) 
md <- mean(g2) - mean(g1)
semd<-sp*sqrt(1/n1+1/n2)
rbind(
  md + c(-1,1) * qt(0.975, n1+n2-2)*semd,
  t.test(g2, g1, paired = FALSE, var.equal = TRUE)$conf,
  t.test(g2, g1, paired = TRUE)$conf
)

g1 <- sleep$extra[1 : 10]
g2 <- sleep$extra[11 : 20] 
difference <- g2 - g1
mn <- mean(difference)
s <- sd(difference)
n <- 10

mn + c(-1, 1) * qt(.975, n-1) * s / sqrt(n)
t.test(difference)
t.test(g2, g1, paired = TRUE)
t.test(extra ~ I(relevel(group, 2)), paired = TRUE, data = sleep)


library(datasets)
data(ChickWeight)

##define weight gain or loss
wideCW <- dcast(ChickWeight, Diet + Chick ~ Time, value.var = "weight") 
names(wideCW)[-(1 : 2)] <- paste("time", names(wideCW)[-(1 : 2)], sep = "") 
wideCW <- mutate(wideCW, 
                 gain = time21 - time0
)

## Plotting the raw data
g <- ggplot(ChickWeight, 
            aes(x = Time, 
                y = weight,
                colour = Diet, 
                group = Chick
                )
            )
g <- g + geom_line()
g <- g + stat_summary(aes(group = 1), 
                      geom = "line", 
                      fun.y = mean, 
                      size = 1, 
                      col = "black"
                      )
g <- g + facet_grid(. ~ Diet)
g

## Weight gain by diet
g <- ggplot(wideCW, 
            aes(x = factor(Diet),
                y = gain, 
                fill = factor(Diet)
                )
            )
g <- g + geom_violin(col = "black", size = 2)
g

wideCW14 <- subset(wideCW, Diet %in% c(1, 4))
rbind(
  t.test(gain ~ Diet, paired = FALSE, var.equal = TRUE, data = wideCW14)$conf, 
  t.test(gain ~ Diet, paired = FALSE, var.equal = FALSE, data = wideCW14)$conf
)


wideCW14 <- subset(wideCW, Diet %in% c(1, 4)) 
t.test(gain ~ Diet, 
       paired = FALSE, 
       var.equal = TRUE, 
       data = wideCW14
       )

t.test(gain ~ Diet, 
       paired = FALSE, 
       var.equal = FALSE, 
       data = wideCW14
)
