require(ggplot2)

delta <- 0.01

x.mu1 <- 0.25
x.s1 <- 0.05
x.mu2 <- 0.5
x.s2 <- 0.02
x.mu3 <- 0.75
x.s3 <- 0.05

y.mu2 <- sqrt(0.33^2-0.167^2)
y.s2 <- 0.2

saw <- function(x, y) {
  n <- length(x)
  xmin <- x[1]
  xmax <- x[n]

  
  f <- function(t) {
    if (t<=xmin) {
      return(y[1])
    } else {
        if (t>= xmax) {
          return(y[n])
        } else {
            k <- which(x>= t)[1]
            a <- (y[k] - y[k-1])/(x[k]-x[k-1])
            b <- y[k] - a*x[k]
            return(a*t+b)
          }
      }
  }
  return(function(s) { sapply(s, f)})
}


sapply(t, function(t) {t+2})

x <- c(0, 0.5, 1)
y <- c(0, 1, 0)
tent <- saw(x, y)
t <- seq(-0.2,1,0.1)
t
tent(t)
plot(t, tent(t))

pulse.gen <- function() {
  x1 <- rnorm(1, x.mu1, x.s1)
  x2 <- max(rnorm(1, x.mu2, x.s2), x1+delta)
  x3 <- max(rnorm(1, x.mu3, x.s3), x2+delta)
  y2 <- max(rnorm(1, y.mu2, y.s2), delta)
  return(saw(c(x1,x2,x3), c(0,y2,0)))
}

whiteNoise.gen <- function(x, sigma) {
  y <- rnorm(length(x), 0, sigma)
  return(saw(x,y))
}

x <- seq(0,1,0.01)
Ns <- seq(0,2.9,0.1)
mx <- matrix(x)
for (N in Ns) {
  f.wn <- whiteNoise.gen(seq(0,1,0.02), 0.006)
  f.pulse <- pulse.gen()
  y <- f.pulse(x) + f.wn(x) + N
  mx <- cbind(mx,y)
}

gg <- ggplot(data=NULL, aes(x=x))
K <- dim(mx)[2]

for (k in 2:K) {
  gg <- gg+geom_line(aes(y=mx[,k]))
}

require(data.table)
colnames(mx) <- c("x", Ns)
dt <- data.table(mx)
require(reshape2)
dt2 <- melt(dt, id.vars="x")

ggplot(dt2, aes(x, value, by=variable)) + geom_line() +theme_bw()
 
